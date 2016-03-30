# exclude.R

source("0-library.R")

# locations ----
raw.locations <- read_edw_data(exclude.dir, "locations")

tmp.locations <- raw.locations %>%
    group_by(pie.id) %>%
    arrange(arrive.datetime) %>%
    mutate(diff.unit = ifelse(is.na(unit.to) | is.na(lag(unit.to)) | 
                                  unit.to != lag(unit.to), TRUE, FALSE),
           unit.count = cumsum(diff.unit)) %>%
    group_by(pie.id, unit.count) %>%
    summarize(location = first(unit.to),
              arrive.datetime = first(arrive.datetime),
              depart.recorded = last(depart.datetime)) %>%
    mutate(depart.calculated = lead(arrive.datetime)) %>%
    rowwise %>%
    mutate(depart.datetime = get_depart(depart.recorded, depart.calculated),
           unit.length.stay = difftime(depart.datetime, arrive.datetime, units = "days")) %>%
    select(-depart.recorded, -depart.calculated) %>%
    ungroup 

## remove patients with more than one ICU admission
# combines multiple rows of data when the patient didn't leave ICU
excl.mult.icu <- tmp.locations %>%
    filter(location == micu) %>%
    summarize(count.icu = n()) %>%
    filter(count.icu > 1)
    
pts.include <- pts.identified %>%
    filter(pie.id %in% pts.eligible$pie.id,
           !(pie.id %in% excl.mult.icu$pie.id)) %>%
    group_by(pie.id) %>%
    summarize(admit.type = first(admit.type),
              discharge.datetime = first(discharge.datetime))

## exclude patients who came from another facility
excl.transfer <- pts.include %>%
    filter(admit.type == "Direct") %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.transfer$pie.id))

## exclude patients who came from another ICU
tmp.icu <- c("Hermann 3 Shock Trauma Intensive Care Unit", 
             "Hermann 3 Transplant Surgical ICU",
             "HVI Cardiac Care Unit",
             "HVI Cardiovascular Intensive Care Unit",
             "Jones 7 J Elective Neuro ICU",
             "Jones 8 W Burn Unit")

# excl.icu <- pts.include %>%
#     filter(unit.from %in% tmp.icu) %>%
#     select(pie.id) %>%
#     distinct
tmp.icu.arrive <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id,
           location == "Cullen 2 E Medical Intensive Care Unit") %>%
    rename(micu.arrive = arrival) %>%
    select(pie.id, micu.arrive)

excl.icu <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id) %>%
    left_join(tmp.icu.arrive, by = "pie.id") %>%
    filter(location %in% tmp.icu,
           arrival < micu.arrive) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.icu$pie.id))

## remove patients with MICU stay < 1 day
excl.los <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id,
           location == micu,
           unit.los < 1) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.los$pie.id))

## exclude if any of these: cisatracurium, rocuronium, or vecuronium
excl.meds <- filter(raw.meds.incl, pie.id %in% pts.include$pie.id,
                    str_detect(med, "cisatracurium|rocuronium|vecuronium"),
                    rate.units != "") %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.meds$pie.id))

## labs which were included in query
levels.labs <- c("ur.preg", "ser.preg")

## read in lab data
raw.labs.excl <- list.files(data.dir, pattern="^labs_exclude", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              lab.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              lab = factor(Clinical.Event),
              result = Clinical.Event.Result) %>%
    mutate(result = str_replace(result, "&gt;", ""),
           result = str_replace(result, "&lt;", ""),
           lab = str_to_lower(lab),
           lab = str_replace(lab, "u preg", "ur.preg"),
           lab = str_replace(lab, "s preg", "ser.preg"),
#            lab = str_replace(lab, "creatinine lvl", "scr"),
#            lab = str_replace(lab, "bili total", "t.bili"),
           lab = factor(lab, levels=levels.labs))

## check for pregnancy by lab test
excl.preg <- filter(raw.labs.excl, pie.id %in% pts.include$pie.id,
                    lab == "ur.preg" | lab == "ser.preg",
                    result == "Positive") %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.preg$pie.id))

## read in diagnosis codes
raw.diagnosis <- list.files(data.dir, pattern="^diagnosis", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              diag.code = ICD9.Diagnosis.Code,
              diag.type = factor(Diagnosis.Type),
              diag.primary = factor(Diagnosis.Code.Sequence))

## read in exclusion codes
ref.excl.codes <- read.csv("Lookup/exclusion_codes.csv", colClasses = "character")

# ## read in ICD9-CCS codes
# ref.ccs.diag <- read.csv("Lookup/icd9_ccs_diagnosis.csv", colClasses="character") %>%
#     transmute(ccs.code = as.numeric(CCS.CATEGORY),
#               icd9.code = ICD9.CODE.FRMT) 
# 
# ## find the ICD9 codes for the desired exclusions by CCS code
# tmp.ccs <- filter(ref.excl.codes, type == "CCS") %>% 
#     mutate(ccs.code = as.numeric(code)) %>%
#     inner_join(ref.ccs.diag, by="ccs.code")
# 
# ## ICD9 codes for non-CCS code exclusions
# tmp.icd9 <- filter(ref.excl.codes, type=="ICD9") %>% 
#     mutate(icd9.code = code) %>%
#     inner_join(ref.ccs.diag, by="icd9.code")
# 
# ## create one table with all ICD9 codes that should be excluded
# tmp.excl.icd9 <- bind_rows(tmp.ccs, tmp.icd9) %>%
#     select(disease.state, icd9.code) %>%
#     group_by(disease.state)

tmp.excl.icd9 <- icd9_lookup(ref.excl.codes)


## check for exclusions diagnosis: pregnant, alcohol withdrawal, cardiac arrest,
## status epilepticus
excl.diag <- filter(raw.diagnosis, pie.id %in% pts.include$pie.id,
                    diag.code %in% tmp.excl.icd9$icd9.code) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.diag$pie.id))

## remove patients without any diagnosis codes
excl.nodiag <- filter(pts.include, !(pie.id %in% raw.diagnosis$pie.id)) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.nodiag$pie.id))

## read in MPP data
raw.mpp <- list.files(data.dir, pattern="^mpp", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              mpp = factor(MPP..which.generated.order., exclude="")) 

## remove patients with alcohol withdrawal, comfort care, or hypothermia MPP
excl.mpp <- filter(raw.mpp, pie.id %in% pts.include$pie.id,
                   str_detect(mpp, "Alcohol Withdrawal|Comfort Care|Hypothermia")) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.mpp$pie.id))

## read in procedure data to look for tracheostomy
raw.procs <- list.files(data.dir, pattern="^procedure", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              proc.code = ICD9.Procedure.Code,
              proc.date = mdy(Procedure.Date))

## trach procedure codes
trach <- c("31.1", "31.21", "31.29", "31.72", "31.74", "97.37")

excl.proc <- filter(raw.procs, pie.id %in% pts.include$pie.id,
                    proc.code %in% trach) %>%
    select(pie.id) %>%
    distinct

pts.include <- filter(pts.include, !(pie.id %in% excl.proc$pie.id))

# check for patients who were on the vent outside of their MICU stay
excl.vent.icu <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id,
           location == micu) %>%
    inner_join(tmp.vent, by = "pie.id") %>%
    mutate(admit.vent.stop = difftime(stop.datetime, arrival, units = "hours"),
           depart.vent.start = ifelse(is.na(calc.depart), difftime(start.datetime, depart, units = "hours"), difftime(start.datetime, calc.depart, units = "hours"))) %>%
    filter(admit.vent.stop < 0 | depart.vent.start > 0)

pts.include <- filter(pts.include, !(pie.id %in% excl.vent.icu$pie.id))

## split the patients up into groups
edw.pie <- split(pts.include$pie.id, ceiling(seq_along(pts.include$pie.id)/500))
## combine the id's in each group into a string, separated by semi-colon
edw.pie <- lapply(edw.pie, str_c, collapse=";")

print(edw.pie)
