# exclude.R

source("0-library.R")

# locations ----
raw.locations <- read_edw_data(exclude.dir, "locations")

# determine the location history by hospital unit
tmp.locations <- tidy_data("locations", unit.data = raw.locations)

# remove patients with more than one ICU admission
excl.mult.icu <- tmp.locations %>%
    group_by(pie.id) %>%
    filter(location == micu) %>%
    summarize(count.icu = n()) %>%
    filter(count.icu > 1)
    
pts.include <- anti_join(pts.eligible, excl.mult.icu, by = "pie.id") 

# transfers ----
raw.facility <- read_edw_data(exclude.dir, "facility")

excl.transfer <- inner_join(raw.facility, pts.include, by = "pie.id") %>%
    filter(admit.type == "Direct") 

pts.include <- anti_join(pts.include, excl.transfer, by = "pie.id") 

# icu stay ----
ref.icu <- c("Hermann 3 Shock Trauma Intensive Care Unit", 
             "Hermann 3 Transplant Surgical ICU",
             "HVI Cardiac Care Unit",
             "HVI Cardiovascular Intensive Care Unit",
             "Jones 7 J Elective Neuro ICU",
             "Jones 8 W Burn Unit")

tmp.icu.arrive <- inner_join(tmp.locations, pts.include, by = "pie.id") %>%
    filter(location == micu) %>%
    rename(micu.arrive = arrive.datetime) %>%
    select(pie.id, micu.arrive)

# remove any patients who were in another icu before the micu
excl.icu <- inner_join(tmp.locations, pts.include, by = "pie.id") %>%
    left_join(tmp.icu.arrive, by = "pie.id") %>%
    filter(location %in% ref.icu,
           arrive.datetime < micu.arrive) %>%
    distinct(pie.id) 

pts.include <- anti_join(pts.include, excl.icu, by = "pie.id") 

# remove patients with MICU stay < 1 day
excl.los <- inner_join(tmp.locations, pts.include, by = "pie.id") %>%
    filter(location == micu,
           unit.length.stay < 1)

pts.include <- anti_join(pts.include, excl.los, by = "pie.id")

# paralytics ----
raw.meds.excl <- read_edw_data(exclude.dir, "paralytics", "meds_continuous")

excl.meds <- filter(raw.meds.excl, med.rate.units != "") %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.meds, by = "pie.id")


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
