# 
# tidy.R
# 
# tidy the data for analysis

source("library.R")

# raw data ---------------------------------------------------------------------

# Get demographics for included patients
raw.demograph <- list.files(data.dir, pattern="^demographics", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              age = as.numeric(Age..Years..Visit.),
              sex = factor(Sex),
              race = factor(Race, exclude = ""),
              los = as.numeric(LOS..Actual.),
              disposition = factor(Discharge.Disposition, exclude = ""),
              fin = Formatted.Financial.Nbr) 

# Get height and weight data
raw.height.weight <- list.files(data.dir, pattern="^ht_wt", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result),
              unit = factor(Clinical.Event.Result.Units),
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time))

# Get home medication data
raw.home.meds <- list.files(data.dir, pattern="^home_meds", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              home.med = str_to_lower(Order.Catalog.Short.Description),
              home.med.order = Order.Catalog.Mnemonic)

# Get lab data
raw.labs <- list.files(data.dir, pattern="^labs[^_exclude]", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              lab = factor(Clinical.Event),
              result = Clinical.Event.Result,
              lab.datetime = mdy_hms(Clinical.Event.End.Date.Time))

# Get medication data
raw.meds <- list.files(data.dir, pattern="^medications", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              med = Clinical.Event,
              med.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              dose = as.numeric(Clinical.Event.Result),
              dose.unit = factor(Clinical.Event.Result.Units, exclude = ""),
              rate = as.numeric(Infusion.Rate),
              rate.unit = factor(Infusion.Rate.Unit, exclude = ""),
              route = factor(Route.of.Administration...Short, exclude = ""),
              iv.event = factor(IV.Event.Desc, exclude = ""))

# Get vitals data
raw.vitals <- list.files(data.dir, pattern="^vitals", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              vital = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result),
              vital.datetime = mdy_hms(Clinical.Event.End.Date.Time))

# output patient list ----------------------------------------------------------
tmp <- raw.demograph %>%
    select(fin, age:disposition)

# write.csv(tmp, "patient_list.csv", row.names = FALSE)

# patient demographics ---------------------------------------------------------
data.demograph <- raw.demograph %>%
    select(-fin) 

# MICU LOS -----------------------------------------------------------------

tmp.micu.admit <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id,
           location == micu) %>%
    mutate(leave = get_depart(depart, calc.depart))

tmp.los <- tmp.micu.admit %>%
    select(pie.id, unit.los)

# vent duration ------------------------------------------------------------

tmp.vent.duration <- tmp.vent %>%
    filter(pie.id %in% pts.include$pie.id) %>%
    group_by(pie.id) %>%
    summarize(vent.duration = sum(vent.duration))

data.demograph <- data.demograph %>%
    inner_join(tmp.los, by = "pie.id") %>%
    inner_join(tmp.vent.duration, by = "pie.id")

# height and weight --------------------------------------------------------

tmp.weight <- raw.height.weight %>%
    filter(pie.id %in% pts.include$pie.id,
           event == "Weight",
           unit == "kg") %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    summarize(weight = first(result))

tmp.height <- raw.height.weight %>%
    filter(pie.id %in% pts.include$pie.id,
           event == "Height",
           unit == "cm") %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    summarize(height = first(result))

data.demograph <- data.demograph %>%
    inner_join(tmp.weight, by = "pie.id") %>%
    inner_join(tmp.height, by = "pie.id")

# PMH ----------------------------------------------------------------------

ref.pmh.codes <- read.csv("Lookup/pmh_codes.csv", colClasses = "character")

tmp.pmh.codes <- icd9_lookup(ref.pmh.codes) %>%
    ungroup %>%
    mutate(disease.state = factor(disease.state))

data.pmh <- raw.diagnosis %>%
    filter(pie.id %in% pts.include$pie.id,
           diag.type == "Final") %>%
    inner_join(tmp.pmh.codes, by = c("diag.code" = "icd9.code")) %>%
    group_by(pie.id, disease.state) %>%
    select(pie.id, disease.state) %>%
    distinct %>%
    mutate(value = TRUE) %>%
    spread(disease.state, value, fill = FALSE, drop = FALSE) %>%
    full_join(select(pts.include, pie.id), by = "pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), arf:seizure)
    
# home medications ---------------------------------------------------------

# get list of desired home medications by class
ref.home.meds <- read.csv("Lookup/home_meds.csv", colClasses = "character")

# there are some duplicate meds in anticonvulsants
tmp <- ref.home.meds %>%
    filter(med.class != "anticonvulsants")

# lookup the meds which are in all classes except anticonvulsants
tmp.meds.list <- med_lookup(tmp$med.class) 

tmp <- ref.home.meds %>%
    filter(med.class == "anticonvulsants")

# get meds in anticonvulsant class, remove any already in the med list
tmp.meds.seizure <- med_lookup(tmp$med.class) %>%
    filter(!(med.name %in% tmp.meds.list$med.name))

# add remaining anticonvulsant meds to the list
tmp.meds.list <- bind_rows(tmp.meds.list, tmp.meds.seizure) %>%
    mutate(med.name = str_to_lower(med.name))

ref.home.meds <- ref.home.meds %>%
    mutate(med.class = factor(med.class))

data.home.meds.long <- raw.home.meds %>%
    filter(pie.id %in% pts.include$pie.id,
           home.med %in% tmp.meds.list$med.name) %>%
    inner_join(tmp.meds.list, by = c("home.med" = "med.name")) %>%
    mutate(med.class = factor(med.class, levels = ref.home.meds$med.class))

tmp <- !is.na(str_extract(levels(data.home.meds.long$med.class),"combinations"))
levels(data.home.meds.long$med.class)[tmp == TRUE] <- "narcotic analgesics"

data.home.meds <- data.home.meds.long %>%
    group_by(pie.id, med.class) %>%
    select(pie.id, med.class) %>%
    distinct %>%
    mutate(value = TRUE) %>%
    spread(med.class, value, fill = FALSE, drop = FALSE) %>%
    full_join(select(pts.include, pie.id), by = "pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), -pie.id)

names(data.home.meds) <- make.names(names(data.home.meds))

# daily labs -------------------------------------------------------------------
# all AST / ALT values during ICU stay

tmp.lfts <- raw.labs %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(lab == "AST" | lab == "ALT",
           lab.datetime >= arrival,
           lab.datetime <= leave) %>%
    group_by(pie.id, lab) %>%
    arrange(lab.datetime) %>%
    select(pie.id:lab.datetime) %>%
    mutate(result = as.numeric(result)) %>%
    filter(!is.na(result))

data.labs.lfts.long <- tmp.lfts

# APACHE-II --------------------------------------------------------------------
# most abnormal values in first 24 hours of ICU stay

labs.list <- c("Creatinine Lvl", "Glasgow Coma Score", "PaO2", "POC A pH", 
               "POC A PO2", "Potassium Lvl", "Sodium Lvl", "WBC", "Hct", "FIO2 (%)")

tmp.labs <- raw.labs

tmp <- !is.na(str_extract(levels(tmp.labs$lab),"POC A PO2"))
levels(tmp.labs$lab)[tmp == TRUE] <- "PaO2"

# get min and max lab values in first 24 hours of ICU admission
tmp.apache.labs <- tmp.labs %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(lab %in% labs.list,
           lab.datetime >= arrival,
           lab.datetime <= arrival + days(1)) %>%
    group_by(pie.id, lab) %>%
    mutate(result = as.numeric(result)) %>%
    filter(!is.na(result)) %>%
    summarize(min.lab = min(result),
              max.lab = max(result))

tmp.min <- tmp.apache.labs %>%
    select(-max.lab) %>%
    spread(lab, min.lab)

tmp.max <- tmp.apache.labs %>%
    select(-min.lab) %>%
    spread(lab, max.lab)

tmp.apache.labs <- inner_join(tmp.min, tmp.max, by = "pie.id")

names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "\\.x", "\\.min")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "\\.y", "\\.max")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Creatinine Lvl", "scr")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Glasgow Coma Score", "gcs")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "PaO2", "pao2")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "POC A pH", "ph")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Potassium Lvl", "k")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Sodium Lvl", "na")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "WBC", "wbc")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Hct", "hct")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "FIO2 \\(%\\)", "fio2")

# combine all temperatures and MAPs
tmp.vitals <- raw.vitals

tmp <- !is.na(str_extract(levels(tmp.vitals$vital),"Temperature"))
levels(tmp.vitals$vital)[tmp == TRUE] <- "Temperature"

tmp <- !is.na(str_extract(levels(tmp.vitals$vital),"Mean Arterial Pressure"))
levels(tmp.vitals$vital)[tmp == TRUE] <- "Mean Arterial Pressure"

# get min and max vital values in first 24 hours of ICU admission
tmp.apache.vitals <- tmp.vitals %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(vital.datetime >= arrival,
           vital.datetime <= arrival + days(1)) %>%
    group_by(pie.id, vital) %>%
    filter(!is.na(result)) %>%
    summarize(min.vital = min(result),
              max.vital = max(result))

tmp.min <- tmp.apache.vitals %>%
    select(-max.vital) %>%
    spread(vital, min.vital)

tmp.max <- tmp.apache.vitals %>%
    select(-min.vital) %>%
    spread(vital, max.vital)

tmp.apache.vitals <- inner_join(tmp.min, tmp.max, by = "pie.id")

names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "\\.x", "\\.min")
names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "\\.y", "\\.max")
names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "Apical Heart Rate", "hr")
names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "Mean Arterial Pressure", "map")
names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "Respiratory Rate", "rr")
names(tmp.apache.vitals) <- str_replace_all(names(tmp.apache.vitals), "Temperature", "temp")

data.apache <- inner_join(tmp.apache.labs, tmp.apache.vitals, by = "pie.id")

data.apache <- data.apache[, order(colnames(data.apache))]

data.apache <- select(data.apache, pie.id, everything())

# sedatives --------------------------------------------------------------------

# calcualte duration of use, total continuous dose, total bolus dose for each
# med

sedatives.list <- c("dexmedetomidine", "propofol", "lorazepam", 
                    "midazolam", "fentanyl", "hydromorphone", "ketamine")

tmp.sedatives <- raw.meds %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    mutate(med = str_to_lower(med)) %>%
    filter(med %in% sedatives.list,
           med.datetime >= arrival,
           med.datetime <= leave) %>%
    select(pie.id:iv.event) %>%
    mutate(med = factor(med))

# calculate total bolus dose administered
tmp.sedatives.bolus <- tmp.sedatives %>%
    filter(is.na(rate.unit)) %>%
    group_by(pie.id, med, dose.unit) %>%
    summarize(total.bolus.dose = sum(dose)) %>%
    filter(total.bolus.dose > 0,
           dose.unit != "patch",
           dose.unit != "mL") %>%
    # rowwise %>%
    # mutate(total.bolus.dose = convert_ml(med, dose.unit, total.bolus.dose)) %>%
    ungroup %>%
    group_by(pie.id, med) %>%
    summarize(total.bolus.dose = sum(total.bolus.dose))

# use auc to summarize continuous dose
tmp.sedatives.cont <- tmp.sedatives %>%
    filter(!is.na(rate.unit)) %>%
    group_by(pie.id, med) %>%
    arrange(med.datetime) %>%
    mutate(duration = as.numeric(difftime(lead(med.datetime), med.datetime, units = "hours")),
           run.time = as.numeric(difftime(med.datetime, first(med.datetime), units = "hours")))

tmp.sedatives.auc <- tmp.sedatives.cont %>%
    summarize(total.cont.dose = auc(run.time, rate)) %>%
    filter(total.cont.dose > 0)

# calculate duration as sum of time on drip
tmp.sedatives.time <- tmp.sedatives.cont %>%
    filter(rate > 0) %>%
    summarize(total.cont.duration = sum(duration, na.rm = TRUE)) %>%
    filter(total.cont.duration > 0)

data.sedatives <- inner_join(tmp.sedatives.auc, tmp.sedatives.time, by = c("pie.id", "med")) %>%
    full_join(tmp.sedatives.bolus, by = c("pie.id", "med")) %>%
    inner_join(select(data.demograph, pie.id, weight), by = "pie.id") %>%
    rowwise %>%
    mutate(time.wt.avg.rate = total.cont.dose / total.cont.duration,
           total.dose = total_dose(med, total.cont.dose, total.cont.duration, weight) + total.bolus.dose)

# daily assessments ----

tmp.rass <- raw.labs %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(lab == "RASS Score",
           lab.datetime >= arrival,
           lab.datetime <= leave) %>%
    mutate(lab.date = floor_date(lab.datetime, unit = "day"),
           result = as.numeric(result)) %>%
    group_by(pie.id, lab.date) %>%
    summarize(min.rass = min(result),
              max.rass = max(result)) 

tmp.cam.icu <- raw.labs %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(lab == "(CAM-ICU) Interpretation",
           lab.datetime >= arrival,
           lab.datetime <= leave,
           result != "Unable to assess") %>%
    mutate(lab.date = floor_date(lab.datetime, unit = "day"),
           result = ifelse(result == "Positive", TRUE, FALSE)) %>%
    group_by(pie.id, lab.date) %>%
    summarize(num.cam.icu.pos = sum(result))

data.assessments <- full_join(tmp.rass, tmp.cam.icu, by = c("pie.id", "lab.date"))
