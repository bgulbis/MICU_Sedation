# 
# tidy.R
# 
# tidy the data for analysis

source("0-library.R")

# # Get home medication data
# raw.home.meds <- list.files(data.dir, pattern="^home_meds", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               home.med = str_to_lower(Order.Catalog.Short.Description),
#               home.med.order = Order.Catalog.Mnemonic)
# 
# # Get lab data
# raw.labs <- list.files(data.dir, pattern="^labs[^_exclude]", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               lab = factor(Clinical.Event),
#               result = Clinical.Event.Result,
#               lab.datetime = mdy_hms(Clinical.Event.End.Date.Time))
# 
# # Get medication data
# raw.meds.cont <- list.files(data.dir, pattern="^cont_meds", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               med = Clinical.Event,
#               med.datetime = mdy_hms(Clinical.Event.End.Date.Time),
#               rate = as.numeric(Infusion.Rate),
#               rate.unit = factor(Infusion.Rate.Unit, exclude = ""),
#               event.id = Event.ID)
# 
# # Get medication data
# raw.meds.bolus <- list.files(data.dir, pattern="^bolus_meds", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               med = Clinical.Event,
#               med.datetime = mdy_hms(Clinical.Event.End.Date.Time),
#               dose = as.numeric(Clinical.Event.Result),
#               dose.unit = factor(Clinical.Event.Result.Units, exclude = ""),
#               route = factor(Route.of.Administration...Short, exclude = ""),
#               event.id = Event.ID)
# 
# # Get vitals data
# raw.vitals <- list.files(data.dir, pattern="^vitals", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               vital = factor(Clinical.Event),
#               result = as.numeric(Clinical.Event.Result),
#               vital.datetime = mdy_hms(Clinical.Event.End.Date.Time))
# 
# raw.labs.abg <- read_edw_data(data.dir, "labs_abg", "labs")
    
# patient id's ----
pts.ids <- read_edw_data(data.dir, "identifiers", "id")

# write_csv(pts.ids, paste0(export.dir, "patient_list.csv"))

# demographics ----
tidy.demographics <- read_edw_data(data.dir, "demographics") 

# add micu los to demographics
tmp.micu <- semi_join(tmp.micu, pts.include, by = "pie.id") %>%
    mutate(unit.length.stay = as.numeric(unit.length.stay)) 

data.demographics <- inner_join(tidy.demographics, 
                                select(tmp.micu, pie.id, unit.length.stay), 
                                by = "pie.id") %>%
    select(-(visit.type:facility))

rm(tidy.demographics)

# add vent duration to demographics
tmp.vent.duration <- semi_join(tmp.vent, pts.include, by = "pie.id") %>%
    group_by(pie.id) %>%
    summarize(vent.duration = sum(vent.duration))

data.demographics <- inner_join(data.demographics, tmp.vent.duration, 
                                by = "pie.id")

# add measurements to demographics
raw.measures <- read_edw_data(data.dir, "measures")

tmp.measure <- semi_join(raw.measures, pts.include, by = "pie.id") %>%
    filter(measure == "Weight",
           measure.units == "kg") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(weight = first(measure.result))

data.demographics <- inner_join(data.demographics, tmp.measure, by = "pie.id")

tmp.measure <- semi_join(raw.measures, pts.include, by = "pie.id") %>%
    filter(measure == "Height",
           measure.units == "cm") %>%
    group_by(pie.id) %>%
    arrange(measure.datetime) %>%
    summarize(height = first(measure.result))

data.demographics <- inner_join(data.demographics, tmp.measure, by = "pie.id")

rm(raw.measures, tmp.measure)

# PMH ----
ref.pmh.codes <- read_data(lookup.dir, "pmh_codes.csv")

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
# ast >75, alt >81
tmp.lfts.sum <- tmp.lfts %>%
    filter((lab == "AST" & result > 75) | (lab == "ALT" & result > 81)) %>%
    group_by(pie.id, lab) %>%
    summarize(num.high = n()) %>%
    spread(lab, num.high, fill = 0) %>%
    mutate_each(funs(ifelse(. > 0, TRUE, FALSE)), -pie.id) %>%
    full_join(select(pts.include, pie.id), by = "pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), -pie.id)
    
names(tmp.lfts.sum) <- str_to_lower(names(tmp.lfts.sum))

data.demograph <- inner_join(data.demograph, tmp.lfts.sum, by = "pie.id")

# APACHE-II --------------------------------------------------------------------
# most abnormal values in first 24 hours of ICU stay

labs.list <- c("creatinine lvl", "glasgow coma score", "pao2", "poc a ph", 
               "poc a po2", "poc a pco2", "potassium lvl", "sodium lvl", "wbc", 
               "hct", "fio2 (%)")

tmp.labs <- mutate(raw.labs, lab = str_to_lower(as.character(lab)))
tmp.abg <- anti_join(raw.labs.abg, tmp.labs, by = c("pie.id", "lab.datetime", "lab")) %>%
    rename(result = lab.result)
tmp.labs <- bind_rows(tmp.labs, tmp.abg) %>%
    mutate(lab = ifelse(lab == "poc a po2", "pao2", lab))

# tmp <- !is.na(str_extract(levels(tmp.labs$lab),"poc a po2"))
# levels(tmp.labs$lab)[tmp == TRUE] <- "pao2"

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
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "creatinine lvl", "scr")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "glasgow coma score", "gcs")
# names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "PaO2", "pao2")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "poc a ph", "ph")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "poc a pco2", "pco2")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "potassium lvl", "k")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "sodium lvl", "na")
# names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "WBC", "wbc")
# names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "Hct", "hct")
names(tmp.apache.labs) <- str_replace_all(names(tmp.apache.labs), "fio2 \\(%\\)", "fio2")

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

data.apache <- select(data.demograph, pie.id, age) %>%
    inner_join(tmp.apache.labs, by = "pie.id") %>%
    inner_join(tmp.apache.vitals, by = "pie.id")

data.apache <- data.apache[, order(colnames(data.apache))]

data.apache <- select(data.apache, pie.id, everything())

# sedatives --------------------------------------------------------------------

# calcualte duration of use, total continuous dose, total bolus dose for each
# med

# calculate total bolus dose administered
tmp.sedatives.bolus <- raw.meds.bolus %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    mutate(med = str_to_lower(med)) %>%
    filter(med.datetime >= arrival,
           med.datetime <= leave) %>%
    group_by(pie.id, med) %>%
    summarize(total.bolus.dose = sum(dose)) 

# use auc to summarize continuous dose
tmp.sedatives.cont <- raw.meds.cont %>%
    anti_join(raw.meds.bolus, by = "event.id") %>%
    inner_join(tmp.micu.admit, by = "pie.id") %>%
    filter(med.datetime >= arrival,
           med.datetime <= leave) %>%
    inner_join(select(data.demograph, pie.id, weight), by = "pie.id") %>%
    rowwise %>%
    mutate(rate = convert_units(med, rate.unit, rate, weight)) %>%
    ungroup %>%
    group_by(pie.id, med) %>%
    arrange(med.datetime) %>%
    select(pie.id:event.id)
    
tmp.sedatives.last <- tmp.sedatives.cont %>%
    summarize(last.datetime = last(med.datetime))

tmp.sedatives.cont <- tmp.sedatives.cont %>%
    inner_join(tmp.sedatives.last, by = c("pie.id", "med"))  %>%
    filter(med.datetime == last.datetime | !is.na(rate.unit)) %>%
    mutate(duration = as.numeric(difftime(lead(med.datetime), med.datetime, units = "hours")),
           run.time = as.numeric(difftime(med.datetime, first(med.datetime), units = "hours")))

# turn off scientific notation
options(scipen = 999)

tmp.sedatives.auc <- tmp.sedatives.cont %>%
    summarize(dose.auc = auc(run.time, rate)) %>%
    filter(dose.auc > 0)

# calculate duration as sum of time on drip
tmp.sedatives.time <- tmp.sedatives.cont %>%
    filter(rate > 0) %>%
    summarize(cont.duration = sum(duration, na.rm = TRUE)) %>%
    filter(cont.duration > 0)

data.sedatives <- select(data.demograph, pie.id, weight) %>%
    inner_join(tmp.sedatives.auc, by = "pie.id") %>%
    inner_join(tmp.sedatives.time, by = c("pie.id", "med")) %>%
    rowwise %>%
    mutate(time.wt.avg.rate = dose.auc / cont.duration,
           total.cont.dose = total_dose(med, dose.auc, weight)) %>%
    full_join(tmp.sedatives.bolus, by = c("pie.id", "med")) %>%
    mutate(total.dose = sum(c(total.cont.dose, total.bolus.dose), na.rm = TRUE),
           med = str_to_lower(med)) %>%
    ungroup %>%
    mutate(med = factor(med))

options(scipen = 0)

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

tmp.camicu <- data.assessments %>%
    group_by(pie.id) %>%
    summarize(cam.icu.num = sum(num.cam.icu.pos, na.rm = TRUE)) %>%
    mutate(cam.icu.pos = ifelse(cam.icu.num > 0, TRUE, FALSE)) %>%
    select(-cam.icu.num)

data.demograph <- inner_join(data.demograph, tmp.camicu, by = "pie.id")

# groups ----

# determine number of benzo patients
tmp <- data.sedatives %>%
    ungroup %>%
    filter(med == "midazolam" | str_to_lower(med) == "lorazepam",
           !is.na(cont.duration)) %>%
    group_by(pie.id) %>%
    select(pie.id) %>%
    distinct %>%
    mutate(bzd = TRUE)

data.demograph <- data.demograph %>%
    left_join(tmp, by = "pie.id") %>%
    mutate(bzd = ifelse(is.na(bzd), FALSE, bzd))

# export ----

# export.list <- ls(pattern = "data.")
# lapply(1:length(export.list), function(i) write.csv(export.list[[i]], 
#                                                      file = paste0("Export/", names(export.list[i]), ".csv"), row.names = FALSE))
# lapply(seq_along(export.list), function(i) print("Export/", names(export.list[i])))

# write.csv(data.apache, paste0(export.dir, "data_apache.csv"), row.names = FALSE)
# write.csv(data.assessments, paste0(export.dir, "data_assessments.csv"), row.names = FALSE)
# write.csv(data.demograph, paste0(export.dir, "data_demograph.csv"), row.names = FALSE)
# write.csv(data.home.meds, paste0(export.dir, "data_home_meds.csv"), row.names = FALSE)
# write.csv(data.home.meds.long, paste0(export.dir, "data_home_meds_list.csv"), row.names = FALSE)
# write.csv(data.labs.lfts.long, paste0(export.dir, "data_lfts_list.csv"), row.names = FALSE)
# write.csv(data.pmh, paste0(export.dir, "data_pmh.csv"), row.names = FALSE)
# write.csv(data.sedatives, paste0(export.dir, "data_sedatives.csv"), row.names = FALSE)

