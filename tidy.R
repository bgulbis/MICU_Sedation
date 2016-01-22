# 
# tidy.R
# 
# tidy the data for analysis

source("library.R")

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

# get MICU LOS -----------------------------------------------------------------

tmp.micu.admit <- tmp.locations %>%
    filter(pie.id %in% pts.include$pie.id,
           location == micu)

tmp.los <- tmp.micu.admit %>%
    select(pie.id, unit.los)

# get vent duration ------------------------------------------------------------

tmp.vent.duration <- tmp.vent %>%
    filter(pie.id %in% pts.include$pie.id) %>%
    group_by(pie.id) %>%
    summarize(vent.duration = sum(vent.duration))

data.demograph <- data.demograph %>%
    inner_join(tmp.los, by = "pie.id") %>%
    inner_join(tmp.vent.duration, by = "pie.id")

# get height and weight --------------------------------------------------------

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

# get PMH ----------------------------------------------------------------------

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
    
# get home medications ---------------------------------------------------------

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
    spread(med.class, value, fill = FALSE, drop = FALSE)
    