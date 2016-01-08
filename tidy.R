# 
# tidy.R
# 
# tidy the data for analysis

source("library.R")

# set the directory containing the data
data.dir <- "Data"

# compress medication data files
gzip_files(data.dir)

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
              home.med = Order.Catalog.Mnemonic)

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

# output patient list
tmp <- raw.demograph %>%
    select(fin, age:disposition)

write.csv(tmp, "patient_list.csv", row.names = FALSE)

# patient demographics
data.demograph <- raw.demograph %>%
    select(-fin) 

# get MICU LOS

# get vent duration
