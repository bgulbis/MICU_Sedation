# 
# tidy.R
# 
# tidy the data for analysis

source("library.R")

# set the directory containing the data
data.dir <- paste(base.dir, "Data", sep = "")

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

# Get location data
raw.locations <- list.files(data.dir, pattern="^locations", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              location = factor(Person.Location...Nurse.Unit..To., exclude = ""),
              duration = as.numeric(Days.at.Location),
              arrival = mdy_hms(Location.Arrival.Date..amp..Time),
              depart = mdy_hms(Location.Depart.Date..amp..Time))

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

# get MICU admit / discharge date/time; some patients have two entries with a
# slightly different admit or discharge date, will use the earliest admit date
# and latest discharge date
tmp.micu <- raw.locations %>%
    filter(location == "Cullen 2 E Medical Intensive Care Unit") %>%
    select(-location) %>%
    group_by(pie.id) %>%
    summarize(arrival = min(arrival),
              depart = max(depart)) %>%
    mutate(icu.los = as.numeric(difftime(depart, arrival, units = "days")))

data.demograph <- inner_join(data.demograph, tmp.micu, by = "pie.id")

# get vent duration
tmp <- data.demograph %>%
    select(pie.id, arrival, depart)

tmp.vent <- raw.vent %>% 
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    mutate(duration = as.numeric(difftime(lead(event.datetime), event.datetime, units="hours")),
           vent = ifelse(event == "Vent Start Time" & lead(event) == "Vent Stop Time", TRUE, NA)) %>%
    inner_join(tmp, by = "pie.id") %>%
    # filter(event == "Vent Stop Time" & (event.datetime < arrival | event.datetime > depart))
    mutate(vent.before = ifelse(event.datetime < arrival, TRUE, FALSE),
           before.time = difftime(event.datetime, arrival, units = "hours"),
           vent.after = ifelse(event.datetime > depart, TRUE, FALSE),
           after.time = difftime(event.datetime, depart, units = "hours")) 
    # filter(vent.before == TRUE)
    
tmp <- tmp.vent %>%
    filter(before.time < -24) %>%
    select(pie.id) %>%
    distinct %>%
    inner_join(tmp.vent, by = "pie.id")

