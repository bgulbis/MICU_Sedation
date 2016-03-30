# include.R

source("0-library.R")

# inclusion criteria ----
raw.vent <- read_edw_data(include.dir, "vent_start")

tmp.vent <- raw.vent %>%
    filter(!is.na(vent.datetime)) %>%
    group_by(pie.id) %>%
    arrange(vent.datetime) %>%
    mutate(diff.event = ifelse(is.na(lag(vent.event)) | vent.event != lag(vent.event), TRUE, FALSE),
           event.count = cumsum(diff.event)) %>%
    ungroup %>%
    group_by(pie.id, event.count) %>%
    summarize(event = first(vent.event),
              first.event.datetime = first(vent.datetime), 
              last.event.datetime = last(vent.datetime)) %>%
    ungroup %>%
    group_by(pie.id) %>%
    left_join(pts.screen, by = "pie.id") %>%
    mutate(stop.datetime = lead(last.event.datetime),
           vent.duration = ifelse(is.na(stop.datetime), difftime(discharge.datetime, first.event.datetime, units = "hours"), difftime(stop.datetime, first.event.datetime, units = "hours"))) %>%
    filter(event == "vent start time") %>%
    select(pie.id, start.datetime = first.event.datetime, stop.datetime, vent.duration) %>%
    ungroup


raw.vent.old <- list.files("Data-Orig", pattern="^vent", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event = factor(Clinical.Event),
              # vent.event = str_to_lower(Clinical.Event),
              # vent.datetime = mdy_hms(Clinical.Event.Date.Result))
              event.datetime = mdy_hms(Clinical.Event.Date.Result))

## remove patients with < 24 hours vent
## ?? cumulative vent time or continuous 24 hrs
## ?? should it be within a certain amount of time of icu admission
tmp.discharge <- pts.identified %>%
    select(pie.id, discharge.datetime) %>%
    distinct

tmp.vent.old <- raw.vent.old %>%
    filter(!is.na(event.datetime)) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    mutate(diff.event = ifelse(is.na(lag(event)) | event != lag(event), TRUE, FALSE),
           event.count = cumsum(diff.event)) %>%
    ungroup %>%
    group_by(pie.id, event.count) %>%
    summarize(event = first(event),
              first.event.datetime = first(event.datetime), 
              last.event.datetime = last(event.datetime)) %>%
    ungroup %>%
    group_by(pie.id) %>%
    left_join(tmp.discharge, by = "pie.id") %>%
    mutate(stop.datetime = lead(last.event.datetime),
           vent.duration = ifelse(is.na(stop.datetime), difftime(discharge.datetime, first.event.datetime, units = "hours"), difftime(stop.datetime, first.event.datetime, units = "hours"))) %>%
    filter(event == "Vent Start Time") %>%
    select(-event.count, -event, -last.event.datetime) %>%
    rename(start.datetime = first.event.datetime)

limit.vent <- tmp.vent %>%
    filter(vent.duration >= 24) %>%
    distinct(pie.id) 

limit.vent.old <- tmp.vent.old %>%
    filter(vent.duration >= 24) %>%
    distinct(pie.id) 

raw.meds.incl <- read_edw_data(include.dir, "meds_continuous")

## read in medication data and tidy variables
raw.meds.incl <- list.files(data.dir, pattern="^meds_include", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              admin.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              med = str_to_lower(Clinical.Event),
              dose = as.numeric(Clinical.Event.Result),
              dose.units = factor(Clinical.Event.Result.Units),
              rate = as.numeric(Infusion.Rate),
              rate.units = factor(Infusion.Rate.Unit),
              route = factor(Route.of.Administration...Short))

## must have one of these
# lorazepam, midazolam, dexmedetomidine, propofol, fentanyl, hydromorphone, ketamine
limit.meds <- filter(raw.meds.incl, str_detect(med, "lorazepam|midazolam|dexmedetomidine|propofol|fentanyl|hydromorphone|ketamine"),
                     rate.units != "") %>%
    select(pie.id) %>%
    distinct

# make a list of patients who are eligible to be screened
pts.eligible <- filter(pts.identified, pie.id %in% pts.screen$pie.id,
                       pie.id %in% limit.vent$pie.id,
                       pie.id %in% limit.meds$pie.id) %>%
    select(pie.id) %>%
    distinct
