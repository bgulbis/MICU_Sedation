# include.R

source("0-library.R")

# vent inclusion criteria ----
raw.vent <- read_edw_data(include.dir, "vent_start")

tmp.vent <- raw.vent %>%
    filter(!is.na(vent.datetime)) %>%
    group_by(pie.id) %>%
    arrange(vent.datetime) %>%
    mutate(diff.event = ifelse(is.na(lag(vent.event)) | 
                                   vent.event != lag(vent.event), TRUE, FALSE),
           event.count = cumsum(diff.event)) %>%
    group_by(pie.id, event.count) %>%
    summarize(event = first(vent.event),
              first.event.datetime = first(vent.datetime), 
              last.event.datetime = last(vent.datetime)) %>%
    group_by(pie.id) %>%
    left_join(pts.screen, by = "pie.id") %>%
    mutate(stop.datetime = lead(last.event.datetime),
           vent.duration = ifelse(is.na(stop.datetime), 
                                  difftime(discharge.datetime, 
                                           first.event.datetime, 
                                           units = "hours"), 
                                  difftime(stop.datetime, 
                                           first.event.datetime, 
                                           units = "hours"))) %>%
    filter(event == "vent start time") %>%
    select(pie.id, start.datetime = first.event.datetime, stop.datetime, 
           vent.duration) %>%
    ungroup

limit.vent <- tmp.vent %>%
    filter(vent.duration >= 24) %>%
    distinct(pie.id) 

rm(raw.vent)

# med inclusion criteria ----
limit.meds <- read_edw_data(include.dir, "meds_continuous") %>%
    filter(med.rate.units != "") %>%
    distinct(pie.id)

# eligible patients ----
pts.eligible <- semi_join(pts.screen, limit.vent, by = "pie.id") %>%
    semi_join(limit.meds, by = "pie.id") 

eligible.pie <- concat_encounters(pts.eligible$pie.id, 750)
print(eligible.pie)
