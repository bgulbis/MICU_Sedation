# exclude.R

source("0-library.R")

# locations ----
tmp.locations <- read_edw_data(exclude.dir, "locations") 

# determine the location history by hospital unit
tmp.locations <- tidy_data("locations", unit.data = tmp.locations)

# remove patients with more than one ICU admission
excl.mult.icu <- tmp.locations %>%
    group_by(pie.id) %>%
    filter(location == micu) %>%
    summarize(count.icu = n()) %>%
    filter(count.icu > 1)
    
pts.include <- anti_join(pts.eligible, excl.mult.icu, by = "pie.id") 

# transfers ----
excl.transfer <- read_edw_data(exclude.dir, "facility") %>%
    semi_join(pts.include, by = "pie.id") %>%
    filter(admit.type == "Direct") %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.transfer, by = "pie.id") 

# icu stay ----
ref.icu <- c("Hermann 3 Shock Trauma Intensive Care Unit", 
             "Hermann 3 Transplant Surgical ICU",
             "HVI Cardiac Care Unit",
             "HVI Cardiovascular Intensive Care Unit",
             "Jones 7 J Elective Neuro ICU",
             "Jones 8 W Burn Unit")

tmp.icu.arrive <- semi_join(tmp.locations, pts.include, by = "pie.id") %>%
    filter(location == micu) %>%
    rename(micu.arrive = arrive.datetime) %>%
    select(pie.id, micu.arrive)

# remove any patients who were in another icu before the micu
excl.icu <- semi_join(tmp.locations, pts.include, by = "pie.id") %>%
    left_join(tmp.icu.arrive, by = "pie.id") %>%
    filter(location %in% ref.icu,
           arrive.datetime < micu.arrive) %>%
    distinct(pie.id) 

pts.include <- anti_join(pts.include, excl.icu, by = "pie.id") 

# remove patients with MICU stay < 1 day
excl.los <- semi_join(tmp.locations, pts.include, by = "pie.id") %>%
    filter(location == micu,
           unit.length.stay < 1) %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.los, by = "pie.id")

# paralytics ----
excl.meds <- read_edw_data(exclude.dir, "paralytics", "meds_continuous") %>%
    inner_join(pts.include, by = "pie.id") %>%
    filter(med.rate.units != "") %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.meds, by = "pie.id")

# pregnancy ----

excl.preg <- read_edw_data(exclude.dir, "labs_preg", "labs") %>%
    semi_join(pts.include, by = "pie.id") %>%
    filter(lab.result == "Positive") %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.preg, by = "pie.id")

# comorbidities ----

tmp.diagnosis <- read_edw_data(exclude.dir, "diagnosis") %>%
    semi_join(pts.include, by = "pie.id") 
    
ref.excl.codes <- read_data(lookup.dir, "exclusion_codes.csv")

excl.diagnosis <- tidy_data("diagnosis", ref.data = ref.excl.codes, 
                            pt.data = tmp.diagnosis)

pts.include <- anti_join(pts.include, excl.diagnosis, by = "pie.id")

excl.nodiag <- anti_join(pts.include, tmp.diagnosis, by = "pie.id") %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.nodiag, by = "pie.id")

# mpp ----
excl.mpp <- read_edw_data(exclude.dir, "mpp") %>%
    semi_join(pts.include, by = "pie.id") %>%
    filter(str_detect(mpp, "Alcohol Withdrawal|Comfort Care|Hypothermia")) %>%
    distinct(pie.id)

pts.include <- anti_join(pts.include, excl.mpp, by = "pie.id")

# trach ----
ref.trach <- c("31.1", "31.21", "31.29", "31.72", "31.74", "97.37")

excl.proc <- read_edw_data(exclude.dir, "procedures") %>%
    semi_join(pts.include, by = "pie.id") %>%
    filter(proc.code %in% ref.trach) %>%
    distinct(pie.id)
   
pts.include <- anti_join(pts.include, excl.proc, by = "pie.id")

# vent ----

# check for patients who were on the vent outside of their MICU stay
excl.vent.icu <- tmp.locations %>%
    semi_join(pts.include, by = "pie.id") %>%
    filter(location == micu) %>%
    inner_join(tmp.vent, by = "pie.id") %>%
    mutate(admit.vent.stop = difftime(stop.datetime, arrive.datetime, 
                                      units = "hours"),
           depart.vent.start = difftime(start.datetime, depart.datetime, 
                                        units = "hours")) %>%
    filter(admit.vent.stop < 0 | depart.vent.start > 0)

pts.include <- anti_join(pts.include, excl.vent.icu, by = "pie.id")

include.pie <- concat_encounters(pts.include$pie.id, 750)
print(include.pie)
