# manual.R

source("library.R")
library(readxl)

# raw.manual <- read.csv("Manual/manual_review.csv")
raw.manual <- read_excel("Manual/manual_review.xlsx") %>%
    filter(!is.na(fin)) %>%
    mutate(fin = as.character(fin),
           exclude = ifelse(!is.na(exclude), TRUE, FALSE)) %>%
    inner_join(select(raw.demograph, pie.id, fin), by = "fin") %>%
    select(pie.id, everything(), -(fin:disposition))

names(raw.manual) <- str_to_lower(make.names(names(raw.manual)))

tmp.manual <- raw.manual %>%
    rename(smoking = smoking.,
           num.packs.day = number.of.packs.day,
           num.years.smk = number.of.years,
           pack.years = pak.year,
           organ.insuff = severe.organ.insufficiency.or.immunocompromised.,
           organ.insuff.type = if.yes) %>%
    mutate(organ.insuff.type = ifelse(str_to_lower(organ.insuff.type) == "no", NA, organ.insuff.type)) %>%
    mutate_each(funs(factor(str_to_lower(str_trim(.)), exclude = c("na", "nk", NA))), -starts_with("num"), -pack.years, -pie.id, -diagnosis) %>%
    mutate_each(funs(as.numeric), starts_with("num"), pack.years) %>%
    mutate_each(funs(ifelse(. == "yes", TRUE, (ifelse(. == "no", FALSE, NA)))), alcohol.use, illicit.drug.use, arf, organ.insuff) %>%
    mutate(exclude = as.logical(exclude))

data.manual <- filter(tmp.manual, exclude == FALSE) %>%
    select(-exclude, -arf)

# final list of included patients
pts.include.man <- data.manual$pie.id

analyze.demograph <- data.demograph %>%
    filter(pie.id %in% pts.include.man) %>%
    mutate(group = factor(ifelse(bzd == TRUE, "BZD", "No BZD"))) %>%
    inner_join(select(data.manual, pie.id, diagnosis.categories), by = "pie.id") %>%
    select(pie.id, group, everything(), -bzd)
    
# make table with patients and group to bind to other data tables
data.include <- select(analyze.demograph, pie.id, group)

analyze.pmh <- inner_join(data.include, data.pmh, by = "pie.id") %>%
    inner_join(select(data.manual, pie.id, -starts_with("diagnosis"), -starts_with("organ")), by = "pie.id")
    
analyze.home.meds <- inner_join(data.include, data.home.meds, by = "pie.id")

analyze.apache <- inner_join(data.include, data.apache, by = "pie.id") %>%
    inner_join(select(data.manual, pie.id, starts_with("organ")), by = "pie.id")

analyze.sedatives <- inner_join(data.include, data.sedatives, by = "pie.id")
