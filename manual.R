# manual.R

library(readxl)

raw.manual <- read_excel("Manual/manual_review.xlsx") %>%
    mutate(fin = as.character(fin)) %>%
    inner_join(select(raw.demograph, pie.id, fin), by = "fin") %>%
    select(pie.id, everything(), -(fin:disposition))

names(raw.manual) <- str_to_lower(make.names(names(raw.manual)))

raw.manual <- raw.manual %>%
    rename(smoking = smoking.,
           num.packs.day = number.of.packs.day,
           num.years.smk = number.of.years,
           pack.years = pak.year,
           organ.insuff = severe.organ.insufficiency.or.immunocompromised.,
           organ.insuff.type = if.yes) %>%
    mutate_each(funs(factor(str_to_lower(str_trim(.)), exclude = c("na", "nk"))), -starts_with("num"), -pack.years, -pie.id) %>%
    mutate_each(funs(as.numeric), starts_with("num"), pack.years)