# screen.R

source("0-library.R")

pts.identified <- read_edw_data(screen.dir, "patients")

pts.screen <- pts.identified %>%
    filter(discharge.datetime < mdy("9/1/2015"),
           age >= 18) %>%
    distinct(pie.id)

edw.pie <- concat_encounters(pts.screen$pie.id, 750)
print(edw.pie)

comp <- read_data("Screen-Orig", "micu") %>%
    mutate(discharge.datetime = ymd_hms(`Discharge Date & Time`),
           pie.id = as.character(`PowerInsight Encounter Id`))

comp2 <- comp %>%
    filter(discharge.datetime < mdy("9/1/2015")) %>%
    distinct(pie.id)

comp3 <- anti_join(pts.screen, comp2, by = "pie.id")
