# screen.R

source("0-library.R")

pts.identified <- read_edw_data(screen.dir, "patients")

pts.screen <- pts.identified %>%
    filter(discharge.datetime < mdy("9/1/2015"),
           age >= 18) %>%
    distinct(pie.id)

edw.pie <- concat_encounters(pts.screen$pie.id, 750)
print(edw.pie)
