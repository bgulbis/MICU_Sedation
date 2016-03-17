# report.R

source("library.R")

library(tableone)
library(ReporteRs)

# get data
if (!exists("data.demograph")) data.demograph <- readRDS("Analysis/data_demograph.Rds")
if (!exists("data.assessments")) data.assessments <- readRDS("Analysis/data_assessments.Rds")
if (!exists("data.lfts")) data.lfts <- readRDS("Analysis/data_lfts.Rds")
if (!exists("data.sedatives")) data.sedatives <- readRDS("Analysis/data_sedatives.Rds")

test <- data.demograph %>%
    mutate(group = ifelse(bzd == TRUE, "BZD", "No BZD")) %>%
    select(-pie.id, -bzd, -diagnosis)

vars <- names(test)

cont <- keep(test, is.numeric)
contVars <- names(cont)

cat <- discard(test, is.numeric)
catVars <- names(cat)

tab <- CreateTableOne(vars, strata = "group", data = test, factorVars = catVars)
tab1 <- print(tab)
# make Word document
mydoc <- docx()

# add table
mydoc <- addTitle(mydoc, "Results", level = 3)
mytable <- FlexTable(tab1, add.rownames = TRUE, header.cell.props = cellProperties(background.color = "#003366"), 
                     header.text.props = textBold(color = "white", font.family = "Calibri"))
mytable[] <- textProperties(font.family = "Calibri")
mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")

mydoc <- addFlexTable(mydoc, mytable)

# continous data
mydoc <- addTitle(mydoc, "Detailed Continuous Data", level = 3)
mytable <- FlexTable(print(summary(tab$ContTable)), add.rownames = TRUE, header.cell.props = cellProperties(background.color = "#003366"), 
                     header.text.props = textBold(color = "white", font.family = "Calibri"))
mytable[] <- textProperties(font.family = "Calibri")
mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")

mydoc <- addFlexTable(mydoc, mytable)


writeDoc(mydoc, file = "Analysis/results.docx")
# docx( ) %>% 
#     addFlexTable( mtcars %>%
#                       FlexTable( header.cell.props = cellProperties( background.color =  "#003366" ),
#                                  header.text.props = textBold( color = "white" ),
#                                  add.rownames = TRUE ) %>%
#                       setZebraStyle( odd = "#DDDDDD", even = "#FFFFFF" ) ) %>%
#     writeDoc( file = "exemple.docx" )


# mydoc <- addTitle(mydoc, "Letter of Intent", level = 3)
# mytable <- vanilla.table(loi.table)
# mytable[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
# mytable[, to = "header"] <- parCenter()
# mytable[] <- textProperties(font.size = 8, font.family = "Calibri")
# mytable <- setFlexTableWidths(mytable, widths = c(1.5, 0.75, 5.25))
# mytable[, 2] <- parCenter()
# mytable[, 3] <- parLeft()
# mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
# mydoc <- addFlexTable(mydoc, mytable)
