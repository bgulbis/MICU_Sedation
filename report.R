# report.R

source("library.R")

library(tableone)
library(ReporteRs)
library(purrr)
library(broom)

# get data
if (!exists("data.demograph")) data.demograph <- readRDS("Analysis/data_demograph.Rds")
if (!exists("data.assessments")) data.assessments <- readRDS("Analysis/data_assessments.Rds")
if (!exists("data.lfts")) data.lfts <- readRDS("Analysis/data_lfts.Rds")
if (!exists("data.sedatives")) data.sedatives <- readRDS("Analysis/data_sedatives.Rds")

normal_test <- function(x) {
    y <- keep(x, is.numeric)
    map_df(y, possibly(shapiro.test, 
                       otherwise = list(statistic = NA, p.value = NA, 
                                        method = "Shapiro-Wilk normality test", 
                                        data.name = ".x[[i]]"))) %>%
        mutate(data.name = names(y)) %>%
        select(data.name, method, statistic, p.value)
}

project <- pot("Sedation with Benzodiazepines in MICU", 
               textProperties(font.weight = "bold", font.size = 14, font.family = "Calibri"))
authors <- pot("Elizabeth Franco, Jen Cortes", 
               textProperties(font.weight = "bold", font.size = 12, font.family = "Calibri"))
date <- pot(format(Sys.Date(), "%B %d, %Y"),
            textProperties(font.weight = "bold", font.size = 11, font.family = "Calibri"))

test <- data.demograph %>%
    mutate(group = ifelse(bzd == TRUE, "BZD", "No BZD")) %>%
    select(-pie.id, -bzd, -diagnosis)

vars <- names(test)
vars <- vars[vars != "group"]

cont <- keep(test, is.numeric)
contVars <- names(cont)

# normality testing
nrml <- normal_test(cont)

not.nrml <- filter(nrml, p.value < 0.05)
not.nrmlVars <- not.nrml$data.name

cat <- discard(test, is.numeric)
catVars <- names(cat)

tab <- CreateTableOne(vars, strata = "group", data = test, factorVars = catVars)
tab1 <- print(tab, printToggle = FALSE, nonnormal = not.nrmlVars)
# make Word document
mydoc <- docx(template = "Templates/results_template.docx")
# styles(mydoc)
mydoc <- declareTitlesStyles(mydoc, stylenames = c("Title", "Heading1", "Heading2"))

# title

mydoc <- addParagraph(mydoc, project, bookmark = "Start", par.properties = parProperties(text.align = "center"))
mydoc <- addParagraph(mydoc, authors, par.properties = parProperties(text.align = "center"))
mydoc <- addParagraph(mydoc, date, par.properties = parProperties(text.align = "center"))

# add table
mydoc <- addTitle(mydoc, "Results", level = 3)
mytable <- FlexTable(tab1, add.rownames = TRUE, header.cell.props = cellProperties(background.color = "#003366"), 
                     header.text.props = textBold(color = "white", font.family = "Calibri"))
mytable[] <- textProperties(font.family = "Calibri")
mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")

mydoc <- addFlexTable(mydoc, mytable)

# sedatives ----

sed <- data.sedatives %>% 
    mutate(group = ifelse(bzd == TRUE, "BZD", "No BZD")) %>%
    select(med, group, time.wt.avg.rate:total.dose) 
    
vars <- names(sed)
vars <- vars[!(vars %in% c("med", "group"))]

test <- sed %>%
    select(-group) %>%
    slice_rows("med") %>%
    by_slice(normal_test)

x <- NULL
for(i in 1:length(test$med)) {
    tbl <- filter(sed, med == test$med[[i]]) %>%
        select(-med)
    vars <- names(tbl)
    vars <- vars[vars != "group"]
    tbl1 <- CreateTableOne(vars, strata = "group", data = tbl)
    y <- matrix(c("", "", "", ""), ncol = 4, dimnames = list(as.character(test$med[[i]])))
    y <- rbind(y, print(tbl1, printToggle = FALSE, nonnormal = vars))
    x <- rbind(x, y)
}

mydoc <- addParagraph(mydoc, "")

mydoc <- addTitle(mydoc, "Sedatives", level = 3)
mytable <- FlexTable(x, add.rownames = TRUE, header.cell.props = cellProperties(background.color = "#003366"), 
                     header.text.props = textBold(color = "white", font.family = "Calibri"))
mytable[] <- textProperties(font.family = "Calibri")
mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")

mydoc <- addFlexTable(mydoc, mytable)

writeDoc(mydoc, file = "Analysis/results.docx")
