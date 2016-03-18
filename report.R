# report.R

source("library.R")

library(tableone)
library(ReporteRs)
library(purrr)
library(broom)

# get data
# if (!exists("data.demograph")) data.demograph <- readRDS("Analysis/data_demograph.Rds")
# if (!exists("data.assessments")) data.assessments <- readRDS("Analysis/data_assessments.Rds")
# if (!exists("data.lfts")) data.lfts <- readRDS("Analysis/data_lfts.Rds")
# if (!exists("data.sedatives")) data.sedatives <- readRDS("Analysis/data_sedatives.Rds")

normal_test <- function(x) {
    y <- keep(x, is.numeric)
    map_df(y, possibly(shapiro.test, 
                       otherwise = list(statistic = NA, p.value = NA, 
                                        method = "Shapiro-Wilk normality test", 
                                        data.name = ".x[[i]]"))) %>%
        mutate(data.name = names(y)) %>%
        select(data.name, method, statistic, p.value)
}

result_table <- function(mydoc, test, table.title) {
    vars <- names(test)
    vars <- vars[!(vars %in% c("pie.id", "group"))]
    
    cont <- keep(test, is.numeric)
    contVars <- names(cont)
    
    cram <- keep(test, is.logical)
    cramVars <- names(cram)
    
    not.nrmlVars <- ""
    
    # if there are continuous variables, perform normality testing
    if (length(cont) > 0) {
        nrml <- normal_test(cont)
        
        not.nrml <- filter(nrml, p.value < 0.05)
        not.nrmlVars <- not.nrml$data.name
    }
    
    cat <- discard(test, is.numeric)
    catVars <- names(cat)
    
    tab <- CreateTableOne(vars, strata = "group", data = test, factorVars = catVars)
    tab1 <- print(tab, printToggle = FALSE, nonnormal = not.nrmlVars, cramVars = cramVars)
    
    mydoc <- addParagraph(mydoc, "")
    mydoc <- addTitle(mydoc, table.title, level = 3)
    mytable <- FlexTable(tab1, add.rownames = TRUE, header.cell.props = cellProperties(background.color = "#003366"), 
                         header.text.props = textBold(color = "white", font.family = "Calibri"))
    mytable[] <- textProperties(font.family = "Calibri", font.size = 10)
    mytable <- setZebraStyle(mytable, odd = "#eeeeee", even = "white")
    
    addFlexTable(mydoc, mytable)
}

project <- "Sedation with Benzodiazepines in MICU"
authors <- "Elizabeth Franco, Jen Cortes"
date <- format(Sys.Date(), "%B %d, %Y")

# make Word document
mydoc <- docx(template = "Templates/results_template.docx")
# styles(mydoc)
mydoc <- declareTitlesStyles(mydoc, stylenames = c("TitleDoc", "SubtitleCentered", "rTableLegend"))

mydoc <- addParagraph(mydoc, project, stylename = "TitleDoc", bookmark = "start")
mydoc <- addTitle(mydoc, authors, level = 2)
mydoc <- addTitle(mydoc, date, level = 2)

# demographics ----
mydoc <- result_table(mydoc, analyze.demograph, "Demographics")

# pmh ----
mydoc <- result_table(mydoc, analyze.pmh, "Past Medical History")

# home meds ----
mydoc <- result_table(mydoc, analyze.home.meds, "Home Medications")

# sedatives ----

sed <- analyze.sedatives %>% 
    select(med, group, time.wt.avg.rate:total.dose) 
    
vars <- names(sed)
vars <- vars[!(vars %in% c("med", "group"))]

test <- sed %>%
    select(-group) %>%
    slice_rows("med") %>%
    by_slice(normal_test)

# x <- NULL
for(i in 1:length(test$med)) {
    tbl <- filter(sed, med == test$med[[i]]) %>%
        select(-med)
    
    mydoc <- result_table(mydoc, tbl, paste0("Continuous Medications: ", as.character(test$med[[i]])))
}

ref <- pot("Data processed using ") + R.version.string + " on a " + 
    .Platform$OS.type + " " + .Platform$r_arch + " system."
prepby <- "Prepared by: Brian Gulbis"
citeTxt <- pot(citation())

mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, "Citation", stylename = "SectionTitle")
mydoc <- addParagraph(mydoc, prepby)
mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, ref)
mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, "To cite R in publications use:")
mydoc <- addParagraph(mydoc, citeTxt)

writeDoc(mydoc, file = "Analysis/results.docx")
