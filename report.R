# report.R

source("library.R")

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
    group_by(med) %>%
    summarize(count = n())

# x <- NULL
for(i in 1:length(test$med)) {
    tbl <- filter(sed, med == test$med[[i]]) %>%
        select(-med)
    
    mydoc <- result_table(mydoc, tbl, paste0("Continuous Medications: ", as.character(test$med[[i]])))
}

ref <- pot("Data processed using ") + R.version.string + " on a " + 
    .Platform$OS.type + " " + .Platform$r_arch + " system."
prepby <- "Prepared by: Brian Gulbis"
citeTxt <- pot(citation()$textVersion)

mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, "Citation", stylename = "SectionTitle")
mydoc <- addParagraph(mydoc, prepby)
mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, ref)
mydoc <- addParagraph(mydoc, "")
mydoc <- addParagraph(mydoc, "To cite R in publications, use:")
mydoc <- addParagraph(mydoc, citeTxt)

writeDoc(mydoc, file = "Analysis/results.docx")
