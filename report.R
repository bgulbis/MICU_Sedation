# report.R

source("library.R")

# create docx object with project title and authors
project <- "Sedation with Benzodiazepines in MICU"
authors <- "Elizabeth Franco, Jen Cortes"

mydoc <- result_docx(project, authors)

# add results tables
mydoc <- result_table(mydoc, analyze.demograph, "Demographics")
mydoc <- result_table(mydoc, analyze.pmh, "Past Medical History")
mydoc <- result_table(mydoc, analyze.home.meds, "Home Medications")

# add result table for each continuous agent
mydoc <- result_table2(mydoc, analyze.sedatives, "med", "Continuous Medications")

# add citation and write docx to Word
write_docx(mydoc, file = "Analysis/results.docx")
