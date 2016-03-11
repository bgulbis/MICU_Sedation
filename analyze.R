# analyze.R

source("library.R")

# save data to files for use in analysis
tmp <- select(data.manual, -starts_with("organ"))

data.demograph <- filter(data.demograph, pie.id %in% pts.include) %>%
    inner_join(data.pmh, by = "pie.id") %>%
    inner_join(tmp, by = "pie.id") %>%
    inner_join(data.home.meds, by = "pie.id")
    
saveRDS(data.demograph, paste(analysis.dir, "data_demograph.Rds", sep="/"))
write.csv(data.demograph, paste0(export.dir, "data_demograph.csv"), row.names = FALSE)

data.assessments <- filter(data.assessments, pie.id %in% pts.include)
saveRDS(data.assessments, paste(analysis.dir, "data_assessments.Rds", sep="/"))
write.csv(data.assessments, paste0(export.dir, "data_assessments.csv"), row.names = FALSE)

data.lfts <- filter(data.labs.lfts.long, pie.id %in% pts.include)
saveRDS(data.lfts, paste(analysis.dir, "data_lfts.Rds", sep="/"))
write.csv(data.lfts, paste0(export.dir, "data_lfts.csv"), row.names = FALSE)

data.sedatives <- filter(data.sedatives, pie.id %in% pts.include)
saveRDS(data.sedatives, paste(analysis.dir, "data_sedatives.Rds", sep="/"))
write.csv(data.sedatives, paste0(export.dir, "data_sedatives.csv"), row.names = FALSE)

tmp <- select(data.manual, pie.id, starts_with("organ"))

data.apache <- filter(data.apache, pie.id %in% pts.include) %>%
    inner_join(tmp, by = "pie.id")
saveRDS(data.apache, paste(analysis.dir, "data_apache.Rds", sep="/"))
write.csv(data.apache, paste0(export.dir, "data_apache.csv"), row.names = FALSE)

