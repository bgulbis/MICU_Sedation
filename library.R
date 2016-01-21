##
## library.R
##
## source file for all libraries needed for data tidying and analysis
## 
library(dplyr)
library(stringr)
library(lubridate)
library(BGTools)
library(tidyr)

# set the location name for MICU
micu <- "Cullen 2 E Medical Intensive Care Unit"

# set the directory containing the data
data.dir <- "Data"

# compress medication data files
gzip_files(data.dir)
