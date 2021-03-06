##
## library.R
##
## source file for all libraries needed for data tidying and analysis
## 
library(BGTools)
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(MESS)

# set the location name for MICU
micu <- "Cullen 2 E Medical Intensive Care Unit"

# set the directory containing the data
screen.dir <- "Screen"
include.dir <- "Include"
exclude.dir <- "Exclude"
lookup.dir <- "Lookup"
data.dir <- "Data"
export.dir <- "Export"
analysis.dir <- "Analysis"

# compress medication data files
gzip_files(include.dir)
gzip_files(exclude.dir)
gzip_files(data.dir)

# function to convert drugs documented in non-standard rates to equivalent doses
convert_units <- function(drug, unit, dose, weight) {
    drug <- str_to_lower(drug)
    
    if (!is.na(unit)) {
        if (drug == "ketamine") {
            # mg/hr to mg/kg/hr
            if (unit == "mg/hr") {
                dose <- dose / weight
            }
        } else if (drug == "fentanyl") {
            # mg/hr to mcg/hr
            if (unit == "mg/hr") {
                dose <- dose * 1000
            } else if (unit == "microgram/kg/min") {
                # mcg/kg/min? - think this is actually mcg/min
                dose <- dose * 60
            }
        }
    }
    
    dose
}

# function to calculate total amount of drug infused
total_dose <- function(drug, auc, weight) {
    drug <- str_to_lower(drug)
    
    if (drug == "propofol") {
        return(auc * weight * 60 / 1000)
    } else if (drug == "dexmedetomidine" || drug == "ketamine") {
        return(auc * weight)
    } else {
        return(auc)
    }
}

# interpret CAM-ICU
interpret_cam_icu <- function(assess) {
    result <- "Negative"
    print(str(assess))
    get_result <- function(x) {
        if (x == "Positive") {
            return(x)
        } 
    }
    
    lapply(assess, get_result)
    
    return(result)
}
