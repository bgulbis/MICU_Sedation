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
library(MESS)

# set the location name for MICU
micu <- "Cullen 2 E Medical Intensive Care Unit"

# set the directory containing the data
data.dir <- "Data"

# compress medication data files
gzip_files(data.dir)

# function used to update MICU depart time if calculated depart time is NA
get_depart <- function(x, y) {
    if (is.na(y)) {
        x
    } else {
        y
    }
}

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
    } else if (drug == "dexmedetomidine" | drug == "ketamine") {
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
