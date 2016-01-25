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

# function to convert drugs documented in mL to equivalent doses
convert_ml <- function(drug, unit, dose) {
    if (unit == "mL") {
        if (drug == "propofol") {
            return(dose * 10)
        } else if (drug == "fentanyl") {
            return(dose * 50)
        } else if (drug == "dexmedetomidine") {
            return(dose * 4)
        }
    } else {
        return(dose)
    }
}

# function to calculate total amount of drug infused
total_dose <- function(drug, dose, duration, weight) {
    if (drug == "propofol") {
        return(dose * weight * duration * 60 / 1000)
    } else if (drug == "dexmedetomidine") {
        return(dose * weight * duration)
    } else {
        return(dose * duration)
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
