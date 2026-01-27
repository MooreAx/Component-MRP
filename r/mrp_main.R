# main caller file

rm(list = ls())

# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)


#HPS file name
#hps_filename <- "HPS/HPS_MRP 01.05.26.xlsx"


#call scripts
source("r/process_indented_bom.R")
#source("r/read_HPS.R")
source("r/build_hps_inputs.R")
source("r/bom_exploded_demand.R")
source("r/process_OPO.R")

print("Main complete")


#ref parts
mape_bias_path <- "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/R/mape_bias/Intermediates"
RefParts <- read_csv(
  paste(
    mape_bias_path,
    "RefParts.csv",
    sep = "/"
  )
)


