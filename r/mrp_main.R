# main caller file

rm(list = ls())

# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)


#HPS file name
hps_filename <- "HPS/HPS_MRP 12.15.25.xlsx"


#call scripts
source("r/process_indented_bom.R")
source("r/read_HPS.R")
source("r/bom_exploded_demand.R")
source("r/process_OPO.R")

print("Main complete")