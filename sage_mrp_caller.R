# main caller file

rm(list = ls())

# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

#call scripts
source("Process_x3_tables.R")
source("Build_X3_BOM_function.R")
source("Build_X3_subBOMs_function.R")

source("write_wide_pp.R")


#build exploded demand
#source("Sage_explode_demand.R")
