# build inputs for automated HPS file (mainly for reading into python)

rm(list = ls())

# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

#read inventory data
folder <- "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/Inventory/OHWSI"

latest_file <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE) %>%
  tibble(file = .) %>%
  mutate(
    # extract the first YYYY-MM-DD in the filename
    file_date = str_extract(basename(file), "\\d{4}-\\d{2}-\\d{2}") %>% ymd()
  ) %>%
  filter(!is.na(file_date)) %>%
  arrange(desc(file_date)) %>%
  slice(1) %>%
  pull(file)

# read the latest file, process
inv <- read_excel(latest_file, skip = 1, col_types = "text") %>%
  clean_names() %>%
  filter(qa_status %in% c("A", "eComm-A", "AWP", "QWP", "QAP")) %>%
  rename(part = name) %>%
  mutate(
    available = parse_number(available),
    thc_percent_31 = parse_number(thc_percent_31),
    label_thc = ifelse(str_detect(label_thc, "[^0-9.+-]"), NA, label_thc),
    label_thc = parse_number(label_thc),
    manufactured = parse_number(manufactured),
    manufactured = as.Date(manufactured, origin = "1899-12-30"),
    pack_date = parse_number(pack_date),
    pack_date = as.Date(manufactured, origin = "1899-12-30"),
  ) %>%
  rename(
    thc_pc = thc_percent_31,
    manufactured_date = manufactured,
    lot_number = number
  ) %>%
  select(part, site, lot_number, pool, qa_status, available, warehouse, thc, label_thc, manufactured_date, pack_date) %>%
  group_by(part, site, lot_number, pool, qa_status, warehouse, thc, label_thc, manufactured_date, pack_date) %>%
  summarise(
    available = sum(available),
    .groups = "drop"
  ) %>%
  filter(
    #only allow 10dddd(xx)?, 20dddd(xx)? and PGddddd(xx)?
    str_detect(part, "^(?:(?:10|20)\\d{4}(?:[a-zA-Z]{2})?|PG\\d{5}(?:[a-zA-Z]{2})?)$"),
    !(lot_number %in% c("RETURN", "RESALE")),
    available > 0
  )

