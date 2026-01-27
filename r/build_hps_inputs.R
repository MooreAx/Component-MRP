# build inputs for HPS file replacement
# Note: The HPS is being retired effective Jan 19, 2026. Supply plan estimates
# will be based off of anticipated shortages from the forecast netter. Supply
# will be built to cover shortages expected in the following month

# load libraries
library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)

#read production plan
production_plan <- read_csv(
  paste(
    "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/supply_model",
    "Production Plan.csv",
    sep = "/"
  )
)

pp_horizon <- max(production_plan$date)
  
#read shorts from allocator
shorts <- read_csv(
  paste(
    "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/forecast_netter/outputs",
    "shortlog.csv",
    sep = "/"
  )
)

shorts_by_month <- shorts %>%
  filter(date > pp_horizon) %>%
  mutate(
    month = floor_date(date, unit = "month")
  ) %>%
  group_by(part, month) %>%
  summarise(
    short = sum(short),
    .groups = "drop"
  ) %>%
  mutate(
    build_date = month - months(1),
    build_date = pmax(build_date, pp_horizon + weeks(1))
  )

#assemble into supply plan
supply_no_hps <- bind_rows(
  production_plan %>%
    filter(prod_class == "FG") %>%
    select(
      uid = item,
      date = SOW,
      supply = quantity
    ) %>%
    mutate(
      type = "supply plan"
    ),
  shorts_by_month %>%
    select(
      uid = part,
      date = build_date,
      supply = short
    ) %>%
    mutate(
      type = "estimated short"
    )
)

supply <- supply_no_hps %>%
  group_by(uid, date) %>%
  summarise(
    supply = sum(supply),
    .groups = "drop"
  )




