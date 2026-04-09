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
) %>%
  filter(quantity > 0)


#validate PP - ensure there is *exactly* 1x FG-labelled item for each SKU
production_plan_check <- production_plan %>%
  filter(
    str_detect(item, "10\\d{4}|20\\d{4}")
  ) %>%
  mutate(
    parts_clean = str_replace(item, "-UL", "")
  ) %>%
  select(parts_clean, process, prod_class) %>%
  distinct() %>%
  group_by(parts_clean, prod_class) %>%
  summarise(
    n = n(), #this is the number of unique production steps per label
    .groups = "drop"
  ) %>%
  pivot_wider(
    values_from = n,
    names_from = prod_class,
    values_fill =  0
  )

missing_prod_step <- production_plan_check %>%
  filter(FG == 0) %>%
  select(parts_clean) %>%
  distinct() %>%
  pull(parts_clean)

multiple_prod_steps <- production_plan_check %>%
  filter(FG > 1) %>%
  select(parts_clean) %>%
  distinct() %>%
  pull(parts_clean)

#Print output for copy pasting **need to wrap in a warning**
cat(
  str_flatten(
    c(
      "** PRODUCTION PLAN WARNINGS **",
      "--> The following items are missing a 'FG'-labelled production step:",
      missing_prod_step,
      "--> The following items have multipe 'FG'-labelled production steps:",
      if (length(multiple_prod_steps)>0) multiple_prod_steps else "nil"
    ),
    collapse = '\n'
  )
)


unique_parts <- production_plan_check %>% 
  select(parts_clean) %>% 
  distinct() %>%
  pull(parts_clean)



pp_horizon <- max(production_plan$date)
pp_horizon <- pp_horizon + days((8 - wday(pp_horizon, week_start = 1)) %% 7)

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
    build_date = month - months(1), #build it the month before
    build_date = build_date + days((8 - wday(build_date, week_start = 1)) %% 7), #first monday of the month
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




