#read HPS to get "demand", i.e., the supply plan

HPS <- read_xlsx(
  paste(
    "data",
    hps_filename,
    sep = "/"
  ),
  sheet="HPS",
  col_types = "text"
) %>%
  clean_names()

supply_plan <- HPS %>%
  filter(analysis %in% c("Planned Order", "Supply Order")) %>%
  select(-c(on_hand)) %>%
  pivot_longer(
    cols = starts_with("x"),
    names_to = "date",
    values_to = "quantity",
    names_prefix = "x"
  ) %>%
  mutate(
    date = as.numeric(date),
    date = as.Date(date, origin = "1899-12-30"),
    date = date + days(1),
    quantity = as.numeric(quantity)
  ) %>%
  rename(
    type = analysis
  )

#what is the max date with a supply plan?
supply_plan_horizon <- supply_plan %>%
  filter(type == "Supply Order", quantity > 0) %>%
  summarise(max_date = max(date)) %>%
  pull(max_date)

supply <- supply_plan %>%
  pivot_wider(
    names_from = type,
    values_from = quantity,
    values_fill = 0
  ) %>%
  clean_names() %>%
  mutate(
    supply = case_when(
      date <= supply_plan_horizon ~ supply_order,
      .default = planned_order
    )
  ) %>%
  filter(
    supply > 0
  ) %>%
  select(uid, description, form, date, supply)
