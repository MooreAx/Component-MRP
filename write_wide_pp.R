rm(list = ls())

production_plan <- read_csv(
  paste(
    "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/supply_model",
    "Production Plan.csv",
    sep = "/"
  )
) %>%
  clean_names() %>%
  mutate(
    #convert bom_codes
    bom_code = case_when(
      bom == "M" ~ 1, #manufacturing
      bom == "L" ~ 2, #labeling
      bom == "M-CMO" ~ 75, #cmo
      .default = NA
    )
  )




start_sow <- production_plan %>% filter(
  quantity > 0
) %>%
  select(sow) %>%
  distinct() %>%
  pull() %>%
  min()

pp_wide <- production_plan %>%
  filter(sow >= start_sow) %>%
  arrange(item, date) %>%
  select(-date) %>%
  pivot_wider(
    names_from = sow,
    values_from = quantity
  )

pp_wide %>% write_csv("bom_outputs/pp_wide.csv")
