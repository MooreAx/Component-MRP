#process the indented bom

library(tidyverse)
library(janitor)
library(readxl)

indented_bom <- read_xlsx(
  "data/Indented BOM.xlsx",
  skip=1,
  col_types="text",
  trim_ws = FALSE,
  ) %>%
  clean_names() %>%
  rename(
    indented_part = part_1,
    site = site_3,
    critical_path = path,
    quantity_per = quantity_per_8,
    extended_quantity_per = quantity_per_9,
    bom_scrap = scrap,
    yield_fraction = fraction,
    lead_time_this_level = this_level,
    lead_time_cumulative = cumulative,
    part_source_type = type_14,
    part_source_supplier = supplier,
    part_source_pool = pool,
    demand_window_first_date = first_date,
    demand_window_last_date = last_date,
    product_type = type_19,
    product_status = status,
    interval_to_expiry_offset = expiry_offset,
    parent_part = part_23,
    parent_site = site_24,
    bom_basekey = basekey,
    bom_code = code
  ) %>%
  mutate(
    part = str_remove_all(indented_part, "^(( )+\\.)+ "),
    .after = indented_part
  ) %>%
  mutate(
    level = as.integer(level),
    critical_path = case_when(
      critical_path == "Yes" ~ TRUE,
      critical_path == "No" ~ FALSE),
    quantity_per = parse_number(quantity_per),
    extended_quantity_per = parse_number(extended_quantity_per),
    bom_scrap = parse_number(bom_scrap)/100,
    yield_fraction = parse_number(yield_fraction),
    lead_time_this_level = parse_number(lead_time_this_level),
    lead_time_cumulative = parse_number(lead_time_cumulative)
  )


#add top level assembly
exploded_bom <- indented_bom %>%
  filter(
    extended_quantity_per > 0
  ) %>%
  group_by(group = cumsum(level == 0)) %>% #group by top-level assemblies
  mutate(
    top_level_assembly = first(part), .before = indented_part,
    bom_line = row_number(),
    bom_lines = max(bom_line) #add number of bom lines (to identify top level assys without any subcomps)
    ) %>%
  ungroup() %>%
  select(-group)



#are there any duplicate combinations of top_level_assembly - part?

test <- exploded_bom %>%
  group_by(top_level_assembly, indented_part, part, assembly) %>%
  summarise(
    n=n()
  )

dups <- test %>% filter(n>1)
