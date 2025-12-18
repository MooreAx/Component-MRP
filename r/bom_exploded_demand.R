#takes demanded supply and explodes it down the bom

# ___ constants (ish) ___

subcomponent_leadtime <- 0 #weeks

start_date = min(supply_plan$date)


# ___ BOM explosion ___

sub_component_demand <- supply %>%
  select(-description) %>%
  left_join(
    exploded_bom %>%
      select(-c(demand_window_first_date, demand_window_last_date,
                interval_to_expiry_offset, constraints, parent_part, parent_site,
                bom_basekey, bom_code)),
    join_by(uid == top_level_assembly),
    relationship = "many-to-many"
  ) %>%
  mutate(
    exploded_qty = supply * extended_quantity_per
  ) %>%
  rename(top_level_assembly = uid) %>%
  select(top_level_assembly, bom_line, indented_part, level, part, description, 
         assembly, product_status, product_type, extended_quantity_per, 
         lead_time_this_level, lead_time_cumulative, part_source_type,
         part_source_supplier,
         #add "bom_lines" after bom_line for TLAs without subcomps
         exploded_qty, date) %>%
  group_by(top_level_assembly) %>%
  arrange(date, top_level_assembly, bom_line) %>%
  
  # ___ apply lead sub component lead time ___
  mutate(
    date = case_when(
      level == 0 ~ date, #no adjustment for top level parts
      level >= 1 ~ date - weeks(subcomponent_leadtime)
    )
  )


#this is dangerous -- missing weeks are not checked. add expand(date) step here.
scd_wide <- sub_component_demand %>%
  filter(date >= start_date) %>%
  arrange(date) %>%
  pivot_wider(
    names_from = date,
    values_from = exploded_qty,
    values_fill = 0
  ) %>%
  group_by(top_level_assembly) %>%
  arrange(top_level_assembly, bom_line) %>%
  ungroup()

#write to csv
sub_component_demand %>% write_csv("scd_long.csv")
scd_wide %>% write_csv("scd_wide.csv")


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

# read the latest file
inv <- read_excel(latest_file, skip = 1, col_types = "text") %>%
  clean_names() %>%
  filter(qa_status %in% c("A", "eComm-A", "AWP", "QWP", "QAP")) %>%
  rename(part = name) %>%
  mutate(available = parse_number(available)) %>%
  group_by(part) %>%
  summarise(
    available = sum(available)
  )
    

#assemble MRP columns
MRP <- sub_component_demand %>%
#  mutate(
#    include = case_when(
#      bom_lines == 1 ~ TRUE,
#      bom_lines > 1 & level > 0 ~ TRUE,
#      .default = FALSE
#    )
#  ) %>%
  filter(level > 0) %>% #change to filter(include) if including TLAs
  ungroup() %>%
  select(part, description, product_type, lead_time_this_level, part_source_type, part_source_supplier) %>%
  distinct() %>%
  left_join(
    inv,
    join_by(part),
    relationship = "one-to-one"
  )

#each part repeats 5x in the MRP file. assemble duplicated rows here.
MRP_expanded <- MRP %>%
  slice(rep(1:n(), each = 5)) %>%
  group_by(row_id = rep(1:nrow(MRP), each = 5)) %>%
  mutate(
    line = row_number(),
    available = case_when(
      #use characters so it views nicely in Excel
      line == 4 & is.na(available) ~ "0",
      line == 4 ~ as.character(available),
      .default = ""
    )
  ) %>%
  ungroup() %>%
  select(-row_id)

#sort by part number

MRP_expanded %>% write_csv("MRP_expanded.csv")