rm(list = ls())


#import BOMs

read_bom <- function(path) {
  read_csv(path) %>%
    mutate(top_level_assembly = as.character(top_level_assembly))
}

BOMs_manufacturing <- read_bom("bom_outputs/SageBOM_manufacturing.csv")
BOMs_labeling      <- read_bom("bom_outputs/SageBOM_labeling.csv")
BOMs_subco         <- read_bom("bom_outputs/SageBOM_subco.csv")

BOMs_manufacturing_tp <- read_bom("bom_outputs/SageBOM_manufacturing_tp.csv")
BOMs_labeling_tp      <- read_bom("bom_outputs/SageBOM_labeling_tp.csv")
BOMs_subco_tp         <- read_bom("bom_outputs/SageBOM_subco_tp.csv")


#Bind rows, creating 1 giant vertical BOM for joining to production plan
BOMs <- bind_rows(
  BOMs_manufacturing,
  BOMs_labeling,
  BOMs_subco
)

BOMs_tp <- bind_rows(
  BOMs_manufacturing_tp,
  BOMs_labeling_tp,
  BOMs_subco_tp
)


#read production plan:
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

#get distinct pp items (by bom_code) so we know which parts to exclude
distinct_pp_items <- production_plan %>%
  select(item, bom_code) %>%
  distinct()


#explode demand
exploded_demand <- production_plan %>% 
  filter(quantity > 0) %>%
  left_join(
    BOMs_tp %>%
      filter(level > 0),
    join_by(item == top_level_assembly, bom_code),
    relationship = "many-to-many"
  ) %>%
  
  #we only want the components that are not planned separately (that do not exist in "distinct_pp_items")
  anti_join(
    distinct_pp_items,
    join_by(part == item, bom_code)
  ) %>%
  
  #explode the qty
  mutate(
    exploded_qty = quantity * extended_quantity
  )

#get invalid boms -- these should be merged with the production plan data so that they can be highlighted in the main file
non_existant_boms <- exploded_demand[!complete.cases(exploded_demand),] %>% select(source_file, description, item, bom) %>% distinct()


#these are material requirements for parts **not** planned separately deriving from the production plan
exploded_demand_complete <- exploded_demand %>%
  filter(!is.na(exploded_qty)) %>%
  select(tla = item, tla_description = description, bom_code, 
         sow, part, part_description = description1, tla_qty = quantity,
         exploded_qty, stock_unit, status)


#now we have to repeat for estimated planning requirements from the planning horizon to 1 year out (HPS territory)


#get production plan horizon and start date
pp_horizon <- max(production_plan$sow)
start_date = min(production_plan$sow)

all_weeks <- seq.Date(
  from = start_date,
  to = start_date + weeks(52),
  by = "week"
)

sentinel <- tibble(
  tla = "__sentinel__",
  sow = all_weeks,
  exploded_qty = 0
)


#read shorts from allocator
shorts <- read_csv(
  paste(
    "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/forecast_netter/outputs",
    "shortlog.csv",
    sep = "/"
  )
)


shorts_by_month <- shorts %>%
  filter(date > pp_horizon) %>% #beyond production plan!
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


#expand weeks to ensure no missing columns when pivoted wide
estimated_production_plan_wide <- shorts_by_month %>%
  select(
    part, sow = build_date, qty = short
  ) %>%
  group_by(part, sow) %>%
  summarise(
    qty = sum(qty),
    .groups = "drop"
  ) %>%
  complete(
    part,
    sow = all_weeks,
    fill = list(qty = 0)
  ) %>%
  group_by(part, sow) %>%
  arrange(part, sow) %>%
  ungroup() %>%
  pivot_wider(
    names_from = sow,
    values_from = qty
  )

#write estimated production plan to show work
estimated_production_plan_wide %>% write_csv("Sage_Estimated_Production_Plan_wide.csv")


#build these by applying non-tp BOMs

exploded_demand_shorts <- shorts_by_month %>%
  mutate(
    bom_code = 1
  ) %>%
  left_join(
    BOMs %>% rename(component = part),
    join_by(part == top_level_assembly, bom_code),
    relationship = "many-to-many"
  ) %>%
  
  #items not planned separately
  anti_join(
    distinct_pp_items,
    join_by(component == item, bom_code)
  ) %>%
  
  #explode the qty
  mutate(
    exploded_qty = short * extended_quantity
  )

#get invalid boms (perhaps they're in another bom code?)
non_existant_boms_shorts <- exploded_demand_shorts[!complete.cases(exploded_demand_shorts),] %>% select(part, bom_code) %>% distinct()
### FIX ME: there are way too many here ^^.
### Perhaps earlier filtering of boms by items in the production plan was a bad idea (some of these are not in the pp)
### come back to this.



#remove stuff with no bom
exploded_demand_shorts_complete <- exploded_demand_shorts %>%
  filter(!is.na(exploded_qty)) %>%
  select(tla = part, bom_code, 
         sow = build_date, part = component, part_description = description1, tla_qty = short,
         exploded_qty, stock_unit, status)


## now we have the complete set of demand: pp + estimated pp

sub_component_demand <- bind_rows(
  exploded_demand_complete %>%
    mutate(type = "production plan"),
  exploded_demand_shorts_complete %>%
    mutate(type = "shorts")
) %>%
  group_by(across(-exploded_qty)) %>%
  summarise(
    exploded_qty = sum(exploded_qty),
    .groups = "drop"
  )


#normal MRP transformations


#there is a better way to do this using complete and nesting
#missing weeks are expanded via the sentinel row binding
scd_wide <- sub_component_demand %>%
  group_by(tla, bom_code, sow, part, part_description, stock_unit, type) %>%
  summarise(
    exploded_qty = sum(exploded_qty),
    .groups = "drop"
  ) %>%
  bind_rows(sentinel) %>%
  filter(sow >= start_date) %>%
  arrange(sow) %>%
  pivot_wider(
    names_from = sow,
    values_from = exploded_qty,
    values_fill = 0
  ) %>%
  group_by(type, tla) %>%
  arrange(type, tla) %>%
  ungroup() %>%
  filter(tla != "__sentinel__")


scd_wide %>% write_csv("scd_wide_sage.csv")

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
  filter(qa_status %in% c("A", "eComm-A", "AWP", "QWP", "QAP", "QW1")) %>%
  rename(part = name) %>%
  mutate(available = parse_number(quantity)) %>% #using quantity, which is lot number
  group_by(part) %>%
  summarise(
    available = sum(available) 
  )


#assemble MRP columns
MRP <- sub_component_demand %>%
  ungroup() %>%
  select(part, description = part_description) %>%
  distinct() %>%
  left_join(
    inv,
    join_by(part),
    relationship = "one-to-one"
  )

#each part repeats 5x in the MRP file. assemble duplicated rows here.
MRP_expanded_Sage <- MRP %>%
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

MRP_expanded_Sage %>% write_csv("MRP_expanded_sage.csv")

