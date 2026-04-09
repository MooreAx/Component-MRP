#the idea here is to create boms corresponding with each step in the production
#process, as detailed in the production plan.

#each production step in the pp, regardless of how it is labelled, must have
#its own top level assembly. each of these top level assemblies has to be
#exploded down to max depth, or to the level of another top level assembly


#step 1: identify roots -- basically any processing step, regardless of label

#re-read production plan, do not filter for qty > 0 (allow qty = 0) because
#we need to allow for the termination of a bom at a node that is a seprate step,
#but that doesn't have any volume.

production_plan <- read_csv(
  paste(
    "C:/Users/alex.moore/OneDrive - Canopy Growth Corporation/Documents/Working Folder/supply_model",
    "Production Plan.csv",
    sep = "/"
  )
)



terminal_parts <- production_plan %>% filter(
  plan_date == max(plan_date)
) %>%
  select(item) %>%
  distinct() %>%
  pull(item)


#modify the traversal function. what we want is:
# -- explode each production-plan item fully except when you hit another
#    production-plan item — then stop recursion at that node.
# -- During traversal: do not expand nodes whose part is in terminal_parts, 
#    unless it is the root of the current explosion


#recursive bom traversal function, with terminal parts awareness
build_bom_tp <- function(frontier,
                      edges,
                      stop_parts = NULL,
                      level = 0,
                      max_level = 10) {
  
  #this function walks through the bom level by level, asking at each level "what comes next"
  
  #stop conditions: expand until there are no children or until level 10
  if (level >= max_level || nrow(frontier) == 0) {
    return(tibble())
  }
  
  #remove nodes that should not be exploded further
  if (is.null(stop_parts)) {
    expandable_frontier <- frontier
  } else {
    expandable_frontier <- frontier %>%
      filter(!(part %in% stop_parts & level > 0))
  }
  
  
  #at each step, we only expand the previous step's children -- the "frontier"
  children <- expandable_frontier %>%
    inner_join(
      edges %>% 
        rename(
          child = part,
          quantity_multiplier = quantity,
          child_qty_link_code = link_qty_code,
          child_scrap = scrap_factor_percent
        ),
      join_by(part == parent),
      relationship = "many-to-many") %>%
    transmute(
      top_level_assembly,
      parent = part,
      part = child,
      level = level + 1,
      sort_path = paste(sort_path, child, sep = "."),
      quantity = quantity_multiplier,
      scrap_factor_percent = child_scrap,
      extended_quantity = case_when(
        child_qty_link_code == 1 ~ extended_quantity * quantity / (1 - scrap_factor_percent/100),
        child_qty_link_code == 2 ~ quantity #fixed
      ),
      link_qty_code = child_qty_link_code
    )
  
  bind_rows(
    children,
    build_bom_tp(
      frontier = children,
      edges = edges,
      stop_parts = stop_parts,
      level = level + 1,
      max_level = max_level
    )
  )
}


#identify "roots" (i.e. top level assemblies)
roots <- BOMD %>%
  filter(parent %in% terminal_parts)%>%
  distinct(parent) %>%
  rename(top_level_assembly = parent)



#set up the starting data frame
seed <- roots %>%
  transmute(
    top_level_assembly,
    parent = NA,
    part = top_level_assembly,
    level = 0,
    sort_path = top_level_assembly,
    quantity = 1,
    link_qty_code = NA,
    extended_quantity = 1,
    scrap_factor_percent = NA,
  )


#apply the recursive function
sagex3_bom_tp <- bind_rows(
  seed,
  build_bom_tp(
    frontier = seed,
    stop_parts = terminal_parts,
    edges = edges,
    max_level = 20
  )
) %>%
  #assign sequence:
  group_by(top_level_assembly) %>%
  arrange(sort_path, .by_group = TRUE) %>%
  mutate(
    indented_part = str_c(str_dup("  . ", level), part),
    bom_line = row_number()
  ) %>%
  group_by(top_level_assembly, level) %>%
  mutate(
    sequence = row_number()
  ) %>%
  ungroup() %>%
  relocate(indented_part, .before = part) %>%
  
  #pull in descriptions and uoms, etc.
  left_join(
    x3ITMMASTER_clean %>%
      select(
        part,
        description1,
        stock_unit,
        status = product_status_dtl
      ),
    join_by(part),
    relationship = "many-to-one"
  ) %>%
  mutate(
    link_type = case_when(
      link_qty_code == 1 ~ "Proportional",
      link_qty_code == 2 ~ "Fixed",
    )
  )

#clean bom for exporting

final_sagex3_bom_tp <- sagex3_bom_tp %>%
  select(
    top_level_assembly, bom_line, level, part, indented_part, description1,
    link_quantity = quantity, link_type, scrap_factor_percent, extended_quantity, stock_unit, status
  )

final_sagex3_bom_tp %>% write_csv("sagebom_heirarchy_tp.csv")

