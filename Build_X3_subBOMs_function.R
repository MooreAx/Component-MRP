# April 5 - UPDATE PRODUCTION PLAN WITH BOM_CODE AND THEN RUN & DEBUG

#the idea here is to create boms corresponding with each step in the production
#process, as detailed in the production plan.

#each production step in the pp, regardless of how it is labelled, must have
#its own top level assembly. each of these top level assemblies has to be
#exploded down to max depth, or to the level of another top level assembly

##UPDATE - MAKE FUNCTION

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


build_full_bom_tp <- function(input_bom_code, input_bom_type, production_plan, BOMH, BOMD, ITMM) {
  
  BOMH <- BOMH %>%
    filter(
      bom_code == input_bom_code,
      bom_type == input_bom_type
    )
  
  BOMD <- BOMD %>%
    filter(
      bom_code == input_bom_code,
      bom_type == input_bom_type,
      stock_link_qty > 0
    ) %>%
    #add base quantity
    left_join(
      BOMH %>% 
        select(part, parent_base_qty = base_qty),
      join_by(parent == part),
      relationship = "many-to-one"
    ) %>%
    
    #add uom parent
    left_join(
      ITMM %>%
        select(part, parent_description = description1, parent_stock_unit = stock_unit),
      join_by(parent == part),
      relationship = "many-to-one"
    ) %>% 
    
    #add uom component
    left_join(
      ITMM %>%
        select(part, component_description = description1, component_stock_unit = stock_unit),
      join_by(component == part),
      relationship = "many-to-one"
    ) %>%
    
    select(
      parent,
      parent_description,
      parent_base_qty,
      parent_stock_unit,
      component,
      component_description,
      component_type,
      component_type_dtl,
      stock_link_qty,
      component_stock_unit,
      link_qty_code,
      link_qty_code_dtl,
      scrap_factor_percent,  
      qty_rounding,
      qty_rounding_dtl,
      
      bom_code,
      bom_type,
      bom_type_dtl,
      sequence,
      routing_operation
    )
  
  #BOMD IS A "DIRECTIONAL GRAPH"
  #BOM EXPLOSION REQUIRES "GRAPH TRANSVERSAL"
  
  #find duplicates -- parent-components can have different routings!
  dup_edges <- BOMD %>%
    count(parent, component, routing_operation) %>%
    filter(n > 1) %>%
    left_join(
      BOMD,
      join_by(parent, component, routing_operation)
    )
  
  if (nrow(dup_edges) > 0) {
    stop("Duplicate BOM edges detected")
  }
  
  #identify "edges" (i.e. parent / child relationships)
  edges <- BOMD %>%
    # Normalized qty: how much of component (in its STU) is needed per 1 unit of parent (in parent's STU)
    rename(part = component) %>%
    mutate(
      quantity = case_when(
        link_qty_code == 1 ~ stock_link_qty / parent_base_qty, #proportional
        link_qty_code == 2 ~ stock_link_qty, #fixed
      ),
      .after = stock_link_qty
    )
  
  #get terminal parts within this bom code
  terminal_parts <- production_plan %>% 
    filter(
      bom_code == input_bom_code #add to production plan
    ) %>%
    select(item) %>%
    distinct() %>%
    pull(item)
  
  #identify "roots" (i.e. top level assemblies)
  roots <- BOMD %>%
  filter(
    bom_code == input_bom_code,
    bom_type == input_bom_type,
    parent %in% terminal_parts
    ) %>%
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
    mutate(
      bom_code = input_bom_code,
      bom_type = input_bom_type
    ) %>%
    select(
      top_level_assembly, bom_code, bom_type, bom_line, level, part, indented_part, description1,
      link_quantity = quantity, link_type, scrap_factor_percent, extended_quantity, stock_unit, status
    )

  
}

BOMs_manufacturing_tp <- build_full_bom_tp(
  input_bom_code = 1, #manufacturing
  input_bom_type = 2, #manufacturing
  production_plan,
  BOMH,
  BOMD,
  ITMM
)

BOMs_labeling_tp <- build_full_bom_tp(
  input_bom_code = 2, #labeling
  input_bom_type = 2,  #manufacturing
  production_plan,
  BOMH,
  BOMD,
  ITMM
)

BOMs_subco_tp <- build_full_bom_tp(
  input_bom_code = 75, #subcontracting
  input_bom_type = 3,   #subcontracting
  production_plan,
  BOMH,
  BOMD,
  ITMM
)

BOMs_manufacturing_tp %>% write_csv("bom_outputs/SageBOM_manufacturing_tp.csv")
BOMs_labeling_tp %>% write_csv("bom_outputs/SageBOM_labeling_tp.csv")
BOMs_subco_tp %>% write_csv("bom_outputs/SageBOM_subco_tp.csv")

