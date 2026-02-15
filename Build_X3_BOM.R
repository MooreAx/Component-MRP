### construct hierarchical BOM using BOMH, BOMD, and ITMMASTER from X3 via SnowFlake
# ---> must run Process_x3_tables.R to get objects


ITMM <- x3ITMMASTER_clean

BOMH <- x3BOM_clean %>%
  filter(
    bom_code == 1, #manufacturing
    bom_type == 2, #manufacturing
  ) 

BOMD <- x3BOMD_clean %>%
  filter(
    bom_code == 1, #manufacturing
    bom_type == 2, #manufacturing
    bom_qty > 0
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


## have got to here ##



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


# -------------- PICK UP HERE - sunday feb 1.

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



#identify "roots" (i.e. top level assemblies)
roots <- BOMD %>%
  filter(str_detect(parent, "^(?:10\\d{4}|20\\d{4})(?:-UL)?$")) %>%
  #filter(!(parent %in% component)) %>%
  distinct(parent) %>%
  rename(top_level_assembly = parent)

#recursive bom traversal function
build_bom <- function(frontier,
                      edges,
                      level = 0,
                      max_level = 10) {
  
  #this function walks through the bom level by level, asking at each level "what comes next"
  
  #stop conditions: expand until there are no children or until level 10
  if (level >= max_level || nrow(frontier) == 0) {
    return(tibble())
  }
  
  #at each step, we only expand the previous step's children -- the "frontier"
  children <- frontier %>%
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
    build_bom(
      frontier = children,
      edges = edges,
      level = level + 1,
      max_level = max_level
    )
  )
}

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
sagex3_bom <- bind_rows(
  seed,
  build_bom(
    frontier = seed,
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

final_sagex3_bom <- sagex3_bom %>%
  select(
    top_level_assembly, bom_line, level, part, indented_part, description1,
    link_quantity = quantity, link_type, scrap_factor_percent, extended_quantity, stock_unit, status
  )

final_sagex3_bom %>% write_csv("sagebom_heirarchy.csv")

#ok sweet. now filter just for items in the latest PP.

pp_items <- production_plan %>% filter(
  prod_class == "FG"
) %>%
  select(item) %>%
  distinct() %>%
  pull(item)


#send these to production planners
current_boms_for_checking <- final_sagex3_bom %>%
  filter(top_level_assembly %in% pp_items)

current_boms_for_checking %>% write_csv("2026-02-14 sage_boms_with_qtys.csv")



#compare with indented bom from rapid
# -- run process_indented_bom.R

### need to add sort path this and re-compare.

rapid_bom <- exploded_bom %>%
  select(
    top_level_assembly,
    indented_part,
    part = part,
    level,
    rr_extended_qty = extended_quantity_per
  ) %>%
  
  filter(
    str_detect(top_level_assembly,"^10\\d{4}$|^20\\d{4}$")
  ) %>%
  
  #add sort part using accumulate
  group_by(top_level_assembly) %>%
  mutate(
    sort_path = accumulate(
      seq_along(part),
      .init = character(),
      function(stack, i) {
        stack <- stack[seq_len(level[i])]
        stack[level[i] + 1] <- part[i]
        stack
      }
    )[-1] %>% 
      map_chr(~ paste(.x, collapse = ".")),
    .after = part
  ) %>%
  select(-level)

x3_bom <- sagex3_bom %>%
  select(
    top_level_assembly,
    indented_part,
    part,
    x3_extended_qty = extended_quantity,
    sort_path
  )

delta <- full_join(
  rapid_bom,
  x3_bom,
  join_by(top_level_assembly, indented_part, part, sort_path),
) %>%
  mutate(
    is_same = x3_extended_qty == rr_extended_qty,
    diff = abs(x3_extended_qty - rr_extended_qty)
  )

delta %>% write_csv("bom_discrap.csv")
