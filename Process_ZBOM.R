### ZBOM downloaded from Sage. Attempting to switch input from Rapid to ZBOM.

rm(list = ls())

zbom <- read_delim(
  "data/ZBOMS.csv",
  delim = ";",
  quote = "\"",
  trim_ws = TRUE,
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  #drop last column
  select(-last_col()) %>%
  mutate(
    bom_code = parse_number(bom_code)
  )

#bom code 1 = manufactured parts
#bom code 75 = subco parts

zbom_manufactured <- zbom %>%
  filter(
    bom_code == 1,
    quantity > 0,
    input_byproduct == "Normal"
  ) %>%
  mutate(
    quantity = parse_number(quantity),
    base_quantity = parse_number(base_quantity)
  )

#write csv
zbom_manufactured %>% write_csv("data/zbom_manufactured.csv")

#get part descriptions
descriptions1 <- zbom_manufactured %>%
  transmute(
    part = output_product,
    description = output_description,
    priority = 1
  )

descriptions2 <- zbom_manufactured %>%
  transmute(
    part = component,
    description = component_description,
    priority = 2
  )

descriptions <- bind_rows(descriptions1, descriptions2) %>%
  arrange(part, priority) %>%
  group_by(part) %>%
  slice(1) %>%
  ungroup() %>%
  select(-priority)


#BOM IS A "DIRECTIONAL GRAPH"
#BOM EXPLOSION REQUIRES "GRAPH TRANSVERSAL"

#find duplicates
dup_edges <- zbom_manufactured %>%
  count(output_product, component) %>%
  filter(n > 1) %>%
  left_join(
    zbom_manufactured,
    join_by(output_product, component)
  )


#identify "edges" (i.e. parent / child relationships)
edges <- zbom_manufactured %>%
  select(
    parent = output_product,
    part = component,
    quantity = quantity / base_quantity
  ) %>%
  drop_na(parent, part) %>%
  distinct()
         
#identify "roots" (i.e. top level assemblies)
roots <- zbom_manufactured %>%
  filter(str_detect(output_product, "^10\\d{4}$|^20\\d{4}$")) %>%
  filter(!(output_product %in% component)) %>%
  distinct(output_product) %>%
  rename(top_level_assembly = output_product)

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
          quantity_this_level = quantity
          ),
      join_by(part == parent),
      relationship = "many-to-many") %>%
    transmute(
      top_level_assembly,
      parent = part,
      part = child,
      level = level + 1,
      sort_path = paste(sort_path, child, sep = "."),
      quantity = quantity_this_level,
      extended_quantity = extended_quantity * quantity 
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
    part   = top_level_assembly,
    level  = 0,
    sort_path = top_level_assembly,
    quantity = 1,
    extended_quantity = 1
  )

#apply the recursive function
sage_bom <- bind_rows(
  seed,
  build_bom(
    frontier = seed,
    edges = edges,
    max_level = 10
  )
) %>%
  #assign sequence:
  group_by(top_level_assembly, level) %>%
  mutate(
    sequence = row_number()
    ) %>%
  group_by(top_level_assembly) %>%
  arrange(sort_path, .by_group = TRUE) %>%
  mutate(
    indented_part = str_c(str_dup("  . ", level), part),
    bom_line = row_number()
    ) %>%
  ungroup() %>%
  relocate(indented_part, .before = part)
  

