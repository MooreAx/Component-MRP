#data from snowflake


#ITEM MASTER
x3ITMMASTER <- read_csv(
  "data/ITMMASTER.csv",
  col_types = cols(.default = "c")
)

x3ITMMASTER_clean <- x3ITMMASTER %>%
  select(
    part = ITMREF_0,
    description_axx = YDES3AXX_0,
    description1 = ITMDES1_0,
    description2 = ITMDES2_0,
    description3 = ITMDES3_0,
    stock_unit = STU_0,
    product_status = ITMSTA_0,
    category = TCLCOD_0,
    
    purchase_unit = PUU_0,
    purchase_to_stock_unit_conversion = PUUSTUCOE_0,
    
    sales_unit = SAU_0,
    sales_to_stock_unit_conversion = SAUSTUCOE_0,
    
    statistical_unit = SSU_0, #"Normalize" units so analyses remain meaningful when mixing heterogeneous products
    statistical_to_stock_unit_conversion = SSUSTUCOE_0,
    
    packing_unit_0 = PCU_0,
    packing_to_stock_unit_conversion_0 = PCUSTUCOE_0,
    packing_unit_1 = PCU_1,
    packing_to_stock_unit_conversion_1 = PCUSTUCOE_1,
    
    accounting_code = ACCCOD_0,
    management_mode = STDFLG_0,
    standard = ITMSTD_0,
    category = TCLCOD_0,
    search_key = SEAKEY_0,
    
    item_weight = ITMWEI_0,
    weight_unit = WEU_0,
    
    
  ) %>%
  mutate(
    product_status = parse_number(product_status),
    product_status_dtl = case_when(
      product_status == 1 ~ "Active",
      product_status == 2 ~ "In development",
      product_status == 3 ~ "On shortage",
      product_status == 4 ~ "Not renewed",
      product_status == 5 ~ "Obsolete",
      product_status == 6 ~ "Not usable"
    ),
    
    management_mode = parse_number(management_mode),
    management_mode_dtl = case_when(
      management_mode == 1 ~ "Not managed",
      management_mode == 2 ~ "By project",
      management_mode == 3 ~ "Available stock",
      management_mode == 4 ~ "By order"
    ),

    sales_to_stock_unit_conversion = parse_number(sales_to_stock_unit_conversion),
    statistical_to_stock_unit_conversion = parse_number(statistical_to_stock_unit_conversion),
    packing_to_stock_unit_conversion_0 = parse_number(packing_to_stock_unit_conversion_0),
    packing_to_stock_unit_conversion_1 = parse_number(packing_to_stock_unit_conversion_1),
    purchase_to_stock_unit_conversion = parse_number(purchase_to_stock_unit_conversion),
    
    item_weight = parse_number(item_weight),
  )



#bom headers
x3BOM <- read_csv(
  "data/BOM.csv",
  col_types = cols(.default = "c")
  )

x3BOM_clean <- x3BOM %>%
  select(
    part = ITMREF_0,
    bom_code = BOMALT_0,
    use_status = USESTA_0,
    prototype = NPIPRO_0,
    base_qty = BASQTY_0,
    management_unit = QTYCOD_0,
    bom_type = BOMALTTYP_0,
    
  ) %>%
  
  mutate(
    bom_code = parse_number(bom_code),
    
    use_status = parse_number(use_status),
    use_status_dtl = case_when(
      use_status == 1 ~ "In development",
      use_status == 2 ~ "Available to use"
    ),
    
    prototype = parse_number(prototype),
    prototype_dtl = case_when(
      prototype == 1 ~ "YES",
      prototype == 2 ~ "NO"
    ),
    
    base_qty = parse_number(base_qty),
    
    #how the quantities of the components needed to make this parent product are made up
    management_unit = parse_number(management_unit),
    management_unit_dtl = case_when(
      management_unit == 1 ~ "one",
      management_unit == 2 ~ "per hundred",    #Components must be in multiples of a hundred.
      management_unit == 3 ~ "per thousand",   #Components must be in multiples of one thousand.
      management_unit == 4 ~ "percentage",     #Components must be defined as a percentage of the total list of components
      management_unit == 5 ~ "by lot"          #Components are defined by the size of a specified 'lot'.
    ),
    
    bom_type = parse_number(bom_type),
    bom_type_dtl = case_when(
      bom_type == 1 ~ "Sales (Kit)",
      bom_type == 2 ~ "Manufacturing",
      bom_type == 3 ~ "Subcontracting"
    )
    
  ) %>%
  #bring in descriptions
  left_join(
    x3ITMMASTER_clean %>% 
      select(part, description1),
    join_by(part),
    relationship = "many-to-one",
  ) %>%
  relocate(description1, .after = part)


#bom detail
x3BOMD <- read_csv(
  "data/BOMD.csv",
  col_types = cols(.default = "c")
)

x3BOMD_clean <- x3BOMD %>%
  select(
    parent = ITMREF_0,
    bom_code = BOMALT_0,
    sequence = BOMSEQ_0, #user assigned sequence
    sequence_remainder = BOMSEQNUM_0, #system assigned sequence?
    component = CPNITMREF_0,
    component_type = CPNTYP_0,
    link_description = BOMSHO_0,
    bom_type = BOMALTTYP_0,
    
    #link qty code: Use this field to define how the quantity of this component
    #in the stock unit that is needed to make the parent product is determined
    #   --Proportional: The quantity of this component is multiplied by the 
    #     quantity required for the parent product.
    #   --Fixed: The quantity of this component is fixed, regardless of the
    #     quantity required of the parent product.
    
    link_qty_code = LIKQTYCOD_0,
    
    
    qty_rounding = QTYRND_0,
    
    bom_uom = BOMUOM_0, #unit of measure (volume or mass) for this component
    bom_uom_to_stock_uom_conversion = BOMSTUCOE_0, # conversion coefficient between the operation unit for this component and the storage unit
    bom_qty = BOMQTY_0, #Quantity of this component in the selected unit (field BOMUOM) that is needed to make the parent product
    stock_link_qty = LIKQTY_0, #Quantity of this component in the stock unit that is needed to make the parent product. The default quantity is calculated from the entered quantity using the UOM-STK coefficient (field BOMSTUCOE)
    
    #The percentage of the component that you expect to be lost during the production process
    scrap_factor_percent = SCA_0,
    routing_operation = CPNOPE_0,
    routing_operation_suffix = OPENUMLEV_0,
    
    #The operation lead-time is used during launch and in the MRP calculations.
    #It is used to calculate the date of the component (material) requirement
    #with respect to the production start date of the parent product.
    #It is expressed in calendar days. You can enter a negative value.
    
    operation_lead_time = BOMOFS_0,
    
    link_review_index = LIKRLE_0,
    type_of_supply = SCOFLG_0,
    selection_formula = FORSEL_0,
    quantity_formula = FORQTY_0
  ) %>%
  
  mutate(
    bom_code = parse_number(bom_code),
    sequence = parse_number(sequence),
    sequence_remainder = parse_number(sequence_remainder),
    bom_uom_to_stock_uom_conversion = parse_number(bom_uom_to_stock_uom_conversion),
    bom_qty = parse_number(bom_qty),
    stock_link_qty = parse_number(stock_link_qty),
    scrap_factor_percent = parse_number(scrap_factor_percent),
    routing_operation = parse_number(routing_operation),
    routing_operation_suffix = parse_number(routing_operation_suffix),
    operation_lead_time = parse_number(operation_lead_time),
    
    component_type = parse_number(component_type),
    component_type_dtl = case_when(
      component_type == 1 ~ "Normal",
      component_type == 2 ~ "Option",
      component_type == 3 ~ "Variant",
      component_type == 4 ~ "By-product",
      component_type == 5 ~ "Text",
      component_type == 6 ~ "Costing",
      component_type == 7 ~ "Service",
      component_type == 8 ~ "Multiper option",
      component_type == 9 ~ "Normal (with formula)"
    ),
    
    bom_type = parse_number(bom_type),
    bom_type_dtl = case_when(
      bom_type == 1 ~ "Sales (Kit)",
      bom_type == 2 ~ "Manufacturing",
      bom_type == 3 ~ "Subcontracting",
    ),
    
    link_qty_code = parse_number(link_qty_code),
    link_qty_code_dtl = case_when(
      link_qty_code == 1 ~ "Proportional",
      link_qty_code == 2 ~ "Fixed"
    ),
    
    qty_rounding = parse_number(qty_rounding),
    qty_rounding_dtl = case_when(
      qty_rounding == 1 ~ "Nearest",
      qty_rounding == 2 ~ "Greater than",
      qty_rounding == 3 ~ "Less than"
    ),
    
    type_of_supply = parse_number(type_of_supply),
    type_of_supply_dtl = case_when(
      type_of_supply == 1 ~ "Internal",
      type_of_supply == 2 ~ "To be sent to the subcontractor",
      type_of_supply == 3 ~ "Supplied by the subcontractor"
    )
  ) %>%
  
  #bring in descriptions
  left_join(
    x3ITMMASTER_clean %>% 
      select(part, description1),
    join_by(component == part),
    relationship = "many-to-one",
  ) %>%
  relocate(description1, .after = component)


#remove the main tables ... too big
#rm(x3BOM, x3BOMD, x3ITMMASTER)
