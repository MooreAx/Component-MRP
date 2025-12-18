# OPO processing -- file from sage

OPO_raw <- read_delim(
  "data/ZPODTL.csv",
  delim = ";",
  quote = "",
  col_types = cols(.default = "c"),
  ) %>%
  clean_names() %>%
  mutate(
    across(everything(), ~str_replace_all(., '^"|"$', ''))
  )

MRP_parts <- MRP %>% pull(part) %>% unique()

OPO <- OPO_raw %>%
  mutate(
    remaining_to_be_received = parse_number(remaining_to_be_received),
    exp_receipt_date = str_replace_all(exp_receipt_date, "[^0-9]", "") %>% mdy(),
    revised_commit_date = str_replace_all(revised_commit_date, "[^0-9]", "") %>% na_if("") |> mdy(),
    arrival_date = coalesce(revised_commit_date, exp_receipt_date),
    arrival_sow = case_when(
      wday(arrival_date, week_start = 1) %in% 1:2 ~ floor_date(arrival_date, "week", week_start = 1),
      .default = ceiling_date(arrival_date, "week", week_start = 1)
    )
  ) %>% select(
    product, order_no, company_name,
    remaining_to_be_received,
    exp_receipt_date, revised_commit_date, arrival_date, arrival_sow
  ) %>%
  filter(
    product %in% MRP_parts,
    remaining_to_be_received != 0,
  ) %>%
  arrange(arrival_date)

OPO %>% write_csv(
  "data/processed_OPO.csv"
)
