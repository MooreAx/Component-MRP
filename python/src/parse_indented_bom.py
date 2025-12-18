#read indented bom into pandas

import pandas as pd
import janitor

df = pd.read_excel("data/Indented BOM.xlsx", skiprows=1).clean_names()


#rename
df = (
    df
    .rename(columns={"path_": "critical_path",
                    "quantity_per_1": "extended_quantity_per",
                    "scrap": "bom_scrap",
                    "yield": "yield_fraction",
                    "this_level": "lead_time_this_level",
                    "cumulative": "lead_time_cumulative",
                    "type": "part_source_type",
                    "supplier": "part_source_supplier",
                    "pool": "part_source_pool",
                    "first_date": "demand_window_first_date",
                    "last_date": "demand_window_last_date",
                    "type_1": "product_type",
                    "status": "product_status",
                    "expiry_offset": "interval_to_expiry_offset",
                    "part_1": "parent_part",
                    "site_1": "parent_site",
                    "basekey": "bom_basekey",
                    "code": "bom_code"})
)

# if level loaded as floats/strings, coerce to int (safe-ish)
df['level'] = pd.to_numeric(df['level'], errors='coerce').fillna(0).astype(int)

# create a group id that increments whenever a level 0 row appears
grp = (df['level'] == 0).cumsum()

# for each group take the first 'part' value and broadcast it to all rows in group
df['top_level_0_assembly'] = df.groupby(grp)['part'].transform('first')

df.to_csv("data/Processed_BOM.csv")