pacman::p_load(tidyverse, nomisr, janitor)
# get the list of nomis datasets
nd_tbl <- nomis_data_info()
# have a look
nd_tbl |> glimpse()
# find a dataset
nd_tbl |> 
  filter(name.value |> str_detect("sex")) |> 
  view()
# get lsoa list for this dataset (2011 = TYPE 298)
# NM_2028_1 is the 2021 dataset for sex , TYPE151 is the LSOA geography
# nmd gives a table of the UK LSOA's
nmd <- nomis_get_metadata("NM_2028_1", concept = "GEOGRAPHY", type = "TYPE151", time = "latest") |> 
  glimpse()

# Now we get a list of the West of England LSOAs
woe_codes_tbl <- nmd |> 
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |> 
  glimpse()

woe_geogs <- woe_codes_tbl |> pull(id)

# Now we get the sex by LSOA data for WoE
sex_raw_tbl <- nomis_get_data(id = "NM_2028_1",
                              geography = woe_geogs,
                              time = "latest",
                              tidy = TRUE)

# Now we re - shape the data so that there's a column for each measure
# and clean up the names
sex_raw_tbl |> 
  select(geography_code, obs_value, c_sex_name, measures_name) |> 
  pivot_wider(id_cols = geography_code,
              names_from = c("c_sex_name", "measures_name"),
              values_from = obs_value, names_sep = "_") |> 
  clean_names()




