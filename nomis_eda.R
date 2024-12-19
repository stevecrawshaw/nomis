pacman::p_load(tidyverse, nomisr, janitor, lemon, sf)
# get the list of nomis datasets
nd_tbl <- nomis_data_info()
# have a look
nd_tbl |> glimpse()

sex_by_age_id <- nd_tbl |> 
  filter(name.value |> str_detect("RM121")) |> pull(id)
# find a dataset
sex_id <- nd_tbl |> 
  filter(name.value |> str_detect("TS008")) |> 
  pull(id)

age_id <- nd_tbl |> 
  filter(name.value |> str_detect("TS007A")) |> 
  pull(id)
# get lsoa list for this dataset (2011 = TYPE 298)
# 
# NM_2028_1 is the 2021 dataset for sex , TYPE151 is the LSOA geography FOR 2021 census
# 
# nmd gives a table of the UK LSOA's

woe_la_codes_tbl <- nomis_get_metadata(sex_by_age_id, concept = "GEOGRAPHY", type = "TYPE423", time = "latest") |> 
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |> 
  glimpse()

woe_la_geogs <- woe_la_codes_tbl |> pull(id)

nmd <- nomis_get_metadata(age_id, concept = "GEOGRAPHY", type = "TYPE151", time = "latest") |> 
  glimpse()

# Now we get a list of the West of England LSOAs
woe_codes_tbl <- nmd |> 
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |> 
  glimpse()

woe_geogs <- woe_codes_tbl |> pull(id)

# Now we get the sex by LSOA data for WoE
sex_raw_tbl <- nomis_get_data(id = sex_id,
                              geography = woe_geogs,
                              time = "latest",
                              tidy = TRUE)

age_raw_tbl <- nomis_get_data(id = age_id,
                              geography = woe_geogs,
                              time = "latest",
                              tidy = TRUE)

sex_by_age_raw_tbl <- nomis_get_data(id = sex_by_age_id,
                                     geography = woe_la_geogs,
                                     time = "latest",
                                     tidy = TRUE)


pyramid_plot_tbl <- sex_by_age_raw_tbl |> 
  select(geography_name,
         c_2021_age_24_name,
         c_2021_age_24_code,
         c_sex_name,
         obs_value) |>
  filter(c_2021_age_24_name != "Total",
         c_sex_name != "All persons") |>
  mutate(c_2021_age_24_fct = str_remove(c_2021_age_24_code, "_") |> 
           as.integer(),
         c_2021_age_24_code = NULL,
         c_2021_age_24_name = fct_reorder(c_2021_age_24_name, c_2021_age_24_fct),
         five_year_bands) |>
  glimpse()

# Now we re - shape the data so that there's a column for each measure
# and clean up the names
sex_raw_tbl |> 
  pivot_wider(id_cols = geography_code,
              names_from = c("c_sex_name", "measures_name"),
              values_from = obs_value, names_sep = "_") |> 
  clean_names()


age_raw_tbl |> 
  pivot_wider(id_cols = geography_code,
              names_from = c("c_2021_age_19_name", "measures_name"),
              values_from = obs_value, names_sep = "_") |> 
  clean_names() |> 
  glimpse()

# We can encapsulate this in a function
# The function takes the raw table and the column name
# and pivots the table

pivot_nomis <- function(raw_tbl, c_name) {
  q_name = enquo(c_name)
  raw_tbl |> 
    pivot_wider(id_cols = geography_code,
                names_from = c(!!q_name, "measures_name"),
                values_from = obs_value, names_sep = "_") |> 
    clean_names()
}

# use the function to create a table for sex and age

sex_lsoa_tbl <- pivot_nomis(sex_raw_tbl, c_sex_name) |> glimpse()

age_lsoa_tbl <- pivot_nomis(age_raw_tbl, c_2021_age_19_name) |> glimpse()


# Now we join
# 

joined_tbl <- sex_lsoa_tbl |> 
  left_join(age_lsoa_tbl, by = "geography_code") |> 
  glimpse()

# Now we can do some analysis
# pivot longer
sex_long <- sex_lsoa_tbl |> 
  select(ends_with("value"), -starts_with("all")) |> 
  rename_with(~str_remove(., "_value"), everything()) |> 
  pivot_longer(cols = everything(),
               names_to = "sex",
               values_to = "lsoa_population")

# make histograms for male and female
sex_long |> 
  ggplot(aes(x = lsoa_population)) +
  geom_histogram(bins = 30) +
  facet_wrap(~sex)


# Make an age pyramid by sex
# 
lsoa_geog_sf <- st_read("https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/lep_lsoa_geog/exports/geojson?lang=en&timezone=Europe%2FLondon")

lsoa_la_tbl <- lsoa_geog_sf |> 
  select(lsoa21cd, lad24cd, lad24nm) |>
  st_drop_geometry()  |> glimpse()

pyramid_plot_tbl |> 
  ggplot(mapping = aes(
    x = ifelse(c_sex_name == "Male",
               -obs_value, obs_value),
    y = c_2021_age_24_name,
    fill = c_sex_name)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x = "Population",
       y = "Age band",
       fill = "Sex") +
  facet_wrap(~geography_name) +
  theme_minimal()
