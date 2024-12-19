pacman::p_load(tidyverse, nomisr, janitor, lemon, sf)
# get the list of nomis datasets
nd_tbl <- nomis_data_info()
# have a look
nd_tbl |> glimpse()

sex_by_age_id <- nd_tbl |> 
  filter(name.value |> str_detect("RM121")) |>
  pull(id)
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
# TYPE423 is the LA geography for 2021 census

woe_la_codes_tbl <- nomis_get_metadata(sex_by_age_id, 
                                       concept = "GEOGRAPHY",
                                       type = "TYPE423",
                                       time = "latest") |> 
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |> 
  glimpse()

woe_la_geogs <- woe_la_codes_tbl |> pull(id)

nomis_lsoa <- nomis_get_metadata(age_id, concept = "GEOGRAPHY", type = "TYPE151", time = "latest") |> 
  glimpse()

# Now we get a list of the West of England LSOAs
woe_lsoa_codes_tbl <- nomis_lsoa |> 
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |> 
  glimpse()

woe_lsoa_geogs <- woe_lsoa_codes_tbl |> pull(id)

# Now we get the sex by LSOA data for WoE
sex_raw_tbl <- nomis_get_data(id = sex_id,
                              geography = woe_lsoa_geogs,
                              time = "latest",
                              tidy = TRUE)

age_raw_tbl <- nomis_get_data(id = age_id,
                              geography = woe_lsoa_geogs,
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
  # remove the total and all persons
  filter(c_2021_age_24_name != "Total",
         c_sex_name != "All persons") |>
  mutate(c_2021_age_24_fct = str_remove(c_2021_age_24_code, "_") |> 
           as.integer(),
         c_2021_age_24_code = NULL,
         c_2021_age_24_name = fct_reorder(c_2021_age_24_name, c_2021_age_24_fct),
    # collapse the age bands as some are not five year in the original data
         five_year_bands = fct_collapse(
           c_2021_age_24_name,
           "0-4" = c("Aged 2 years and under", "Aged 3 to 4 years"),
           "5-9" = c("Aged 5 to 7 years", "Aged 8 to 9 years"),
           "10-14" = c("Aged 10 to 14 years"),
           "15-19" = c("Aged 15 years",
                       "Aged 16 to 17 years",
                       "Aged 18 to 19 years"),
           "20-24" = c("Aged 20 to 24 years"),
           "25-29" = c("Aged 25 to 29 years"),
           "30-34" = c("Aged 30 to 34 years"),
           "35-39" = c("Aged 35 to 39 years"),
           "40-44" = c("Aged 40 to 44 years"),
           "45-49" = c("Aged 45 to 49 years"),
           "50-54" = c("Aged 50 to 54 years"),
           "55-59" = c("Aged 55 to 59 years"),
           "60-64" = c("Aged 60 to 64 years"),
           "65-69" = c("Aged 65 years", "Aged 66 to 69 years"),
           "70-74" = c("Aged 70 to 74 years"),
           "75-79" = c("Aged 75 to 79 years"),
           "80-84" = c("Aged 80 to 84 years"),
           "85+" = c("Aged 85 years and over")
          )) |>
  # group to get the total for five year bands
  group_by(geography_name, c_sex_name, five_year_bands) |>
  summarise(obs_value = sum(obs_value),
            order_fct = mean(c_2021_age_24_fct)) |>
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

pyramid_plot_tbl |> 
  ggplot(mapping = aes(
    x = ifelse(c_sex_name == "Male",
               -obs_value, obs_value),
    y = five_year_bands,
    fill = c_sex_name)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x = "Population",
       y = "Age band",
       fill = "Sex",
       title = "Age pyramids for Unitary Authorities") +
  facet_wrap(~geography_name) +
  theme_minimal()
