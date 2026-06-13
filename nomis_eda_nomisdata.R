pacman::p_load(tidyverse, nomisdata, janitor, lemon, sf, ggthemes, ggtext, glue)
pacman::p_load(rsdmx)
# Find dataset IDs by name pattern (replaces nomis_data_info() + filter)
sex_by_age_id <- search_datasets(name = "*RM121*") |> pull(id)
sex_id        <- search_datasets(name = "*TS008*")  |> pull(id)
age_id        <- search_datasets(name = "*TS007A*") |> pull(id)

# WoE LA codes — TYPE423 is the LA geography for 2021 census
# get_codes() replaces nomis_get_metadata(); column names may differ from nomisr's label.en
woe_la_codes_tbl <- get_codes(sex_by_age_id,
                               concept = "geography",
                               type    = "TYPE423") |>
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |>
  glimpse()

woe_la_geogs <- woe_la_codes_tbl |> pull(id)

# LSOA codes — TYPE151 is the LSOA geography for 2021 census
# lsoa call takes a while
nomis_lsoa <- get_codes(age_id,
                         concept = "geography",
                         type    = "TYPE151") |>
  glimpse()

woe_lsoa_codes_tbl <- nomis_lsoa |>
  filter(label.en |> str_detect("Bristol|North Somerset|Bath|South Gloucestershire")) |>
  glimpse()

woe_lsoa_geogs <- woe_lsoa_codes_tbl |> pull(id)

# Fetch data — tidy_names() replaces the tidy = TRUE parameter in nomis_get_data()
sex_raw_tbl <- fetch_nomis(sex_id,
                            geography = woe_lsoa_geogs,
                            time      = "latest") |>
  tidy_names()

age_raw_tbl <- fetch_nomis(age_id,
                            geography = woe_lsoa_geogs,
                            time      = "latest") |>
  tidy_names()

sex_by_age_raw_tbl <- fetch_nomis(sex_by_age_id,
                                   geography = woe_la_geogs,
                                   time      = "latest") |>
  tidy_names()

pyramid_plot_tbl <- sex_by_age_raw_tbl |>
  select(geography_name,
         c2021_age_24_name,
         c2021_age_24_code,
         c_sex_name,
         obs_value) |>
  filter(c2021_age_24_name != "Total",
         c_sex_name != "All persons") |>
  mutate(c2021_age_24_fct = str_remove(c2021_age_24_code, "_") |>
           as.integer(),
         c2021_age_24_code = NULL,
         c2021_age_24_name = fct_reorder(c2021_age_24_name, c2021_age_24_fct),
         five_year_bands = fct_collapse(
           c2021_age_24_name,
           "0-4"  = c("Aged 2 years and under", "Aged 3 to 4 years"),
           "5-9"  = c("Aged 5 to 7 years", "Aged 8 to 9 years"),
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
           "85+"   = c("Aged 85 years and over")
         )) |>
  group_by(geography_name, c_sex_name, five_year_bands) |>
  summarise(obs_value = sum(obs_value),
            order_fct = mean(c2021_age_24_fct)) |>
  glimpse()

pivot_nomis <- function(raw_tbl, c_name) {
  q_name = enquo(c_name)
  raw_tbl |>
    pivot_wider(id_cols   = geography_code,
                names_from  = c(!!q_name, "measures_name"),
                values_from = obs_value,
                names_sep   = "_") |>
    clean_names()
}

sex_lsoa_tbl <- pivot_nomis(sex_raw_tbl, c_sex_name) |> glimpse()
age_lsoa_tbl <- pivot_nomis(age_raw_tbl, c2021_age_19_name) |> glimpse()

joined_tbl <- sex_lsoa_tbl |>
  left_join(age_lsoa_tbl, by = "geography_code") |>
  glimpse()

sex_long <- sex_lsoa_tbl |>
  select(ends_with("value"), -starts_with("all")) |>
  rename_with(~str_remove(., "_value"), everything()) |>
  pivot_longer(cols      = everything(),
               names_to  = "sex",
               values_to = "lsoa_population")

sex_long |>
  ggplot(aes(x = lsoa_population)) +
  geom_histogram(bins = 30) +
  facet_wrap(~sex)

chart_theme <- function() {
  theme_clean() +
    theme(legend.background  = element_blank(),
          axis.text          = element_text(size = 10),
          axis.title         = element_text(size = 12),
          legend.text        = element_text(size = 12),
          plot.title         = element_text(size = 14),
          plot.subtitle      = element_markdown(lineheight = 1.1),
          plot.background    = element_rect(linewidth = NULL))
}

pyramid_plot <- pyramid_plot_tbl |>
  ggplot(mapping = aes(
    x    = ifelse(c_sex_name == "Male", -obs_value, obs_value),
    y    = five_year_bands,
    fill = c_sex_name)) +
  geom_col() +
  scale_x_symmetric(labels = abs) +
  labs(x       = "Population",
       y       = "Age band",
       fill    = "Sex",
       title   = "Age pyramids for Unitary Authorities",
       caption = glue("Derived from NOMIS dataset RM121 ({sex_by_age_id}): 2021 census data.")) +
  facet_wrap(~geography_name) +
  theme_minimal() +
  chart_theme()

pyramid_plot

ggsave("plots/pyramid_plot_la.png",
       pyramid_plot,
       width  = 10,
       height = 8,
       units  = "in",
       bg     = "white")

make_mid_int <- function(range_chr) {
  if (str_detect(range_chr, "-")) {
    out <- range_chr |>
      str_split("-") |>
      pluck(1) |>
      as.numeric() |>
      mean() |>
      round()
  } else {
    out <- str_sub(range_chr, 1, 2) |>
      as.numeric() |>
      sum(2)
  }
}

age_sex_ods_lep_tbl <- pyramid_plot_tbl |>
  mutate(mid_range_int  = map_int(five_year_bands, ~make_mid_int(.x)),
         order_fct      = NULL,
         geography_name = str_remove(geography_name, ", City of")) |>
  rename("age_band"       = five_year_bands,
         "population"     = obs_value,
         "local_authority" = geography_name,
         "sex"            = c_sex_name)

age_sex_ods_lep_tbl |>
  write_csv("data/age_sex_ods_lep.csv", na = "")

age_lsoa_tbl |>
  mutate(total_percent = NULL) |>
  rename("lsoa_code" = geography_code) |>
  write_csv("data/age_lsoa.csv", na = "")
