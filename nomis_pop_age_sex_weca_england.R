# Population by five-year age band and sex for the four West of England
# unitary authorities, with England as a comparator, 2002-2025
# NOMIS NM_31_1: Population estimates - local authority based by five year age band
# Generated 2026-10-23 from a nomis-extract session. Re-run to refresh.
# Source: https://www.nomisweb.co.uk/datasets/NM_31_1
#
# CAVEATS
# - ONS mid-year estimates (30 June), April 2023 local authority boundaries.
# - estimate_status flags years before backcast_before as backcast. The cut-off
#   is an assumption (estimates rebased to the 2011 Census); check it against
#   the ONS release notes and change backcast_before if needed.
# - pct_of_total is the cell's share of that area's all-age, both-sex
#   population in that year, so areas of different size are comparable.

library(nomisdata)
library(dplyr)
library(readr)
library(stringr)

# nomisdata reads NOMIS_API_KEY; the nomis MCP uses NOMIS_UID. Same key.
# Without one, NOMIS silently caps every result at 25,000 rows.
key <- Sys.getenv("NOMIS_API_KEY", unset = Sys.getenv("NOMIS_UID"))
if (!nzchar(key)) {
  stop("Set NOMIS_API_KEY in ~/.Renviron")
}
set_api_key(key)

# First year treated as not backcast.
backcast_before <- 2011

weca_geographies <- c(
  "1778384918", # Bath and North East Somerset
  "1778384919", # Bristol, City of
  "1778384920", # North Somerset
  "1778384921", # South Gloucestershire
  "2092957699" # England (comparator)
)

# Under 1 and 1-4 are separate bands in this dataset; MAKE merges them into
# one 0-4 band. Codes 3-19 are 5-9 ... 80-84, 85 and over.
age_bands <- c(
  "MAKE|Aged 0 - 4 years|1;2",
  3:19
)

pop_age_sex_weca_england_tbl <- fetch_nomis(
  "NM_31_1",
  geography = weca_geographies,
  time = 2002:2025,
  measures = 20100, # value (count of persons)
  sex = c(5, 6), # 5 Male, 6 Female
  age = age_bands,
  select = c(
    "date_name",
    "geography_name",
    "geography_code",
    "sex_name",
    "age_name",
    "obs_value"
  )
) |>
  tidy_names() |>
  rename(year = date_name, population = obs_value) |>
  mutate(
    year = as.integer(year),
    estimate_status = if_else(year < backcast_before, "backcast", "post-2011"),
    pct_of_total = 100 * population / sum(population),
    age_name = str_remove(age_name, "Aged ") |>
      str_remove(" years") |>
      str_remove_all(" "),
    .by = c(geography_code, year)
  ) |>
  arrange(geography_name, year, sex_name)

stopifnot(nrow(pop_age_sex_weca_england_tbl) == 4320)
glimpse(pop_age_sex_weca_england_tbl)

write_csv(
  pop_age_sex_weca_england_tbl,
  "data/pop_age_sex_weca_england.csv",
  na = ""
)
