# Script to elucidate the codes needed to introspect nomis data

library("tidyverse")
library("nomisdata")
library("httr2")
library("skimr")
library("sf")


# get the api key from a config file and persist it
nomis_id <- config::get("uid", config = "nomis", file = "../config.yml")
set_api_key(nomis_id, persist = TRUE)
#enable caching to avoid re - downloading
enable_cache(file.path(tempdir(), "nomis_cache"))
# a pre defined url from the nomis API
#employment rate
"https://www.nomisweb.co.uk/api/v01/dataset/NM_189_1.data.csv?geography=1778384918...1778384921&industry=37748736&employment_status=1&measure=1&measures=20100"

nomis_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190614...1774190617&date=latestMINUS84,latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=45&measures=20599,21001,21002,21003"
httr2::url_parse(nomis_url)

nm_17_5_tbl <- read_csv(nomis_url)
nm_17_5_tbl |> skim()
dataset <- "NM_17_1"
ds <- "NM_1_1"


# get all geography TYPE codes
types <- get_codes(ds, "geography", "TYPE") |>
  view()

# get all geographies for a specific type
#  - in this case local authorities post 2023

types <- get_codes(ds, "geography", "TYPE424") |>
  glimpse()

# or LSOA from 2021 - but no LSOA21CD !!!
get_codes(ds, "geography", "TYPE151", "*bristol*") |>
  view()

emp_data_tbl <- fetch_nomis(
  dataset,
  geography = c("E06000022", "E06000023", "E06000024", "E06000025"),
  measures = 20100
)

# Datasets

all_datasets_tbl <- search_datasets("*")

# filter for a dataset where you know the TS (Topic Summary) number
# or other survey number
# https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?theme=93
all_datasets_tbl |>
  filter(str_starts(name.value, "TS058"))
# a specific term
ttw <- search_datasets("*Travel*")

travel_to_work_ts058_tbl <- ttw |>
  select(
    id,
    annotations.annotation,
    components.dimension,
    components.attribute,
    name.value
  ) |>
  filter(str_starts(name.value, "TS058"))

ttw_ds_id <- travel_to_work_ts058_tbl |>
  pull(id)

# this will get all the geographical codes for the dataset
# which match the search criteria
# it doesn't indicate the geography type, but codes in the form
# Bristol 001A are LSOA
fetch_codelist(ttw_ds_id, "geography", "*bristol*") |> view()

ttw_tbl <- fetch_nomis(
  ttw_ds_id,
  time = "latest",
  geography = c("TYPE424", "TYPE151"),
  measures = 20100
  # select = c("DATE", "GEOGRAPHY_CODE", "OBS_VALUE")
)
# some datasets already have geography names
add_geography_names(ttw_tbl, dataset_id = ttw_ds_id) |>
  glimpse()

ttw_tbl |>
  filter(GEOGRAPHY_TYPECODE == "151") |>
  glimpse()

bristol <- lookup_geography("Bristol")

dataset_overview(dataset) |> view()


search_datasets(keywords = "employment") |>
  select(id, uri, description.value, name.value) |>
  view()

describe_dataset() |>
  view()

get_codes("NM_17_1")
explore_dataset("NM_17_1")

#fetch spatial limited to 1000 records due to sourced from kml
unemployment_spatial <- fetch_spatial(
  "NM_1_1",
  time = "latest",
  geography = "TYPE151", # Regions
  measures = 20201, # Rate
  sex = 7,
  parse_sf = TRUE # Parse to sf object
)
