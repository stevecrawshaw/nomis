library("tidyverse")
library("nomisdata")
library("httr2")
library("skimr")


# get the ap key from a config file and persist it
nomis_id <- config::get("uid", config = "nomis", file = "../config.yml")
set_api_key(nomis_id, persist = TRUE)
# a pre defined url from the nomis API
nomis_url <- "https://www.nomisweb.co.uk/api/v01/dataset/NM_17_5.data.csv?geography=1774190614...1774190617&date=latestMINUS84,latestMINUS80,latestMINUS76,latestMINUS72,latestMINUS68,latestMINUS64,latestMINUS60,latestMINUS56,latestMINUS52,latestMINUS48,latestMINUS44,latestMINUS40,latestMINUS36,latestMINUS32,latestMINUS28,latestMINUS24,latestMINUS20,latestMINUS16,latestMINUS12,latestMINUS8,latestMINUS4,latest&variable=45&measures=20599,21001,21002,21003"
httr2::url_parse(nomis_url)

nm_17_5_tbl <- read_csv(nomis_url)

nm_17_5_tbl |> skim()

dataset <- "NM_17_1"

emp_data_tbl <- fetch_nomis(
  dataset,
  # time = "all",
  geography = c("E06000022", "E06000023", "E06000024", "E06000025"),
  measures = 20100
)

types <- get_codes(dataset, "geography", "TYPE") |>
  view()

fetch_codelist(dataset, "geography", "*bristol*") |> view()

bristol <- lookup_geography("Bristol")

dataset_overview(dataset) |> view()

add_geography_names(data, dataset_id = "NM_1_1")

search_datasets(keywords = "employment") |>
  select(id, uri, description.value, name.value) |>
  view()

describe_dataset() |>
  view()

get_codes("NM_17_1")
explore_dataset("NM_17_1")
