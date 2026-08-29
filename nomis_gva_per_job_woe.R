# GVA per job for the four West of England unitary authorities, plus a WECA total
# NOMIS NM_2400_1: UK small area gross value added estimates (experimental)
# NOMIS NM_189_1:  Business Register and Employment Survey (open access)
# Generated 2026-08-29 from a nomis-extract session. Re-run to refresh.
# Source: https://www.nomisweb.co.uk/datasets/NM_2400_1
#         https://www.nomisweb.co.uk/datasets/NM_189_1
#
# CAVEATS
# - GVA is current prices, so the series is nominal. Deflate before reading
#   any post-2021 rise as real productivity growth.
# - This is GVA per job, not ONS's GVA per hour. BRES excludes most
#   self-employment, so it does not reconcile with published productivity.
# - BRES job counts are rounded to the nearest 1,000 at local authority level.
# - Both series are workplace-based: output is booked where it is produced,
#   which inflates districts hosting large-output employers.

library(nomisdata)
library(dplyr)
library(readr)

# nomisdata reads NOMIS_API_KEY; the nomis MCP uses NOMIS_UID. Same key.
# Without one, NOMIS silently caps every result at 25,000 rows.
key <- Sys.getenv("NOMIS_API_KEY", unset = Sys.getenv("NOMIS_UID"))
if (!nzchar(key)) stop("Set NOMIS_API_KEY in ~/.Renviron")
set_api_key(key)

woe_las <- c(
  "1778384918",  # Bath and North East Somerset
  "1778384919",  # Bristol, City of
  "1778384920",  # North Somerset
  "1778384921"   # South Gloucestershire
)

# A single extra row totalling the four authorities. No native NOMIS code
# covers all four: the combined authority code excludes North Somerset.
woe_total <- "MAKE|WECA|1778384918;1778384919;1778384920;1778384921"

woe_geographies <- c(woe_las, woe_total)

gva_cols <- c("date_name", "geography_name", "geography_code", "obs_value")

# GVA runs 1998-2023; the last 10 years only.
gva_tbl <- fetch_nomis(
  "NM_2400_1",
  geography = woe_geographies,
  time      = 2014:2023,
  measures  = 20100,  # value
  cell      = 0,      # Gross value added in pounds million
  select    = gva_cols
) |>
  tidy_names() |>
  rename(year = date_name, gva_gbp_m = obs_value)

# BRES runs 2015-2024, so it starts a year later than the GVA request above.
jobs_tbl <- fetch_nomis(
  "NM_189_1",
  geography         = woe_geographies,
  time              = 2015:2024,
  measures          = 20100,     # value
  industry          = 37748736,  # Total
  employment_status = 4,         # Employment (employees + working proprietors)
  measure           = 1,         # Count
  select            = gva_cols
) |>
  tidy_names() |>
  rename(year = date_name, jobs = obs_value)

# Inner join: the derived series only exists across the 2015-2023 overlap.
# One row per area per year on both sides, so a one-to-one join must hold.
gva_per_job_tbl <- gva_tbl |>
  inner_join(
    jobs_tbl |> select(year, geography_code, jobs),
    by           = join_by(year, geography_code),
    relationship = "one-to-one"
  ) |>
  mutate(gva_per_job = round(gva_gbp_m * 1e6 / jobs)) |>
  arrange(geography_name, year)

stopifnot(nrow(gva_per_job_tbl) > 0)
glimpse(gva_per_job_tbl)

gva_per_job_tbl |>
  filter(geography_name == "WECA") |>
  select(year, gva_gbp_m, jobs, gva_per_job) |>
  print(n = Inf)

# write_csv(gva_per_job_tbl, "data/woe_gva_per_job.csv", na = "")
