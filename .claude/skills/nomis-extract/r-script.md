# Reproducible R script from a confirmed spec

On-demand reference for [`SKILL.md`](SKILL.md) step 7. Translate the spec the user confirmed in step 5 into a standalone R script that sources the same rows as a tibble with the `nomisdata` package, then prove it does.

Write the script only after a successful fetch, so the row count and value total are available to check against. Follow the `tidyverse-patterns` skill for every line of R: native `|>`, `join_by()`, `.by =` instead of `group_by()` + `ungroup()`.

## Translating the MCP call

`fetch_data(dataset_id, filters, select)` maps onto `fetch_nomis()` argument for argument.

| `filters` key | `fetch_nomis()` argument |
| --- | --- |
| `geography`, `sex`, `measures`, `time`, `date` | named argument of the same name |
| any other dimension (`item`, `c_age`, `gender`, `age`, `measure`, `c2021_*`) | lower-case named argument, passed through `...` |
| a comma-joined code string `"a,b,c"` | a character vector `c("a", "b", "c")` |
| a `MAKE\|...` string or a `<code>TYPE<nnn>` string | one string, verbatim — both work unchanged |
| the `select` list | `select = c("date_name", ...)` |

`build_params()` upper-cases every `...` name and comma-joins its values, so lower-case R argument names produce the right query.

**`measures` is not `measure`.** `measures` is the near-universal value dimension (`20100`). `measure` is a real dimension in its own right in some datasets — `NM_162_1` uses it for count vs proportion. Conflating them returns plausible wrong numbers rather than an error.

## Template

```r
# <topic> for <areas>, <period>
# NOMIS <dataset_id>: <dataset name>
# Generated <YYYY-MM-DD> from a nomis-extract session. Re-run to refresh.
# Source: https://www.nomisweb.co.uk/datasets/<dataset_id>

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

claimants_woe_tbl <- fetch_nomis(
  "NM_162_1",
  geography = woe_las,
  time      = "latest",
  measures  = 20100,  # value
  gender    = 0,      # Total
  age       = 0,      # All categories: Age 16+
  measure   = 1,      # Claimant count
  select    = c("date_name", "geography_name", "geography_code", "obs_value")
) |>
  tidy_names() |>
  arrange(geography_name)

stopifnot(nrow(claimants_woe_tbl) > 0)
glimpse(claimants_woe_tbl)

# write_csv(claimants_woe_tbl, "data/claimants_woe.csv", na = "")
```

Rules for filling it in:

- **ASCII only.** A hook rejects any R file containing an em dash, smart quote or other non-ASCII character. Write `:` or `--` in comments, never an em dash, and spell out area names without accents.
- Name the tibble `<topic>_<area>_tbl`; name the file `nomis_<topic>_<area>.R` in the project root.
- **Every opaque code carries its label as an inline comment.** This is what makes the script auditable months later — a bare `gender = 0` is unreadable, `gender = 0,  # Total` is not.
- Lift code lists longer than two values into a named vector above the call, one code per line with its label.
- Apply the relevant quirk from [`reference.md`](reference.md) as a pipeline step, not a comment: `NM_2014_1` needs `distinct()` after `tidy_names()`; `NM_162_1` gets a comment that counts are rounded to the nearest 5, so a `MAKE` aggregate may differ from a manual sum by a few units.
- Include the `write_csv()` line commented out unless the user asked for a file.
- No analysis, charting or reshaping. The script sources data and stops.

## Divergences from the MCP

- **Environment variable.** `NOMIS_UID` for the MCP, `NOMIS_API_KEY` for `nomisdata`. The template's `Sys.getenv()` fallback covers both; keep it.
- **Default columns.** `fetch_data` defaults to five columns; `fetch_nomis()` with no `select` returns all ~40. Always pass `select` to match the confirmed spec.
- **Row caps.** `nomisdata` paginates at 100,000 records per request with a key, 25,000 without. A download that needed `fetch_data_to_file` still works, but says nothing on screen while it runs.
- `select` names may be given lower-case; `build_params()` upper-cases them and the response is snake-cased again by `tidy_names()`.

## Verify it

Always run the script and compare against the fetch that produced the spec. Do not claim reproducibility without this.

```bash
Rscript nomis_<topic>_<area>.R
```

Check two things against the MCP result:

1. row count matches
2. `sum(obs_value)` matches

If either diverges, fix the script — a dimension defaulted to a total, or `select` dropped a breakdown that was splitting rows. Report the comparison to the user with both figures. If the script cannot be made to match, say so and hand over the closest version with the divergence named.
