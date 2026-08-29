---
name: nomis-extract
description: Extract a specific subset of ONS data from NOMIS via the nomis MCP. Use when the user wants NOMIS / nomisweb data, or ONS labour-market, census, or population figures for an area — claimant count, Annual Population Survey, census tables (TS/RM codes), population estimates, jobs, earnings — at local-authority, LSOA, MSOA, ward, or ITL level, including for the West of England, WECA, or its councils (Bath and North East Somerset, Bristol, North Somerset, South Gloucestershire). Also use when the user wants a reproducible R script that sources NOMIS data with the nomisdata package.
---

# NOMIS data extraction

Turn a vague data request into a confirmed spec, then download exactly that subset with the `mcp__nomis__*` tools. Work the steps in order. Build **the spec** as you go: dataset, one decision per dimension, columns, output. Call `fetch_data` or `fetch_data_to_file` only after the user has confirmed the written spec in step 5.

Read [`r-script.md`](r-script.md) at step 7, or whenever the user asks for an R script for NOMIS data. Read [`reference.md`](reference.md) whenever the request touches geography (any area, level, "all LSOAs in X", WECA / West of England / LEP totals), the `MAKE` aggregate syntax, or a `search_datasets` error.

## 1. Scope: topic, geography, timescale

Ask the user about all three axes in one batch of questions. Pin every axis before searching — a wrong dataset wastes the whole chain.

- **Topic** — the subject and measure (counts, rates, proportions?).
- **Geography** — which areas, and at which level (unitary authority, LSOA, MSOA, ward, ITL2/3, region). If they name WECA, the West of England, or the LEP, read `reference.md` for what that footprint means.
- **Timescale** — a single latest period, a specific date, or a time series.

## 2. Identify the dataset

- `search_datasets` with a **single bare keyword** (`claimant`, `earnings`, `population`, or a census code like `TS009`). `reference.md` explains why multi-word queries fail.
- Prefer `status` "Current (being actively updated)".
- `get_dataset_dimensions` to confirm the dataset offers the geography level from step 1 and the breakdowns the user wants.
- Tell the user the dataset id, its name, and a one-line description. Confirm it is the right one before continuing.

## 3. Columns: which dimensions appear in the output

- List every dimension `get_dataset_dimensions` returned.
- For each, ask the user: break it out in the output, or fix it to one value or a total?
- Ask which columns to keep. Default: date, geography name, geography code, value.

## 4. Filters: resolve each dimension to codes

- For each dimension the user is filtering, ask which values they want (which ages, which sexes, which industries).
- `search_codes` for every dimension — issue these calls in one parallel batch.
- Geography: use the latest vintage type from `reference.md`. Resolve each area name to its code. For every area of a level inside a parent (all wards in a council), use the `<parentcode>TYPE<nnn>` form. For a custom total across areas (a WECA figure), use `MAKE` — see `reference.md`.

## 5. Confirm the spec

Present the spec as a written block and get an explicit yes:

```
Dataset:   NM_x_x — <name>
Geography: <areas>, type TYPE<nnn>, <code list | parentTYPE form | MAKE string>
<dimension>: <selected labels>
...
Time:      <periods>
Measure:   <measure>
Columns:   <columns>
Output:    <inline rows | CSV file>
```

## 6. Fetch

- Estimate the row count: multiply the number of selected values across every dimension (geography x time x each dimension x measures). Small and wanted inline → `fetch_data`. Otherwise `fetch_data_to_file`.
- Read `total` and `truncated` in the response. Report a truncated result as incomplete; narrow the spec or switch to `fetch_data_to_file`. Never total or rank truncated rows.
- Report the file path, the row count against the estimate, and any per-dataset cleanup from `reference.md`.

## 7. Offer a reproducible R script

After a successful fetch, offer to write the same query as a standalone R script using the `nomisdata` package, so the analyst can re-run it without the MCP.

If the user accepts, read [`r-script.md`](r-script.md) and follow it: translate the confirmed spec into `fetch_nomis()`, write `nomis_<topic>_<area>.R` in the project root, then run it with `Rscript` and check its row count and `sum(obs_value)` against the fetch above. Report both figures.
