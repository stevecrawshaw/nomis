# NOMIS extraction reference

On-demand reference for [`SKILL.md`](SKILL.md). West of England areas, the `MAKE` aggregate syntax, geography vintages, and call-efficiency notes.

## West of England geography codes

The numeric NOMIS geography code is global — the same value works in every dataset that carries that area.

| Area | GSS | NOMIS code |
|---|---|---|
| Bath and North East Somerset | E06000022 | 1778384918 |
| Bristol, City of | E06000023 | 1778384919 |
| North Somerset | E06000024 | 1778384920 |
| South Gloucestershire | E06000025 | 1778384921 |
| West of England Combined Authority | E47000009 | 1853882377 |
| West of England LEP | E37000037 | 1925185566 |

### What "WECA" / "West of England" / "LEP" means as a footprint

Analysts almost always mean **all four unitary authorities**: North Somerset + Bristol + South Gloucestershire + Bath and North East Somerset. North Somerset joined the combined authority in 2025; the LEP already covered all four.

No single native NOMIS code reliably covers all four:

- The combined-authority code (E47000009) and the ITL2 "West of England" (TLK5) historically cover **Bristol + BANES + South Gloucestershire only** — they exclude North Somerset.
- ITL3: `TLK51` Bristol; `TLK52` Bath & North East Somerset and South Gloucestershire; `TLK61` North Somerset (which sits under ITL2 `TLK6` "North Somerset, Somerset and Dorset").

**For any WECA / West of England / LEP total or statistic, build the four-authority aggregate with `MAKE`** (below). Only use a native code if the user explicitly wants the three-authority combined-authority boundary.

## MAKE: custom aggregate areas

Combine codes within a dimension into one user-defined total. Works for `geography`, age, `industry`, `occupation`.

```
MAKE|name|code1;code2;code3
```

The result is a single row labelled `name`. Standard WECA / West of England / LEP total:

```
geography = MAKE|WECA|1778384918;1778384919;1778384920;1778384921
```

For a WECA total broken down by another dimension (age, sex), keep that dimension in the filter as normal — `MAKE` only collapses geography.

## Latest geography vintages

Analysts want the most recent boundary set. Prefer:

| Level | Type | Vintage |
|---|---|---|
| Unitary / district | `TYPE424` | April 2023 |
| Unitary / county | `TYPE423` | April 2023 |
| LSOA | `TYPE151` | 2021 |
| MSOA | `TYPE152` | 2021 |
| Ward | `TYPE182` | May 2025 |
| ITL1 / ITL2 / ITL3 | `TYPE419` / `TYPE420` / `TYPE421` | January 2025 |
| Combined authority | `TYPE442` | May 2025 |
| LEP | `TYPE459` | April 2021 |

Type codes differ between datasets (LEP is `TYPE215` in some). Always take the actual code from `get_dataset_dimensions`; the table is the common case.

## All areas of a level within a parent

To pull every child area of a level inside a parent, append the type to the parent code:

```
geography = 1778384919TYPE151              all 2021 LSOAs in Bristol
geography = 1778384919TYPE151,1778384921TYPE151   Bristol + South Gloucestershire LSOAs
```

This avoids resolving hundreds of child codes by hand. Do not `search_codes` a geography type like `TYPE151` or `TYPE182` just to browse — those return hundreds of rows.

## time filter shorthands

`latest`, `latestMINUS12`, `previous`, `prevyear`, `first`, or a date: `2024`, `2026-07`.

## search_datasets multi-word bug

Multi-word queries and field-prefixed patterns (`name-*population*`) throw `'NoneType' object has no attribute 'get'`. Use one bare keyword and filter the results yourself. Census codes work as-is (`TS009`, `RM121`).

## Efficient call sequence

1. `search_datasets` — one bare keyword.
2. `get_dataset_dimensions` — the dataset's dimensions and its geography type codes.
3. `search_codes` for every dimension you filter — **one parallel batch**.
4. Optional single `fetch_data` probe to confirm the geography string resolves.
5. `fetch_data_to_file` for the real download.

`check_auth` is worth one call if a result row count looks like a round 25,000 — without `NOMIS_UID` the API silently caps there.

## Known dataset quirks

- **NM_2014_1** (small-area population by single year of age): `c_age` code `0` is "All Ages", not age 0. It returns a duplicated "All Ages" row per area/sex — `SELECT DISTINCT` when loading. `gender`: 0 Total, 1 Male, 2 Female. `measures`: 20100 is the count.
- **NM_1_1** (claimant / JSA): `sex` 7 is total; `item` 1 is the headline count; `measures` 20100 is persons.
- **NM_162_1** (Claimant count by sex and age): the modern UC+JSA measure, monthly series populated back to January 1986. `gender` 0/1/2 = Total/Male/Female; `age` 0 = "All categories: Age 16+", with nested bands (2/3/4 headline, 10-20 five-year). `measure` 1 = count, 2 = proportion of residents 16-64 (returned only on the age-16+ rows — blank on any age-split row); `measures` 20100. Counts are rounded to the nearest 5, so a MAKE aggregate can differ from a manual sum by a few units.
