# Function to find the visit_occurrences during which patient had a dx of interest + specialty of interest

Function to find the visit_occurrences during which patient had a dx of
interest + specialty of interest

## Usage

``` r
find_fact_spec_conc_pcnt(
  cohort,
  fact_codes,
  fact_tbl,
  care_site,
  provider,
  vocabulary_field,
  time = FALSE
)
```

## Arguments

- cohort:

  table with at least person_id

- fact_codes:

  codeset with \_concept_id for the dx/px/etc of interest

- fact_tbl:

  cdm_tbl in which to search for fact_codes

- care_site:

  boolean indicating whether to search care_site (at visit level) for
  specialty

- provider:

  boolean indicating whether to search provider (at visit level) for
  specialty

- vocabulary_field:

  name of the field in the fact_tbl that indicates the vocabulary type
  of the codes

- time:

  boolean indicating whether the analysis should be conducted
  longitudinally

## Value

table with all occurrences of the fact_codes for the cohort, and visit
info only if visit was to a specialty in the codeset
