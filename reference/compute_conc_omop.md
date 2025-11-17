# Function to compute fact + visit specialty concordance

Function to compute fact + visit specialty concordance

## Usage

``` r
compute_conc_omop(
  cohort,
  grouped_list = c("site"),
  codeset_tbl = read_codeset("conc_codesets", col_types = "cccc"),
  care_site,
  provider,
  visit_detail = FALSE,
  visit_type_tbl = NULL,
  age_gp_tbl = NULL,
  time = FALSE
)
```

## Arguments

- cohort:

  the cohort for which to look for events

- grouped_list:

  a vector to group input by. Defaults to `site`. If `year` is in
  `grouped_list`, results are returned by year of `visit_start_date`

- codeset_tbl:

  table in the file_subdirectory with the columns: domain: name of the
  domain domain_tbl: name of the cdm_tbl concept_field: column name in
  the domain_tbl for which to search the codeset concept_ids date_field:
  column name in the domain_tbl to use for date filtering codeset_name:
  name of a codeset in the specs directory

- care_site:

  boolean indicating whether to search care_site (at visit level) for
  specialty

- provider:

  boolean indicating whether to search provider (at visit level) for
  specialty

- visit_detail:

  TRUE if want to use the visit_detail table to identify specialty
  visits FALSE if want to use visit_occurrence table (default)

- visit_type_tbl:

  if provided, a map from visit_concept_id to a visit_type
  classification. if not required, should be `NULL`

- age_gp_tbl:

  if provided, table with the columns: min_age: minimum age, in years,
  for the given categorization max_age: maximum age, in years, for the
  given categorization group: label to be used for the categorization

- time:

  boolean indicating whether to compute over time or not

## Value

table with cols: codeset_name: name of the codeset in the specs
directory with event facts specialty_concept_id: concept_id of specialty
from either care_site or provider num_visits: number of visits with the
specialty+fact ... any columns in the `grouped_list`
