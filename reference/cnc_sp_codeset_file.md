# Clinical Events & Specialties Codeset File Sample

A sample version of the file structure expected for the `codeset_tbl`
parameter in the `cnc_sp_process` function. The user should recreate
this file and include their own domain definitions.

## Usage

``` r
cnc_sp_codeset_file
```

## Format

### `cnc_sp_codeset_file`

A data frame with 5 columns

- domain:

  A string label for the domain of interest

- domain_tbl:

  The name of the CDM table where the variable can be found

- concept_field:

  The field in the default_tbl that should be used to join to the
  codeset

- date_field:

  The date field in the default_tbl that should be used to filter the
  dataset to the cohort period and for longitudinal analyses

- vocabulary_field:

  (PCORnet only) The name of the column in the domain table where the
  vocabulary type is stored

- codeset_name:

  The name of the codeset as found in the specs directory; file
  extension should not be included

## Details

The codeset listed in the `codeset_name` column should be kept in the
`file_subdirectory` indicated in `initialize_dq_session`
