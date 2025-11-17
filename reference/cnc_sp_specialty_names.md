# Clinical Events & Specialties - Specialty Naming Sample

A sample version of how the list of specialties output by
`cnc_sp_process` should be labelled for use in the `cnc_sp_output`
function.

## Usage

``` r
cnc_sp_specialty_names
```

## Format

### `cnc_sp_specialty_names`

A data frame with 3 columns

- specialty_concept_id:

  The specialty_concept_id identified in `cnc_sp_process`

- specialty_concept_name:

  If a vocab_tbl was provided, the concept_name associated with the
  specialty_concept_id; otherwise will default to "No vocabulary table
  input"

- specialty_name:

  A user-provided identifier for the specialty; can be repeated across
  rows to group specialties together
