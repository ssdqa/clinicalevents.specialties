# Clinical Events & Specialties Visit File Sample – OMOP version

A sample version of the file structure expected for the `visit_type_tbl`
parameter in the `cnc_sp_process` function. The user should recreate
this file and include their own domain definitions.

## Usage

``` r
cnc_sp_visit_file_omop
```

## Format

### cnc_sp_visit_file_omop

A data frame with 2 columns

- visit\_(detail)\_concept_id:

  The visit_concept_id or visit_detail_concept_id as it appears in the
  visit_occurrence or visit_detail table

- visit_type:

  A string to label the visit type of the visit_concept_id
