# Clinical Events & Specialties Visit File Sample – PCORnet version

A sample version of the file structure expected for the `visit_type_tbl`
parameter in the `cnc_sp_process` function. The user should recreate
this file and include their own domain definitions.

## Usage

``` r
cnc_sp_visit_file_pcornet
```

## Format

### cnc_sp_visit_file_pcornet

A data frame with 2 columns

- enc_type:

  The enc_type as it appears in the encounter table

- visit_type:

  A string to label the visit type of the enc_type
