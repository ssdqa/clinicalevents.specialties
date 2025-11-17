# Clinical Events and Specialties

This is a concordance module that will assess the types of specialty
care received by and quality of specialty data found in a study sample.
The user will provide a clinical codeset of interest (`codeset_tbl`)
with an associated domain and will be able to stratify results by: visit
type (with user-provided groupings in `visit_type_tbl`), cluster (an
additional column added to `codeset_tbl` with subgroupings), or time

## Usage

``` r
cnc_sp_process(
  cohort,
  multi_or_single_site = "single",
  anomaly_or_exploratory = "exploratory",
  omop_or_pcornet,
  age_groups = NULL,
  codeset_tbl,
  care_site = FALSE,
  provider = TRUE,
  visit_detail = FALSE,
  visit_type_tbl = NULL,
  time = FALSE,
  time_span = c("2012-01-01", "2020-01-01"),
  time_period = "year",
  vocab_tbl = NULL
)
```

## Arguments

- cohort:

  *tabular input* \|\| **required**

  The cohort to be used for data quality testing. This table should
  contain, at minimum:

  - `site` \| *character* \| the name(s) of institutions included in
    your cohort

  - `person_id` / `patid` \| *integer* / *character* \| the patient
    identifier

  - `start_date` \| *date* \| the start of the cohort period

  - `end_date` \| *date* \| the end of the cohort period

  Note that the start and end dates included in this table will be used
  to limit the search window for the analyses in this module.

- multi_or_single_site:

  *string* \|\| defaults to `single`

  A string, either `single` or `multi`, indicating whether a single-site
  or multi-site analysis should be executed

- anomaly_or_exploratory:

  *string* \|\| defaults to `exploratory`

  A string, either `anomaly` or `exploratory`, indicating what type of
  results should be produced.

  Exploratory analyses give a high level summary of the data to examine
  the fact representation within the cohort. Anomaly detection analyses
  are specialized to identify outliers within the cohort.

- omop_or_pcornet:

  *string* \|\| **required**

  A string, either `omop` or `pcornet`, indicating the CDM format of the
  data

  - `omop`: run the
    [`cnc_sp_process_omop()`](https://ssdqa.github.io/clinicalevents.specialties/reference/cnc_sp_process_omop.md)
    function against an OMOP CDM instance

  - `pcornet`: run the
    [`cnc_sp_process_pcornet()`](https://ssdqa.github.io/clinicalevents.specialties/reference/cnc_sp_process_pcornet.md)
    function against a PCORnet CDM instance

- age_groups:

  *tabular input* \|\| defaults to `NULL`

  If you would like to stratify the results by age group, create a table
  or CSV file with the following columns and use it as input to this
  parameter:

  - `min_age` \| *integer* \| the minimum age for the group (i.e. 10)

  - `max_age` \| *integer* \| the maximum age for the group (i.e. 20)

  - `group` \| *character* \| a string label for the group (i.e. 10-20,
    Young Adult, etc.)

  If you would *not* like to stratify by age group, leave as `NULL`

- codeset_tbl:

  *tabular input* \|\| **required**

  A table defining the clinical event of interest, containing the
  following:

  - `domain` \| *character* \| a string label for the domain where the
    event is defined

  - `domain_tbl` \| *character* \| the name of the CDM table where the
    event is defined

  - `concept_field` \| *character* \| the string name of the field in
    the domain table where the concepts are located

  - `date_field` \| *character* \| the name of the field in the domain
    table with the date that should be used for temporal filtering

  - `codeset_name` \| *character* \| name of the codeset with concepts
    defining the clinical event

  The codeset file identified by this table can optionally contain a
  `cluster` column specifying subgroups of the codeset, and if so, the
  results will be stratified by cluster

  To see an example of the structure of this file, see
  [`?clinicalevents.specialties::cnc_sp_codeset_file`](https://ssdqa.github.io/clinicalevents.specialties/reference/cnc_sp_codeset_file.md)

- care_site:

  *boolean* \|\| defaults to `FALSE`

  A boolean indicating whether care site/facility specialty values
  should be included in the analysis. If both `provider` and `care_site`
  are TRUE, provider specialty will be prioritized.

- provider:

  *boolean* \| defaults to `TRUE`

  A boolean indicating whether care provider specialty values should be
  included in the analysis. If both `provider` and `care_site` are TRUE,
  provider specialty will be prioritized.

- visit_detail:

  *boolean* \|\| defaults to `FALSE`

  For OMOP analyses only – a boolean indicating whether the visit_detail
  table should be used as the primary visit table to identify specialty
  visits. If left FALSE, visit_occurrence will be used.

- visit_type_tbl:

  *tabular input* \|\| defaults to `NULL`

  A table defining available visit types to be used as an optional
  stratification. This table should contain the following field:

  - `visit_concept_id` / `visit_detail_concept_id` or `enc_type` \|
    *integer* / *character* \| the visit type identifier that represents
    the visit type of interest (ex: 9201 or IP)

  - `visit_type` \| *character* \| a string description of the visit
    type

  To see an example of the structure of this file, see
  [`?clinicalevents.specialties::cnc_sp_visit_file_omop`](https://ssdqa.github.io/clinicalevents.specialties/reference/cnc_sp_visit_file_omop.md)
  or
  [`?clinicalevents.specialties::cnc_sp_visit_file_pcornet`](https://ssdqa.github.io/clinicalevents.specialties/reference/cnc_sp_visit_file_pcornet.md)

- time:

  *boolean* \|\| defaults to `FALSE`

  A boolean to indicate whether to execute a longitudinal analysis

- time_span:

  *vector - length 2* \|\| defaults to `c('2012-01-01', '2020-01-01')`

  A vector indicating the lower and upper bounds of the time series for
  longitudinal analyses

- time_period:

  *string* \|\| defaults to `year`

  A string indicating the distance between dates within the specified
  time_span. Defaults to `year`, but other time periods such as `month`
  or `week` are also acceptable

- vocab_tbl:

  *tabular input* \|\| defaults to `NULL`

  A vocabulary table containing concept names that will be used to
  retrieve labels for the specialty concepts (ex: the OMOP concept
  table)

## Value

This function will return two dataframes:

- A table containing all of the specialties associated with the clinical
  event, which can be further grouped by the user before feeding this
  into the cnc_sp_output function

- A table containing counts of visits, optionally stratified by visit,
  cluster, and/or time period, with each specialty for the visits
  meeting criteria (i.e. those with the clinical fact provided)

For a more detailed description of output specific to each check type,
see the PEDSpace metadata repository

## Examples

``` r
#' Source setup file
source(system.file('setup.R', package = 'clinicalevents.specialties'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'cnc_sp_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)
#> Connected to: :memory:@NA
#> To see environment settings, run `get_argos_default()`

## Turn off SQL trace for this example
config('db_trace', FALSE)

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000),
                #RSQLite does not store date objects,
                #hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Prepare input tables
cnc_sp_visit_tbl <- dplyr::tibble(visit_concept_id = c(9201,9202,9203),
                                  visit_type = c('inpatient', 'outpatient', 'emergency'))

cnc_sp_concept_tbl <- dplyr::tibble(domain = 'Hypertension',
                                    domain_tbl = 'condition_occurrence',
                                    concept_field = 'condition_concept_id',
                                    date_field = 'condition_start_date',
                                    vocabulary_field = NA,
                                    codeset_name = 'dx_hypertension')

#' Execute `cnc_sp_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
cnc_sp_process_example <- cnc_sp_process(cohort = cohort,
                                         omop_or_pcornet = 'omop',
                                         multi_or_single_site = 'single',
                                         anomaly_or_exploratory = 'exploratory',
                                         codeset_tbl = cnc_sp_concept_tbl,
                                         visit_type_tbl = cnc_sp_visit_tbl,
                                         time = FALSE) %>%
  suppressMessages()
#> ┌ Output Function Details ─────────────────────────────────────────┐
#> │ You can optionally use this dataframe in the accompanying        │
#> │ `cnc_sp_output` function. Here are the parameters you will need: │
#> │                                                                  │
#> │ Always Required: cnc_sp_process_output, cnc_sp_process_names     │
#> │ Required for Check: top_n                                        │
#> │ Optional: facet_vars, specialty_filter                           │
#> │                                                                  │
#> │ See ?cnc_sp_output for more details.                             │
#> └──────────────────────────────────────────────────────────────────┘

cnc_sp_process_example$cnc_sp_process_output
#> # A tibble: 1 × 7
#>   specialty_concept_id cluster          visit_type codeset_name num_visits site 
#>                  <dbl> <chr>            <chr>      <chr>             <int> <chr>
#> 1             38004446 Essential hyper… outpatient dx_hyperten…          5 comb…
#> # ℹ 1 more variable: output_function <chr>
cnc_sp_process_example$cnc_sp_process_names
#> # A tibble: 1 × 2
#>   specialty_concept_id specialty_concept_name   
#>                  <dbl> <chr>                    
#> 1             38004446 No vocabulary table input

#' Execute `cnc_sp_output` function
cnc_sp_output_example <-
  cnc_sp_output(cnc_sp_process_output =
                  cnc_sp_process_example$cnc_sp_process_output,
                cnc_sp_process_names =
                  cnc_sp_process_example$cnc_sp_process_names %>%
                    dplyr::mutate(specialty_name = 'General Pediatrics'),
                facet_vars = c('visit_type')) %>%
  suppressMessages()

cnc_sp_output_example


#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(cnc_sp_output_example)

{"x":{"data":[{"orientation":"v","width":1,"base":0.55000000000000004,"x":[0.5],"y":[0.89999999999999991],"text":"Specialty:  General Pediatrics <br />Number of Visits:  5 <br />Proportion:  1","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(255,77,111,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"General Pediatrics","legendgroup":"General Pediatrics","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":52.529680365296812,"r":7.3059360730593621,"b":37.260273972602747,"l":130.77625570776257},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Proportion of Visits with each of the top 15 Specialties","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.050000000000000003,1.05],"tickmode":"array","ticktext":["0.00","0.25","0.50","0.75","1.00"],"tickvals":[0,0.25,0.5,0.75,1],"categoryorder":"array","categoryarray":["0.00","0.25","0.50","0.75","1.00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":"","hoverformat":".2f"},"annotations":[{"text":"Proportion","x":0.5,"y":0,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis","yshift":-21.917808219178088},{"text":"Specialty","x":0,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis","xshift":-115.43378995433793},{"text":"outpatient","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":11.68949771689498},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,1.6000000000000001],"tickmode":"array","ticktext":["General Pediatrics"],"tickvals":[1],"categoryorder":"array","categoryarray":["General Pediatrics"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":"","hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","layer":"below","x0":0,"x1":1,"y0":0,"y1":23.37899543378996,"yanchor":1,"ysizemode":"pixel"}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"19577df60eca":{"x":{},"y":{},"fill":{},"text":{},"type":"bar"}},"cur_data":"19577df60eca","visdat":{"19577df60eca":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
