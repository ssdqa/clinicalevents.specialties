#' Clinical Events and Specialties
#'
#' This is a concordance module that will assess the types of specialty care received by and
#' quality of specialty data found in a study sample. The user will provide a
#' clinical codeset of interest (`codeset_tbl`) with an associated domain
#' and will be able to stratify results by: visit type
#' (with user-provided groupings in `visit_type_tbl`), cluster
#' (an additional column added to `codeset_tbl` with subgroupings), or time
#'
#' @param cohort *tabular input* || **required**
#'
#'   The cohort to be used for data quality testing. This table should contain,
#'   at minimum:
#'   - `site` | *character* | the name(s) of institutions included in your cohort
#'   - `person_id` / `patid` | *integer* / *character* | the patient identifier
#'   - `start_date` | *date* | the start of the cohort period
#'   - `end_date` | *date* | the end of the cohort period
#'
#'   Note that the start and end dates included in this table will be used to
#'   limit the search window for the analyses in this module.
#'
#' @param multi_or_single_site *string* || defaults to `single`
#'
#'   A string, either `single` or `multi`, indicating whether a single-site or
#'   multi-site analysis should be executed
#'
#' @param anomaly_or_exploratory *string* || defaults to `exploratory`
#'
#'   A string, either `anomaly` or `exploratory`, indicating what type of results
#'   should be produced.
#'
#'   Exploratory analyses give a high level summary of the data to examine the
#'   fact representation within the cohort. Anomaly detection analyses are
#'   specialized to identify outliers within the cohort.
#'
#' @param omop_or_pcornet *string* || **required**
#'
#'   A string, either `omop` or `pcornet`, indicating the CDM format of the data
#'
#'    - `omop`: run the [cnc_sp_process_omop()] function against an OMOP CDM instance
#'    - `pcornet`: run the [cnc_sp_process_pcornet()] function against a PCORnet CDM instance
#'
#' @param age_groups *tabular input* || defaults to `NULL`
#'
#'   If you would like to stratify the results by age group, create a table or
#'   CSV file with the following columns and use it as input to this parameter:
#'
#'   - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#'   - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#'   - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#'   If you would *not* like to stratify by age group, leave as `NULL`
#'
#' @param codeset_tbl *tabular input* || **required**
#'
#'   A table defining the clinical event of interest, containing the following:
#'   - `domain` | *character* | a string label for the domain where the event is defined
#'   - `domain_tbl` | *character* | the name of the CDM table where the event is defined
#'   - `concept_field` | *character* | the string name of the field in the domain table where the concepts are located
#'   - `date_field` | *character* | the name of the field in the domain table with the date that should be used for temporal filtering
#'   - `codeset_name` | *character* | name of the codeset with concepts defining the clinical event
#'
#'   The codeset file identified by this table can optionally contain a `cluster` column
#'   specifying subgroups of the codeset, and if so, the results will be stratified by cluster
#'
#'   To see an example of the structure of this file, see `?clinicalevents.specialties::cnc_sp_codeset_file`
#'
#' @param care_site *boolean* || defaults to `FALSE`
#'
#'   A boolean indicating whether care site/facility specialty values should be included
#'   in the analysis. If both `provider` and `care_site` are TRUE, provider specialty will
#'   be prioritized.
#'
#' @param provider *boolean* | defaults to `TRUE`
#'
#'   A boolean indicating whether care provider specialty values should be included
#'   in the analysis. If both `provider` and `care_site` are TRUE, provider specialty will
#'   be prioritized.
#'
#' @param visit_detail *boolean* || defaults to `FALSE`
#'
#'   For OMOP analyses only -- a boolean indicating whether the visit_detail table should
#'   be used as the primary visit table to identify specialty visits. If left FALSE, visit_occurrence
#'   will be used.
#'
#' @param visit_type_tbl *tabular input* || defaults to `NULL`
#'
#'   A table defining available visit types to be used as an optional stratification.
#'   This table should contain the following field:
#'   - `visit_concept_id` / `visit_detail_concept_id` or `enc_type` | *integer* / *character* | the visit type identifier that represents the visit type of interest (ex: 9201 or IP)
#'   - `visit_type` | *character* | a string description of the visit type
#'
#'   To see an example of the structure of this file, see `?clinicalevents.specialties::cnc_sp_visit_file_omop` or
#'   `?clinicalevents.specialties::cnc_sp_visit_file_pcornet`
#'
#' @param time *boolean* || defaults to `FALSE`
#'
#'   A boolean to indicate whether to execute a longitudinal analysis
#'
#' @param time_span *vector - length 2* || defaults to `c('2012-01-01', '2020-01-01')`
#'
#'   A vector indicating the lower and upper bounds of the time series for longitudinal analyses
#'
#' @param time_period *string* || defaults to `year`
#'
#'   A string indicating the distance between dates within the specified time_span.
#'   Defaults to `year`, but other time periods such as `month` or `week` are
#'   also acceptable
#'
#' @param vocab_tbl *tabular input* || defaults to `NULL`
#'
#'   A vocabulary table containing concept names that will be used to retrieve labels
#'   for the specialty concepts (ex: the OMOP concept table)
#'
#' @return This function will return two dataframes:
#' - A table containing all of the specialties associated with the clinical event, which can be further grouped by the user before feeding this into the cnc_sp_output function
#' - A table containing counts of visits, optionally stratified by visit, cluster, and/or time period, with each specialty for the visits meeting criteria (i.e. those with the clinical fact provided)
#'
#' For a more detailed description of output specific to each check type, see the PEDSpace metadata repository
#'
#'
#' @import argos
#' @import squba.gen
#' @import dplyr
#' @import cli
#' @importFrom purrr reduce
#' @importFrom stringr str_wrap
#'
#' @example inst/example-cnc_sp_process_output.R
#'
#' @export
#'
#'
cnc_sp_process <- function(cohort,
                           multi_or_single_site = 'single',
                           anomaly_or_exploratory = 'exploratory',
                           omop_or_pcornet,
                           age_groups=NULL,
                           codeset_tbl,
                           care_site=FALSE,
                           provider=TRUE,
                           visit_detail = FALSE,
                           visit_type_tbl=NULL,
                           time=FALSE,
                           time_span=c('2012-01-01', '2020-01-01'),
                           time_period='year',
                           vocab_tbl=NULL){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue'),
                            inform = list(color = 'green')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  # if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}
  if(!provider && !care_site){cli::cli_abort('Please set at least one of {.code provider} or {.care_site} to TRUE')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'cnc_sp',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    cnc_sp_rslt <- cnc_sp_process_omop(cohort = cohort,
                                       multi_or_single_site=multi_or_single_site,
                                       age_groups=age_groups,
                                       codeset_tbl=codeset_tbl,
                                       care_site = care_site,
                                       provider = provider,
                                       visit_detail = visit_detail,
                                       visit_type_tbl=visit_type_tbl,
                                       time=time,
                                       time_span=time_span,
                                       time_period=time_period,
                                       vocab_tbl=vocab_tbl)


  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    cnc_sp_rslt <- cnc_sp_process_pcornet(cohort = cohort,
                                          multi_or_single_site=multi_or_single_site,
                                          age_groups=age_groups,
                                          codeset_tbl=codeset_tbl,
                                          care_site = care_site,
                                          provider = provider,
                                          visit_type_tbl=visit_type_tbl,
                                          time=time,
                                          time_span=time_span,
                                          time_period=time_period,
                                          vocab_tbl=vocab_tbl)


  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  cnc_sp_rslt[[2]] <- cnc_sp_rslt[[2]] %>% mutate(output_function = output_type$string)

  print(cli::boxx(c('You can optionally use this dataframe in the accompanying',
  '`cnc_sp_output` function. Here are the parameters you will need:', '', output_type$vector, '',
  'See ?cnc_sp_output for more details.'), padding = c(0,1,0,1),
  header = cli::col_cyan('Output Function Details')))

  return(cnc_sp_rslt)

}
