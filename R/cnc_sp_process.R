#' Clinical Events and Specialties
#'
#' This is a concordance module that will assess the types of specialty care received by and
#' quality of specialty data found in a study sample. The user will provide a
#' clinical codeset of interest (`codeset_tbl`) with an associated domain
#' and will be able to stratify results by: visit type
#' (with user-provided groupings in `visit_type_tbl`), cluster
#' (an additional column added to `codeset_tbl` with subgroupings), or time
#'
#' @param cohort *tabular input* | A dataframe with the cohort of patients for your study. Should include the columns:
#' - `site` | *character*
#' - `person_id` / `patid` | *integer* / *character*
#' - `start_date` | *date*
#' - `end_date` | *date*
#' @param multi_or_single_site *string* | Option to run the function on a single vs multiple sites
#'                      - `single`: run on a single site, or treat all of the sites as one
#'                      - `multi`: run on a group of sites, treating each site separately
#' @param anomaly_or_exploratory *string* | string indicating whether to generate output for anomaly detection or exploratory analysis:
#'                          - `anomaly` for anomaly detection
#'                          - `exploratory` for exploratory analysis
#' @param omop_or_pcornet *string* | Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [cnc_sp_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [cnc_sp_process_pcornet()] function against a PCORnet CDM instance
#' @param age_groups *tabular input* | If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age` | *integer* | the minimum age for the group (i.e. 10)
#' - `max_age` | *integer* | the maximum age for the group (i.e. 20)
#' - `group` | *character* | a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param codeset_tbl *tabular input* | table in the specs directory with the columns:
#' - `domain` | *character* | name of the domain
#' - `domain_tbl` | *character* | name of the cdm_tbl
#' - `concept_field` | *character* | column name in the domain_tbl for which to search the codeset concept_ids
#' - `date_field` | *character* | column name in the domain_tbl to be used for time-based filtering
#' - `codeset_name` | *character* | name of a codeset that exists as a csv file in the specs directory.
#'
#' The codeset can optionally contain a `cluster` column specifying subgroups of the codeset,
#' and if so, the results will be stratified by cluster
#'
#' @param care_site *boolean* | TRUE if want to look at care_site specialty
#'                  (specialty_concept_id in the care_site table / facility_type in the encounter table)
#'                  FALSE if do not want to look at care_site specialty
#' @param provider *boolean* | TRUE if want to look at provider specialty
#'                  (specialty_concept_id / provider_specialty_primary in the provider table)
#'                  FALSE if do not want to look at provider specialty
#'                  IF both `provider` and `care_site` are both TRUE,
#'                  provider specialty will be prioritized if provider and care_site are discordant for the visit
#' @param visit_detail *boolean* | OMOP ONLY -- TRUE if want to use the visit_detail table to identify specialty visits
#'                     FALSE if want to use visit_occurrence table (default)
#' @param visit_type_tbl *tabular input* | a table that defines available visit types that are called in `visit_types.` defaults to the provided
#'                       `cnc_sp_visit_file_(omop/pcornet)` file, which contains the following fields:
#' - `visit_concept_id` / `visit_detail_concept_id` or `enc_type`: the visit_(detail)_concept_id or enc_type that represents the visit type of interest (i.e. 9201 or IP)
#' - `visit_type`: the string label to describe the visit type; this label can be used multiple times
#'                 within the file if multiple visit_concept_ids/enc_types represent the visit type
#' @param time *boolean* | TRUE if results should be over time. Defaults to FALSE
#' @param time_span *vector - length 2* | if time=TRUE, vector containing minimum and maximum dates over which to measure
#' @param time_period *string* | if time=TRUE, indicates time period (e.g. 'year', 'month') over which to measure
#' @param vocab_tbl *tabular input* | location of vocabulary table containing concept_id to concept_name mapping. If a vocabulary table is not available, will default to NULL
#' @return 2 tables:
#'            1 table containing all of the specialties in the results of the
#'            DQ check, with the columns:
#'                  - `specialty_concept_id`: an identifier for the specialty based on the data model
#'                  - `specialty_concept_name`: if a `vocab_tbl` is provided, the name of the specialty
#'                    that corresponds to each specialty_concept_id. If no `vocab_tbl` is provided,
#'                    defaults to 'No vocabulary table input'
#'            1 table containing counts of visits, optionally stratified by visit and/or time period,
#'            with each specialty for the visits meeting criteria (i.e. those with the clinical fact provided)
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
