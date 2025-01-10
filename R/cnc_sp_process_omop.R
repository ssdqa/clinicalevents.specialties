#' Concordance: Clinical Events and Specialties -- OMOP
#'
#' This is a check that will assess quality of specialty data in a study sample
#' The user will provide a clinical codeset of interest with an associated domain
#' and will be able to stratify results by:
#'                visit type (user will provide groupings)
#'                time
#' @param cohort - A dataframe with the cohort of patients for your study. Should include the columns:
#'          `site` | `person_id` | `start_date` | `end_date`
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#'                      - 'single': run on a single site, or treat all of the sites as one
#'                      - 'multi': run on a group of sites, treating each site separately
#' @param age_groups If you would like to stratify the results by age group,  create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age`: the minimum age for the group (i.e. 10)
#' - `max_age`: the maximum age for the group (i.e. 20)
#' - `group`: a string label for the group (i.e. 10-20, Young Adult, etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as NULL
#' @param codeset_tbl table in the specs directory with the columns:
#' - domain: name of the domain
#' - domain_tbl: name of the cdm_tbl
#' - concept_field: column name in the domain_tbl for which to search the codeset concept_ids
#' - date_field: column name in the domain_tbl to be used for time-based filtering
#' - codeset_name: name of a codeset that exists as a csv file in the specs directory. The codeset can optionally contain a `cluster` column specifying subgroups of the codeset, and if so, the results will be stratified by cluster
#' @param care_site TRUE if want to look at care_site specialty
#'                  FALSE if do not want to look at care_site specialty
#' @param provider TRUE if want to look at provider specialty
#'                  FALSE if do not want to look at provider specialty
#'                  IF both `provider` and `care_site` are both TRUE,
#'                  provider specialty will be prioritized if provider and care_site are discordant for the visit
#' @param visit_detail TRUE if want to use the visit_detail table to identify specialty visits
#'                     FALSE if want to use visit_occurrence table (default)
#' @param visit_type_tbl - a csv file that defines available visit types that are called in `visit_types.` defaults to the provided
#'                           `conc_visit_types.csv` file, which contains the following fields:
#'                           - visit_concept_id: the visit_concept_id that represents the visit type of interest (i.e. 9201)
#'                           - visit_type: the string label to describe the visit type; this label can be used multiple times
#'                                          within the file if multiple visit_concept_ids represent the visit type
#'
#'                           This CSV can be altered to fit the users needs, or another csv with the same columns and formatting can be supplied.
#' @param time TRUE if results should be over time. Defaults to FALSE
#' @param time_span if time=TRUE, vector containing minimum and maximum dates over which to measure
#' @param time_period if time=TRUE, indicates time period (e.g. 'year', 'month') over which to measure
#' @param vocab_tbl location of vocabulary table containing concept_id to concept_name mapping. If a vocabulary table is not available, will default to NULL
#' @return 2 tables:
#'            1 table stored as a csv file in the `results` directory containing all of the specialties in the results of the
#'            DQ check, with the columns:
#'                  - specialty_concept_id: an identifier for the specialty based on the data model
#'                  - specialty_concept_name: if a `vocab_tbl` is provided, the name of the specialty
#'                    that corresponds to each specialty_concept_id. If no `vocab_tbl` is provided,
#'                    defaults to 'No vocabulary table input'
#'            1 table containing counts of visits, optionally stratified by visit and/or time period,
#'            with each specialty for the visits meeting criteria (i.e. those with the clinical fact provided)
#'
#'
cnc_sp_process_omop <- function(cohort,
                                multi_or_single_site='multi',
                                age_groups=NULL,
                                codeset_tbl=NULL,
                                care_site,
                                provider,
                                visit_detail,
                                visit_type_tbl=NULL,
                                time=FALSE,
                                time_span=c('2012-01-01', '2020-01-01'),
                                time_period='year',
                                vocab_tbl=NULL){

  message('Preparing cohort')
  ## Step 0: Site check
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  ## Step 1: Prepare cohort
  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = NULL, codeset = NULL)

  ## Include age groups, if desired
  if(is.data.frame(age_groups)){
    grouped_list_prep<-grouped_list%>%
      append('age_grp')
  }else{grouped_list_prep<-grouped_list}

  ### Include visit types, if desired
  if(is.data.frame(visit_type_tbl)){
    grouped_list_prep<-grouped_list_prep%>%
      append('visit_concept_id')
  }

  ## Step 2: Run function
  message('Computing specialty concordance')
  site_output<-list()
  # not over time
  if(!time){
    for(k in 1:length(site_list_adj)){
      site_list_thisrnd <- site_list_adj[[k]]
      # filters by site
      cohort_site <- cohort_prep %>% filter(!!sym(site_col)%in%c(site_list_thisrnd))

      conc_site <- compute_conc_omop(cohort=cohort_site,
                                     grouped_list=grouped_list_prep,
                                     codeset_tbl=codeset_tbl,
                                     care_site=care_site,
                                     provider=provider,
                                     visit_detail = visit_detail,
                                     visit_type_tbl=visit_type_tbl,
                                     age_gp_tbl=age_groups)
      site_output[[k]]<-conc_site%>%mutate(site=site_list_thisrnd) %>% collect()
    }
    conc_tbl<-reduce(.x=site_output,
                       .f=dplyr::union)

  }
  else{
    # over time
    conc_tbl<-compute_fot(cohort=cohort_prep,
                            site_list=site_list_adj,
                            site_col=site_col,
                            time_span=time_span,
                            time_period=time_period,
                            reduce_id=NULL,
                            check_func=function(dat){
                              compute_conc_omop(cohort=dat,
                                                grouped_list=grouped_list_prep,
                                                codeset_tbl=codeset_tbl,
                                                care_site=care_site,
                                                provider=provider,
                                                visit_detail = visit_detail,
                                                visit_type_tbl=visit_type_tbl,
                                                age_gp_tbl=age_groups,
                                                time=TRUE)
                            })

    conc_tbl <- conc_tbl %>% collect()
  }

  ## Pulling specialty name
  spec_names<-join_to_vocabulary(tbl=conc_tbl,
                                 vocab_tbl=vocab_tbl,
                                 col='specialty_concept_id',
                                 vocab_col = 'concept_id')%>%
    distinct(specialty_concept_id, concept_name)%>%
    rename(specialty_concept_name=concept_name) %>% collect()

  output_list <- list('cnc_sp_process_names' = spec_names,
                      'cnc_sp_process_output' = conc_tbl %>% replace_site_col())

  cli::cli_inform('Both tables required for output generation have been output in a list.')
  cli::cli_alert_warning('Be sure to classify the specialties into groups in the specialty_name field of {.code cnc_sp_process_names}')

  return(output_list)
}
