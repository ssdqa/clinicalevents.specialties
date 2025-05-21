
#' Source setup file
source(system.file('setup.R', package = 'clinicalevents.specialties'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'cnc_sp_process_test',
                      working_directory = getwd(),
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = system.file('extdata',
                                        package = 'clinicalevents.specialties'),
                      cdm_schema = NA)

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
                                         time = FALSE)

cnc_sp_process_example$cnc_sp_process_output
cnc_sp_process_example$cnc_sp_process_names

#' Execute `cnc_sp_output` function
cnc_sp_output_example <-
  cnc_sp_output(cnc_sp_process_output =
                  cnc_sp_process_example$cnc_sp_process_output,
                cnc_sp_process_names =
                  cnc_sp_process_example$cnc_sp_process_names %>%
                    dplyr::mutate(specialty_name = 'General Pediatrics'),
                facet_vars = c('visit_type'))

cnc_sp_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(cnc_sp_output_example)
