
## Test error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(cnc_sp_process(cohort = cht,
                              codeset_tbl = NULL,
                              multi_or_single_site = 'test',
                              omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(cnc_sp_process(cohort = cht,
                              codeset_tbl = NULL,
                              multi_or_single_site = 'single',
                              omop_or_pcornet = 'test'))
})

test_that('one of provider / care site must be true', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(cnc_sp_process(cohort = cht,
                              codeset_tbl = NULL,
                              multi_or_single_site = 'single',
                              omop_or_pcornet = 'omop',
                              provider = FALSE,
                              care_site = FALSE))
})


## testing that code runs
test_that('no time -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'cnc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  cnc_cdst_file <- dplyr::tibble('domain' = 'hypertension',
                                 'domain_tbl' = 'condition_occurrence',
                                 'concept_field' = 'condition_concept_id',
                                 'date_field' = 'condition_start_date',
                                 'codeset_name' = 'dx_hypertension')

  visit_type_file <- dplyr::tibble('visit_concept_id' = 9202,
                                   'visit_type' = 'outpatient')

  expect_no_error(cnc_sp_process(cohort = cohort,
                                 multi_or_single_site = 'single',
                                 omop_or_pcornet = 'omop',
                                 codeset_tbl = cnc_cdst_file,
                                 care_site = FALSE,
                                 provider = TRUE,
                                 visit_type_tbl = visit_type_file))

})

test_that('time -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'cnc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  cnc_cdst_file <- dplyr::tibble('domain' = 'hypertension',
                                 'domain_tbl' = 'condition_occurrence',
                                 'concept_field' = 'condition_concept_id',
                                 'date_field' = 'condition_start_date',
                                 'codeset_name' = 'dx_hypertension')

  visit_type_file <- dplyr::tibble('visit_concept_id' = 9202,
                                   'visit_type' = 'outpatient')

  expect_no_error(cnc_sp_process(cohort = cohort,
                                 multi_or_single_site = 'single',
                                 omop_or_pcornet = 'omop',
                                 codeset_tbl = cnc_cdst_file,
                                 care_site = FALSE,
                                 provider = TRUE,
                                 time = TRUE,
                                 visit_type_tbl = visit_type_file))

})


test_that('no time -- pcornet', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'cnc_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testspecs',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  cnc_cdst_file <- dplyr::tibble('domain' = 'hypertension',
                                 'domain_tbl' = 'condition_occurrence',
                                 'concept_field' = 'condition_concept_id',
                                 'date_field' = 'condition_start_date',
                                 'vocabulary_field' = 'dx_type',
                                 'codeset_name' = 'dx_hypertension')

  visit_type_file <- dplyr::tibble('visit_concept_id' = 9202,
                                   'visit_type' = 'outpatient')

  expect_error(cnc_sp_process(cohort = cohort,
                              multi_or_single_site = 'single',
                              omop_or_pcornet = 'pcornet',
                              codeset_tbl = cnc_cdst_file,
                              care_site = FALSE,
                              provider = TRUE,
                              visit_type_tbl = visit_type_file))

})
