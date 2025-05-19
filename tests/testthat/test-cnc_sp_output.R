
test_that('single site, exploratory, no time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','a','a','a'),
                                   'specialty_concept_id' = c(1,2,3,4,5,6),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ss_exp_cs',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})

test_that('single site, anomaly, no time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','a','a','a'),
                                   'specialty_concept_id' = c(1,2,3,4,5,6),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ss_anom_cs',
                                facet_vars = NULL,
                                specialty_filter = NULL))


})

test_that('multi site, exploratory, no time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','b','b','b'),
                                   'specialty_concept_id' = c(1,2,3,4,5,6),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ms_exp_cs',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})

test_that('multi site, anomaly, no time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','b','b','b'),
                                   'specialty_concept_id' = c(1,2,3,4,5,6),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ms_anom_cs',
                                facet_vars = NULL,
                                specialty_filter = NULL))

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','b','b','b'),
                                   'specialty_concept_id' = c(1,2,3,1,2,3),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,1,2,3),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'heme', 'heme2', 'heme3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_warning(cnc_sp_output(cnc_sp_process_output = cnc_count_input %>% mutate(num_visits = 10),
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ms_anom_cs',
                                facet_vars = NULL,
                                specialty_filter = NULL))


})


test_that('single site, exploratory, across time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','a','a','a'),
                                   'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01',
                                                    '2021-01-01', '2022-01-01', '2023-01-01'),
                                   'time_increment' = c('year', 'year', 'year', 'year', 'year', 'year'),
                                   'specialty_concept_id' = c(1,1,1,1,1,1),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ss_exp_la',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})

test_that('single site, anomaly, across time -- year', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','a','a','a'),
                                   'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01',
                                                    '2021-01-01', '2022-01-01', '2023-01-01'),
                                   'time_increment' = c('year', 'year', 'year', 'year', 'year', 'year'),
                                   'specialty_concept_id' = c(1,1,1,1,1,1),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ss_anom_la',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})

test_that('single site, anomaly, across time -- month', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','a','a','a'),
                                   'time_start' = c('2018-01-01', '2018-02-01', '2018-03-01',
                                                    '2018-04-01', '2018-05-01', '2018-06-01'),
                                   'time_increment' = c('month', 'month', 'month', 'month', 'month', 'month'),
                                   'specialty_concept_id' = c(1,1,1,1,1,1),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ss_anom_la',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})


test_that('multi site, exploratory, across time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','b','b','b'),
                                   'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01',
                                                    '2018-01-01', '2019-01-01', '2020-01-01'),
                                   'time_increment' = c('year', 'year', 'year', 'year', 'year', 'year'),
                                   'specialty_concept_id' = c(1,1,1,1,1,1),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_no_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ms_exp_la',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})

test_that('multi site, anomaly, across time', {

  cnc_count_input <- dplyr::tibble('site' = c('a','a','a','b','b','b'),
                                   'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01',
                                                    '2018-01-01', '2019-01-01', '2020-01-01'),
                                   'time_increment' = c('year', 'year', 'year', 'year', 'year', 'year'),
                                   'specialty_concept_id' = c(1,1,1,1,1,1),
                                   'cluster' = c('test','test','test','test','test','test'),
                                   'codeset_name' = c('dx_test','dx_test','dx_test',
                                                      'dx_test','dx_test','dx_test'),
                                   'num_visits' = c(10,20,30,40,50,60))

  cnc_spec_input <- dplyr::tibble('specialty_concept_id' = c(1,2,3,4,5,6),
                                  'specialty_concept_name' = c('heme', 'heme2', 'heme3',
                                                               'onco', 'onco2', 'onco3'),
                                  'specialty_name' = c('heme', 'heme', 'heme', 'onco', 'onco', 'onco'))

  expect_error(cnc_sp_output(cnc_sp_process_output = cnc_count_input,
                                cnc_sp_process_names = cnc_spec_input,
                                output_function = 'cnc_sp_ms_anom_la',
                                facet_vars = NULL,
                                specialty_filter = c('heme', 'onco')))


})
