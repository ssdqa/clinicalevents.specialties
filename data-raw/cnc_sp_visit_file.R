## code to prepare `cnc_sp_visit_file_omop` dataset goes here

cnc_sp_visit_file_omop <- tidyr::tibble('visit_(detail)_concept_id' = c(9201, 9202, 9203, 581399, 9201, 9202, 9203, 581399),
                                        'visit_type' = c('inpatient', 'outpatient', 'emergency', 'outpatient',
                                                         'all', 'all', 'all', 'all'))

usethis::use_data(cnc_sp_visit_file_omop, overwrite = TRUE)

## code to prepare `cnc_sp_visit_file_pcornet` dataset goes here

cnc_sp_visit_file_pcornet <- tidyr::tibble('enc_type' = c('IP', 'AV', 'ED', 'TH', 'IP', 'AV', 'ED', 'TH'),
                                        'visit_type' = c('inpatient', 'outpatient', 'emergency', 'outpatient',
                                                         'all', 'all', 'all', 'all'))

usethis::use_data(cnc_sp_visit_file_pcornet, overwrite = TRUE)
