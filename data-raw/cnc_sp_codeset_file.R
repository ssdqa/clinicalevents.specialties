## code to prepare `cnc_sp_codeset_file` dataset goes here

cnc_sp_codeset_file <- tidyr::tibble('domain' = c('JIA diagnosis', 'SCD diagnosis'),
                                     'domain_tbl' = c('condition_occurrence', 'diagnosis'),
                                     'concept_field' = c('condition_concept_id', 'dx'),
                                     'date_field' = c('condition_start_date', 'admit_date'),
                                     'vocabulary_field' = c(NA, 'dx_type'),
                                     'codeset_name' = c('dx_jia', 'dx_scd'))

usethis::use_data(cnc_sp_codeset_file, overwrite = TRUE)
