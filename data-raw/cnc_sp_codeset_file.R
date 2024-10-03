## code to prepare `cnc_sp_codeset_file` dataset goes here

cnc_sp_codeset_file <- tidyr::tibble('domain' = 'JIA diagnosis',
                                     'domain_tbl' = 'condition_occurrence',
                                     'concept_field' = 'condition_concept_id',
                                     'date_field' = 'condition_start_date',
                                     'codeset_name' = 'dx_jia')

usethis::use_data(cnc_sp_codeset_file, overwrite = TRUE)
