## code to prepare `cnc_sp_specialty_names` dataset goes here

cnc_sp_specialty_names <- tidyr::tibble('specialty_concept_id' = c(38004491, 45756818, 38004446, 45756805, 903279),
                                        'specialty_concept_name' = c('Rheumatology', 'Pediatric Rheumatology', 'General Practice',
                                                                     'Pediatric Cardiology', 'Advanced Heart Failure and Transplant Cardiology'),
                                        'specialty_name' = c('Rheumatology', 'Rheumatology', 'General Medicine', 'Cardiology',
                                                             'Cardiology'))

usethis::use_data(cnc_sp_specialty_names, overwrite = TRUE)
