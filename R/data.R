
#' Clinical Events & Specialties Visit File Sample -- OMOP version
#'
#' A sample version of the file structure expected for the `visit_type_tbl`
#' parameter in the `cnc_sp_process` function. The user should recreate
#' this file and include their own domain definitions.
#'
#' @format ## cnc_sp_visit_file_omop
#' A data frame with 2 columns and 8 rows
#' \describe{
#'   \item{visit_concept_id}{The visit_concept_id as it appears in the visit_occurrence table}
#'   \item{visit_type}{A string to label the visit type of the visit_concept_id; this string is what should be referenced in the `visit_types` parameter}
#' }
#'
"cnc_sp_visit_file_omop"

#' Clinical Events & Specialties Codeset File Sample
#'
#' A sample version of the file structure expected for the `codeset_tbl`
#' parameter in the `cnc_sp_process` function. The user should recreate
#' this file and include their own domain definitions.
#'
#' The codeset listed in the `codeset_name` column should be kept in the
#' `file_subdirectory` indicated in `initialize_dq_session`
#'
#' @format ## `cnc_sp_codeset_file`
#' A data frame with 2 rows and 5 columns
#' \describe{
#'   \item{domain}{A string label for the domain of interest}
#'   \item{domain_tbl}{The name of the CDM table where the variable can be found}
#'   \item{concept_field}{The field in the default_tbl that should be used to join to the codeset}
#'   \item{date_field}{The date field in the default_tbl that should be used to filter the dataset to the cohort period and for longitudinal analyses}
#'   \item{vocabulary_field}{(PCORnet only) The name of the column in the domain table where the vocabulary type is stored}
#'   \item{codeset_name}{The name of the codeset as found in the specs directory; file extension should not be included}
#' }
#'
"cnc_sp_codeset_file"

#' Clinical Events & Specialties - Specialty Naming Sample
#'
#' A sample version of how the list of specialties output by `cnc_sp_process` should be
#' labelled for use in the `cnc_sp_output` function.
#'
#' @format ## `cnc_sp_specialty_names`
#' A data frame with 5 rows and 3 columns
#' \describe{
#'   \item{specialty_concept_id}{The specialty_concept_id identified in `cnc_sp_process`}
#'   \item{specialty_concept_name}{If a vocab_tbl was provided, the concept_name associated with the specialty_concept_id; otherwise will default to "No vocabulary table input"}
#'   \item{specialty_name}{A user-provided identifier for the specialty; can be repeated across rows to group specialties together}
#' }
#'
"cnc_sp_specialty_names"
