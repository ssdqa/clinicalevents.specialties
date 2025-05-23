% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnc_sp_process_subfuncs_omop.R
\name{compute_conc_omop}
\alias{compute_conc_omop}
\title{Function to compute fact + visit specialty concordance}
\usage{
compute_conc_omop(
  cohort,
  grouped_list = c("site"),
  codeset_tbl = read_codeset("conc_codesets", col_types = "cccc"),
  care_site,
  provider,
  visit_detail = FALSE,
  visit_type_tbl = NULL,
  age_gp_tbl = NULL,
  time = FALSE
)
}
\arguments{
\item{cohort}{the cohort for which to look for events}

\item{grouped_list}{a vector to group input by. Defaults to \code{site}.
If \code{year} is in \code{grouped_list}, results are returned by year of \code{visit_start_date}}

\item{codeset_tbl}{table in the file_subdirectory with the columns:
domain: name of the domain
domain_tbl: name of the cdm_tbl
concept_field: column name in the domain_tbl for which to search the codeset concept_ids
date_field: column name in the domain_tbl to use for date filtering
codeset_name: name of a codeset in the specs directory}

\item{care_site}{boolean indicating whether to search care_site (at visit level) for specialty}

\item{provider}{boolean indicating whether to search provider (at visit level) for specialty}

\item{visit_detail}{TRUE if want to use the visit_detail table to identify specialty visits
FALSE if want to use visit_occurrence table (default)}

\item{visit_type_tbl}{if provided, a map from visit_concept_id to a visit_type classification.
if not required, should be \code{NULL}}

\item{age_gp_tbl}{if provided, table with the columns:
min_age: minimum age, in years, for the given categorization
max_age: maximum age, in years, for the given categorization
group: label to be used for the categorization}

\item{time}{boolean indicating whether to compute over time or not}
}
\value{
table with cols:
codeset_name: name of the codeset in the specs directory with event facts
specialty_concept_id: concept_id of specialty from either care_site or provider
num_visits: number of visits with the specialty+fact
... any columns in the \code{grouped_list}
}
\description{
Function to compute fact + visit specialty concordance
}
