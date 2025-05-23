% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnc_sp_process_subfuncs_omop.R
\name{find_fact_spec_conc_omop}
\alias{find_fact_spec_conc_omop}
\title{Function to find the visit_occurrences during which patient had a dx of interest +
specialty of interest}
\usage{
find_fact_spec_conc_omop(
  cohort,
  visit_id,
  fact_codes,
  fact_tbl,
  care_site,
  provider,
  time = FALSE
)
}
\arguments{
\item{cohort}{table with at least person_id}

\item{visit_id}{the ID column associated with the visit table of interest
(either visit_occurrence_id or visit_detail_id)}

\item{fact_codes}{codeset with _concept_id for the dx/px/etc of interest}

\item{fact_tbl}{cdm_tbl in which to search for fact_codes}

\item{care_site}{boolean indicating whether to search care_site (at visit level) for specialty}

\item{provider}{boolean indicating whether to search provider (at visit level) for specialty}

\item{time}{boolean indicating whether the analysis should be conducted longitudinally}
}
\value{
table with all occurrences of the fact_codes for the cohort, and visit info only
if visit was to a specialty in the codeset
}
\description{
Function to find the visit_occurrences during which patient had a dx of interest +
specialty of interest
}
