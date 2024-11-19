
#' Function to compute fact + visit specialty concordance
#'
#' @param cohort the cohort for which to look for events
#' @param grouped_list a vector to group input by. Defaults to `site`.
#'                      If `year` is in `grouped_list`, results are returned by year of `visit_start_date`
#' @param codeset_tbl table in the file_subdirectory with the columns:
#'                        domain: name of the domain
#'                        domain_tbl: name of the cdm_tbl
#'                        concept_field: column name in the domain_tbl for which to search the codeset concept_ids
#'                        date_field: column name in the domain_tbl to use for date filtering
#'                        codeset_name: name of a codeset in the specs directory
#' @param care_site boolean indicating whether to search care_site (at visit level) for specialty
#' @param provider boolean indicating whether to search provider (at visit level) for specialty
#' @param visit_type_tbl if provided, a map from visit_concept_id to a visit_type classification.
#'                       if not required, should be `NULL`
#' @param age_gp_tbl if provided, table with the columns:
#'                     min_age: minimum age, in years, for the given categorization
#'                     max_age: maximum age, in years, for the given categorization
#'                     group: label to be used for the categorization
#' @param time boolean indicating whether to compute over time or not
#'
#' @return table with cols:
#'                        codeset_name: name of the codeset in the specs directory with event facts
#'                        specialty_concept_id: concept_id of specialty from either care_site or provider
#'                        num_visits: number of visits with the specialty+fact
#'                        ... any columns in the `grouped_list`
#'
compute_conc_omop <- function(cohort,
                              grouped_list=c('site'),
                              codeset_tbl=read_codeset("conc_codesets", col_types = 'cccc'),
                              care_site,
                              provider,
                              visit_type_tbl=NULL,
                              age_gp_tbl=NULL,
                              time=FALSE) {
  # split input table
  codeset_results <- list()
  codeset_list <- split(codeset_tbl, seq(nrow(codeset_tbl)))

  # build grouped list
  grp_vis <- grouped_list %>% append(c('visit_occurrence_id'))
  grp_vis_spec <- grp_vis %>% append(c('spec_flag','total_gp_ct'))
  grp_spec <- grouped_list%>%append(c('specialty_concept_id', 'cluster'))
  if(time){grp_spec<-grp_spec%>%append(c('time_start','time_increment'))}


  for (i in 1:length(codeset_list)) {

    codeset_name = codeset_list[[i]]$codeset_name
    fact_col_name <- sym(codeset_list[[i]]$concept_field)

    message(paste0('Starting codeset ', codeset_list[[i]]$codeset_name))

    domain_tbl_use <- cdm_tbl(paste0(codeset_list[[i]]$domain_tbl))%>%
      inner_join(cohort) %>%
      filter(!!sym(codeset_list[[i]]$date_field) >= start_date,
             !!sym(codeset_list[[i]]$date_field) <= end_date) %>%
      rename(concept_id=!!fact_col_name)

    codes_to_use <- load_codeset(codeset_name)

    # find occurrences of codeset codes with specialty
    visit_specs <- find_fact_spec_conc_omop(cohort,
                                            fact_codes=codes_to_use,
                                            fact_tbl=domain_tbl_use,
                                            care_site=care_site,
                                            provider=provider,
                                            time=time)
    if(!is.null(age_gp_tbl)){

      new_person <- build_birth_date(cohort = cohort,
                                     person_tbl = cdm_tbl('person'))

      visit_specs <- visit_specs %>%
        collect() %>%
        inner_join(new_person %>% select(person_id, birth_date)) %>%
        mutate(visit_age=as.numeric((visit_start_date-birth_date)/365.25)) %>%
        merge(age_gp_tbl) %>%
        mutate(age_grp = case_when(visit_age >= min_age & visit_age <= max_age ~ group,
                                   TRUE ~ as.character(NA)))%>%
        filter(!is.na(age_grp))%>%
        select(-c(birth_date, min_age, max_age, group))
    }

    # calculate the concordance per the grouping parameters
    conc_prop<- visit_specs %>%
      group_by(!!!syms(grp_spec))%>%
      summarise(num_visits=n_distinct(visit_occurrence_id)) %>%
      ungroup()%>%
      mutate(codeset_name=codeset_name) %>% collect()

    if(!is.null(visit_type_tbl)){
      grp_vis_type <- grp_spec[!grp_spec=='visit_concept_id']
      grp_vis_type <- grp_vis_type %>% append(c('visit_type','codeset_name'))

      visit_type_vec <- visit_type_tbl %>% distinct(visit_concept_id) %>% pull()

      conc_prop <- conc_prop %>%
        filter(visit_concept_id %in% visit_type_vec) %>%
        left_join(visit_type_tbl, by = 'visit_concept_id') %>%
        group_by(!!!syms(grp_vis_type)) %>%
        summarise(num_visits=sum(num_visits, na.rm=TRUE)) %>%
        ungroup()
    }

    codeset_results[[codeset_name]] <- conc_prop


  }

  codeset_results_all <-
    reduce(.x=codeset_results,
           .f=dplyr::union_all)
}

#' Function to find the visit_occurrences during which patient had a dx of interest +
#' specialty of interest
#'
#' @param cohort table with at least person_id
#' @param fact_codes codeset with _concept_id for the dx/px/etc of interest
#' @param fact_tbl cdm_tbl in which to search for fact_codes
#' @param care_site boolean indicating whether to search care_site (at visit level) for specialty
#' @param provider boolean indicating whether to search provider (at visit level) for specialty
#' @param time boolean indicating whether the analysis should be conducted longitudinally
#'
#' @return table with all occurrences of the fact_codes for the cohort, and visit info only
#'         if visit was to a specialty in the codeset
find_fact_spec_conc_omop <- function(cohort,
                                     fact_codes,
                                     fact_tbl,
                                     care_site,
                                     provider,
                                     time=FALSE){

  if(!'cluster'%in%colnames(fact_codes)){fact_codes<-fact_codes%>%mutate(cluster=concept_name)}
  if(!'category'%in%colnames(fact_codes)){fact_codes<-fact_codes%>%mutate(category='all')}

  message('Finding code occurrences')
  fact_occurrences <-
    fact_tbl %>% select(person_id, visit_occurrence_id, concept_id)%>%
    inner_join(cohort) %>%
    # select(person_id, concept_id,
    #        visit_occurrence_id, site) %>%
    inner_join(select(fact_codes,
                      concept_id, concept_name,
                      category, cluster)) %>%
    compute_new(temporary=TRUE)

  message('Finding specialties')
  if(time){
    visits <- cdm_tbl('visit_occurrence') %>%
      inner_join(cohort)%>%
      filter(visit_start_date >= start_date,
             visit_start_date <= end_date) %>%
      filter(visit_start_date>=time_start,
               visit_start_date<=time_end)
  }else{
    visits <-
      cdm_tbl('visit_occurrence') %>%
      inner_join(cohort) %>%
      filter(visit_start_date >= start_date,
             visit_start_date <= end_date)
  }


  if(care_site&provider){
    pv_spec <-  visits %>% select(visit_occurrence_id, visit_concept_id,
                                  visit_start_date, provider_id)%>%
      inner_join(select(fact_occurrences, visit_occurrence_id))%>%
      left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id)),
                by = 'provider_id')%>%
      rename(specialty_concept_id_pv=specialty_concept_id) %>%
      select(visit_occurrence_id,visit_concept_id,visit_start_date,specialty_concept_id_pv)

    cs_spec <- visits %>% select(visit_occurrence_id, visit_concept_id,
                                 visit_start_date, care_site_id)%>%
      inner_join(select(fact_occurrences,visit_occurrence_id))%>%
      left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id)),
                by = 'care_site_id')%>%
      rename(specialty_concept_id_cs=specialty_concept_id)%>%
      select(visit_occurrence_id,visit_concept_id,visit_start_date,specialty_concept_id_cs)

    spec_full <-
      visits %>%
      left_join(pv_spec) %>%
      left_join(cs_spec) %>%
      mutate(specialty_concept_id=case_when(!is.na(specialty_concept_id_pv)~specialty_concept_id_pv,
                                            !is.na(specialty_concept_id_cs)~specialty_concept_id_cs,
                                            TRUE~NA_integer_))%>%
      select(-c(specialty_concept_id_pv,specialty_concept_id_cs)) %>%
      select(visit_occurrence_id, visit_concept_id, visit_start_date, specialty_concept_id) %>%
      distinct() %>% compute_new()

  }else if(provider&!care_site){
    spec_full <- visits %>%
      select(visit_occurrence_id, visit_concept_id, visit_start_date, provider_id)%>%
      inner_join(select(fact_occurrences, visit_occurrence_id))%>%
      left_join(select(cdm_tbl('provider'),c(provider_id, specialty_concept_id)),
                by = 'provider_id') %>%
      distinct() %>% compute_new()
  }else if(care_site&!provider){
    spec_full <- visits %>%
      select(visit_occurrence_id, visit_concept_id, visit_start_date, care_site_id)%>%
      inner_join(select(fact_occurrences, visit_occurrence_id))%>%
      left_join(select(cdm_tbl('care_site'),c(care_site_id, specialty_concept_id)),
                by = 'care_site_id') %>%
      distinct() %>% compute_new()
  }

  spec_final <-
    inner_join(
      spec_full,
      fact_occurrences
    ) %>%
    compute_new(temporary = TRUE)


  return(spec_final)

}

#' Function to generate a table of concept_id + concept_name for a set of concepts
#'
#' @param tbl table with a specialty_concept_id column
#' @param concept_col the column in the vocabulary table that should be used to join to
#' the input tbl
#' @param vocab table with a concept_id and concept_name column, defaulting to the
#'              vocabulary_tbl `concept`
#'
#' @return table with distinct specialty_concept_id | concept_name
#'
#' @importFrom purrr set_names
#'
find_distinct_concepts <- function(tbl,
                                   concept_col = 'concept_id',
                                   vocab=vocabulary_tbl('concept')){

  tbl_distinct <- tbl %>% distinct(specialty_concept_id)

  join_cols <- purrr::set_names(concept_col, 'specialty_concept_id')

  vocab%>%select(!!sym(concept_col), concept_name)%>%
    inner_join(tbl_distinct, by = join_cols, copy=TRUE)%>%
    rename(specialty_concept_id=concept_id,
           specialty_concept_name=concept_name)
}
