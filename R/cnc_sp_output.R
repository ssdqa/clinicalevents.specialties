
#' Clinical Events and Specialties -- Output Generation
#'
#' Using BOTH tabular outputs generated by `cnc_sp_process`, this function will build a graph to
#' visualize the results. Each function configuration will output a bespoke ggplot. Theming can
#' be adjusted by the user after the graph has been output using `+ theme()`. Most graphs can
#' also be made interactive using `make_interactive_squba()`
#'
#' @param cnc_sp_process_output *tabular input* | output from the `cnc_sp_process` function
#' @param cnc_sp_process_names *tabular input* | classified names from the output from the `cnc_sp_process` function,
#'                              expected to minimally contain the columns:
#'                                - specialty_concept_id: unchanged from the `cnc_sp_process` output
#'                                - specialty_name: the assigned classification
#'                              this table will be joined to `cnc_sp_process_output`, so all specialty_concept_id in the `conc_process_output` should be in `conc_process_names`
#' @param facet_vars *vector* | vector of variable names to facet by
#' @param top_n *integer* | integer value for choosing the "top n" to display per check, with meaning dependent on the context of the check
#' @param specialty_filter *string or vector* | an optional filter to apply to the specialty_name field to narrow down results
#' @param n_mad *integer* | number of MAD from the median for which to flag anomalies
#'              defaults to 3
#' @param p_value *numeric* | the p value to be used as a threshold in the multi-site anomaly detection analysis
#'
#' @return the corresponding visualization/s for the site level (multi/single), time dimension,
#'         and analysis level (exploratory/anomaly detection) specified
#'
#' @example inst/example-cnc_sp_process_output.R
#'
#' @export
#'
cnc_sp_output <- function(cnc_sp_process_output,
                          cnc_sp_process_names,
                          facet_vars=NULL,
                          top_n=15,
                          n_mad=3L,
                          specialty_filter=NULL,
                          p_value=0.9){

  # extract output function
  output_function <- cnc_sp_process_output %>% collect() %>% distinct(output_function) %>% pull()

  # pull apart output function
  if(grepl('_ms_', output_function)){multi_or_single_site <- 'multi'}else{multi_or_single_site <- 'single'}
  if(grepl('_exp_', output_function)){anomaly_or_exploratory <- 'exploratory'}else{anomaly_or_exploratory <- 'anomaly'}
  if(grepl('_la', output_function)){time <- TRUE}else{time <- FALSE}

  # determine color/fill value based on type of plot to be produced
  if((multi_or_single_site=='single'&anomaly_or_exploratory=='exploratory')|
     (multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'&!time)){
    color_var<-'specialty_name'
  }else if (multi_or_single_site=='multi'&anomaly_or_exploratory=='exploratory'){
    color_var<-'site'
  }else{color_var<-NULL}


  message('Preparing data for visualization')
  # Check cluster/visit facetting
  if('cluster'%in%facet_vars&'visit_type'%in%facet_vars){
    cli::cli_abort("Please choose only one of cluster or visit_type for stratification")}

  # Start grouped list
  if(!is.null(facet_vars)){
    vars_no_cs<-facet_vars[!facet_vars=='codeset_name']
    gp_vars <- c('codeset_name')%>%append(vars_no_cs)
  }else{
    gp_vars<-c('codeset_name')
  }

  # Add time variables
  if(time){
    gp_vars <- gp_vars %>%append(c('time_start','time_increment'))
  }

  # Add site variable
  if(multi_or_single_site=='multi'){
    gp_vars <- gp_vars %>% append('site')
  }

  # Add cluster for ss anom
  if(multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'&!time){
    gp_vars <- gp_vars %>% append('cluster') %>% unique()
  }

  # Add specialty name
  spec_gp_vars <- gp_vars %>% append(c('specialty_name'))

  # compute proportions
  conc_output_denom <- cnc_sp_process_output %>%
    inner_join(cnc_sp_process_names, by = 'specialty_concept_id')%>%
    group_by(!!!syms(gp_vars))%>%
    summarise(total=sum(num_visits, na.rm=TRUE))%>%
    ungroup()

  conc_output_pp <- cnc_sp_process_output %>%
    inner_join(cnc_sp_process_names, by = 'specialty_concept_id')%>%
    group_by(!!!syms(spec_gp_vars))%>%
    summarise(n=sum(num_visits, na.rm = TRUE))%>%
    ungroup()%>%
    inner_join(conc_output_denom)%>%
    mutate(prop=n/total)

  if('concept_id'%in%colnames(conc_output_pp)){
    conc_output_pp <- select(vocabulary_tbl('concept'),c(concept_id, concept_name))%>%
      inner_join(conc_output_pp,by='concept_id', copy=TRUE) %>%
      collect()
  }

  # MS ANOM (AT/NT)
  if(anomaly_or_exploratory=='anomaly'&multi_or_single_site=='multi'){
    if(time){
      conc_output_pp<-ms_anom_euclidean(fot_input_tbl=conc_output_pp,
                                        grp_vars=spec_gp_vars[!spec_gp_vars%in%c('time_start', 'time_increment')],
                                        var_col='prop')
    }else{
      gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='site']
      conc_output_pp <- compute_dist_anomalies(df_tbl = conc_output_pp,
                                               grp_vars = gp_vars_no_site,
                                               var_col = 'prop',
                                               denom_cols = c(gp_vars_no_site, 'total'))

      conc_output_pp <- detect_outliers(df_tbl = conc_output_pp,
                                        tail_input = 'both',
                                        p_input = p_value,
                                        column_analysis = 'prop',
                                        column_variable = 'specialty_name')
    }
  }

  ## SS ANOM NT
  if(anomaly_or_exploratory=='anomaly'&multi_or_single_site=='single'&!time){
    gp_vars_no_site<-spec_gp_vars[!spec_gp_vars=='cluster']

    conc_output_pp <- compute_dist_anomalies(df_tbl = conc_output_pp %>% mutate(site = 'placeholder'),
                                             grp_vars = gp_vars_no_site,
                                             var_col = 'prop',
                                             denom_cols = c(gp_vars_no_site, 'total'))

    conc_output_pp <- detect_outliers(df_tbl = conc_output_pp,
                                      tail_input = 'both',
                                      p_input = p_value,
                                      column_analysis = 'prop',
                                      column_variable = 'specialty_name')
  }

  if(!is.null(specialty_filter)){
    conc_output_pp<-conc_output_pp%>%filter(specialty_name%in%specialty_filter)
  }

  message('Building visualization')
  ## SINGLE SITE, EXPLORATORY
  if(multi_or_single_site=='single'&anomaly_or_exploratory=='exploratory'){
    # over time
    if(time){
      conc_output_plot <- cnc_sp_ss_exp_la(data_tbl=conc_output_pp,
                                           facet=facet_vars)

    }else{
      # not over time
      conc_output_pp <- insert_top_n_indicator(dat=conc_output_pp,
                                               gp_cols=facet_vars,
                                               val_col="prop",
                                               n=top_n)%>%
        filter(top_n_indicator)

      conc_output_plot <- cnc_sp_ss_exp_cs(data_tbl=conc_output_pp,
                                           facet=facet_vars,
                                           x_var='specialty_name',
                                           y_var='prop',
                                           fill_var=color_var,
                                           top_n=top_n)
    }

  }else if(multi_or_single_site=='single'&anomaly_or_exploratory=='anomaly'){
    ## SINGLE SITE, ANOMALY
    if(time){
      # over time
      if(!is.null(facet_vars)){
       id_col <- ifelse('cluster' %in% facet_vars, 'cluster', 'visit_type')
       }else{id_col <- 'specialty_name'}

      conc_output_pp <- insert_top_n_indicator(dat=conc_output_pp,
                                               gp_cols=id_col,
                                               val_col="total",
                                               n=top_n,
                                               sum_first=TRUE)%>%
        filter(top_n_indicator)

      conc_output_plot<-cnc_sp_ss_anom_la(process_output=conc_output_pp,
                                          filt_list=list(specialty_name=specialty_filter),
                                          ct_col='prop',
                                          id_col=id_col,
                                          denom_col='n',
                                          name_col='specialty_name',
                                          facet=facet_vars %>% append('specialty_name') %>% unique(),
                                          plot_title_text=paste0('Proportion of Visits with Specialties'))
    }else{
      # not over time
      conc_output_plot <- cnc_sp_ss_anom_cs(data_tbl=conc_output_pp,
                                            facet = facet_vars)
    }

  }else if(multi_or_single_site=='multi'&anomaly_or_exploratory=='exploratory'){
    ## MULTI SITE, EXPLORATORY
    if(time){
      # over time
      conc_output_pp<-insert_top_n_indicator(conc_output_pp,
                                             gp_cols=c('specialty_name'),
                                             val_col = "n",
                                             n=top_n,
                                             sum_first=TRUE)%>%
        filter(top_n_indicator)

      facet_vars <- facet_vars %>% append(c('specialty_name')) %>% unique()
      conc_output_plot <- cnc_sp_ms_exp_la(data_tbl=conc_output_pp,
                                           facet=facet_vars)
    }else{
      # not over time
      conc_output_pp <- insert_top_n_indicator(dat=conc_output_pp,
                                               gp_cols=c('site', facet_vars),
                                               val_col="prop",
                                               n=top_n)%>%
        filter(top_n_indicator)

      conc_output_plot <- cnc_sp_ms_exp_cs(data_tbl=conc_output_pp,
                                           facet = facet_vars)
    }
    ## MULTI SITE, ANOMALY
  }else if(multi_or_single_site=='multi'&anomaly_or_exploratory=='anomaly'){
    if(time){
      # over time
      conc_output_plot <- cnc_sp_ms_anom_la(process_output=conc_output_pp,
                                            grp_vars=c('specialty_name', 'time_start',
                                                        'mean_allsiteprop'),
                                            specialty_filter=specialty_filter)

    }else{
      # not over time
      conc_output_plot <- cnc_sp_ms_anom_cs(process_output=conc_output_pp,
                                            title="Specialty")
    }

  }
  return(conc_output_plot)
}
