
#' @import ggplot2
#' @import ggiraph
#' @importFrom stringr str_remove_all
#' @importFrom tidyr tibble
#' @importFrom tidyr unite
#' @importFrom qicharts2 qic
#' @importFrom timetk plot_anomalies
#' @importFrom timetk plot_anomalies_decomp
#' @importFrom plotly layout
#' @importFrom graphics text
#'
NULL


#' Function to insert a column into a table indicating whether the record is in the top n
#' for the given group
#'
#' @param dat table containing the data
#' @param gp_cols columns to group by
#'                  the top n will be taken from each group, so the total indicated as "top" hits will be # of groups * n
#' @param val_col column to order by when determining the top n
#' @param n number of records to indicate as top within the group
#' @param sum_first boolean indicating whether a value should be grouped and summed prior to identifying the top N
#'
#' @return the original `dat` table with all original columns,
#'         plus a column `top_n_indicator` which is TRUE if the record is in the top n
#'         and FALSE if not
insert_top_n_indicator<-function(dat,
                                 gp_cols,
                                 val_col,
                                 n,
                                 sum_first=FALSE){
  if(sum_first){
    top_hits <- dat %>%
      ungroup()%>%
      group_by(!!!syms(gp_cols))%>%
      summarise(sumn=sum(!!sym(val_col)))%>%
      ungroup()%>%
      slice_max(order_by = sumn,n=n)%>%
      ungroup()%>%
      mutate(top_n_indicator=TRUE)%>%
      select(-sumn)
  }else{
    top_hits <- dat %>%
      group_by(!!!syms(gp_cols))%>%
      slice_max(order_by = !!sym(val_col),n=n) %>%
      ungroup()%>%
      mutate(top_n_indicator=TRUE)
  }

  dat %>%
    left_join(top_hits)%>%
    mutate(top_n_indicator=case_when(is.na(top_n_indicator)~FALSE,
                                     TRUE~TRUE))
}

#' * Single Site, Exploratory, No Time*
#' Function to produce output for clinical event concordance with specialty
#'      for single site, exploratory, not over time
#'
#' @param data_tbl table with the data to plot
#' @param facet list of one or more variables to facet the plot on
#' @param x_var variable to plot on the x axis
#' @param y_var variable to plot on the y axis
#' @param fill_var variable to fill bars with
#' @param top_n an integer indicating the top N number of specialties to display on the graph
#'
#' @return a bar plot based on the values of `x_var`, `y_var`, `fill_var`, `facet`
#'
cnc_sp_ss_exp_nt <- function(data_tbl,
                             facet,
                             x_var,
                             y_var,
                             fill_var,
                             top_n) {

  data_tbl <- data_tbl %>%
    mutate(text=paste("Specialty: ", specialty_name,
                      "\nNumber of Visits: ",format(n,big.mark=","),
                      "\nProportion: ",round(prop,2)))

  facet_name<-str_remove_all(as.character(deparse(facet)),"\\(|\\\"|\\)")

  plt <- ggplot(data_tbl,
         aes(y=!!sym(x_var), x=!! sym(y_var), fill=!!sym(fill_var), text=text)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    facet_wrap((facet), labeller = label_wrap_gen())+
    scale_fill_ssdqa()+
    #coord_flip() +
    theme_minimal() +
    scale_y_discrete(labels = label_wrap_gen()) +
    # theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
    #       panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))+
    labs(title=paste0("Proportion of Visits with each of the top ",top_n," Specialties"),
         y = 'Specialty',
         x = 'Proportion')

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)

}

#' *Multi-Site, Exploratory, No Time*
#' Function to produce output for clinical event concordance with specialty
#'      for multi site, exploratory, not over time
#'
#' @param data_tbl table with the data to plot
#'
#' @return a dot plot of specialty against proportion of visits with that specialty
#'         at each site, with dot color representing site
#'
cnc_sp_ms_exp_nt <- function(data_tbl){
  dat_to_plot<-data_tbl%>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nSite: ",site))
  plt<-ggplot(dat_to_plot, aes(x=specialty_name,
                  y=prop,
                  colour=site,
                  text=text))+
    geom_point()+
    scale_color_ssdqa()+
    coord_flip()+
    theme_minimal() +
    labs(x = 'Proportion',
         y = 'Specialty',
         color = 'Site')

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)

}

#' *Single Site, Exploratory, Across Time*
#' Function to produce output for clinical event concordance with specialty
#'      for single site, exploratory, across time
#'
#' @param data_tbl table with the data to plot
#' @param facet list of one or more variables to facet the plot on
#'
#' @return a plotly line plot of the proportion of visits with each specialty
#'          against time, with line color representing specialty
cnc_sp_ss_exp_at <- function(data_tbl,
                             facet=NULL){
  dat_to_plot<-data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nTime Start: ",time_start)) %>%
    arrange(time_start)

  if(is.null(facet)){
  plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
    geom_line(aes(group=specialty_name))+
    #scale_color_manual(values=pal_map)+
    scale_color_ssdqa()+
    theme_minimal()+
    labs(x = 'Time',
         y = 'Proportion',
         color = 'Specialty')
  }else{
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=specialty_name,text=text))+
      geom_line(aes(group=specialty_name))+
      #scale_color_manual(values=pal_map)+
      scale_color_ssdqa()+
      facet_wrap((facet))+
      theme_minimal()+
      labs(x = 'Time',
           y = 'Proportion',
           color = 'Specialty')
  }

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)
}

#' * Multi-Site, Exploratory, Across Time*
#' Function to produce output for clinical event concordance with specialty
#'      for multi site, exploratory, across time

#' @param data_tbl table which must contain the cols: time_start | codeset_name | specialty_name | site
#' @param facet if supplied, variable to facet the plot by
#' @return line plot, with time on x axis, proportion on y, line color determined by site
#'              with a dotted line for the all-site mean
cnc_sp_ms_exp_at <- function(data_tbl,
                             facet=NULL){
  # compute all site mean
   all_site_mean <- data_tbl%>%
     group_by(time_start, codeset_name, !!!syms(facet))%>%
     summarise(prop=mean(prop, na.rm=TRUE))%>%
     ungroup()%>%
     mutate(site="all")

   # set up scheme for line types: differentiate "all" from site names
   site_names<-data_tbl%>%distinct(site)%>%pull()
   n_sites<-length(site_names)
   line_vals<-c("dotted",rep("solid",n_sites))
   line_breaks<-c("all",site_names)

  dat_to_plot<-data_tbl %>%
    bind_rows(all_site_mean)%>%
    mutate(text=paste("Site: ",site,
                      "\nProportion of site's visits: ",round(prop,2),
                      "\nTime Start: ",time_start))


  if(is.null(facet)){
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=site,text=text))+
      geom_line(aes(linetype=site, group = site))+
      scale_color_ssdqa()+
      scale_linetype_manual(values=line_vals,breaks=line_breaks, guide = 'none')+
      theme_minimal() +
      labs(x = 'Time',
           y = 'Proportion',
           color = 'Site')
  }else{
    plt<-ggplot(dat_to_plot, aes(x=time_start,y=prop,color=site,text=text))+
      geom_line(aes(linetype=site, group = site))+
      scale_color_ssdqa()+
      facet_wrap((facet), labeller = label_wrap_gen())+
      scale_linetype_manual(values=line_vals,breaks=line_breaks, guide = 'none')+
      theme_minimal() +
      labs(x = 'Time',
           y = 'Proportion',
           color = 'Site')
  }

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = TRUE)

  return(plt)
}


#' *Single Site, Anomaly, No Time*
#'
#' Function to produce output for clinical event concordance with specialty
#'      for single site, anomaly, not over time
#' @param data_tbl table which must contain the cols: specialty_name | prop | median | n_mad
#' @param facet vector of variable names to be used to facet the graph
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of visits
#'         for a given cluster, and the size of the dot represents the mean proportion
#'         across all specialties
#'
cnc_sp_ss_anom_nt<- function(data_tbl,
                             facet = NULL){

  dat_to_plot <- data_tbl %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nProportion: ",round(prop,2),
                      "\nMean proportion:",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))

  #mid <- median(dat_to_plot$prop, na.rm=TRUE)

  plt<-ggplot(dat_to_plot %>% mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group',
                                                     'not outlier', anomaly_yn)), #%>% filter(anomaly_yn != 'no outlier in group'),
              aes(x=specialty_name, y=cluster, text=text, color=prop))+
    geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
    geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                           aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(19,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
    theme_minimal() +
    facet_wrap((facet)) +
    #theme(axis.text.x = element_text(angle=60, hjust = 1)) +
    labs(size="",
         title=paste0('Anomalous Proportion of Visits with Specialty per Cluster'),
         subtitle = 'Dot size is the mean proportion per domain',
         y = 'Cluster',
         x = 'Specialty') +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')

  plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)

  return(plt)

}

#' *Single Site, Anomaly, Across Time*
#'
#' Control chart looking at proportion of visits with specialty over time
#'     and a reference table
#'
#'
#' @param process_output dataframe output by the corresponding ssdqa check
#' @param filt_list a named list with names equal to the column name/s that must exist in the `process_output`
#'                                    and values equal to the values on which to filter
#'                                    e.g. filt_list=list(concepts=c(first_concept, second_concept),
#'                                                        another_column_name=c('some_value_found_in_another_column_name'))
#' @param ct_col a numeric column with counts associated with the variable of interest
#' @param denom_col a numeric column with counts to be summed across time to provide an overall summary count
#' @param id_col the primary identifier for variables in the table (i.e. specialty_name)
#' @param name_col column with descriptive names for the id_col
#' @param facet the variables by which you would like to facet the graph; defaults to NULL
#' @param plot_title_text text to display as the plot title. Will be tacked on to the text 'Control Chart: '
#'
#' @return if time_increment is year, outputs a list where:
#'          the first element is a P prime control chart that highlights points in time that are anomalous
#'          the second element is a gt table with the total counts based on the specified stratification
#'         otherwise, outputs a list with a plot_anomalies & plot_anomalies_decomp
#'         graph from the timetk package
#'
#'
cnc_sp_ss_anom_at <- function(process_output,
                              filt_list=NULL,
                              ct_col,
                              denom_col,
                              id_col,
                              name_col,
                              facet=NULL,
                              plot_title_text){

  time_inc <- process_output %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()

  if(!is.null(filt_list[[1]])){
    # applying all defined filters
    for(i in 1:length(filt_list)){
      var_name<-names(filt_list[i])
      var_value<-filt_list[[i]]

      if(exists('c_added')){
        c_added<-c_added%>%filter(!!sym(var_name)%in%var_value)
      }else{
        # first time around, construct new df
        c_added<-process_output%>%filter(!!sym(var_name)%in%var_value)
      }
    }}else(c_added<-process_output)


  if(time_inc == 'year'){

    c_final <- c_added %>% group_by(!!!syms(facet), time_start, !!sym(ct_col)) %>%
      unite(facet_col, !!!syms(facet), sep = '\n')

    c_plot <- qic(data = c_final, x = time_start, y = n, chart = 'pp', facets = ~facet_col,
                  title = 'Control Chart: Specialty Usage Over Time', show.grid = TRUE, n = total,
                  ylab = 'Proportion', xlab = 'Time')

    op_dat <- c_plot$data

    new_pp <- ggplot(op_dat,aes(x,y)) +
      geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
      geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +
      geom_line(aes(x,cl)) +
      geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
      geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
      geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
      facet_wrap(~facet1, scales="free_y") +
      ggtitle(label = paste('Control Chart: ', plot_title_text)) +
      labs(x = 'Time',
           y = 'Proportion')+
      theme_minimal()

    new_pp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output_int <- new_pp

    # same here with passing in name of column
    # is there a need to apply an additional filter here, since c_added already has filtering applied?
    if(!'site'%in%colnames(c_added)){
      c_added<-c_added%>%mutate(site='combined')
    }
    ref_tbl <- generate_ref_table(tbl = c_added,
                                  id_col=id_col,
                                  denom=denom_col,
                                  name_col=name_col,
                                  time = TRUE)

    output <- list(output_int, ref_tbl)

  }else{

    anom_to_plot <- anomalize_ss_anom_at(fot_input_tbl = c_added,
                                         grp_vars = facet,
                                         time_var = 'time_start',
                                         var_col = ct_col)

    anomalies <-
      plot_anomalies(.data=anom_to_plot,
                     .facet_vars = facet,
                     .facet_ncol = 2,
                     .date_var=time_start,
                     .interactive = FALSE,
                     .title = paste0('Anomalies for ', plot_title_text)) #%>%
      #layout(title = paste0('Anomalies for ', plot_title_text))

    decomp <-
      plot_anomalies_decomp(.data=anom_to_plot,
                            .facet_vars = facet,
                            .date_var=time_start,
                            .interactive = FALSE,
                            .title = paste0('Anomalies for ', plot_title_text)) #%>%
      #layout(title = paste0('Anomalies for ', plot_title_text))

    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- list(anomalies, decomp)

  }

  return(output)

}

#' **Multi-Site Across Time Anomaly**
#' Function to generate output displaying the Euclidean distance between two time series
#'
#' @param process_output output from `cnc_sp_process`
#'                       should contain the columns specified in `grp_vars`
#' @param grp_vars vector of variables used for grouping when identifying all-site summary statistics
#' @param specialty_filter a string indicating the SINGLE specialty of intereset that should be displayed at one time
#'
#' @return three graphs:
#'    1) line graph that shows the smoothed proportion of visits with a
#'    specialty across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of visits with a
#'    specialty across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#'
cnc_sp_ms_anom_at <- function(process_output,
                              grp_vars,
                              specialty_filter=NULL){

  if(length(specialty_filter) > 1){cli::cli_abort('Please choose one specialty to view for this analysis')}

  filt_op <- process_output %>% filter(specialty_name %in% specialty_filter)

  if('cluster' %in% colnames(filt_op)){facet <- 'cluster'}else if('visit_type' %in% colnames(filt_op)){facet <- 'visit_type'}else{facet <- NULL}

  allsites <-
    filt_op %>%
    select(!!!syms(grp_vars), !!sym(facet))%>%#time_start,concept_id,mean_allsiteprop)
    distinct() %>%
    rename(prop=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion: ",prop),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion: ",prop))

  dat_to_plot <-
    filt_op %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  p <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_ssdqa() +
    theme_minimal() +
    facet_wrap((facet)) +
    #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(y = 'Proportion (Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion of ', specialty_filter, ' Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site,
               group=site, text=text_raw)) +
    scale_color_ssdqa() +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    theme_minimal() +
    facet_wrap((facet)) +
    #theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Raw Proportion of ', specialty_filter, ' Across Time'))

  t <- dat_to_plot %>%
    distinct(site, !!sym(facet), dist_eucl_mean, site_loess) %>%
    group_by(site, !!sym(facet), dist_eucl_mean) %>%
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nEuclidean Distance: ', dist_eucl_mean,
                            '\nAverage Loess Proportion: ', mean_site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
    geom_col_interactive() +
    facet_wrap((facet)) +
    # geom_text(aes(label = dist_eucl_mean), vjust = 2, size = 3,
    #           show.legend = FALSE) +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    # theme(legend.position = 'bottom',
    #       legend.text = element_text(angle = 45, vjust = 0.9, hjust = 1),
    #       axis.text.x = element_text(face = 'bold')) +
    labs(fill = 'Avg. Proportion \n(Loess)',
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance for ', specialty_filter))

  p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  output <- list(p,
                 q,
                 t)

  return(output)
}

#' *Multi-Site, Anomaly, No Time*
#'
#' @param process_output the output from the cnc_sp check, summarized to be used in the multi site anomaly detection check
#' @param title text containing the title for the plot
#' @param text_wrapping_char the number of characters for the specialty names to be displayed on the plot
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of visits
#'         for a given specialty, and the size of the dot represents the mean proportion
#'         across all sites
#'
cnc_sp_ms_anom_nt<-function(process_output,
                            title,
                            text_wrapping_char = 60){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))


  dat_to_plot <- process_output %>%
    mutate(text=paste("Specialty: ",specialty_name,
                      "\nSite: ",site,
                      "\nProportion: ",round(prop,2),
                      "\nMean proportion:",round(mean_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2)))


  plt<-ggplot(dat_to_plot %>% mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group',
                                                         'not outlier', anomaly_yn)),
              aes(x=site, y=specialty_name, text=text, color=prop))+
    geom_point_interactive(aes(size=mad_val,shape=anomaly_yn, tooltip = text))+
    scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
    scale_shape_manual(values=c(20,8))+
    scale_y_discrete(labels = function(x) str_wrap(x, width = text_wrapping_char)) +
    theme_minimal() +
    #theme(axis.text.x = element_text(angle=60)) +
    labs(y = "Specialty",
         x = 'Site',
         size="",
         title=paste0('Anomalous Proportion of Visits per ', title, ' by Site')) +
    guides(color = guide_colorbar(title = 'Proportion'),
           shape = guide_legend(title = 'Anomaly'),
           size = 'none')

  plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                              'tooltip' = TRUE)

  return(plt)
}
