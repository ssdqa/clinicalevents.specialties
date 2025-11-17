# **Multi-Site Anomaly Longitudinal** Function to generate output displaying the Euclidean distance between two time series

**Multi-Site Anomaly Longitudinal** Function to generate output
displaying the Euclidean distance between two time series

## Usage

``` r
cnc_sp_ms_anom_la(
  process_output,
  grp_vars,
  specialty_filter = NULL,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output from `cnc_sp_process` should contain the columns specified in
  `grp_vars`

- grp_vars:

  vector of variables used for grouping when identifying all-site
  summary statistics

- specialty_filter:

  a string indicating the SINGLE specialty of intereset that should be
  displayed at one time

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

three graphs:

1.  line graph that shows the smoothed proportion of visits with a
    specialty across time computation with the Euclidean distance
    associated with each line

2.  line graph that shows the raw proportion of visits with a specialty
    across time computation with the Euclidean distance associated with
    each line

3.  a bar graph with the Euclidean distance value for each site, with
    the average proportion as the fill
