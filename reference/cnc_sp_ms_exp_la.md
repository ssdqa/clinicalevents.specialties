# 

- Multi-Site, Exploratory, Longitudinal\* Function to produce output for
  clinical event concordance with specialty for multi site, exploratory,
  across time

- Multi-Site, Exploratory, Longitudinal\* Function to produce output for
  clinical event concordance with specialty for multi site, exploratory,
  across time

## Usage

``` r
cnc_sp_ms_exp_la(data_tbl, facet = NULL, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- data_tbl:

  table which must contain the cols: time_start \| codeset_name \|
  specialty_name \| site

- facet:

  if supplied, variable to facet the plot by

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

line plot, with time on x axis, proportion on y, line color determined
by site with a dotted line for the all-site mean
