# *Multi-Site, Exploratory, Cross-Sectional* Function to produce output for clinical event concordance with specialty for multi site, exploratory, not over time

*Multi-Site, Exploratory, Cross-Sectional* Function to produce output
for clinical event concordance with specialty for multi site,
exploratory, not over time

## Usage

``` r
cnc_sp_ms_exp_cs(data_tbl, facet = NULL, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- data_tbl:

  table with the data to plot

- facet:

  list of one or more variables to facet the plot on

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot of specialty against proportion of visits with that specialty
at each site, with dot color representing site
