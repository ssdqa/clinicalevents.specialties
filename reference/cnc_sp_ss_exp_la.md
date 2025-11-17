# *Single Site, Exploratory, Longitudinal* Function to produce output for clinical event concordance with specialty for single site, exploratory, across time

*Single Site, Exploratory, Longitudinal* Function to produce output for
clinical event concordance with specialty for single site, exploratory,
across time

## Usage

``` r
cnc_sp_ss_exp_la(data_tbl, facet = NULL)
```

## Arguments

- data_tbl:

  table with the data to plot

- facet:

  list of one or more variables to facet the plot on

## Value

a plotly line plot of the proportion of visits with each specialty
against time, with line color representing specialty
