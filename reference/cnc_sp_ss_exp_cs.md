# 

- Single Site, Exploratory, Cross-Sectional\* Function to produce output
  for clinical event concordance with specialty for single site,
  exploratory, not over time

- Single Site, Exploratory, Cross-Sectional\* Function to produce output
  for clinical event concordance with specialty for single site,
  exploratory, not over time

## Usage

``` r
cnc_sp_ss_exp_cs(data_tbl, facet, x_var, y_var, fill_var, top_n)
```

## Arguments

- data_tbl:

  table with the data to plot

- facet:

  list of one or more variables to facet the plot on

- x_var:

  variable to plot on the x axis

- y_var:

  variable to plot on the y axis

- fill_var:

  variable to fill bars with

- top_n:

  an integer indicating the top N number of specialties to display on
  the graph

## Value

a bar plot based on the values of `x_var`, `y_var`, `fill_var`, `facet`
