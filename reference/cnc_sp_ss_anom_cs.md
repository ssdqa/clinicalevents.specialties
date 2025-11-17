# *Single Site, Anomaly, Cross-Sectional*

Function to produce output for clinical event concordance with specialty
for single site, anomaly, not over time

## Usage

``` r
cnc_sp_ss_anom_cs(data_tbl, facet = NULL)
```

## Arguments

- data_tbl:

  table which must contain the cols: specialty_name \| prop \| median \|
  n_mad

- facet:

  vector of variable names to be used to facet the graph

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of visits for
a given cluster, and the size of the dot represents the mean proportion
across all specialties
