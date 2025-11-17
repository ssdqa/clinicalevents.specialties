# *Multi-Site, Anomaly, Cross-Sectional*

*Multi-Site, Anomaly, Cross-Sectional*

## Usage

``` r
cnc_sp_ms_anom_cs(
  process_output,
  title,
  text_wrapping_char = 60,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  the output from the cnc_sp check, summarized to be used in the multi
  site anomaly detection check

- title:

  text containing the title for the plot

- text_wrapping_char:

  the number of characters for the specialty names to be displayed on
  the plot

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of visits for
a given specialty, and the size of the dot represents the mean
proportion across all sites
