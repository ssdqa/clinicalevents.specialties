# Function to insert a column into a table indicating whether the record is in the top n for the given group

Function to insert a column into a table indicating whether the record
is in the top n for the given group

## Usage

``` r
insert_top_n_indicator(dat, gp_cols, val_col, n, sum_first = FALSE)
```

## Arguments

- dat:

  table containing the data

- gp_cols:

  columns to group by the top n will be taken from each group, so the
  total indicated as "top" hits will be \# of groups \* n

- val_col:

  column to order by when determining the top n

- n:

  number of records to indicate as top within the group

- sum_first:

  boolean indicating whether a value should be grouped and summed prior
  to identifying the top N

## Value

the original `dat` table with all original columns, plus a column
`top_n_indicator` which is TRUE if the record is in the top n and FALSE
if not
