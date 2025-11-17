# Function to generate a table of concept_id + concept_name for a set of concepts

Function to generate a table of concept_id + concept_name for a set of
concepts

## Usage

``` r
find_distinct_concepts(
  tbl,
  concept_col = "concept_id",
  vocab = vocabulary_tbl("concept")
)
```

## Arguments

- tbl:

  table with a specialty_concept_id column

- concept_col:

  the column in the vocabulary table that should be used to join to the
  input tbl

- vocab:

  table with a concept_id and concept_name column, defaulting to the
  vocabulary_tbl `concept`

## Value

table with distinct specialty_concept_id \| concept_name
