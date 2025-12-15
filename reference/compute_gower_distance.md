# Compute Gower Distance Matrix

Computes pairwise Gower distances, which handle mixed numeric and
categorical variables.

## Usage

``` r
compute_gower_distance(x, y = NULL, search_space = NULL)
```

## Arguments

- x:

  (`data.table`) First set of points.

- y:

  (`data.table`) Second set of points (optional; if NULL, computes
  distances within x).

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))
  For normalization bounds.

## Value

Distance matrix with nrow(x) rows and nrow(y) columns (or nrow(x)
columns if y is NULL).
