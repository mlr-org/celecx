# LCE Prediction Data

Methods for the intermediate
[mlr3::PredictionData](https://mlr3.mlr-org.com/reference/PredictionData.html)
representation of
[PredictionLCE](https://mlr-org.github.io/celecx/reference/PredictionLCE.md):
validation, missingness detection, row filtering, and concatenation
across resampling iterations (which re-applies the attributes of matrix
predict-type columns that row-subsetting drops).

## Usage

``` r
# S3 method for class 'PredictionDataLCE'
check_prediction_data(pdata, ...)

# S3 method for class 'PredictionDataLCE'
is_missing_prediction_data(pdata, ...)

# S3 method for class 'PredictionDataLCE'
filter_prediction_data(pdata, row_ids, ...)

# S3 method for class 'PredictionDataLCE'
c(..., keep_duplicates = TRUE)
```

## Arguments

- pdata:

  (`PredictionDataLCE`)  
  Named list of prediction columns, inheriting from
  `"PredictionDataLCE"`.

- ...:

  (`PredictionDataLCE` objects \| ignored)  
  For the [`c()`](https://rdrr.io/r/base/c.html) method, the objects to
  concatenate; ignored otherwise.

- row_ids:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices to keep.

- keep_duplicates:

  (`logical(1)`)  
  If `FALSE`, rows with row ids that reappear in later objects are
  removed, keeping the last occurrence.
