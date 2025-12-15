# Search Terminated Error

Creates a condition indicating that a SearchInstance has terminated.
This is similar to bbotk's `terminated_error` but for SearchInstance.

## Usage

``` r
search_terminated_error(search_instance)
```

## Arguments

- search_instance:

  ([SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md))  
  The SearchInstance that terminated.

## Value

A condition object with class `search_terminated_error`.
