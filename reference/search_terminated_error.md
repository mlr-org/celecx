# Search Terminated Error

Creates a condition indicating that a SearchInstance has terminated.
Inherits from bbotk's `terminated_error`.

## Usage

``` r
search_terminated_error(search_instance)
```

## Arguments

- search_instance:

  ([SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md))  
  The SearchInstance that terminated.

## Value

A condition object with class `search_terminated_error` (inherits from
`terminated_error`, `error`, `condition`).
