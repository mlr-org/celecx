# Get Target IDs from Codomain

Returns IDs of parameters tagged as targets (minimize, maximize, or
learn).

## Usage

``` r
codomain_target_ids(codomain)
```

## Arguments

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  or
  [bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html))  
  The codomain to inspect.

## Value

Character vector of target parameter IDs.

## Details

This is a convenience wrapper around `codomain$target_ids` that works
with both ParamSet and Codomain objects. If you know you have a Codomain
object, you can use `codomain$target_ids` directly.
