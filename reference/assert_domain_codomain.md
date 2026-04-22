# Validate Domain and Codomain Structure

Validates that a domain and codomain have valid structure for use with
Objective classes. Uses paradox's `assert_param_set` for type validation
and adds checks for non-empty domain and presence of targets.

## Usage

``` r
assert_domain_codomain(domain, codomain)
```

## Arguments

- domain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the input space.

- codomain:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Parameter set describing the output space.

## Value

Named list with `domain_ids`, `codomain_ids`, and `codomain_target_ids`.

## Details

Uses `assert_param_set(domain, no_untyped = TRUE)` which rejects
ParamUty.

Requires bbotk \>= 0.9.0, as codomains can have "learn" tags for active
learning in addition to "minimize"/"maximize".
