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
ParamUty. Note that paradox's `$check_dt()` cannot be used for
construction-time validation because we accept data formats that paradox
doesn't (e.g., factor columns for ParamFct, which paradox expects as
character). Our validation handles the conversion/coercion that prepares
data for paradox-compatible queries.

As of bbotk \>= 0.9.0, codomains can have "learn" tags in addition to
"minimize"/"maximize", so we check for any target tags.
