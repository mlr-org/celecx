# LHS Candidate Generator

Generates candidates using Latin Hypercube Sampling, which provides good
space-filling properties.

## Usage

``` r
candidate_generator_lhs()
```

## Value

A candidate generator function.

## Details

LHS ensures that each variable's marginal distribution is uniformly
covered, even with a relatively small number of samples. Good default
choice for most problems.

Uses
[`paradox::generate_design_lhs()`](https://paradox.mlr-org.com/reference/generate_design_lhs.html)
internally.
