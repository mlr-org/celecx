# Random Candidate Generator

Generates candidates using pure random sampling.

## Usage

``` r
candidate_generator_random()
```

## Value

A candidate generator function.

## Details

Simpler and faster than LHS, but may have poor coverage with small n.
Use when speed matters more than coverage, or when n is very large.

Uses
[`paradox::generate_design_random()`](https://paradox.mlr-org.com/reference/generate_design_random.html)
internally.
