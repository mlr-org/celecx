# Sobol Candidate Generator

Generates candidates using Sobol quasi-random sequences, which have even
better space-filling properties than LHS for low discrepancy.

## Usage

``` r
candidate_generator_sobol()
```

## Value

A candidate generator function.

## Details

Sobol sequences are deterministic and provide excellent coverage.
However, they're designed for continuous spaces; categorical variables
are handled by discretizing.
