# Mixed Candidate Generator

Combines global and local sampling for a balance of exploration and
exploitation.

## Usage

``` r
candidate_generator_mixed(
  global_fraction = 0.5,
  global_generator = NULL,
  local_generator = NULL
)
```

## Arguments

- global_fraction:

  (`numeric(1)`)  
  Fraction of candidates from global sampling (LHS). Default 0.5.

- global_generator:

  (`function`)  
  Generator for global samples. Default is LHS.

- local_generator:

  (`function`)  
  Generator for local samples. Default is local generator.

## Value

A candidate generator function.

## Details

Splits the requested n candidates between global and local generators.
Useful for maintaining exploration while focusing on promising regions.
