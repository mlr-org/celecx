# Local Candidate Generator

Generates candidates locally around promising regions, identified from
the archive. Good for exploitation/refinement.

## Usage

``` r
candidate_generator_local(n_centers = 5L, radius = 0.1)
```

## Arguments

- n_centers:

  (`integer(1)`)  
  Number of archive points to sample around. Points are selected by
  their y values (best first).

- radius:

  (`numeric(1)`)  
  Sampling radius around each center, as a fraction of the domain range.
  Default 0.1 (10% of range).

## Value

A candidate generator function.

## Details

Algorithm:

1.  Select n_centers best points from archive

2.  For each center, generate n/n_centers local samples

3.  Local samples are uniform within a hypercube around the center

Note: This generator needs access to the archive, so the signature is
extended to `function(search_space, n, archive)`.
