# Local Penalization Batch Strategy

After selecting each point, penalizes nearby candidates before selecting
the next. Encourages spatial diversity in the batch.

## Usage

``` r
batch_strategy_local_penalization(bandwidth = 0.1, penalization = Inf)
```

## Arguments

- bandwidth:

  (`numeric(1)`)  
  Penalization bandwidth. Points within this distance (normalized) of a
  selected point have their scores penalized. Default is 0.1 (10% of
  domain range).

- penalization:

  (`numeric(1)`)  
  How much to worsen scores of nearby points. Added to acquisition
  score. Default is Inf (effectively removes nearby candidates).

## Value

A batch strategy function.

## Details

Algorithm:

1.  Select the best candidate

2.  Penalize scores of candidates within `bandwidth` of the selected
    point

3.  Select the next best (from penalized scores)

4.  Repeat until batch is complete

Distance is computed using Gower distance (handles mixed types).

## References

Ginsbourger, D., Le Riche, R., & Carraro, L. (2010). Kriging is
well-suited to parallelize optimization.
