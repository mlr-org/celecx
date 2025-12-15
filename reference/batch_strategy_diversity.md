# Diversity Batch Strategy

Combines acquisition score with distance to already-selected points.
Trades off exploitation (good acquisition) with exploration (diversity).

## Usage

``` r
batch_strategy_diversity(diversity_weight = 0.5)
```

## Arguments

- diversity_weight:

  (`numeric(1)`)  
  Weight for distance component. Range `[0, 1]`.

  - 0: Pure acquisition (same as greedy)

  - 1: Pure diversity (ignore acquisition scores)

  - 0.5: Equal weight to both

## Value

A batch strategy function.

## Details

Combined score for candidate i:

    combined = (1 - w) * normalized_acq[i] + w * min_distance_to_selected[i]

After each selection, distances are recomputed.
