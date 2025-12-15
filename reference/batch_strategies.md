# Batch Selection Strategies

Strategies for selecting multiple points from scored candidates. These
are used by
[BatchProposer](https://celecx.mlr-org.com/reference/BatchProposer.md)
to construct batches for parallel evaluation.

## Details

When batch_size \> 1, selecting the top-k candidates by acquisition
score may result in redundant evaluations (e.g., many points clustered
around the current best). Batch strategies address this by diversifying
the selection.

All batch strategy functions return a **strategy function** with
signature:

    function(candidates, scores, batch_size, surrogate, archive, search_space)

The strategy function returns integer indices into the candidates table.

## Available Strategies

- [`batch_strategy_greedy()`](https://celecx.mlr-org.com/reference/batch_strategy_greedy.md):
  Select top-k by acquisition score (baseline)

- [`batch_strategy_local_penalization()`](https://celecx.mlr-org.com/reference/batch_strategy_local_penalization.md):
  Penalize around selected points

- [`batch_strategy_diversity()`](https://celecx.mlr-org.com/reference/batch_strategy_diversity.md):
  Mix acquisition score with distance to selected
