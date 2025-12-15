# Pool-Based Optimizer

Optimizer that generates a pool of candidates and evaluates them all.
This is an extension of
[bbotk::OptimizerBatchRandomSearch](https://bbotk.mlr-org.com/reference/mlr_optimizers_random_search.html)
that allows for more flexible candidate generation.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Parameters

- `candidate_generator` (`function`)  
  Function to generate candidate points. Must have signature
  `function(search_space, n)` and return a `data.table` of n candidates.
  Default is
  [`candidate_generator_lhs()`](https://celecx.mlr-org.com/reference/candidate_generator_lhs.md).

- `pool_size` (`integer(1)`)  
  Number of candidates to generate and evaluate. If `NULL`, calculates
  remaining evaluations from the terminator (requires
  [bbotk::TerminatorEvals](https://bbotk.mlr-org.com/reference/mlr_terminators_evals.html)).
  Default is to use the remaining evaluations from the terminator.

## Examples

``` r
if (FALSE) { # \dontrun{
optimizer <- opt("pool")
optimizer$param_set$set_values(
  candidate_generator = candidate_generator_lhs(),
  pool_size = 100L
)
} # }
```
