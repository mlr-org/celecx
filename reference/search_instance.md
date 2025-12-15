# Create Search Instance

Convenience constructor for
[SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md).

## Usage

``` r
search_instance(objective, terminator, search_space = NULL, ...)
```

## Arguments

- objective:

  ([bbotk::Objective](https://bbotk.mlr-org.com/reference/Objective.html))  
  The objective to evaluate.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Termination criterion.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Optional restricted search space.

- ...:

  Additional arguments passed to SearchInstance\$new().

## Value

A
[SearchInstance](https://celecx.mlr-org.com/reference/SearchInstance.md)
object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create instance with evaluation budget
terminator <- bbotk::TerminatorEvals$new()
terminator$param_set$set_values(n_evals = 100L, k = 0L)
instance <- search_instance(objective, terminator = terminator)
} # }
```
