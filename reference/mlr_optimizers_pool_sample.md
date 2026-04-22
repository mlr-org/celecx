# Sampler-Based Search on Pool-Restricted or Discrete Objectives

Optimizer that samples configurations with a
[SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md)
from a candidate pool when the objective has the `"pool_restricted"`
property (typically by inheriting from
[ObjectivePoolAbstract](https://celecx.mlr-org.com/reference/ObjectivePoolAbstract.md)),
or from the full grid of a completely discrete search space (where all
parameters are
[paradox::p_int](https://paradox.mlr-org.com/reference/Domain.html),
[paradox::p_fct](https://paradox.mlr-org.com/reference/Domain.html), or
[paradox::p_lgl](https://paradox.mlr-org.com/reference/Domain.html)).

When the search space is continuous (not fully discrete) and the
objective is not pool-restricted, this optimizer samples directly from
the search space.

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

## Arguments

- space_sampler:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Sampler used to draw each batch. Default is
  [SpaceSamplerUniform](https://celecx.mlr-org.com/reference/mlr_space_samplers_uniform.md).

- grid_expansion_limit:

  (`integer(1)`)  
  Upper limit on the number of rows to materialize for the full grid for
  discrete search spaces in Objectives that are not pool-restricted. An
  error is raised if the full grid would exceed this limit. This is to
  protect against accidental usage of too much memory. Default is
  `1e7L`.

## Fields

- `space_sampler`:

  ([SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md))  
  Wrapped sampler.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Combined parameter set of this optimizer and the wrapped sampler.

## Parameters

- `batch_size`:

  `integer(1)`  
  Number of configurations per batch. `Inf` evaluates the entire
  remaining pool in a single batch when `replace_samples = "never"`, or
  the full pool in each batch when
  `replace_samples = "between_batches"`. `Inf` is forbidden when the
  objective is not pool-restricted and the search space is not fully
  discrete. Default is `1`.

- `max_batches`:

  `integer(1)`  
  Maximum number of batches to evaluate. `Inf` means no batch limit
  (termination is governed solely by the instance's terminator). Default
  is `Inf`.

- `replace_samples`:

  `character(1)`  
  Controls duplicate handling across evaluations:

  - `"never"`: configurations already present in the archive are
    excluded from sampling. The pool shrinks as evaluations accumulate.

  - `"between_batches"`: each batch is sampled from the full pool, but
    previous batches do not affect future sampling.

  Default is `"never"`.

- `pass_known_pool`:

  `logical(1)`  
  Whether evaluated configurations from the archive are passed to the
  [SpaceSampler](https://celecx.mlr-org.com/reference/SpaceSampler.md)'s
  [`sample()`](https://rdrr.io/r/base/sample.html) method as
  `known_pool`. Default is `TRUE`.

The wrapped sampler's parameters are exposed with a `space_sampler.`
prefix.
