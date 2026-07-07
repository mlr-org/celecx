# Build an `ObjectiveLearner` from Archive Data

Wraps a trained regression learner (the "oracle") as an
[ObjectiveLearner](https://mlr-org.github.io/celecx/dev/reference/ObjectiveLearner.md)
over a normalized version of a search space, suitable for replaying an
active-learning loop forward.

## Usage

``` r
objective_learner_from_archive(
  oracle,
  search_space,
  codomain,
  x_data,
  pool = NULL
)
```

## Arguments

- oracle:

  ([mlr3::LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.html))  
  Trained regression learner mapping the search-space columns to the
  objective value.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Search space whose ids equal the oracle's training features.

- codomain:

  ([bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html))  
  Codomain of the originating run.

- x_data:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Archive feature values used to impute finite bounds for unbounded
  parameters.

- pool:

  ([data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  \| `NULL`)  
  Optional candidate pool whose values are also covered by the imputed
  bounds.

## Value

Named list with `objective`
([ObjectiveLearner](https://mlr-org.github.io/celecx/dev/reference/ObjectiveLearner.md))
and `search_space` (the normalized
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)).

## Details

Normalization makes the search space usable as an untransformed proposal
box: any transformation is dropped (the oracle is fit on, and proposals
are made on, the untransformed scale), and non-finite numeric bounds are
imputed from the range of the supplied data so samplers and terminators
have a finite box. Only `ParamDbl`, `ParamInt`, `ParamFct`, and
`ParamLgl` parameters are supported.
