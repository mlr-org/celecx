# Archive-Backed Surrogate Adapter

Adapter for acquisition functions that need archive, search-space, and
codomain context but do not need model predictions.

`SurrogateNull` inherits from
[mlr3mbo::Surrogate](https://mlr3mbo.mlr-org.com/reference/Surrogate.html)
so that
[mlr3mbo::AcqFunction](https://mlr3mbo.mlr-org.com/reference/AcqFunction.html)
can derive its domain, codomain, and archive through the usual
`$surrogate` active binding. Its update and reset methods are no-ops,
and `$predict()` errors because there is no model.

Creates a new archive-backed surrogate adapter.

Prediction is intentionally unsupported.

## Arguments

- archive:

  (`NULL` \|
  [bbotk::Archive](https://bbotk.mlr-org.com/reference/Archive.html))  
  Archive carrying search-space and observed-evaluation context.

- cols_x:

  (`NULL` \| [`character()`](https://rdrr.io/r/base/character.html))  
  Feature columns. Defaults to `archive$cols_x`.

- cols_y:

  (`NULL` \| [`character()`](https://rdrr.io/r/base/character.html))  
  Target columns. Defaults to `archive$cols_y`.

- xdt:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  New data.

## Fields

- `print_id`:

  (`character(1)`) Short printer id.

- `n_learner`:

  (`integer(1)`) Number of represented target columns.

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Required
  packages.

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Supported
  feature types.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Surrogate
  properties.

- `predict_type`:

  (`character(1)`) Prediction type placeholder.

## See also

Other Acquisition Function:
[`AcqFunctionDist`](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md),
[`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistGSx.md),
[`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIDEAL.md),
[`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIGS.md),
[`AcqFunctionGSy`](https://mlr-org.github.io/celecx/reference/AcqFunctionGSy.md)
