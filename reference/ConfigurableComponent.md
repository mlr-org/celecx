# ConfigurableComponent

Base class for objects that unifies configuration of state.
`ConfigurableComponent`-objects should always have active bindings that
reflect the arguments given to their constructor. They also have a
`$param_set` field containing a
[`paradox::ParamSet`](https://paradox.mlr-org.com/reference/ParamSet.html),
which are used for more fine-grained configuration of the object. Things
that somehow determine how an object can be used (e.g. what kinds of
things it is compatible with), particularly things that can not be
changed after constructions should likely be construction arguments;
others should likely be in `$param_set`.

Initialize a ConfigurableComponent.

Helper for print outputs.

Printer.

Set parameter values and fields in one step.

## Arguments

- id:

  (`character(1)`) Identifier of the object. If left as `NULL`, the
  object does not get an ID.

- param_set:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`) Set of hyperparameters / configuration parameters.

- additional_configuration:

  (`character(1)`) Additional configuration settings. Anything passed
  here must be a field or active binding of the object that is not
  passed as an initialization argument to the (top-level) constructor.

- additional_phash_input:

  ([`character()`](https://rdrr.io/r/base/character.html)) Names of
  fields (in `self` or `private`) whose values should influence the
  persistent hash (`phash`). Useful for subclasses that expose
  additional configuration state outside of constructor arguments or the
  parameter set. Note that entries of `additional_configuration` are
  *not* automatically included (as they may be expensive to compute).
  `additional_phash_input` should reference the data from which
  `additional_configuration` fields are computed.

- ...:

  (named `any`)

- .values:

  (named [`list()`](https://rdrr.io/r/base/list.html))

## Details

The state of the object should be completely determined by:

- arguments passed for initialization, all of which must be accessible
  as fields or active bindings of the object.

- param_set configuration parameteres

- additional fields / active bindings, which must be listed as
  `additional_configuration`.

- there may be other active bindings, but these should not convey any
  relevant state.

## Fields

- `id`:

  (`character(1)`) Identifier of the object. Used in tables, plot and
  text output.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  \| `NULL`) Set of hyperparameters.

- `hash`:

  (`character(1)`) Stable hash that includes id, parameter values (if
  present) and additional configuration settings (from construction or
  class fields) but not state. Makes use of the
  `private$.additional_phash_input()` function to collect additional
  information, which must therefore be implemented by subclasses.

- `phash`:

  (`character(1)`) Hash that includes id and additional configuration
  settings (from construction or class fields) but not parameter values
  and no state. Makes use of the `private$.additional_phash_input()`
  function to collect additional information, which must therefore be
  implemented by subclasses.
