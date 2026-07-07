# celecx: Computer Experiment LEarning Curve eXtrapolation

Batch-sequential active learning for expensive computer experiments,
together with learning-curve extrapolation for forecasting the remaining
evaluation budget. Provides uncertainty-based and pool-based query
strategies (greedy sampling, query-by-committee, inverse-distance-based
active learning, and others) built on the 'bbotk' and 'mlr3mbo'
optimization framework, and callbacks that record held-out surrogate
performance after every batch. The resulting learning curves are
represented as ordinary 'mlr3' tasks with dedicated learners (parametric
curve fits, monotone smoothers, residual-bootstrap, conformal, and
forward-simulation methods), point and distributional measures, and
batch-wise expanding-window resampling, so extrapolation methods can be
benchmarked like regression learners. Trained extrapolators can be
turned into forecasts of the number of additional batches needed to
reach a target performance.

## See also

Useful links:

- <https://mlr-org.github.io/celecx/>

- <https://github.com/mlr-org/celecx>

- Report bugs at <https://github.com/mlr-org/celecx/issues>

## Author

**Maintainer**: Martin Binder <mlr.developer@mb706.com>
