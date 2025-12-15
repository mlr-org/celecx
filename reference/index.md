# Package index

## Package

- [`celecx`](https://celecx.mlr-org.com/reference/celecx-package.md)
  [`celecx-package`](https://celecx.mlr-org.com/reference/celecx-package.md)
  : celecx: Computer Experiment LEarning Curve eXtrapolation

## Active Learning

Main entry points for running active learning experiments.

- [`optimize_active()`](https://celecx.mlr-org.com/reference/optimize_active.md)
  : Run Active Learning
- [`optimizer_active_learning()`](https://celecx.mlr-org.com/reference/optimizer_active_learning.md)
  : Active Learning Optimizer Factory

## Search Instance

Instance classes for managing active learning runs.

- [`SearchInstance`](https://celecx.mlr-org.com/reference/SearchInstance.md)
  : Search Instance
- [`search_instance()`](https://celecx.mlr-org.com/reference/search_instance.md)
  : Create Search Instance
- [`ContextSearch`](https://celecx.mlr-org.com/reference/ContextSearch.md)
  : Context for Search Instance
- [`search_terminated_error()`](https://celecx.mlr-org.com/reference/search_terminated_error.md)
  : Search Terminated Error

## Objectives

Objective functions for computer experiments.

- [`ObjectiveDataset`](https://celecx.mlr-org.com/reference/ObjectiveDataset.md)
  : Objective Function Based on Pre-evaluated Dataset
- [`ObjectiveLearner`](https://celecx.mlr-org.com/reference/ObjectiveLearner.md)
  : Objective Function Based on a Fitted Learner

## Surrogate Learners

Learners with uncertainty quantification for active learning.

- [`mlr_learners_regr.bootstrap_se`](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md)
  [`LearnerRegrBootstrapSE`](https://celecx.mlr-org.com/reference/mlr_learners_regr.bootstrap_se.md)
  : Bootstrap Ensemble Learner with SE Prediction
- [`mlr_learners_regr.quantile_se`](https://celecx.mlr-org.com/reference/mlr_learners_regr.quantile_se.md)
  [`LearnerRegrQuantileSE`](https://celecx.mlr-org.com/reference/mlr_learners_regr.quantile_se.md)
  : Quantile Regression Learner with SE Prediction

## Batch Selection

Strategies for selecting multiple points per iteration.

- [`BatchProposer`](https://celecx.mlr-org.com/reference/BatchProposer.md)
  : Batch Proposer
- [`batch_strategies`](https://celecx.mlr-org.com/reference/batch_strategies.md)
  : Batch Selection Strategies
- [`batch_strategy_diversity()`](https://celecx.mlr-org.com/reference/batch_strategy_diversity.md)
  : Diversity Batch Strategy
- [`batch_strategy_greedy()`](https://celecx.mlr-org.com/reference/batch_strategy_greedy.md)
  : Greedy Batch Strategy
- [`batch_strategy_local_penalization()`](https://celecx.mlr-org.com/reference/batch_strategy_local_penalization.md)
  : Local Penalization Batch Strategy

## Candidate Generators

Methods for generating candidate points in pool-based optimization.

- [`candidate_generators`](https://celecx.mlr-org.com/reference/candidate_generators.md)
  : Candidate Point Generators
- [`candidate_generator_grid()`](https://celecx.mlr-org.com/reference/candidate_generator_grid.md)
  : Grid Candidate Generator
- [`candidate_generator_lhs()`](https://celecx.mlr-org.com/reference/candidate_generator_lhs.md)
  : LHS Candidate Generator
- [`candidate_generator_local()`](https://celecx.mlr-org.com/reference/candidate_generator_local.md)
  : Local Candidate Generator
- [`candidate_generator_mixed()`](https://celecx.mlr-org.com/reference/candidate_generator_mixed.md)
  : Mixed Candidate Generator
- [`candidate_generator_random()`](https://celecx.mlr-org.com/reference/candidate_generator_random.md)
  : Random Candidate Generator
- [`candidate_generator_sobol()`](https://celecx.mlr-org.com/reference/candidate_generator_sobol.md)
  : Sobol Candidate Generator

## Optimizers

Acquisition function optimizers.

- [`mlr_optimizers_pool`](https://celecx.mlr-org.com/reference/mlr_optimizers_pool.md)
  [`OptimizerPool`](https://celecx.mlr-org.com/reference/mlr_optimizers_pool.md)
  : Pool-Based Optimizer

## Metrics Tracking

Track and record metrics during active learning.

- [`MetricsTracker`](https://celecx.mlr-org.com/reference/MetricsTracker.md)
  : Metrics Tracker
- [`metrics_tracker()`](https://celecx.mlr-org.com/reference/metrics_tracker.md)
  : Create Metrics Tracker
- [`celecx.metrics_tracker`](https://celecx.mlr-org.com/reference/celecx.metrics_tracker.md)
  [`CallbackMetricsTracker`](https://celecx.mlr-org.com/reference/celecx.metrics_tracker.md)
  : Metrics Tracker Callback
- [`search_metrics`](https://celecx.mlr-org.com/reference/search_metrics.md)
  : Search Metrics
- [`metric_best_y()`](https://celecx.mlr-org.com/reference/metric_best_y.md)
  : Best Y Metric
- [`metric_integrated_variance()`](https://celecx.mlr-org.com/reference/metric_integrated_variance.md)
  : Integrated Variance Metric
- [`metric_max_variance()`](https://celecx.mlr-org.com/reference/metric_max_variance.md)
  : Maximum Variance Metric
- [`metric_mean_variance()`](https://celecx.mlr-org.com/reference/metric_mean_variance.md)
  : Mean Variance Metric
- [`metric_model_mae()`](https://celecx.mlr-org.com/reference/metric_model_mae.md)
  : Model MAE Metric
- [`metric_model_r2()`](https://celecx.mlr-org.com/reference/metric_model_r2.md)
  : Model R-squared Metric
- [`metric_model_rmse()`](https://celecx.mlr-org.com/reference/metric_model_rmse.md)
  : Model RMSE Metric
- [`metric_regret()`](https://celecx.mlr-org.com/reference/metric_regret.md)
  : Regret Metric
- [`metric_simple_regret()`](https://celecx.mlr-org.com/reference/metric_simple_regret.md)
  : Simple Regret Metric
- [`metric_worst_y()`](https://celecx.mlr-org.com/reference/metric_worst_y.md)
  : Worst Y Metric
- [`make_metric()`](https://celecx.mlr-org.com/reference/make_metric.md)
  : Make Metric Function

## Codomain Helpers

Utilities for working with objective codomains.

- [`codomain_goal()`](https://celecx.mlr-org.com/reference/codomain_goal.md)
  : Determine Goal from Codomain
- [`codomain_has_learn()`](https://celecx.mlr-org.com/reference/codomain_has_learn.md)
  : Check if Codomain Has Learning Targets
- [`codomain_has_optimize()`](https://celecx.mlr-org.com/reference/codomain_has_optimize.md)
  : Check if Codomain Has Optimization Targets
- [`codomain_helpers`](https://celecx.mlr-org.com/reference/codomain_helpers.md)
  : Codomain Helpers for Active Learning
- [`codomain_learn_ids()`](https://celecx.mlr-org.com/reference/codomain_learn_ids.md)
  : Get Learning Target IDs
- [`codomain_optimize_ids()`](https://celecx.mlr-org.com/reference/codomain_optimize_ids.md)
  : Get Optimization Target IDs
- [`codomain_target_ids()`](https://celecx.mlr-org.com/reference/codomain_target_ids.md)
  : Get Target IDs from Codomain

## Base Classes

Abstract base classes and utilities.

- [`ConfigurableComponent`](https://celecx.mlr-org.com/reference/ConfigurableComponent.md)
  : ConfigurableComponent
- [`ResultAssignerNull`](https://celecx.mlr-org.com/reference/ResultAssignerNull.md)
  : Null Result Assigner
- [`hash_transform()`](https://celecx.mlr-org.com/reference/hash_transform.md)
  : Create Hash Digest of an Object
