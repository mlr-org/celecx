# Package index

## Package

- [`celecx`](https://mlr-org.github.io/celecx/reference/celecx-package.md)
  [`celecx-package`](https://mlr-org.github.io/celecx/reference/celecx-package.md)
  : celecx: Computer Experiment LEarning Curve eXtrapolation

## Active Learning

High-level entry points for running active learning.

- [`optimize_active()`](https://mlr-org.github.io/celecx/reference/optimize_active.md)
  : Run Active Learning
- [`optimizer_active_learning()`](https://mlr-org.github.io/celecx/reference/optimizer_active_learning.md)
  : Active Learning Optimizer Factory
- [`optimizer_pool_al()`](https://mlr-org.github.io/celecx/reference/optimizer_pool_al.md)
  : Convenience Constructor for Pool-Based Active Learning Optimizers

## Search Instance

Instance, context, and error for managing a search run.

- [`SearchInstance`](https://mlr-org.github.io/celecx/reference/SearchInstance.md)
  : Search Instance
- [`search_instance()`](https://mlr-org.github.io/celecx/reference/search_instance.md)
  : Create Search Instance
- [`ContextSearch`](https://mlr-org.github.io/celecx/reference/ContextSearch.md)
  : Context for Search Instance
- [`search_terminated_error()`](https://mlr-org.github.io/celecx/reference/search_terminated_error.md)
  : Search Terminated Error

## Optimizers

Optimizer base classes and concrete optimizers over search spaces or
candidate pools.

- [`OptimizerSearchAbstract`](https://mlr-org.github.io/celecx/reference/OptimizerSearchAbstract.md)
  : Abstract Base Class for Search-Compatible Optimizers
- [`OptimizerPoolAbstract`](https://mlr-org.github.io/celecx/reference/OptimizerPoolAbstract.md)
  : Abstract Base Class for Pool-Aware Optimizers
- [`OptimizerAL`](https://mlr-org.github.io/celecx/reference/OptimizerAL.md)
  : Proposer-Based Active Learning Optimizer
- [`mlr_optimizers_pool_random`](https://mlr-org.github.io/celecx/reference/mlr_optimizers_pool_random.md)
  [`OptimizerPoolRandom`](https://mlr-org.github.io/celecx/reference/mlr_optimizers_pool_random.md)
  : Random Search on Pool-Restricted or Discrete Objectives
- [`mlr_optimizers_pool_sample`](https://mlr-org.github.io/celecx/reference/mlr_optimizers_pool_sample.md)
  [`OptimizerPoolSample`](https://mlr-org.github.io/celecx/reference/mlr_optimizers_pool_sample.md)
  : Sampler-Based Search on Pool-Restricted or Discrete Objectives

## Active Learning Proposers

Proposers that build active-learning batches inside OptimizerAL.

- [`ALContext`](https://mlr-org.github.io/celecx/reference/ALContext.md)
  : Active Learning Proposal Context
- [`ALProposer`](https://mlr-org.github.io/celecx/reference/ALProposer.md)
  : Active Learning Proposer
- [`ALProposerScoreAbstract`](https://mlr-org.github.io/celecx/reference/ALProposerScoreAbstract.md)
  : Abstract Base Class for Score-Based Active Learning Proposers
- [`ALProposerScore`](https://mlr-org.github.io/celecx/reference/ALProposerScore.md)
  : Score-Based Active Learning Proposer
- [`ALProposerSequentialScore`](https://mlr-org.github.io/celecx/reference/ALProposerSequentialScore.md)
  : Sequential Score-Based Active Learning Proposer
- [`ALProposerSequentialReference`](https://mlr-org.github.io/celecx/reference/ALProposerSequentialReference.md)
  : Sequential Reference Active Learning Proposer
- [`ALProposerPseudoLabel`](https://mlr-org.github.io/celecx/reference/ALProposerPseudoLabel.md)
  : Pseudo-Label Active Learning Proposer
- [`ALProposerPortfolio`](https://mlr-org.github.io/celecx/reference/ALProposerPortfolio.md)
  : Portfolio Active Learning Proposer

## Score Modifiers

Modifiers that adjust acquisition scores during sequential batch
construction.

- [`ALScoreModifier`](https://mlr-org.github.io/celecx/reference/ALScoreModifier.md)
  : Active Learning Score Modifier
- [`ALScoreModifierDiversity`](https://mlr-org.github.io/celecx/reference/ALScoreModifierDiversity.md)
  : Diversity Active Learning Score Modifier
- [`ALScoreModifierLocalPenalization`](https://mlr-org.github.io/celecx/reference/ALScoreModifierLocalPenalization.md)
  : Local-Penalization Active Learning Score Modifier
- [`ALScoreModifierNone`](https://mlr-org.github.io/celecx/reference/ALScoreModifierNone.md)
  : No-Op Active Learning Score Modifier

## Acquisition Functions

Distance-aware acquisition functions for active learning.

- [`AcqFunctionDist`](https://mlr-org.github.io/celecx/reference/AcqFunctionDist.md)
  : Distance-Aware Acquisition Function Base Class
- [`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistGSx.md)
  : Distance-Aware GSx Acquisition Function
- [`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIDEAL.md)
  : Distance-Aware IDEAL Acquisition Function
- [`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/reference/AcqFunctionDistIGS.md)
  : Distance-Aware iGS Acquisition Function
- [`AcqFunctionGSy`](https://mlr-org.github.io/celecx/reference/AcqFunctionGSy.md)
  : GSy Acquisition Function

## Active Learning Distances

Distance objects, their dictionary, and sugar constructors.

- [`ALDistance`](https://mlr-org.github.io/celecx/reference/ALDistance.md)
  : Active Learning Distance Base Class
- [`ALDistanceGeometry`](https://mlr-org.github.io/celecx/reference/ALDistanceGeometry.md)
  : Geometry-Based Active Learning Distance
- [`mlr_al_distances`](https://mlr-org.github.io/celecx/reference/mlr_al_distances.md)
  : Dictionary of Active Learning Distances
- [`mlr_al_distances_affine`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_affine.md)
  [`ALDistanceAffine`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_affine.md)
  : Affine Active Learning Distance
- [`mlr_al_distances_gower`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_gower.md)
  [`ALDistanceGower`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_gower.md)
  : Gower Active Learning Distance
- [`mlr_al_distances_standardize`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_standardize.md)
  [`ALDistanceStandardize`](https://mlr-org.github.io/celecx/reference/mlr_al_distances_standardize.md)
  : Standardized Active Learning Distance
- [`clx_ald()`](https://mlr-org.github.io/celecx/reference/clx_ald.md) :
  Syntactic Sugar Active Learning Distance Construction
- [`clx_alds()`](https://mlr-org.github.io/celecx/reference/clx_alds.md)
  : Syntactic Sugar Active Learning Distances Construction

## Space Samplers

Space-filling and pool-based samplers, their dictionary, and sugar
constructors.

- [`SpaceSampler`](https://mlr-org.github.io/celecx/reference/SpaceSampler.md)
  : Space Sampler Base Class
- [`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/reference/SpaceSamplerDistance.md)
  : Distance-Based Space Sampler
- [`mlr_space_samplers`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers.md)
  : Dictionary of Space Samplers
- [`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md)
  [`SpaceSamplerChain`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_chain.md)
  : Chained Space Sampler
- [`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md)
  [`SpaceSamplerConditional`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_conditional.md)
  : Conditional Space Sampler
- [`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md)
  [`SpaceSamplerGSx`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_gsx.md)
  : GSx Space Sampler
- [`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md)
  [`SpaceSamplerKMeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmeans.md)
  : K-Means Space Sampler
- [`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md)
  [`SpaceSamplerKMedoids`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_kmedoids.md)
  : K-Medoids Space Sampler
- [`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md)
  [`SpaceSamplerLhs`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_lhs.md)
  : LHS Space Sampler
- [`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md)
  [`SpaceSamplerRelationalKMeans`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_relational_kmeans.md)
  : Relational K-Means Space Sampler
- [`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md)
  [`SpaceSamplerSobol`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_sobol.md)
  : Sobol Space Sampler
- [`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
  [`SpaceSamplerUniform`](https://mlr-org.github.io/celecx/reference/mlr_space_samplers_uniform.md)
  : Uniform Space Sampler
- [`clx_sps()`](https://mlr-org.github.io/celecx/reference/clx_sps.md) :
  Syntactic Sugar Space Sampler Construction
- [`clx_spss()`](https://mlr-org.github.io/celecx/reference/clx_spss.md)
  : Syntactic Sugar Space Samplers Construction

## Objectives

Objective functions and pool-restricted objective wrappers.

- [`ObjectiveDataset`](https://mlr-org.github.io/celecx/reference/ObjectiveDataset.md)
  : Objective Based on Pre-evaluated Dataset
- [`ObjectiveLearner`](https://mlr-org.github.io/celecx/reference/ObjectiveLearner.md)
  : Objective Function Based on a Fitted Learner
- [`ObjectivePoolAbstract`](https://mlr-org.github.io/celecx/reference/ObjectivePoolAbstract.md)
  : Abstract Base Class for Pool-backed Objectives
- [`ObjectivePoolRFun`](https://mlr-org.github.io/celecx/reference/ObjectivePoolRFun.md)
  : Objective Function Based on a Candidate Pool and R Function
- [`ObjectivePoolWrapper`](https://mlr-org.github.io/celecx/reference/ObjectivePoolWrapper.md)
  : Objective Function Wrapping Another Objective on a Candidate Pool

## Surrogate Learners

Regression learners with uncertainty quantification.

- [`mlr_learners_regr.bootstrap_se`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.bootstrap_se.md)
  [`LearnerRegrBootstrapSE`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.bootstrap_se.md)
  : Bootstrap Ensemble Learner with SE Prediction
- [`mlr_learners_regr.deepgp`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.deepgp.md)
  [`LearnerRegrDeepGP`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.deepgp.md)
  : Deep GP Regression Learner
- [`mlr_learners_regr.gpfit`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.gpfit.md)
  [`LearnerRegrGPfit`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.gpfit.md)
  : GPfit Regression Learner
- [`mlr_learners_regr.hetgp`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.hetgp.md)
  [`LearnerRegrHetGP`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.hetgp.md)
  : hetGP Regression Learner
- [`mlr_learners_regr.quantile_se`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.quantile_se.md)
  [`LearnerRegrQuantileSE`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.quantile_se.md)
  : Quantile Regression Learner with SE Prediction
- [`mlr_learners_regr.tgp`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.tgp.md)
  [`LearnerRegrTGP`](https://mlr-org.github.io/celecx/reference/mlr_learners_regr.tgp.md)
  : tgp Regression Learner

## Model Marshaling

S3 methods for (un)marshaling custom learner model states.

- [`marshal_model(`*`<learner_regr_bootstrap_se_state>`*`)`](https://mlr-org.github.io/celecx/reference/marshal_model.learner_regr_bootstrap_se_state.md)
  : Marshal Model for LearnerRegrBootstrapSE State
- [`marshal_model(`*`<learner_regr_quantile_se_state>`*`)`](https://mlr-org.github.io/celecx/reference/marshal_model.learner_regr_quantile_se_state.md)
  : Marshal Model for LearnerRegrQuantileSE State
- [`unmarshal_model(`*`<learner_regr_bootstrap_se_state_marshaled>`*`)`](https://mlr-org.github.io/celecx/reference/unmarshal_model.learner_regr_bootstrap_se_state_marshaled.md)
  : Unmarshal Model for LearnerRegrBootstrapSE State
- [`unmarshal_model(`*`<learner_regr_quantile_se_state_marshaled>`*`)`](https://mlr-org.github.io/celecx/reference/unmarshal_model.learner_regr_quantile_se_state_marshaled.md)
  : Unmarshal Model for LearnerRegrQuantileSE State

## Metrics Tracking

Tracker, callbacks, and helpers for logging metrics during a run.

- [`MetricsTracker`](https://mlr-org.github.io/celecx/reference/MetricsTracker.md)
  : Metrics Tracker
- [`metrics_tracker()`](https://mlr-org.github.io/celecx/reference/metrics_tracker.md)
  : Create Metrics Tracker
- [`make_metric()`](https://mlr-org.github.io/celecx/reference/make_metric.md)
  : Make Metric Function
- [`celecx.metrics_tracker`](https://mlr-org.github.io/celecx/reference/celecx.metrics_tracker.md)
  [`CallbackMetricsTracker`](https://mlr-org.github.io/celecx/reference/celecx.metrics_tracker.md)
  : Metrics Tracker Callback
- [`celecx.forecast_tracker`](https://mlr-org.github.io/celecx/reference/celecx.forecast_tracker.md)
  [`CallbackForecastTracker`](https://mlr-org.github.io/celecx/reference/celecx.forecast_tracker.md)
  : Forecast Tracker Callback

## Metrics

Built-in metric functions for optimization quality and model accuracy.

- [`search_metrics`](https://mlr-org.github.io/celecx/reference/search_metrics.md)
  : Search Metrics
- [`metric_best_y()`](https://mlr-org.github.io/celecx/reference/metric_best_y.md)
  : Best Y Metric
- [`metric_integrated_variance()`](https://mlr-org.github.io/celecx/reference/metric_integrated_variance.md)
  : Integrated Variance Metric
- [`metric_max_variance()`](https://mlr-org.github.io/celecx/reference/metric_max_variance.md)
  : Maximum Variance Metric
- [`metric_mean_variance()`](https://mlr-org.github.io/celecx/reference/metric_mean_variance.md)
  : Mean Variance Metric
- [`metric_model_mae()`](https://mlr-org.github.io/celecx/reference/metric_model_mae.md)
  : Model MAE Metric
- [`metric_model_r2()`](https://mlr-org.github.io/celecx/reference/metric_model_r2.md)
  : Model R-squared Metric
- [`metric_model_rmse()`](https://mlr-org.github.io/celecx/reference/metric_model_rmse.md)
  : Model RMSE Metric
- [`metric_regret()`](https://mlr-org.github.io/celecx/reference/metric_regret.md)
  : Regret Metric
- [`metric_simple_regret()`](https://mlr-org.github.io/celecx/reference/metric_simple_regret.md)
  : Simple Regret Metric
- [`metric_worst_y()`](https://mlr-org.github.io/celecx/reference/metric_worst_y.md)
  : Worst Y Metric

## Codomain Helpers

Accessors for optimization / learning targets in a codomain.

- [`codomain_goals()`](https://mlr-org.github.io/celecx/reference/codomain_goals.md)
  : Determine Goals from Codomain
- [`codomain_target_ids()`](https://mlr-org.github.io/celecx/reference/codomain_target_ids.md)
  : Get Target IDs from Codomain

## Design Generation

Grid and volume helpers for search spaces.

- [`generate_design_grid_celecx()`](https://mlr-org.github.io/celecx/reference/generate_design_grid_celecx.md)
  : Generate a Dependency-Aware Grid Design
- [`generate_default_grid()`](https://mlr-org.github.io/celecx/reference/generate_default_grid.md)
  : Generate Default Grid
- [`compute_domain_volume()`](https://mlr-org.github.io/celecx/reference/compute_domain_volume.md)
  : Compute Domain Volume

## Validation Helpers

Assertion utilities used by domain, codomain, and pool checks.

- [`assert_data_table_param_set()`](https://mlr-org.github.io/celecx/reference/assert_data_table_param_set.md)
  :

  Validate `data.table` Against `ParamSet`

- [`assert_domain_codomain()`](https://mlr-org.github.io/celecx/reference/assert_domain_codomain.md)
  : Validate Domain and Codomain Structure

- [`assert_learner_domain()`](https://mlr-org.github.io/celecx/reference/assert_learner_domain.md)
  : Validate Learner Against Domain

- [`assert_param_type_compatible()`](https://mlr-org.github.io/celecx/reference/assert_param_type_compatible.md)
  : Assert Parameter Type Compatibility

- [`assert_param_uty_custom_check()`](https://mlr-org.github.io/celecx/reference/assert_param_uty_custom_check.md)
  :

  Assert `ParamUty` Custom Checks for a data.table Column

- [`assert_pool_objective_search_space()`](https://mlr-org.github.io/celecx/reference/assert_pool_objective_search_space.md)
  : Assert Pool Objective Search-Space Compatibility

- [`objective_uses_dt_eval()`](https://mlr-org.github.io/celecx/reference/objective_uses_dt_eval.md)
  : Detect Objectives with Native data.table Evaluation Semantics

- [`param_set_topo_ids()`](https://mlr-org.github.io/celecx/reference/param_set_topo_ids.md)
  : Topologically sort parameter IDs by dependency order

- [`param_set_valid_mask_dt()`](https://mlr-org.github.io/celecx/reference/param_set_valid_mask_dt.md)
  :

  Get `logical(nrow(dt))` mask of valid configurations in a data.table

- [`get_col_type()`](https://mlr-org.github.io/celecx/reference/get_col_type.md)
  : Get Column Type from data.table Column

## Base Classes

Shared abstractions and small utilities.

- [`ConfigurableComponent`](https://mlr-org.github.io/celecx/reference/ConfigurableComponent.md)
  : ConfigurableComponent
- [`SurrogateNull`](https://mlr-org.github.io/celecx/reference/SurrogateNull.md)
  : Archive-Backed Surrogate Adapter
- [`ResultAssignerNull`](https://mlr-org.github.io/celecx/reference/ResultAssignerNull.md)
  : Null Result Assigner
- [`hash_transform()`](https://mlr-org.github.io/celecx/reference/hash_transform.md)
  : Create Hash Digest of an Object
