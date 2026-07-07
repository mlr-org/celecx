# Package index

## Package

- [`celecx`](https://mlr-org.github.io/celecx/dev/reference/celecx-package.md)
  [`celecx-package`](https://mlr-org.github.io/celecx/dev/reference/celecx-package.md)
  : celecx: Computer Experiment LEarning Curve eXtrapolation

## Running Active Learning

High-level entry points that construct and run active-learning
optimizers.

- [`optimize_active()`](https://mlr-org.github.io/celecx/dev/reference/optimize_active.md)
  : Run Active Learning
- [`optimizer_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_al.md)
  : Active Learning Optimizer Factory
- [`optimizer_pool_al()`](https://mlr-org.github.io/celecx/dev/reference/optimizer_pool_al.md)
  : Convenience Constructor for Pool-Based Active Learning Optimizers

## Search Instances

Instance, callback context, and termination condition of a search run.

- [`SearchInstance`](https://mlr-org.github.io/celecx/dev/reference/SearchInstance.md)
  : Search Instance
- [`ContextSearch`](https://mlr-org.github.io/celecx/dev/reference/ContextSearch.md)
  : Context for Search Instance
- [`search_terminated_error()`](https://mlr-org.github.io/celecx/dev/reference/search_terminated_error.md)
  : Search Terminated Error

## Optimizers

Optimizer base classes and concrete optimizers over search spaces or
candidate pools.

- [`OptimizerSearchAbstract`](https://mlr-org.github.io/celecx/dev/reference/OptimizerSearchAbstract.md)
  : Abstract Base Class for Search-Compatible Optimizers
- [`OptimizerPoolAbstract`](https://mlr-org.github.io/celecx/dev/reference/OptimizerPoolAbstract.md)
  : Abstract Base Class for Pool-Aware Optimizers
- [`OptimizerAL`](https://mlr-org.github.io/celecx/dev/reference/OptimizerAL.md)
  : Proposer-Based Active Learning Optimizer
- [`mlr_optimizers_pool_sample`](https://mlr-org.github.io/celecx/dev/reference/mlr_optimizers_pool_sample.md)
  [`OptimizerPoolSample`](https://mlr-org.github.io/celecx/dev/reference/mlr_optimizers_pool_sample.md)
  : Sampler-Based Search on Pool-Restricted or Discrete Objectives

## Active Learning Proposers

Proposers that build active-learning batches inside OptimizerAL, and the
context they operate on.

- [`ALContext`](https://mlr-org.github.io/celecx/dev/reference/ALContext.md)
  : Active Learning Proposal Context
- [`ALProposer`](https://mlr-org.github.io/celecx/dev/reference/ALProposer.md)
  : Active Learning Proposer
- [`ALProposerScoreAbstract`](https://mlr-org.github.io/celecx/dev/reference/ALProposerScoreAbstract.md)
  : Abstract Base Class for Score-Based Active Learning Proposers
- [`ALProposerScore`](https://mlr-org.github.io/celecx/dev/reference/ALProposerScore.md)
  : Score-Based Active Learning Proposer
- [`ALProposerSequentialScore`](https://mlr-org.github.io/celecx/dev/reference/ALProposerSequentialScore.md)
  : Sequential Score-Based Active Learning Proposer
- [`ALProposerSequentialReference`](https://mlr-org.github.io/celecx/dev/reference/ALProposerSequentialReference.md)
  : Sequential Reference Active Learning Proposer
- [`ALProposerPseudoLabel`](https://mlr-org.github.io/celecx/dev/reference/ALProposerPseudoLabel.md)
  : Pseudo-Label Active Learning Proposer
- [`ALProposerPortfolio`](https://mlr-org.github.io/celecx/dev/reference/ALProposerPortfolio.md)
  : Portfolio Active Learning Proposer

## Score Modifiers

Modifiers that adjust acquisition scores during sequential batch
construction.

- [`ALScoreModifier`](https://mlr-org.github.io/celecx/dev/reference/ALScoreModifier.md)
  : Active Learning Score Modifier
- [`ALScoreModifierDiversity`](https://mlr-org.github.io/celecx/dev/reference/ALScoreModifierDiversity.md)
  : Diversity Active Learning Score Modifier
- [`ALScoreModifierLocalPenalization`](https://mlr-org.github.io/celecx/dev/reference/ALScoreModifierLocalPenalization.md)
  : Local-Penalization Active Learning Score Modifier
- [`ALScoreModifierNone`](https://mlr-org.github.io/celecx/dev/reference/ALScoreModifierNone.md)
  : No-Op Active Learning Score Modifier

## Acquisition Functions

Active-learning acquisition functions, registered in mlr3mbo’s
dictionary.

- [`AcqFunctionDist`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDist.md)
  : Distance-Aware Acquisition Function Base Class
- [`AcqFunctionDistGSx`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistGSx.md)
  : Distance-Aware GSx Acquisition Function
- [`AcqFunctionDistIDEAL`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistIDEAL.md)
  : Distance-Aware IDEAL Acquisition Function
- [`AcqFunctionDistIGS`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionDistIGS.md)
  : Distance-Aware iGS Acquisition Function
- [`AcqFunctionGSy`](https://mlr-org.github.io/celecx/dev/reference/AcqFunctionGSy.md)
  : GSy Acquisition Function

## Active Learning Distances

Distance objects, their dictionary, and sugar constructors.

- [`ALDistance`](https://mlr-org.github.io/celecx/dev/reference/ALDistance.md)
  : Active Learning Distance Base Class
- [`ALDistanceGeometry`](https://mlr-org.github.io/celecx/dev/reference/ALDistanceGeometry.md)
  : Geometry-Based Active Learning Distance
- [`mlr_al_distances`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances.md)
  : Dictionary of Active Learning Distances
- [`mlr_al_distances_affine`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_affine.md)
  [`ALDistanceAffine`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_affine.md)
  : Affine Active Learning Distance
- [`mlr_al_distances_gower`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_gower.md)
  [`ALDistanceGower`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_gower.md)
  : Gower Active Learning Distance
- [`mlr_al_distances_standardize`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_standardize.md)
  [`ALDistanceStandardize`](https://mlr-org.github.io/celecx/dev/reference/mlr_al_distances_standardize.md)
  : Standardized Active Learning Distance
- [`clx_ald()`](https://mlr-org.github.io/celecx/dev/reference/clx_ald.md)
  : Syntactic Sugar Active Learning Distance Construction
- [`clx_alds()`](https://mlr-org.github.io/celecx/dev/reference/clx_alds.md)
  : Syntactic Sugar Active Learning Distances Construction

## Space Samplers

Space-filling and pool-based samplers, their dictionary, and sugar
constructors.

- [`SpaceSampler`](https://mlr-org.github.io/celecx/dev/reference/SpaceSampler.md)
  : Space Sampler Base Class
- [`SpaceSamplerDistance`](https://mlr-org.github.io/celecx/dev/reference/SpaceSamplerDistance.md)
  : Distance-Based Space Sampler
- [`mlr_space_samplers`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers.md)
  : Dictionary of Space Samplers
- [`mlr_space_samplers_chain`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_chain.md)
  [`SpaceSamplerChain`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_chain.md)
  : Chained Space Sampler
- [`mlr_space_samplers_conditional`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_conditional.md)
  [`SpaceSamplerConditional`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_conditional.md)
  : Conditional Space Sampler
- [`mlr_space_samplers_gsx`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_gsx.md)
  [`SpaceSamplerGSx`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_gsx.md)
  : GSx Space Sampler
- [`mlr_space_samplers_kmeans`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_kmeans.md)
  [`SpaceSamplerKMeans`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_kmeans.md)
  : K-Means Space Sampler
- [`mlr_space_samplers_kmedoids`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_kmedoids.md)
  [`SpaceSamplerKMedoids`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_kmedoids.md)
  : K-Medoids Space Sampler
- [`mlr_space_samplers_lhs`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_lhs.md)
  [`SpaceSamplerLhs`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_lhs.md)
  : LHS Space Sampler
- [`mlr_space_samplers_relational_kmeans`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_relational_kmeans.md)
  [`SpaceSamplerRelationalKMeans`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_relational_kmeans.md)
  : Relational K-Means Space Sampler
- [`mlr_space_samplers_sobol`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_sobol.md)
  [`SpaceSamplerSobol`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_sobol.md)
  : Sobol Space Sampler
- [`mlr_space_samplers_uniform`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_uniform.md)
  [`SpaceSamplerUniform`](https://mlr-org.github.io/celecx/dev/reference/mlr_space_samplers_uniform.md)
  : Uniform Space Sampler
- [`clx_sps()`](https://mlr-org.github.io/celecx/dev/reference/clx_sps.md)
  : Syntactic Sugar Space Sampler Construction
- [`clx_spss()`](https://mlr-org.github.io/celecx/dev/reference/clx_spss.md)
  : Syntactic Sugar Space Samplers Construction

## Objectives

Objectives evaluated by table lookup or fitted learners, and
pool-restricted objective wrappers.

- [`ObjectiveDataset`](https://mlr-org.github.io/celecx/dev/reference/ObjectiveDataset.md)
  : Objective Based on Pre-evaluated Dataset
- [`ObjectiveLearner`](https://mlr-org.github.io/celecx/dev/reference/ObjectiveLearner.md)
  : Objective Function Based on a Fitted Learner
- [`ObjectivePoolAbstract`](https://mlr-org.github.io/celecx/dev/reference/ObjectivePoolAbstract.md)
  : Abstract Base Class for Pool-backed Objectives
- [`ObjectivePoolRFun`](https://mlr-org.github.io/celecx/dev/reference/ObjectivePoolRFun.md)
  : Objective Function Based on a Candidate Pool and R Function
- [`ObjectivePoolWrapper`](https://mlr-org.github.io/celecx/dev/reference/ObjectivePoolWrapper.md)
  : Objective Function Wrapping Another Objective on a Candidate Pool

## Surrogate Regression Learners

Regression learners and wrappers with uncertainty quantification.

- [`mlr_learners_regr.bootstrap_se`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.bootstrap_se.md)
  [`LearnerRegrBootstrapSE`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.bootstrap_se.md)
  : Bootstrap Ensemble Learner with SE Prediction
- [`mlr_learners_regr.deepgp`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.deepgp.md)
  [`LearnerRegrDeepGP`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.deepgp.md)
  : Deep GP Regression Learner
- [`mlr_learners_regr.gpfit`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.gpfit.md)
  [`LearnerRegrGPfit`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.gpfit.md)
  : GPfit Regression Learner
- [`mlr_learners_regr.hetgp`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.hetgp.md)
  [`LearnerRegrHetGP`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.hetgp.md)
  : hetGP Regression Learner
- [`mlr_learners_regr.quantile_se`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.quantile_se.md)
  [`LearnerRegrQuantileSE`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.quantile_se.md)
  : Quantile Regression Learner with SE Prediction
- [`mlr_learners_regr.tgp`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.tgp.md)
  [`LearnerRegrTGP`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_regr.tgp.md)
  : tgp Regression Learner

## Recording Learning Curves

Turning a run into a learning-curve task, online via callback or offline
by replay.

- [`celecx.surrogate_performance`](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)
  [`CallbackSurrogatePerformance`](https://mlr-org.github.io/celecx/dev/reference/celecx.surrogate_performance.md)
  : Surrogate Performance Callback
- [`replay_surrogate_performance()`](https://mlr-org.github.io/celecx/dev/reference/replay_surrogate_performance.md)
  : Offline Surrogate-Performance Replay
- [`replay_surrogate_perf_table()`](https://mlr-org.github.io/celecx/dev/reference/replay_surrogate_perf_table.md)
  : Offline Surrogate-Performance Table
- [`task_lce_from_perf()`](https://mlr-org.github.io/celecx/dev/reference/task_lce_from_perf.md)
  : Build an LCE Task from an Archive and a Performance Table
- [`task_lce_best_so_far()`](https://mlr-org.github.io/celecx/dev/reference/task_lce_best_so_far.md)
  : Best-So-Far Optimization Trace as an LCE Task
- [`TaskLCE`](https://mlr-org.github.io/celecx/dev/reference/TaskLCE.md)
  : Learning Curve Extrapolation Task
- [`lce_link()`](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  [`lce_link_from_range()`](https://mlr-org.github.io/celecx/dev/reference/lce_link.md)
  : Predictive Link Functions for LCE Forecasts

## Learning-Curve Extrapolation

Learners that extrapolate learning curves, and their prediction object.

- [`LearnerLCE`](https://mlr-org.github.io/celecx/dev/reference/LearnerLCE.md)
  : Learning Curve Extrapolation Learner
- [`mlr_learners_lce.bootstrap`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.bootstrap.md)
  [`LearnerLCEBootstrap`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.bootstrap.md)
  : Residual Bootstrap LCE Learner Wrapper
- [`mlr_learners_lce.conformal`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.conformal.md)
  [`LearnerLCEConformal`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.conformal.md)
  : Split-Conformal LCE Learner Wrapper
- [`mlr_learners_lce.featureless`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.featureless.md)
  [`LearnerLCEFeatureless`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.featureless.md)
  : Featureless LCE Learner
- [`mlr_learners_lce.gam`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.gam.md)
  [`LearnerLCEGam`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.gam.md)
  : GAM LCE Learner
- [`mlr_learners_lce.isotonic`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.isotonic.md)
  [`LearnerLCEIsotonic`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.isotonic.md)
  : Isotonic LCE Learner
- [`mlr_learners_lce.parametric_exponential`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_exponential.md)
  [`LearnerLCEParametricExponential`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_exponential.md)
  : Parametric Exponential LCE Learner
- [`mlr_learners_lce.parametric_log`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_log.md)
  [`LearnerLCEParametricLog`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_log.md)
  : Parametric Log LCE Learner
- [`mlr_learners_lce.parametric_logistic`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_logistic.md)
  [`LearnerLCEParametricLogistic`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_logistic.md)
  : Parametric Logistic LCE Learner
- [`mlr_learners_lce.parametric_power_law`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_power_law.md)
  [`LearnerLCEParametricPowerLaw`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.parametric_power_law.md)
  : Parametric Power-Law LCE Learner
- [`mlr_learners_lce.rolling_slope`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.rolling_slope.md)
  [`LearnerLCERollingSlope`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.rolling_slope.md)
  : Rolling Slope LCE Learner
- [`mlr_learners_lce.simulate`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.simulate.md)
  [`LearnerLCESimulate`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.simulate.md)
  : Forward-Simulation LCE Learner
- [`mlr_learners_lce.spline_monotone`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.spline_monotone.md)
  [`LearnerLCESplineMonotone`](https://mlr-org.github.io/celecx/dev/reference/mlr_learners_lce.spline_monotone.md)
  : Monotone Spline LCE Learner
- [`PredictionLCE`](https://mlr-org.github.io/celecx/dev/reference/PredictionLCE.md)
  : LCE Prediction Object

## Evaluating Extrapolators

Measures and resampling schemes for learning-curve tasks.

- [`MeasureLCE`](https://mlr-org.github.io/celecx/dev/reference/MeasureLCE.md)
  : Learning Curve Extrapolation Measure
- [`mlr_measures_lce`](https://mlr-org.github.io/celecx/dev/reference/mlr_measures_lce.md)
  : Per-Batch LCE Loss Measures
- [`mlr_measures_lce_distributional`](https://mlr-org.github.io/celecx/dev/reference/mlr_measures_lce_distributional.md)
  : Distributional LCE Measures
- [`mlr_resamplings_lce.expanding_cv`](https://mlr-org.github.io/celecx/dev/reference/mlr_resamplings_lce.expanding_cv.md)
  [`ResamplingLCE`](https://mlr-org.github.io/celecx/dev/reference/mlr_resamplings_lce.expanding_cv.md)
  : Expanding-Window LCE Cross-Validation

## Batches-to-Target Forecasts

- [`lce_batches_to_target()`](https://mlr-org.github.io/celecx/dev/reference/lce_batches_to_target.md)
  : Batches-to-Target Forecast

## Design Generation

- [`generate_design_grid_celecx()`](https://mlr-org.github.io/celecx/dev/reference/generate_design_grid_celecx.md)
  : Generate a Dependency-Aware Grid Design

## Base Classes

Shared abstractions.

- [`ConfigurableComponent`](https://mlr-org.github.io/celecx/dev/reference/ConfigurableComponent.md)
  : ConfigurableComponent
- [`SurrogateNull`](https://mlr-org.github.io/celecx/dev/reference/SurrogateNull.md)
  : Archive-Backed Surrogate Adapter
- [`ResultAssignerNull`](https://mlr-org.github.io/celecx/dev/reference/ResultAssignerNull.md)
  : Null Result Assigner
