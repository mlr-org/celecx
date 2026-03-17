# celecx 0.1.0

* Gaussian process regression learners:
  * `LearnerRegrDeepGP` / `"regr.deepgp"` (`deepgp` package)
  * `LearnerRegrGPFit` / `"regr.gpfit"` (`GPfit` package)
  * `LearnerRegrHetGP` / `"regr.hetgp"` (`hetGP` package)
  * `LearnerRegrTGP` / `"regr.tgp"` (`tgp` package)
* SE estimation learner wrappers:
  * `LearnerRegrBootstrapSE` / `"regr.bootstrap_se.*"`: bootstrap SE estimation
  * `LearnerRegrQuantileSE` / `"regr.quantile_se.*"`: quantile prediction to SE conversion
* Learning curve extrapolation objects
  * `TaskLCE`: Regression / time series task that keeps extra meta-information relevant for learning curve extrapolation
  * `ResamplingLCEHoldout`, `ResamplingLCECV`: holdout split and rolling / expanding window cross-validation
  * `MeasureLCECoverage`, `MeasureLCEWinkler`: LCE prediction interval coverage and Winkler score (experimental)
* Optimization objectives:
  * `ObjectiveDataset`: pre-evaluated dataset objective
  * `ObjectiveLearner`: learner-based objective
