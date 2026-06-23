#' @include aaa.R
#' @import checkmate
#' @import data.table
#' @import R6
#' @import paradox
#' @import mlr3
#' @import mlr3mbo
#' @import bbotk
#' @import mlr3misc
#' @importFrom stats dnorm pnorm dist var sd median quantile
"_PACKAGE"

paradox_condition_test = NULL

.onLoad = function(libname, pkgname) {
  # nocov start

  # paradox::condition_test is unexported in the CRAN release (paradox 1.0.1).
  # Resolve via the namespace so dependency dispatch works regardless of paradox version.
  paradox_condition_test <<- utils::getFromNamespace("condition_test", "paradox")

  # Register "lce" task type in mlr3 reflections
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  mlr_reflections$task_types = mlr_reflections$task_types[!"lce"]
  mlr_reflections$task_types = setkeyv(rbind(mlr_reflections$task_types, rowwise_table(
    ~type, ~package, ~task, ~learner, ~prediction, ~prediction_data, ~measure,
    "lce", "celecx", "TaskLCE", "LearnerLCE", "PredictionLCE", "PredictionDataLCE", "MeasureLCE"
  ), fill = TRUE), "type")
  # Keys are the selectable learner predict types (and the set of column names a
  # learner's $.predict() may return); each maps to the columns its prediction
  # carries. "se_epistemic" is registered so the (response, se, se_epistemic)
  # triple of the "se" type is an accepted return; it is not selected on its own.
  mlr_reflections$learner_predict_types$lce = list(
    response = "response",
    se = c("response", "se", "se_epistemic"),
    se_epistemic = c("response", "se", "se_epistemic"),
    quantiles = c("response", "quantiles"),
    samples = c("response", "samples"),
    target_reached = c("response", "target_reached")
  )
  mlr_reflections$learner_properties$lce = c(
    "featureless", "missings", "weights", "importance", "selected_features",
    "hotstart_forward", "hotstart_backward", "marshal"
  )
  mlr_reflections$task_col_roles$lce = c(
    "feature", "target", "name", "order", "stratum", "group",
    "weights_learner", "weights_measure", "archive_x", "archive_y"
  )
  mlr_reflections$task_col_roles_optional_newdata$lce = c(
    "weights_learner", "weights_measure", "name", "order", "stratum", "group",
    "archive_x", "archive_y"
  )
  mlr_reflections$task_properties$lce = c(
    "strata", "groups", "weights_learner", "weights_measure"
  )
  mlr_reflections$task_print_col_roles$after = c(
    mlr_reflections$task_print_col_roles$after,
    c("Archive features" = "archive_x", "Archive targets" = "archive_y")
  )
  mlr_reflections$measure_properties$lce = c(
    "na_score", "requires_task", "requires_learner", "requires_model",
    "requires_train_set", "weights", "primary_iters", "requires_no_prediction",
    "obs_loss"
  )

  bbotk_reflections = utils::getFromNamespace("bbotk_reflections", ns = "bbotk")
  bbotk_reflections$objective_properties = union(
    bbotk_reflections$objective_properties, "pool_restricted"
  )

  # Register optimizers in bbotk::mlr_optimizers
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  iwalk(optimizers, function(obj, nm) x$add(nm, obj))

  # Register terminators in bbotk::mlr_terminators
  x = utils::getFromNamespace("mlr_terminators", ns = "bbotk")
  iwalk(terminators, function(obj, nm) x$add(nm, obj))

  # Register learners in mlr3::mlr_learners
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(learners, function(obj, nm) x$add(nm, obj))

  # Register resamplings in mlr3::mlr_resamplings
  x = utils::getFromNamespace("mlr_resamplings", ns = "mlr3")
  iwalk(resamplings, function(obj, nm) x$add(nm, obj))

  # Register measures in mlr3::mlr_measures
  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(measures, function(obj, nm) x$add(nm, obj))

  # Register acquisition functions in mlr3mbo::mlr_acqfunctions
  x = utils::getFromNamespace("mlr_acqfunctions", ns = "mlr3mbo")
  iwalk(acq_functions, function(obj, nm) x$add(nm, obj))

  # Register callbacks in mlr3misc::mlr_callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("celecx.metrics_tracker", load_callback_metrics_tracker)
  x$add("celecx.forecast_tracker", load_callback_forecast_tracker)
  x$add("celecx.surrogate_performance", load_callback_surrogate_performance)
} # nocov end

.onUnload = function(libpaths) {
  # nocov start
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  mlr_reflections$task_types = mlr_reflections$task_types[!"lce"]
  walk(c("learner_predict_types", "learner_properties", "task_col_roles",
    "task_col_roles_optional_newdata", "task_properties", "measure_properties"),
    function(x) {
      mlr_reflections[[x]] = remove_named(mlr_reflections[[x]], "lce")
    })
  mlr_reflections$task_print_col_roles$after = remove_named(
    mlr_reflections$task_print_col_roles$after,
    c("Archive features", "Archive targets")
  )

  walk(names(optimizers), function(id) bbotk::mlr_optimizers$remove(id))
  walk(names(terminators), function(id) bbotk::mlr_terminators$remove(id))
  walk(names(learners), function(id) mlr3::mlr_learners$remove(id))
  walk(names(resamplings), function(id) mlr3::mlr_resamplings$remove(id))
  walk(names(measures), function(id) mlr3::mlr_measures$remove(id))
  walk(names(acq_functions), function(id) mlr3mbo::mlr_acqfunctions$remove(id))
  mlr3misc::mlr_callbacks$remove("celecx.metrics_tracker")
  mlr3misc::mlr_callbacks$remove("celecx.forecast_tracker")
  mlr3misc::mlr_callbacks$remove("celecx.surrogate_performance")
} # nocov end
