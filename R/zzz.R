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

.onLoad = function(libname, pkgname) {
  # nocov start

  # Register "lce" task type in mlr3 reflections
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  mlr_reflections$task_types = mlr_reflections$task_types[!"lce"]
  mlr_reflections$task_types = setkeyv(rbind(mlr_reflections$task_types, rowwise_table(
    ~type, ~package, ~task, ~learner, ~prediction, ~prediction_data, ~measure,
    "lce", "celecx", "TaskLCE", "LearnerRegr", "PredictionRegr", "PredictionDataRegr", "MeasureRegr"
  ), fill = TRUE), "type")
  mlr_reflections$learner_predict_types$lce = mlr_reflections$learner_predict_types$regr
  mlr_reflections$learner_properties$lce = mlr_reflections$learner_properties$regr
  mlr_reflections$task_col_roles$lce = union(
    mlr_reflections$task_col_roles$regr, "key"
  )
  mlr_reflections$task_properties$lce = c(
    "strata", "groups", "weights_learner", "weights_measure", "ordered", "keys"
  )
  mlr_reflections$measure_properties$lce = mlr_reflections$measure_properties$regr

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

  # Register callbacks in mlr3misc::mlr_callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("celecx.metrics_tracker", load_callback_metrics_tracker)
  x$add("celecx.forecast_tracker", load_callback_forecast_tracker)
} # nocov end

.onUnload = function(libpaths) {
  # nocov start
  mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  mlr_reflections$task_types = mlr_reflections$task_types[!"lce"]
  walk(c("learner_predict_types", "learner_properties", "task_col_roles",
    "task_properties", "measure_properties"), function(x) {
    mlr_reflections[[x]] = remove_named(mlr_reflections[[x]], "lce")
  })

  walk(names(optimizers), function(id) bbotk::mlr_optimizers$remove(id))
  walk(names(terminators), function(id) bbotk::mlr_terminators$remove(id))
  walk(names(learners), function(id) mlr3::mlr_learners$remove(id))
  walk(names(resamplings), function(id) mlr3::mlr_resamplings$remove(id))
  walk(names(measures), function(id) mlr3::mlr_measures$remove(id))
  mlr3misc::mlr_callbacks$remove("celecx.metrics_tracker")
  mlr3misc::mlr_callbacks$remove("celecx.forecast_tracker")
} # nocov end
