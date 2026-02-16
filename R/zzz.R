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

  # Register optimizers in bbotk::mlr_optimizers
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  iwalk(optimizers, function(obj, nm) x$add(nm, obj))

  # Register terminators in bbotk::mlr_terminators
  x = utils::getFromNamespace("mlr_terminators", ns = "bbotk")
  iwalk(terminators, function(obj, nm) x$add(nm, obj))

  # Register learners in mlr3::mlr_learners
  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(learners, function(obj, nm) x$add(nm, obj))

  # Register callbacks in mlr3misc::mlr_callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("celecx.metrics_tracker", load_callback_metrics_tracker)
  x$add("celecx.forecast_tracker", load_callback_forecast_tracker)
} # nocov end

.onUnload = function(libpaths) {
  # nocov start
  walk(names(optimizers), function(id) bbotk::mlr_optimizers$remove(id))
  walk(names(terminators), function(id) bbotk::mlr_terminators$remove(id))
  walk(names(learners), function(id) mlr3::mlr_learners$remove(id))
  mlr3misc::mlr_callbacks$remove("celecx.metrics_tracker")
  mlr3misc::mlr_callbacks$remove("celecx.forecast_tracker")
} # nocov end
