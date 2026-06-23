#' @title Learning Curve Extrapolation Measure
#'
#' @name MeasureLCE
#'
#' @include TaskLCE.R
#'
#' @description
#' Abstract base class for performance measures evaluating the surrogate-quality
#' forecast produced by a [LearnerLCE]. Concrete measures compare the predicted
#' per-batch performance to the recorded performance.
#'
#' Measures defined via this base class always aggregate predictions and truths
#' to one value per batch (the within-batch mean) before computing the per-batch
#' loss. Multiple archive rows belong to the same batch and share the same
#' surrogate-quality value, so the aggregation collapses them without affecting
#' the loss; if a future learner predicts differently within a batch, the mean
#' is the natural reduction.
#'
#' @export
MeasureLCE <- R6Class("MeasureLCE",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new LCE measure.
    #'
    #' @param id (`character(1)`)\cr Measure id.
    #' @param param_set ([paradox::ParamSet]).
    #' @param range (`numeric(2)`)\cr Theoretical range of values.
    #' @param minimize (`logical(1)`).
    #' @param average (`character(1)`).
    #' @param aggregator (`function()` or `NULL`).
    #' @param properties (`character()`).
    #' @param predict_type (`character(1)`)\cr One of `"response"`, `"se"`.
    #' @param predict_sets (`character()`).
    #' @param task_properties (`character()`).
    #' @param packages (`character()`).
    #' @param label (`character(1)`).
    #' @param man (`character(1)`).
    initialize = function(id, param_set = ps(), range = c(-Inf, Inf), minimize = NA,
        average = "macro", aggregator = NULL, properties = character(0),
        predict_type = "response", predict_sets = "test",
        task_properties = character(0), packages = character(0),
        label = NA_character_, man = NA_character_) {
      # measures need the task to look up batch_nr for every row.
      properties <- union(properties, "requires_task")
      super$initialize(id = id, task_type = "lce", param_set = param_set,
        range = range, minimize = minimize, average = average,
        aggregator = aggregator, properties = properties,
        predict_type = predict_type, predict_sets = predict_sets,
        task_properties = task_properties, packages = packages,
        label = label, man = man)
    }
  )
)

# Per-row batch numbers for a prediction, aligned to prediction$row_ids.
lce_row_batches <- function(prediction, task) {
  row_ids <- prediction$row_ids
  pk <- task$backend$primary_key
  batch_col <- task$col_roles$feature
  batch_data <- task$backend$data(rows = row_ids, cols = c(pk, batch_col))
  setnames(batch_data, c(pk, batch_col), c("row_id", "batch"))
  batch_data[list(row_ids), on = "row_id"]$batch
}

# Mean-aggregate the named per-row numeric vectors in `cols` (plus `truth`) to
# one row per batch; sum-aggregate `weights`. Returns a data.table with columns
# `batch`, `truth`, the names of `cols`, and (when given) `weight`.
lce_per_batch_cols <- function(prediction, task, cols, weights = NULL) {
  dt <- data.table(batch = lce_row_batches(prediction, task),
    truth = as.numeric(prediction$truth))
  for (nm in names(cols)) set(dt, j = nm, value = as.numeric(cols[[nm]]))
  if (!is.null(weights)) set(dt, j = "weight", value = as.numeric(weights))

  agg <- dt[, c(list(truth = mean(truth)), lapply(.SD, mean)),
    by = "batch", .SDcols = names(cols)]
  if (!is.null(weights)) {
    set(agg, j = "weight", value = dt[, list(weight = sum(weight)), by = "batch"]$weight)
  }
  agg
}

# Collapse predictions and truth values to one row per batch.
# Returns a data.table with columns `batch`, `truth`, `response`, and
# optionally `se` / `weight`. Mean-aggregates `truth`, `response`, `se`;
# sum-aggregates `weight`.
lce_per_batch <- function(prediction, task, weights = NULL) {
  cols <- list(response = prediction$data$response %??% rep(NA_real_, length(prediction$row_ids)))
  if (!is.null(prediction$data$se)) cols$se <- prediction$data$se
  lce_per_batch_cols(prediction, task, cols, weights)
}

# Weighted mean with NULL weights falling back to the plain mean.
lce_weighted_mean <- function(x, weight = NULL) {
  if (is.null(weight)) mean(x) else sum(weight * x) / sum(weight)
}
