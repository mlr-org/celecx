#' @title Best-So-Far Optimization Trace as an LCE Task
#'
#' @name task_lce_best_so_far
#' @include TaskLCE.R
#'
#' @description
#' Builds a [TaskLCE] whose target is the best objective value observed up to
#' (and including) each archive batch -- the progress curve of an optimization
#' run. This is the optimization-mode counterpart of the surrogate-performance
#' tasks built by [CallbackSurrogatePerformance] and
#' [replay_surrogate_performance()]: the same LCE learners, measures, and
#' resamplings apply, forecasting future best-so-far values instead of future
#' model quality.
#'
#' The task has one row per archive evaluation, carrying the archive's feature
#' and target columns in the `archive_x` / `archive_y` roles. The target column
#' `best_so_far` is constant within a batch (batches are evaluated as a whole,
#' so mid-batch improvements only become visible at the batch's end). The
#' optimization direction is taken from the archive codomain's single
#' minimize/maximize-tagged target and travels with the task via its stored
#' codomain; direction-dependent operations (e.g. the `"target_reached"`
#' predict type, [lce_batches_to_target()]) work without a task measure.
#'
#' @param archive ([bbotk::Archive])\cr
#'   Archive of a completed single-target optimization run. Its codomain target
#'   must be tagged `"minimize"` or `"maximize"` (a `"learn"`-tagged target has
#'   no best value).
#' @param link (`character(1)`)\cr
#'   Name of the predictive [lce_link] for the resulting task. `"identity"` by
#'   default; note that objective values are in general not sign-constrained,
#'   so non-identity links only make sense for suitably bounded objectives.
#' @param id (`character(1)`)\cr
#'   Task id. Defaults to `"best_so_far"`.
#' @param label (`character(1)`)\cr
#'   Optional task label.
#'
#' @return [TaskLCE].
#' @export
task_lce_best_so_far <- function(archive, link = "identity", id = "best_so_far",
    label = NA_character_) {
  assert_r6(archive, "Archive")
  assert_string(id, min.chars = 1L)

  archive_x <- archive$cols_x
  archive_y <- archive$cols_y
  if (length(archive_y) != 1L) {
    stopf("task_lce_best_so_far() supports single-target archives only, got targets %s",
      str_collapse(archive_y, quote = "'"))
  }
  minimize <- lce_codomain_minimize(archive$codomain)
  if (is.na(minimize)) {
    stopf(paste0("The archive codomain's target '%s' must be tagged 'minimize' or ",
      "'maximize'; a best-so-far curve is undefined otherwise"), archive_y)
  }
  if ("best_so_far" %in% c(archive_x, archive_y, "batch_nr")) {
    stopf("Column name 'best_so_far' clashes with an archive column")
  }

  if (!nrow(archive$data)) {
    stop("Archive contains no evaluations")
  }
  data <- archive$data[, c(archive_x, archive_y, "batch_nr"), with = FALSE]

  # Best value per batch, then the running best over batches; constant within a
  # batch since a batch's evaluations only become visible at its end.
  best_fun <- if (minimize) min else max
  cum_fun <- if (minimize) cummin else cummax
  per_batch <- data[, list(batch_best = best_fun(.SD[[1L]])), by = "batch_nr",
    .SDcols = archive_y]
  setorderv(per_batch, "batch_nr")
  per_batch[, best_so_far := cum_fun(batch_best)]
  data[, best_so_far := per_batch[list(data$batch_nr), on = "batch_nr", "best_so_far"][[1L]]]

  TaskLCE$new(
    id = id, backend = data, target = "best_so_far", batch_nr = "batch_nr",
    archive_x = archive_x, archive_y = archive_y,
    search_space = archive$search_space, codomain = archive$codomain,
    measure = NULL, pool = NULL, link = link, label = label
  )
}
