#' @title Offline Surrogate-Performance Replay
#'
#' @include CallbackSurrogatePerformance.R
#'
#' @description
#' Reconstructs the surrogate-performance learning curve of a finished
#' active-learning run, offline, from its [bbotk::ArchiveBatch]. For every batch
#' the surrogate is refit on the archive data available up to that batch and
#' scored on a held-out regression task, yielding one performance value per
#' batch and measure. The result is assembled into a [TaskLCE], the same shape
#' produced online by [CallbackSurrogatePerformance].
#'
#' This is the offline twin of [CallbackSurrogatePerformance]: the callback
#' scores the surrogate the optimizer maintains during the run, whereas this
#' function replays the run from a stored archive, refitting `learner` itself.
#'
#' @param archive ([bbotk::ArchiveBatch])\cr
#'   Archive of a finished single-target run.
#' @param learner ([mlr3::LearnerRegr] | [mlr3mbo::SurrogateLearner])\cr
#'   Surrogate model refit per batch. A plain regression learner is wrapped in a
#'   [mlr3mbo::SurrogateLearner] so the fit / predict / output-transform path
#'   matches a live run; a surrogate is used as given (cloned). Either way the
#'   object is cloned, so the caller's model is left untrained.
#' @param task ([mlr3::TaskRegr])\cr
#'   Held-out regression task to score on. Must contain the archive's
#'   search-space columns as features.
#' @param measures (`list()` of [mlr3::Measure])\cr
#'   Regression measures. Named entries use their names as performance-column
#'   names; unnamed entries use the measure id.
#' @param measure (`character(1)` | `NULL`)\cr
#'   Name of the measure to use as the [TaskLCE] target. When `NULL` (default)
#'   and a single measure is given, that measure is used.
#' @param pool ([data.table::data.table] | `NULL`)\cr
#'   Finite candidate pool of the originating run, carried into the resulting
#'   [TaskLCE] for pool-based replay. Its columns must be the archive's
#'   search-space ids. `NULL` (default) for continuous runs.
#' @param link (`character(1)`)\cr
#'   Name of the predictive [lce_link] for the resulting [TaskLCE]. `"identity"`
#'   by default; pass e.g. `"log"` for a non-negative loss metric (see
#'   [lce_link_from_range]).
#' @param id (`character(1)`)\cr
#'   Task id.
#' @param label (`character(1)`)\cr
#'   Optional task label.
#'
#' @return [TaskLCE].
#'
#' @export
replay_surrogate_performance <- function(archive, learner, task,
    measures = list(msr("regr.rsq"), msr("regr.mae")),
    measure = NULL, pool = NULL, link = "identity", id = "surrogate_performance",
    label = NA_character_) {
  assert_r6(archive, "ArchiveBatch")
  assert_r6(task, "TaskRegr")
  if (!nrow(archive$data)) {
    stop("Archive is empty; nothing to replay")
  }
  if (length(archive$cols_y) != 1L) {
    stopf("replay needs a single-target archive, got targets %s",
      str_collapse(archive$cols_y, quote = "'"))
  }
  measures <- normalize_regr_measures(measures)

  # Replay each batch prefix against its own archive snapshot so the surrogate is
  # refit on exactly the data available up to that batch.
  replay_archive <- ArchiveBatch$new(
    search_space = archive$search_space, codomain = archive$codomain)

  # Build a fresh SurrogateLearner bound to the replay archive. Plain learners are
  # wrapped so the fit / predict / output-transform path matches a live run; a
  # surrogate is reconstructed from its parts (rather than $clone()ed, which
  # assumes a non-NULL archive) so the caller's model is left untrained.
  if (inherits(learner, "Surrogate")) {
    src <- assert_r6(learner, "SurrogateLearner")
    in_tr <- src$input_trafo
    out_tr <- src$output_trafo
    surrogate <- SurrogateLearner$new(
      learner = src$learner$clone(deep = TRUE),
      input_trafo = if (is.null(in_tr)) NULL else in_tr$clone(deep = TRUE),
      output_trafo = if (is.null(out_tr)) NULL else out_tr$clone(deep = TRUE),
      archive = replay_archive
    )
    surrogate$param_set$values <- src$param_set$values
  } else {
    surrogate <- SurrogateLearner$new(
      learner = assert_r6(learner, "LearnerRegr")$clone(deep = TRUE),
      archive = replay_archive)
  }

  batches <- sort(unique(archive$data$batch_nr))
  perf <- map_dtr(batches, function(b) {
    replay_archive$data <- archive$data[get("batch_nr") <= b]
    surrogate$update()
    scores <- score_surrogate_on_task(surrogate, task, measures)
    c(list(batch_nr = b), as.list(scores))
  })

  measure_names <- names(measures)
  if (is.null(measure)) {
    if (length(measure_names) != 1L) {
      stopf("Replay computed %i measures; supply 'measure' to pick one of: %s",
        length(measure_names), str_collapse(measure_names, quote = "'"))
    }
    measure <- measure_names
  } else {
    assert_choice(measure, measure_names, .var.name = "measure")
  }

  task_lce_from_perf(archive, perf, measure, measure_obj = measures[[measure]],
    pool = pool, link = link, id = id, label = label)
}
