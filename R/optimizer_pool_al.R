#' @title Convenience Constructor for Pool-Based Active Learning Optimizers
#'
#' @description
#' Creates an [OptimizerGS] or [OptimizerIDEAL] with appropriate settings
#' for a specific active learning method.
#'
#' @param method (`character(1)`)\cr
#'   One of `"gsx"`, `"gsy"`, `"igs"`, `"qbc"`, `"random"`, `"ideal"`.
#' @param learner ([mlr3::LearnerRegr] | `NULL`)\cr
#'   Regression learner. Required for `"gsy"`, `"igs"`, `"qbc"`, `"ideal"`.
#' @param delta (`numeric(1)`)\cr
#'   Exploration weight for IDEAL (default 1).
#' @param n_init (`integer(1)` | `NULL`)\cr
#'   Number of initial samples. `NULL` uses the default (dimension d).
#' @param init_method (`character(1)` | `NULL`)\cr
#'   Optional initialization override: `"gsx"`, `"random"`, or `"kmeans"`.
#'   `NULL` keeps the method-specific default (`"kmeans"` for IDEAL,
#'   `"gsx"` for GSx/GSy/iGS, `"random"` for random/QBC).
#' @param k_qbc (`integer(1)`)\cr
#'   Number of QBC committee members (default 5).
#' @param batch_size (`integer(1)`)\cr
#'   Points per iteration (default 1).
#'
#' @return An [OptimizerGS] or [OptimizerIDEAL] object.
#'
#' @export
optimizer_pool_al <- function(
    method = c("gsx", "gsy", "igs", "qbc", "random", "ideal"),
    learner = NULL, delta = 1, n_init = NULL, init_method = NULL,
    k_qbc = 5L, batch_size = 1L) {
  method <- match.arg(method)
  assert_choice(init_method, c("gsx", "random", "kmeans"), null.ok = TRUE)

  if (method == "ideal") {
    assert_r6(learner, "LearnerRegr")
    optimizer <- OptimizerIDEAL$new(learner = learner)
    pv <- list(delta = delta, batch_size = batch_size)
    if (!is.null(n_init)) pv$n_init <- n_init
    if (!is.null(init_method)) pv$init_method <- init_method
    optimizer$param_set$set_values(.values = pv)
    return(optimizer)
  }

  # GS family
  scoring <- switch(method,
    gsx = scoring_gsx(),
    gsy = scoring_gsy(),
    igs = scoring_igs(),
    qbc = scoring_qbc(k_qbc = k_qbc),
    random = scoring_random()
  )

  if (method %in% c("gsy", "igs", "qbc")) {
    assert_r6(learner, "LearnerRegr")
  }

  optimizer <- OptimizerGS$new(scoring = scoring, learner = learner)
  pv <- list(batch_size = batch_size)
  if (!is.null(n_init)) pv$n_init <- n_init
  if (!is.null(init_method)) pv$init_method <- init_method
  optimizer$param_set$set_values(.values = pv)

  optimizer
}
