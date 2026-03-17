#' @title Convert to Learning Curve Extrapolation Task
#'
#' @description
#' Convert an object to a [TaskLCE].
#'
#' @param x (`any`)\cr
#'   Object to convert.
#' @param ... \cr
#'   Additional arguments passed to [TaskLCE].
#'
#' @return [TaskLCE].
#'
#' @export
as_task_lce = function(x, ...) {
  UseMethod("as_task_lce")
}

#' @rdname as_task_lce
#'
#' @param id (`character(1)`)\cr
#'   Task identifier. Defaults to the variable name of `x`.
#' @param target (`character(1)`)\cr
#'   Name of the target column.
#' @param order (`character(1)`)\cr
#'   Name of the order column (evaluation count).
#' @param key (`character()`)\cr
#'   Optional key column name(s).
#' @param direction (`character(1)`)\cr
#'   `"minimize"` or `"maximize"`.
#'
#' @export
as_task_lce.data.frame = function(x, target, order, key = character(),
    direction = "minimize", id = deparse1(substitute(x)), ...) {
  TaskLCE$new(id = id, backend = x, target = target, order = order,
    key = key, direction = direction, ...)
}

#' @rdname as_task_lce
#' @param clone (`logical(1)`)\cr
#'   Whether to clone the task.
#' @export
as_task_lce.TaskLCE = function(x, clone = FALSE, ...) {
  if (clone) x$clone(deep = TRUE) else x
}

#' @rdname as_task_lce
#' @export
as_task_lce.DataBackend = function(x, target, order, key = character(),
    direction = "minimize", id = deparse1(substitute(x)), ...) {
  TaskLCE$new(id = id, backend = x, target = target, order = order,
    key = key, direction = direction, ...)
}

#' @title Generate New Data for LCE Prediction
#'
#' @description
#' Creates a `data.table` suitable for `learner$predict_newdata()` containing
#' future evaluation count values.
#'
#' @param task ([TaskLCE])\cr
#'   The training task, used to determine column schema.
#' @param n_evals_new (`integerish()`)\cr
#'   Future evaluation counts to predict at.
#'
#' @return [data.table::data.table()] with the order column set to
#'   `n_evals_new`, the target column set to `NA`, and other feature columns
#'   set to `NA`.
#'
#' @export
generate_newdata_lce = function(task, n_evals_new) {
  assert_r6(task, "TaskLCE")
  assert_integerish(n_evals_new, lower = 1L, any.missing = FALSE, min.len = 1L)

  order_col = task$col_roles$order
  target_col = task$target_names
  feature_cols = setdiff(task$feature_names, order_col)

  n = length(n_evals_new)
  dt = data.table(dummy_order = as.numeric(n_evals_new))
  setnames(dt, "dummy_order", order_col)

  # target column (NA)
  set(dt, j = target_col, value = rep(NA_real_, n))

  # other feature columns (NA, matching types)
  for (col in feature_cols) {
    col_type = task$col_info[get("id") == col, "type"][[1L]]
    na_val = switch(col_type,
      integer = NA_integer_,
      numeric = NA_real_,
      character = NA_character_,
      factor = factor(NA, levels = task$levels(col)[[1L]]),
      ordered = ordered(NA, levels = task$levels(col)[[1L]]),
      logical = NA,
      NA_real_
    )
    set(dt, j = col, value = rep(na_val, n))
  }

  dt
}
