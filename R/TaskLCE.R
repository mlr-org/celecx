#' @title Learning Curve Extrapolation Task
#'
#' @description
#' Task for learning curve extrapolation problems, extending [mlr3::TaskRegr].
#'
#' @details
#' A `TaskLCE` wraps learning curve data: a numeric performance metric
#' tracked over an evaluation count (e.g., number of samples queried).
#' The task type is `"lce"`.
#'
#' Unlike [mlr3forecast::TaskFcst], the order column (`n_evals`) is kept as
#' a feature because it is a primary predictor for learning curve models
#' (`metric ~ f(n_evals)`).
#'
#' It is recommended to use [as_task_lce()] for construction.
#'
#' @section Column Roles:
#' * `"target"`: The performance metric being predicted (single numeric column).
#' * `"order"`: The evaluation count column (single integer/numeric column).
#'   Also available as a feature.
#' * `"key"`: Optional grouping column identifying different learning curves
#'   (factor or character).
#' * `"feature"`: Predictor columns, including the order column.
#'
#' @export
TaskLCE = R6Class("TaskLCE",
  inherit = TaskRegr,
  public = list(

    #' @description
    #' Creates a new TaskLCE.
    #'
    #' @param id (`character(1)`)\cr
    #'   Task identifier.
    #' @param backend ([mlr3::DataBackend] | `data.frame`)\cr
    #'   Data backend or data.frame.
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
    #' @param order (`character(1)`)\cr
    #'   Name of the order column (evaluation count).
    #' @param key (`character()`)\cr
    #'   Optional name(s) of key columns identifying separate curves.
    #' @param direction (`character(1)`)\cr
    #'   Whether the target metric is minimized or maximized.
    #' @param label (`character(1)`)\cr
    #'   Optional label.
    #' @param extra_args (`list()`)\cr
    #'   Optional extra arguments.
    initialize = function(id, backend, target, order, key = character(),
        direction = c("minimize", "maximize"),
        label = NA_character_, extra_args = list()) {
      direction = match.arg(direction)
      super$initialize(id = id, backend = backend, target = target,
        label = label, extra_args = extra_args)
      self$task_type = "lce"
      private$.direction = direction

      col_roles = self$col_roles
      col_roles$order = assert_choice(order, col_roles$feature)
      # Keep order in features -- key departure from mlr3forecast
      if (length(key) > 0L) {
        col_roles$key = assert_subset(key, col_roles$feature)
      } else {
        col_roles$key = character()
      }
      self$col_roles = col_roles

      self$extra_args = insert_named(self$extra_args,
        list(order = order, key = key, direction = direction))
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      super$print()
      catf("* Direction: %s", self$direction)
    }
  ),

  active = list(

    #' @field direction (`character(1)`)
    #'   Whether the target metric is `"minimize"` or `"maximize"`.
    direction = function(rhs) {
      assert_ro_binding(rhs)
      private$.direction
    },

    #' @field order ([data.table::data.table()])
    #'   Table with columns `row_id` and `order`.
    order = function(rhs) {
      assert_ro_binding(rhs)
      order_cols = self$col_roles$order
      data = self$backend$data(
        rows = self$row_ids,
        cols = c(self$backend$primary_key, order_cols)
      )
      setnames(data, c("row_id", "order"))[]
    },

    #' @field key ([data.table::data.table()] | `NULL`)
    #'   Table with columns `row_id` and the key variable(s), or `NULL`.
    key = function(rhs) {
      assert_ro_binding(rhs)
      key_cols = self$col_roles$key
      if (length(key_cols) == 0L) {
        return(NULL)
      }
      data = self$backend$data(
        rows = self$row_ids,
        cols = c(self$backend$primary_key, key_cols)
      )
      if (length(key_cols) == 1L) {
        setnames(data, c("row_id", "key"))[]
      } else {
        setnames(data, c("row_id", key_cols))[]
      }
    },

    #' @field properties (`character()`)
    #'   Task properties, including `"keys"` when key columns are set.
    properties = function(rhs) {
      if (missing(rhs)) {
        c(super$properties,
          if (length(self$col_roles$key) > 0L) "keys" else NULL)
      } else {
        super$properties = rhs
      }
    }
  ),

  private = list(
    .direction = NULL
  )
)

#' @export
task_check_col_roles.TaskLCE = function(task, new_roles, ...) {
  order_cols = new_roles[["order"]]
  if (length(order_cols) > 1L) {
    stopf("There may only be up to one column with role 'order'")
  }

  if (length(order_cols) > 0L) {
    col_type = task$col_info[get("id") %in% order_cols, "type"][[1L]]
    if (!all(col_type %in% c("integer", "numeric"))) {
      stopf(
        "Order column '%s' must be an integer or numeric column",
        order_cols
      )
    }
  }

  key_cols = new_roles[["key"]]
  if (length(key_cols) > 0L) {
    key_types = task$col_info[get("id") %in% key_cols, "type"][[1L]]
    if (!all(key_types %in% c("factor", "ordered", "character"))) {
      stopf(
        "Key column(s) %s must be factor, ordered, or character columns",
        str_collapse(key_cols, quote = "'")
      )
    }
  }

  NextMethod()
}
