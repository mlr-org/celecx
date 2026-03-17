#' @title LCE Holdout Resampling
#'
#' @name mlr_resamplings_lce_holdout
#'
#' @description
#' Splits learning curve data into a training set (early observations) and a
#' test set (later observations) based on temporal ordering.
#'
#' @section Parameters:
#' * `ratio` (`numeric(1)`)\cr
#'   Fraction of observations in the training set (default 0.67).
#'
#' @export
ResamplingLCEHoldout = R6Class("ResamplingLCEHoldout",
  inherit = Resampling,
  public = list(

    #' @description
    #' Creates a new instance.
    initialize = function() {
      param_set = ps(
        ratio = p_dbl(lower = 0, upper = 1, tags = "required")
      )
      param_set$set_values(ratio = 0.67)

      super$initialize(
        id = "lce_holdout",
        label = "LCE Holdout",
        param_set = param_set,
        man = "celecx::mlr_resamplings_lce_holdout"
      )
    }
  ),

  active = list(
    #' @field iters (`integer(1)`)
    #'   Always 1.
    iters = function(rhs) {
      assert_ro_binding(rhs)
      1L
    }
  ),

  private = list(
    .sample = function(ids, task, ...) {
      if ("ordered" %nin% task$properties) {
        stopf("Resampling '%s' requires an ordered task, but Task '%s' has no order.",
          self$id, task$id)
      }

      pv = self$param_set$get_values()
      ratio = pv$ratio

      col_roles = task$col_roles
      order_cols = col_roles$order
      key_cols = col_roles$key
      has_key = length(key_cols) > 0L

      dt = task$backend$data(
        rows = ids,
        cols = c(task$backend$primary_key, order_cols, key_cols)
      )
      setnames(dt, task$backend$primary_key, "row_id")

      if (!has_key) {
        setorderv(dt, order_cols)
        n = nrow(dt)
        nr = round(n * ratio)
        return(list(
          train = dt[seq_len(nr), "row_id"][[1L]],
          test = if (n > nr) dt[(nr + 1L):n, "row_id"][[1L]] else integer()
        ))
      }

      setorderv(dt, c(key_cols, order_cols))
      splits = dt[, {
        n_group = .N
        nr = round(n_group * ratio)
        list(
          train = list(.SD[seq_len(nr), "row_id"][[1L]]),
          test = list(
            if (n_group > nr) .SD[(nr + 1L):n_group, "row_id"][[1L]]
            else integer()
          )
        )
      }, by = key_cols]

      list(
        train = unlist(splits$train, use.names = FALSE),
        test = unlist(splits$test, use.names = FALSE)
      )
    },

    .get_train = function(i) {
      self$instance$train
    },

    .get_test = function(i) {
      self$instance$test
    },

    .combine = function(instances) {
      list(
        train = do.call(c, map(instances, "train")),
        test = do.call(c, map(instances, "test"))
      )
    },

    deep_clone = function(name, value) {
      switch(name,
        instance = copy(value),
        param_set = value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include aaa.R
resamplings[["lce_holdout"]] = ResamplingLCEHoldout
