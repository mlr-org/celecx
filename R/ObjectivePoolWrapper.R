#' @title Objective Function Wrapping Another Objective on a Candidate Pool
#'
#' @description
#' An [ObjectivePoolAbstract] that restricts evaluation to a candidate pool and
#' delegates evaluation to another [Objective].
#'
#' @details
#' The wrapped objective is cloned on construction. Its codomain,
#' properties, and package requirements define the corresponding fields of the
#' wrapper.
#'
#' The wrapped objective's domain may differ from the wrapper's domain;
#' in this case, the missing values are reconstructed from the pool columns and
#' superfluous columns are dropped.
#' The pool values must be unique with respect to the wrapper's domain.
#'
#' @include ObjectivePoolAbstract.R
#' @export
ObjectivePoolWrapper <- R6Class("ObjectivePoolWrapper",
  inherit = ObjectivePoolAbstract,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param pool (`data.frame` | `data.table`)\cr
    #'   Candidate pool containing one row per admissible configuration. Must
    #'   contain all wrapped-objective domain columns.
    #' @param objective ([bbotk::Objective])\cr
    #'   Objective to wrap. It is cloned on construction.
    #' @param id (`character(1)`)\cr
    #'   Identifier for the objective.
    #' @param domain ([paradox::ParamSet] | `NULL`)\cr
    #'   Parameter set describing the input space. If `NULL`, the wrapped objective's domain is used.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to check validity of input configurations against the domain.
    #'   This does not affect the wrapped objective's `check_values`.
    #'   When `domain` is `NULL`, the wrapped objective's `check_values` is set to `FALSE`.
    #'   Otherwise, it is unchanged; it is up to the user to decide whether it is desirable for the
    #'   wrapped objective to check values.
    initialize = function(pool, objective, id = objective$id,
        domain = NULL,
        check_values = objective$check_values) {
      assert_r6(objective, "Objective")
      assert_string(id)
      assert_flag(check_values)

      private$.objective <- objective$clone(deep = TRUE)
      if (is.null(domain)) {
        private$.objective$check_values <- FALSE
      }

      # while assert_data_table_param_set asserts data.table-ness, we need to convert first, and for that, we
      # do asserts first.
      assert_data_frame(pool, min.rows = 1L, min.cols = 1L)
      pool <- as.data.table(pool)
      assert_data_table_param_set(
        pool,
        param_set = private$.objective$domain,
        require_uniqueness = FALSE,
        allow_untyped = TRUE,
        .param_set_name = "wrapped objective domain",
        .dt_name = "pool"
      )

      super$initialize(
        pool = pool,
        domain = domain %??% private$.objective$domain$clone(deep = TRUE),
        codomain = private$.objective$codomain$clone(deep = TRUE),
        id = id,
        properties = private$.objective$properties,
        packages = private$.objective$packages,
        check_values = check_values,

        label = "Objective from Candidate Pool and Wrapped Objective",
        man = "celecx::ObjectivePoolWrapper"
      )
    }
  ),

  active = list(
    #' @field objective ([bbotk::Objective])\cr
    #' Wrapped objective.
    objective = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.objective)) stop("objective is read-only")
      private$.objective
    }
  ),

  private = list(
    .objective = NULL,

    .eval_pool = function(matched_pool) {
      if (!nrow(matched_pool)) {
        return(param_set_empty_dt(private$.objective$codomain))
      }

      xdt <- matched_pool[, private$.objective$domain$ids(), with = FALSE]
      if (objective_uses_dt_eval(private$.objective)) {
        return(private$.objective$eval_dt(xdt))
      }

      private$.objective$eval_many(
        map(transpose_list(xdt), discard, is_scalar_na)
      )
    },

    deep_clone = function(name, value) {
      switch(name,
        .objective = value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)
