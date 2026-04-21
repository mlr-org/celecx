#' @title Objective Function Based on a Candidate Pool and R Function
#'
#' @description
#' An [ObjectivePoolAbstract] where matched pool rows are evaluated by a
#' user-supplied R function.
#'
#' @details
#' The evaluation function receives the matched pool rows, not just the domain
#' columns. Extra pool columns are therefore available for candidate
#' identifiers or other metadata needed during evaluation.
#'
#' Duplicate domain configurations in the pool are rejected.
#'
#' @include ObjectivePoolAbstract.R
#' @export
ObjectivePoolRFun <- R6Class("ObjectivePoolRFun",
  inherit = ObjectivePoolAbstract,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param pool (`data.frame` | `data.table`)\cr
    #'   Candidate pool containing one row per admissible configuration. Must
    #'   contain all domain columns. Extra columns are allowed and are passed to
    #'   `fun`.
    #' @param fun (`function`)\cr
    #'   R function that evaluates matched pool rows. It must accept at least one argument,
    #'   the matched pool rows, and return the codomain values as a `data.table`, `data.frame`,
    #'   or `list` that can be converted to a `data.table` using `rbindlist`.
    #' @param domain ([paradox::ParamSet])\cr
    #'   Parameter set describing the input space.
    #' @param codomain ([paradox::ParamSet])\cr
    #'   Parameter set describing the output space.
    #' @param id (`character(1)`)\cr
    #'   Identifier for the objective.
    #' @param properties (`character()`)\cr
    #'   Objective properties such as `"deterministic"` or `"noisy"`.
    #' @param constants ([paradox::ParamSet])\cr
    #'   Constant parameters that are not subject to tuning.
    #' @param packages (`character()`)\cr
    #'   Packages required to evaluate `fun`.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to check validity of input configurations against the domain.
    initialize = function(pool, fun, domain, codomain, id = "pool",
        properties = character(), constants = ps(), packages = character(),
        check_values = TRUE) {
      assert_function(fun)

      private$.fun <- fun

      super$initialize(
        pool = pool,
        domain = domain,
        codomain = codomain,
        id = id,
        properties = properties,
        constants = constants,
        packages = packages,
        check_values = check_values,

        label = "Objective from Candidate Pool and R Function",
        man = "celecx::ObjectivePoolRFun"
      )
    }
  ),

  active = list(
    #' @field fun (`function`)\cr
    #' Evaluation function used on the matched pool rows.
    fun = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.fun)) stop("fun is read-only")
      private$.fun
    }
  ),

  private = list(
    .fun = NULL,

    .eval_pool = function(matched_pool) {
      fun_result <- private$.fun(matched_pool)
      if (is.data.frame(fun_result)) {
        # data.frame *or* data.table (noop for the latter):
        setDT(fun_result, keep.rownames = FALSE)
      } else if (is.list(fun_result)) {
        rbindlist(fun_result, use.names = TRUE, fill = TRUE)
      } else {
        stop("Pool objective evaluation must return a data.frame, data.table, or list that can be coerced using rbindlist")  # nolint
      }
    }
  )
)
