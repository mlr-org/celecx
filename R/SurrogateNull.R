#' @title Archive-Backed Surrogate Adapter
#'
#' @description
#' Adapter for acquisition functions that need archive, search-space, and
#' codomain context but do not need model predictions.
#'
#' `SurrogateNull` inherits from [mlr3mbo::Surrogate] so that
#' [mlr3mbo::AcqFunction] can derive its domain, codomain, and archive through
#' the usual `$surrogate` active binding. Its update and reset methods are
#' no-ops, and `$predict()` errors because there is no model.
#'
#' @export
#' @family Acquisition Function
SurrogateNull <- R6Class("SurrogateNull",
  inherit = Surrogate,

  public = list(

    #' @description
    #' Creates a new archive-backed surrogate adapter.
    #'
    #' @param archive (`NULL` | [bbotk::Archive])\cr
    #'   Archive carrying search-space and observed-evaluation context.
    #' @param cols_x (`NULL` | `character()`)\cr
    #'   Feature columns. Defaults to `archive$cols_x`.
    #' @param cols_y (`NULL` | `character()`)\cr
    #'   Target columns. Defaults to `archive$cols_y`.
    initialize = function(archive = NULL, cols_x = NULL, cols_y = NULL) {

      param_set <- ps(catch_errors = p_lgl(init = FALSE))

      super$initialize(
        learner = NULL,
        archive = archive,
        cols_x = cols_x,
        cols_y = cols_y,
        param_set = param_set
      )
    },

    #' @description
    #' Prediction is intentionally unsupported.
    #'
    #' @param xdt ([data.table::data.table()])\cr
    #'   New data.
    predict = function(xdt) {
      stop("SurrogateNull does not make predictions")
    }
  ),

  active = list(

    #' @field print_id (`character(1)`)
    #' Short printer id.
    print_id = function(rhs) {
      if (!missing(rhs)) stop("$print_id is read-only.")
      "archive"
    },

    #' @field n_learner (`integer(1)`)
    #' Number of represented target columns.
    n_learner = function(rhs) {
      if (!missing(rhs)) stop("$n_learner is read-only.")
      max(1L, length(self$cols_y))
    },

    #' @field packages (`character()`)
    #' Required packages.
    packages = function(rhs) {
      if (!missing(rhs)) stop("$packages is read-only.")
      character()
    },

    #' @field feature_types (`character()`)
    #' Supported feature types.
    feature_types = function(rhs) {
      if (!missing(rhs)) stop("$feature_types is read-only.")
      character()
    },

    #' @field properties (`character()`)
    #' Surrogate properties.
    properties = function(rhs) {
      if (!missing(rhs)) stop("$properties is read-only.")
      character()
    },

    #' @field predict_type (`character(1)`)
    #' Prediction type placeholder.
    predict_type = function(rhs) {
      if (!missing(rhs)) stop("$predict_type is read-only.")
      "response"
    }
  ),

  private = list(
    .update = function() NULL,
    .update_async = function() NULL,
    .reset = function() NULL
  )
)
