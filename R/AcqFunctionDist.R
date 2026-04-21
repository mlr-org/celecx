#' @title Distance-Aware Acquisition Function Base Class
#'
#' @include sugar_al_distances.R
#'
#' @description
#' Abstract base class for distance-aware active-learning acquisition functions.
#'
#' The acquisition constants include a detached copy of the wrapped
#' [ALDistance] parameter set under the `al_distance` prefix. During
#' update and evaluation, those values are copied into the distance before
#' reference points are set or distances are computed.
#'
#' `$fit_pool()` fits the distance on the candidate pool used for scaling and
#' pool-derived acquisition state, or on the search space when `xdt` is `NULL`.
#' `$update()` then sets the distance reference points to the current
#' archive rows in archive order.
#'
#' @export
#' @family Acquisition Function
AcqFunctionDist <- R6Class("AcqFunctionDist",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new distance-aware acquisition function.
    #'
    #' @param id (`character(1)`)\cr
    #'   Acquisition function id.
    #' @param constants ([paradox::ParamSet])\cr
    #'   Changeable constants or parameters of the acquisition function.
    #' @param surrogate (`NULL` | [mlr3mbo::Surrogate])\cr
    #'   Surrogate whose predictions are used in the acquisition function.
    #' @param al_distance (`NULL` | [ALDistance])\cr
    #'   Distance object used by the acquisition function.
    #' @param requires_predict_type_se (`logical(1)`)\cr
    #'   Whether the surrogate must use predict type `"se"`.
    #' @param surrogate_class (`character(1)`)\cr
    #'   Allowed class of the surrogate. Default `"Surrogate"`.
    #' @param direction (`"same"` | `"minimize"` | `"maximize"`)\cr
    #'   Optimization direction of the acquisition function.
    #' @param packages (`character()`)\cr
    #'   Required packages.
    #' @param label (`character(1)`)\cr
    #'   Label for the object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page.
    initialize = function(id,
        constants = ps(),
        surrogate = NULL,
        al_distance = NULL,
        requires_predict_type_se,
        surrogate_class = "Surrogate",
        direction,
        packages = NULL,
        label = NA_character_,
        man = NA_character_) {

      private$.al_distance <- assert_r6(al_distance, "ALDistance", null.ok = TRUE)
      private$.constants_base <- assert_param_set(constants)

      super$initialize(
        id = id,
        constants = acq_function_dist_constants(private$.constants_base, private$.al_distance),
        surrogate = surrogate,
        requires_predict_type_se = requires_predict_type_se,
        surrogate_class = surrogate_class,
        direction = direction,
        packages = packages,
        label = label,
        man = man
      )
    },

    #' @description
    #' Fits the wrapped distance and any subclass pool state on a finite
    #' candidate pool or on the search space.
    #'
    #' @param xdt (`NULL` | `data.table`)\cr
    #'   Candidate configurations in search-space coordinates. `NULL` fits to
    #'   the search space where supported.
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Search space describing the candidate-pool columns.
    #'
    #' @return `self`.
    fit_pool = function(xdt, search_space) {
      if (is.null(private$.al_distance)) {
        return(invisible(self))
      }

      # self$al_distance automatically sets distance hyperparameters from $constants
      self$al_distance$fit_pool(xdt, search_space)
      private$.fit_pool(xdt, search_space)

      invisible(self)
    },

    #' @description
    #' Sets the distance reference points to the current surrogate archive.
    update = function() {
      if (is.null(self$surrogate) || is.null(private$.al_distance)) {
        return(invisible(NULL))
      }

      archive <- self$surrogate$archive
      # self$al_distance automatically sets distance hyperparameters from $constants
      self$al_distance$set_reference_points(
        archive$data[, self$domain$ids(), with = FALSE]
      )

      invisible(NULL)
    }
  ),

  active = list(

    #' @field al_distance (`NULL` | [ALDistance])
    #'   Distance object used by the acquisition function.
    al_distance = function(rhs) {
      if (missing(rhs)) {
        constants <- self$constants$values
        is_distance_constant <- private$.is_distance_constant(constants)
        if (any(is_distance_constant)) {
          distance_values <- constants[is_distance_constant]
          names(distance_values) <- sub("^al_distance\\.", "", names(distance_values))
          private$.al_distance$param_set$set_values(.values = distance_values)
        }
        return(private$.al_distance)
      }
      # we should not do identical(rhs, private$.al_distance) here, because after the assignment, we collect the
      # $constant values again from the object.
      # Note that object$al_distance$param_set$set_values() will not be forwarded transparently into $constants,
      # we would need for $constants to be an active binding-constructed ParamSetCollection for that.

      old_values <- if (is.null(self$constants)) named_list() else self$constants$values
      private$.al_distance <- assert_r6(rhs, "ALDistance", null.ok = TRUE)

      self$constants <- acq_function_dist_constants(private$.constants_base, private$.al_distance)
      values <- old_values[names(old_values) %in% self$constants$ids()]
      if (length(values)) {
        self$constants$set_values(.values = values)
      }

      invisible(private$.al_distance)
    }
  ),

  private = list(
    .al_distance = NULL,
    .constants_base = NULL,

    .fun = function(xdt, ...) {
      constants <- list(...)
      is_distance_constant <- private$.is_distance_constant(constants)

      distances <- self$al_distance$distances(xdt)
      if (!ncol(distances)) {
        # TODO: can this actually happen?
        stop("Acquisition functions require at least one distance reference point")
      }


      invoke(
        private$.fun_dist,
        xdt,
        distances = distances,
        .args = constants[!is_distance_constant]
      )
    },

    .fun_dist = function(xdt, distances, ...) {
      stop("Abstract.")
    },

    .fit_pool = function(xdt, search_space) {
      invisible(NULL)
    },

    .is_distance_constant = function(constants) {
      is_distance_constant <- startsWith(names(constants) %??% character(0), "al_distance.")
      if (any(is_distance_constant) && is.null(private$.al_distance)) {
        stopf("Acquisition function '%s' has distance constants but no ALDistance; This is a bug and should not happen.", self$id)  # nolint
      }
      is_distance_constant
    },
    deep_clone = function(name, value) {
      switch(name,
        .al_distance = if (is.null(value)) NULL else value$clone(deep = TRUE),
        .constants_base = value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)

acq_function_dist_constants <- function(constants, al_distance) {
  constants <- assert_param_set(constants)
  if (is.null(al_distance)) {
    return(constants)
  }

  ParamSetCollection$new(set_names(
    list(constants, al_distance$param_set$clone(deep = TRUE)),
    c("", "al_distance")
  ))
}
