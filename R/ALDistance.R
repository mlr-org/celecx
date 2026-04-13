#' @title Active Learning Distance Base Class
#'
#' @include ConfigurableComponent.R
#'
#' @description
#' Abstract base class for active-learning distances.
#'
#' An `ALDistance` is fitted on a candidate pool or, when the `xdt` passed to
#' `$fit_pool()` is `NULL`, on the search space itself. Concrete distance
#' classes support search-space fitting where the search-space bounds define the
#' scaling or encoding; classes that need empirical pool information throw an
#' error for `NULL` pools. Reference points are set separately before distances
#' are computed.
#'
#' @section Dictionary:
#' This class can be retrieved from [mlr_al_distances] via [clx_ald()] and
#' [clx_alds()].
#'
#' @export
#' @family ALDistance
ALDistance <- R6Class("ALDistance",
  inherit = ConfigurableComponent,

  public = list(

    #' @description
    #' Creates a new active-learning distance.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the distance.
    #' @param packages (`character()`)\cr
    #'   Optional package names required by the distance.
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Configuration parameter set.
    #' @param label (`character(1)`)\cr
    #'   Label for the object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page.
    initialize = function(id,
        packages = character(0),
        param_set = ps(),
        label = NA_character_,
        man = NA_character_) {

      assert_string(id, min.chars = 1L)
      private$.label <- assert_string(label, na.ok = TRUE)
      private$.man <- assert_string(man, na.ok = TRUE)
      private$.packages <- assert_character(packages, min.chars = 1L,
        any.missing = FALSE, unique = TRUE)

      super$initialize(
        id = id,
        param_set = param_set,
        additional_phash_input = c(".label", ".man", ".packages")
      )
    },

    #' @description
    #' Fit the distance on a finite candidate pool or on the search space.
    #'
    #' @param xdt (`NULL` | `data.table`)\cr
    #'   Candidate configurations in search-space coordinates. `NULL` fits the
    #'   distance to the search space itself, where supported by the concrete
    #'   distance.
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Search space describing the candidate-pool columns.
    #'
    #' @return `self`.
    fit_pool = function(xdt, search_space) {
      search_space <- assert_param_set(search_space)

      feature_ids <- search_space$ids()
      if (!is.null(xdt)) {
        assert_data_table(xdt, min.rows = 1L)
        assert_names(names(xdt), must.include = feature_ids)
        xdt <- xdt[, feature_ids, with = FALSE]
      }
      state <- private$.fit_pool(xdt, search_space)

      private$.search_space <- search_space$clone(deep = TRUE)
      private$.n_pool <- if (is.null(xdt)) NULL else nrow(xdt)
      private$.n_reference_points <- NULL
      private$.state <- state

      invisible(self)
    },

    #' @description
    #' Set the reference points used by `distances()`.
    #'
    #' @param xdt (`data.table`)\cr
    #'   Reference points in search-space coordinates.
    #'
    #' @return `self`.
    set_reference_points = function(xdt) {
      if (!self$is_fitted) {
        stopf("ALDistance '%s' is not fitted", self$id)
      }
      assert_data_table(xdt)
      feature_ids <- private$.search_space$ids()
      assert_names(names(xdt), must.include = feature_ids)

      xdt <- xdt[, feature_ids, with = FALSE]
      private$.state <- private$.set_reference_points(
        xdt,
        state = private$.state
      )
      private$.n_reference_points <- nrow(xdt)

      invisible(self)
    },

    #' @description
    #' Compute distances from query points to reference points indexed by `i`.
    #'
    #' @param xdt (`NULL` | `data.table`)\cr
    #'   Configurations in search-space coordinates. For `fit_pool()` only,
    #'   `NULL` fits the distance to the search space itself where supported.
    #' @param i (`NULL` | `integerish()`)\cr
    #'   Optional subset of reference-point indices. `NULL` uses all reference
    #'   points.
    #'
    #' @return Numeric matrix with one row per query point and one column per
    #'   selected reference point.
    distances = function(xdt, i = NULL) {
      if (!self$is_fitted) {
        stopf("ALDistance '%s' is not fitted", self$id)
      }
      if (is.null(private$.n_reference_points)) {
        stopf(
          "ALDistance '%s' has no reference points; call set_reference_points() after fit_pool()",
          self$id
        )
      }
      assert_data_table(xdt)
      feature_ids <- private$.search_space$ids()
      assert_names(names(xdt), must.include = feature_ids)
      xdt <- xdt[, feature_ids, with = FALSE]
      i <- assert_integerish(i, lower = 1L, upper = private$.n_reference_points,
        any.missing = FALSE, null.ok = TRUE, tol = 0, coerce = TRUE)

      distances <- private$.distances(
        xdt,
        state = private$.state,
        i = i
      )
      assert_matrix(
        distances,
        mode = "numeric",
        any.missing = FALSE,
        nrows = nrow(xdt),
        ncols = if (is.null(i)) private$.n_reference_points else length(i)
      )
      distances
    },

    #' @description
    #' Clear all fitted state.
    #'
    #' @return `self`.
    clear = function() {
      private$.search_space <- NULL
      private$.state <- NULL
      private$.n_pool <- NULL
      private$.n_reference_points <- NULL
      invisible(self)
    }
  ),

  active = list(

    #' @field label (`character(1)`)
    #'   Label for this object.
    label = function(rhs) {
      assert_ro_binding(rhs)
      private$.label
    },

    #' @field man (`character(1)`)
    #'   String in the format `[pkg]::[topic]` pointing to a manual page.
    man = function(rhs) {
      assert_ro_binding(rhs)
      private$.man
    },

    #' @field packages (`character()`)
    #'   Required packages.
    packages = function(rhs) {
      assert_ro_binding(rhs)
      private$.packages
    },

    #' @field is_fitted (`logical(1)`)
    #'   Whether the distance was fitted on a pool or search space.
    is_fitted = function(rhs) {
      assert_ro_binding(rhs)
      !is.null(private$.search_space)
    },

    #' @field search_space ([paradox::ParamSet] | `NULL`)
    #'   Search space used during fitting.
    search_space = function(rhs) {
      assert_ro_binding(rhs)
      if (is.null(private$.search_space)) {
        return(NULL)
      }
      private$.search_space$clone(deep = TRUE)
    },

    #' @field n_pool (`integer(1)` | `NULL`)
    #'   Number of pool points. `NULL` if the distance is not fitted or was
    #'   fitted to the search space without a finite pool.
    n_pool = function(rhs) {
      assert_ro_binding(rhs)
      private$.n_pool
    },

    #' @field n_reference_points (`integer(1)` | `NULL`)
    #'   Number of reference points set for distance computation.
    n_reference_points = function(rhs) {
      assert_ro_binding(rhs)
      private$.n_reference_points
    },

    #' @field state (`any`)
    #'   Fitted state returned by the subclass.
    state = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.state)) {
        stop("state is read-only")
      }
      private$.state
    }
  ),

  private = list(
    .label = NULL,
    .man = NULL,
    .packages = NULL,
    .search_space = NULL,
    .n_pool = NULL,
    .n_reference_points = NULL,
    .state = NULL,

    .fit_pool = function(xdt, search_space) {
      stop("Abstract.")
    },

    .set_reference_points = function(xdt, state) {
      state$xdt_reference <- copy(xdt)
      state
    },

    .distances = function(xdt, state, i = NULL) {
      stop("Abstract.")
    },

    .assert_distance_matrix = function(distances, n_rows) {
      assert_matrix(
        distances,
        mode = "numeric",
        any.missing = FALSE,
        nrows = n_rows
      )
      distances
    },

    deep_clone = function(name, value) {
      switch(name,
        .search_space = if (is.null(value)) NULL else value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)
