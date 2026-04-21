#' @title Space Sampler Base Class
#'
#' @include ConfigurableComponent.R
#'
#' @description
#' Abstract base class for objects that sample points from a search space.
#'
#' A `SpaceSampler` can sample directly from a [paradox::ParamSet] or thin down
#' an existing candidate pool. Some samplers may additionally make use of a
#' `known_pool` of already-known points. The intended contract is that `pool`
#' and `known_pool` are disjoint, but this is not checked.
#'
#' If `pool` is given and `n >= nrow(pool)`, the full `pool` is returned
#' directly.
#'
#' @section Dictionary:
#' This class can be retrieved from [mlr_space_samplers] via [clx_sps()] and
#' [clx_spss()].
#'
#' @export
#' @family SpaceSampler
SpaceSampler <- R6Class("SpaceSampler",
  inherit = ConfigurableComponent,

  public = list(

    #' @description
    #' Creates a new space sampler.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the sampler.
    #' @param deterministic (`logical(1)`)\cr
    #'   Whether this sampler is marked as deterministic.
    #' @param packages (`character()`)\cr
    #'   Optional package names required by the sampler.
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Configuration parameter set.
    #' @param label (`character(1)`)\cr
    #'   Label for the object.
    #' @param man (`character(1)`)\cr
    #'   String in the format `[pkg]::[topic]` pointing to a manual page.
    initialize = function(id,
        deterministic,
        packages = character(0),
        param_set = ps(),
        label = NA_character_,
        man = NA_character_,
        additional_phash_input = character(0)) {

      assert_string(id, min.chars = 1L)
      private$.deterministic <- assert_flag(deterministic)
      private$.label <- assert_string(label, na.ok = TRUE)
      private$.man <- assert_string(man, na.ok = TRUE)
      private$.packages <- assert_character(packages, min.chars = 1L,
        any.missing = FALSE, unique = TRUE)

      super$initialize(
        id = id,
        param_set = param_set,
        additional_phash_input = c(
          ".deterministic",
          ".label",
          ".man",
          ".packages",
          additional_phash_input
        )
      )
    },

    #' @description
    #' Sample `n` points from the search space or thin down a pool.
    #'
    #' @param n (`integer(1)`)\cr
    #'   Number of points to sample.
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Search space describing the point columns.
    #' @param pool (`NULL` | `data.table`)\cr
    #'   Optional candidate pool to sample from.
    #' @param known_pool (`NULL` | `data.table`)\cr
    #'   Optional already-known points that may influence the sampler.
    #'
    #' @return `data.table`.
    sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      n <- assert_count(n, positive = TRUE)
      search_space <- assert_param_set(search_space)

      feature_ids <- search_space$ids()
      assert_data_table(pool, null.ok = TRUE)
      if (!is.null(pool)) {
        assert_names(names(pool), must.include = feature_ids)
        pool <- pool[, feature_ids, with = FALSE]
      }

      assert_data_table(known_pool, null.ok = TRUE)
      if (!is.null(known_pool)) {
        assert_names(names(known_pool), must.include = feature_ids)
        known_pool <- known_pool[, feature_ids, with = FALSE]
      }

      if (!is.null(pool) && n >= nrow(pool)) {
        return(pool)
      }

      xdt <- private$.sample(
        n = n,
        search_space = search_space,
        pool = pool,
        known_pool = known_pool
      )
      assert_data_table(xdt, nrows = n)
      assert_names(names(xdt), permutation.of = feature_ids)
      xdt[, feature_ids, with = FALSE]
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

    #' @field deterministic (`logical(1)`)
    #'   Whether the sampler is marked as deterministic.
    deterministic = function(rhs) {
      assert_ro_binding(rhs)
      private$.deterministic
    }
  ),

  private = list(
    .deterministic = NULL,
    .label = NULL,
    .man = NULL,
    .packages = NULL,

    .sample = function(n, search_space, pool = NULL, known_pool = NULL) {
      stop("Abstract.")
    }
  )
)
