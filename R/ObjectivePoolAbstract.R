#' @title Abstract Base Class for Pool-backed Objectives
#'
#' @description
#' Common base class for objectives that restrict admissible evaluations to a
#' finite candidate pool.
#'
#' @details
#' Subclasses define how matched pool rows are turned into codomain values.
#' This base class handles pool validation, exact matching of queried
#' configurations back to the pool, and inspection of queried versus remaining
#' pool rows through the attached archive.
#'
#' Objectives inheriting from this class have the property "pool_restricted",
#' indicating that a `$pool` field is available.
#'
#' @include utils_objective.R utils_param_set.R
#' @export
ObjectivePoolAbstract <- R6Class("ObjectivePoolAbstract",
  inherit = Objective,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param pool (`data.frame` | `data.table`)\cr
    #'   Candidate pool containing one row per admissible configuration. Must
    #'   contain all domain columns.
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
    #'   Packages required to evaluate the objective.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to check validity of input configurations against the domain.
    #' @param label (`character(1)`)\cr
    #'   Human-readable label.
    #' @param man (`character(1)`)\cr
    #'   Help topic in `pkg::topic` format.
    initialize = function(pool, domain, codomain, id = "pool",
        properties = character(), constants = ps(),
        packages = character(), check_values = TRUE,
        label = NA_character_, man = NA_character_) {
      assert_data_frame(pool, min.rows = 1L, min.cols = 1L)

      ids <- assert_domain_codomain(domain, codomain)
      if (domain$has_trafo) {
        stop("Pool-backed objectives do not support domains with trafos")
      }

      dt <- as.data.table(pool)
      assert_data_table_param_set(
        dt,
        param_set = domain,
        require_uniqueness = TRUE,
        .param_set_name = "Domain",
        .dt_name = "pool"
      )

      private$.pool <- dt
      private$.domain_ids <- ids$domain_ids
      private$.codomain_ids <- ids$codomain_ids
      setkeyv(private$.pool, private$.domain_ids)

      super$initialize(
        id = id,
        domain = domain,
        codomain = codomain,
        properties = union(properties, "pool_restricted"),
        constants = constants,
        packages = packages,
        check_values = check_values,
        label = label,
        man = man
      )
    },

    #' @description
    #' Evaluates multiple input values on the objective function.
    #'
    #' @param xdt (`data.table`)\cr
    #'   A data.table with one configuration per row.
    #'
    #' @return [data.table::data.table()] containing codomain columns.
    eval_dt = function(xdt) {
      if (self$check_values) {
        assert_data_table_param_set(
          xdt,
          self$domain,
          require_uniqueness = FALSE,
          min_rows = 0L,
          allow_extra = FALSE,
          .param_set_name = "domain",
          .dt_name = "xdt"
        )
      } else {
        # assert_data_table_param_set asserts these itself, but if we don't check
        # thoroughly, we still want to ensure the following.
        assert_data_table(xdt)
        assert_names(colnames(xdt), permutation.of = private$.domain_ids)
      }
      matched_pool <- private$.match_pool(xdt)
      ydt <- private$.eval_pool(matched_pool)

      if (self$check_values) {
        assert_data_table(ydt, nrows = nrow(xdt))
        assert_names(colnames(ydt), must.include = private$.codomain_ids)
        self$codomain$assert_dt(ydt)
      }
      ydt
    },

    #' @description
    #' Evaluates multiple input values on the objective function.
    #'
    #' @param xss (`list()`)\cr
    #'   A list of lists that contains multiple x values, e.g.
    #'   `list(list(x1 = 1, x2 = 2), list(x1 = 3, x2 = 4))`.
    #'
    #' @return [data.table::data.table()] containing codomain columns.
    eval_many = function(xss) {
      prototype <- private$.pool[0L, private$.domain_ids, with = FALSE]
      xdt <- rbindlist(c(list(prototype), xss), use.names = TRUE, fill = TRUE)
      self$eval_dt(xdt)
    }
  ),

  active = list(
    #' @field pool (`data.table`)\cr
    #' Read-only access to the candidate pool.
    pool = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.pool)) stop("pool is read-only")
      private$.pool
    },

    #' @field pool_size (`integer(1)`)\cr
    #' Number of candidates in the pool.
    pool_size = function(rhs) {
      if (!missing(rhs)) stop("pool_size is read-only")
      nrow(private$.pool)
    }
  ),

  private = list(
    .pool = NULL,
    .domain_ids = NULL,
    .codomain_ids = NULL,

    .match_pool = function(xdt) {
      matched <- private$.pool[
        xdt,
        nomatch = 0L
      ]

      if (nrow(matched) != nrow(xdt)) {
        first_missing <- xdt[!matched, on = private$.domain_ids][1]
        query_str <- as_short_string(as.list(first_missing))
        stopf("Configuration not found in pool: {%s}", query_str)
      }
      matched
    },

    .eval_pool = function(matched_pool) {
      stop("abstract")
    },

    .eval_many = function(xss, ...) {
      stop("private$.eval_many should not be called; call either self$eval_many or private$.evaluate_pool directly")
    },

    deep_clone = function(name, value) {
      switch(name,
        .pool = copy(value),
        super$deep_clone(name, value)
      )
    }
  )
)
