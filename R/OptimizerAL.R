#' @title Proposer-Based Active Learning Optimizer
#'
#' @include OptimizerPoolAbstract.R
#' @include ALProposer.R
#' @include ResultAssignerNull.R
#' @include SurrogateNull.R
#'
#' @description
#' Active-learning optimizer whose outer loop is fixed and whose proposal logic
#' is delegated to an [ALProposer].
#'
#' The optimizer owns canonical run-local surrogates and unwired acquisition
#' function prototypes. Proposers refer to those objects by registry id; the
#' ephemeral [ALContext] wires and updates them lazily.
#'
#' @section Parameters:
#' \describe{
#' \item{`batch_size`}{`integer(1)`\cr
#'   Number of configurations evaluated per active-learning proposal round.}
#' \item{`replace_samples`}{`character(1)`\cr
#'   Whether finite-pool points can be proposed again after a batch.
#'   `"never"` exhausts the pool without replacement; `"between_batches"`
#'   allows repeat evaluations in later batches, while still preventing repeats
#'   within the current proposal batch.}
#' \item{`n_init`}{`integer(1)`\cr
#'   Number of initial evaluations requested before the proposer is used.
#'   If unset, fresh runs use `4 * d` initial evaluations, where `d` is the
#'   search-space dimension, while runs with an already populated archive do not
#'   request additional initial evaluations.}
#' }
#'
#' The optimizer parameter set also exposes the parameter sets of directly owned
#' components: `init_sampler`, `proposer`, `surrogates`, and acquisition-function
#' `constants`.
#'
#' @export
OptimizerAL <- R6Class("OptimizerAL",
  inherit = OptimizerPoolAbstract,

  public = list(

    #' @description
    #' Creates a new active-learning optimizer.
    #'
    #' @param proposer ([ALProposer])\cr
    #'   Proposer used after initialization.
    #' @param surrogates (named `list()` of [mlr3mbo::Surrogate])\cr
    #'   Canonical surrogate registry. Defaults to an archive-only surrogate
    #'   under id `"archive"` for model-free acquisition functions.
    #' @param acq_functions (named `list()` of [mlr3mbo::AcqFunction])\cr
    #'   Acquisition-function prototype registry. Registered acquisition
    #'   functions are treated as unwired prototypes; any pre-set surrogate is
    #'   removed with a warning.
    #' @param init_sampler (`NULL` | [SpaceSampler])\cr
    #'   Sampler for the initial evaluations. `NULL` is only valid when
    #'   `n_init = 0`.
    #' @param result_assigner (`NULL` | [mlr3mbo::ResultAssigner])\cr
    #'   Result assigner used for [bbotk::OptimInstance] objects. Defaults to
    #'   [ResultAssignerNull].
    #' @param grid_expansion_limit (`integer(1)`)\cr
    #'   Upper limit for fully-discrete grid expansion inherited from
    #'   [OptimizerPoolAbstract].
    initialize = function(proposer,
        surrogates = list(archive = SurrogateNull$new()),
        acq_functions,
        init_sampler = SpaceSamplerUniform$new(),
        result_assigner = ResultAssignerNull$new(),
        grid_expansion_limit = 1e7L) {

      private$.proposer <- assert_r6(proposer, "ALProposer")
      private$.init_sampler <- assert_r6(init_sampler, "SpaceSampler", null.ok = TRUE)
      private$.surrogates <- private$.clone_named_r6_list(surrogates, "Surrogate")
      private$.acq_functions <- private$.normalize_acq_functions(acq_functions)
      private$.result_assigner <- assert_r6(result_assigner, "ResultAssigner", null.ok = TRUE)
      private$.own_param_set <- ps(
        batch_size = p_int(lower = 1L, init = 1L, tags = "required"),
        replace_samples = p_fct(c("never", "between_batches"), init = "never", tags = "required"),
        n_init = p_int(lower = 0L)
      )

      packages <- unique(c(
        private$.proposer$packages,
        if (is.null(private$.init_sampler)) character(0) else private$.init_sampler$packages,
        unlist(map(private$.surrogates, "packages"), use.names = FALSE),
        unlist(map(private$.acq_functions, "packages"), use.names = FALSE)
      ))

      super$initialize(
        id = "al",
        param_set = ps(),
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl", "ParamUty"),
        properties = c("dependencies", "single-crit"),  # TODO: want proposer properties here to announce multi-crit support
        packages = packages,
        label = "Active Learning",
        man = "celecx::OptimizerAL",
        grid_expansion_limit = grid_expansion_limit
      )

      # unfortunate hack for param_set, since we need to construct it lazily
      private$.param_set <- NULL
    }
  ),

  active = list(

    #' @field proposer ([ALProposer])
    #' Proposer used after initialization.
    proposer = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.proposer)) {
        stop("proposer is read-only.")
      }
      private$.proposer
    },

    #' @field init_sampler (`NULL` | [SpaceSampler])
    #' Sampler for initial evaluations.
    init_sampler = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.init_sampler)) {
        stop("init_sampler is read-only.")
      }
      private$.init_sampler
    },

    #' @field surrogates (named `list()` of [mlr3mbo::Surrogate])
    #' Canonical surrogate registry.
    surrogates = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.surrogates)) {
        stop("surrogates is read-only.")
      }
      private$.surrogates
    },

    #' @field acq_functions (named `list()` of [mlr3mbo::AcqFunction])
    #' Unwired acquisition-function prototype registry.
    acq_functions = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.acq_functions)) {
        stop("acq_functions is read-only.")
      }
      private$.acq_functions
    },

    #' @field result_assigner (`NULL` | [mlr3mbo::ResultAssigner])
    #' Result assigner.
    result_assigner = function(rhs) {
      if (missing(rhs)) {
        private$.result_assigner
      } else {
        private$.result_assigner <- assert_r6(rhs, "ResultAssigner", null.ok = TRUE)
      }
    },

    #' @field param_set ([paradox::ParamSet])
    #' Combined parameter set of the optimizer and directly owned components.
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        param_sets <- list(private$.own_param_set)
        if (!is.null(private$.init_sampler) && length(private$.init_sampler$param_set$ids())) {
          param_sets$init_sampler <- private$.init_sampler$param_set
        }
        if (length(private$.proposer$param_set$ids())) {
          param_sets$proposer <- private$.proposer$param_set
        }
        for (id in names(private$.surrogates)) {
          param_set <- private$.surrogates[[id]]$param_set
          if (length(param_set$ids())) {
            param_sets[[sprintf("surrogate_%s", id)]] <- param_set
          }
        }
        for (id in names(private$.acq_functions)) {
          constants <- private$.acq_functions[[id]]$constants
          if (length(constants$ids())) {
            param_sets[[sprintf("acq_function_%s", id)]] <- constants
          }
        }
        private$.param_set <- if (length(param_sets) == 1L) {
          param_sets[[1L]]
        } else {
          ParamSetCollection$new(param_sets)
        }
      }
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .proposer = NULL,
    .init_sampler = NULL,
    .surrogates = NULL,
    .acq_functions = NULL,
    .result_assigner = NULL,
    .own_param_set = NULL,
    .param_set = NULL,

    .optimize_discrete = function(inst, candidates) {
      private$.optimize_discrete_continuous(inst, pool = candidates)
    },

    .optimize_continuous = function(inst) {
      private$.optimize_discrete_continuous(inst, pool = NULL)
    },

    .optimize_discrete_continuous = function(inst, pool = NULL) {
      private$.reset_run_resources(inst)

      pv <- private$.own_param_set$get_values()
      feature_ids <- inst$search_space$ids()

      # uniqueN by = feature_ids does not work for empty archive (no columns)
      seen_distinct_points <- if (!nrow(inst$archive$data)) 0L else uniqueN(inst$archive$data, by = feature_ids)
      n_init <- pv$n_init
      if (is.null(n_init)) {
        n_init <- if (seen_distinct_points == 0L) {
          4L * length(feature_ids)
        } else {
          0L
        }
      }

      if (n_init > seen_distinct_points) {
        private$.eval_initial(inst, pool,
          n = min(n_init - seen_distinct_points, private$.evals_remaining(inst))
        )
      }
      if (pv$replace_samples == "never" && !is.null(pool)) {
        if (!nrow(inst$archive$data)) {
          n_available <- nrow(pool)
        } else {
          n_available <- length(pool[!inst$archive$data, on = feature_ids, which = TRUE])
        }
      } else  if (is.null(pool)) {
        n_available <- Inf
      } else {
        n_available <- nrow(pool)
      }

      run_state <- list(
        working_acqs = new.env(parent = emptyenv()),
        working_acqs_search_space = new.env(parent = emptyenv()),
        acq_global_fit_done = new.env(parent = emptyenv())
      )

      while (!inst$is_terminated && n_available > 0L) {
        n_batch <- min(pv$batch_size, n_available, private$.evals_remaining(inst))
        if (n_batch <= 0L) {
          break
        }

        context <- ALContext$new(
          instance = inst,
          pool = pool,
          surrogates = private$.surrogates,
          acq_functions = private$.acq_functions,
          run_state = run_state,
          allow_repeat_evaluations = pv$replace_samples == "between_batches"
        )

        xdt <- private$.proposer$propose(context, n_batch)
        if (!nrow(xdt)) {
          break
        }

        inst$eval_batch(xdt)
      }
    },

    .eval_initial = function(inst, pool, n) {
      if (n <= 0L) {
        return(invisible(NULL))
      }
      if (is.null(private$.init_sampler)) {
        stop("init_sampler must not be NULL when n_init > current archive size")
      }

      feature_ids <- inst$search_space$ids()
      if (!is.null(pool) && nrow(inst$archive$data)) {
        # even if we replace between batches, we do the initial sample distinct from known points, since
        # we don't want duplicates in the initial sample (and if duplicates exist, we don't count them)
        pool <- pool[!inst$archive$data, on = feature_ids]
        if (!nrow(pool)) {
          return(invisible(NULL))
        }
      }

      known_pool <- if (inst$archive$n_evals) {
        unique(inst$archive$data[, feature_ids, with = FALSE])
      } else {
        param_set_empty_dt(inst$search_space)
      }
      xdt <- private$.init_sampler$sample(
        n = n,
        search_space = inst$search_space,
        pool = pool,
        known_pool = known_pool
      )
      inst$eval_batch(xdt)
      return(invisible(NULL))
    },

    # heuristically determines how many evaluations are remaining in the budget
    .evals_remaining = function(inst) {
      if (!inherits(inst$terminator, "TerminatorEvals")) {
        return(Inf)
      }
      status <- inst$terminator$status(inst$archive)
      max(0L, status[["max_steps"]] - status[["current_steps"]])
    },

    .reset_run_resources = function(inst) {
      walk(private$.surrogates, function(surrogate) {
        surrogate$archive <- inst$archive
        surrogate$reset()
      })
      walk(private$.acq_functions, function(acq_function) {
        acq_function$reset()
      })
      invisible(NULL)
    },

    .assign_result = function(inst) {
      if (inherits(inst, "SearchInstance")) {
        return(invisible(NULL))
      }
      if (is.null(private$.result_assigner)) {
        return(assign_result_default(inst))
      }

      private$.result_assigner$assign_result(inst)
    },

    .clone_named_r6_list = function(x, class) {
      assert_list(x, min.len = 1L, names = "unique")
      assert_names(names(x), type = "strict")
      lapply(x, function(obj) {
        assert_r6(obj, class)
        if (identical(class, "Surrogate")) {
          return(private$.clone_surrogate(obj))
        }
        obj$clone(deep = TRUE)
      })
    },

    .clone_surrogate = function(surrogate) {
      # mlr3mbo's Surrogate deep_clone assumes that $archive is non-NULL. OptimizerAL
      # treats registered surrogates as archive-agnostic prototypes, so we need a
      # null-archive-safe clone for the common surrogate classes.
      if (inherits(surrogate, "SurrogateNull")) {
        priv <- get_private(surrogate)
        clone <- SurrogateNull$new(
          archive = NULL,
          cols_x = priv$.cols_x,
          cols_y = priv$.cols_y
        )
        clone$param_set$values <- surrogate$param_set$values
        return(clone)
      }

      if (inherits(surrogate, "SurrogateLearner")) {
        priv <- get_private(surrogate)
        clone <- SurrogateLearner$new(
          learner = surrogate$learner$clone(deep = TRUE),
          input_trafo = if (is.null(surrogate$input_trafo)) NULL else surrogate$input_trafo$clone(deep = TRUE),
          output_trafo = if (is.null(surrogate$output_trafo)) NULL else surrogate$output_trafo$clone(deep = TRUE),
          archive = NULL,
          cols_x = priv$.cols_x,
          col_y = priv$.cols_y
        )
        clone$param_set$values <- surrogate$param_set$values
        return(clone)
      }

      surrogate$clone(deep = TRUE)
    },

    .normalize_acq_functions = function(acq_functions) {
      acq_functions <- private$.clone_named_r6_list(acq_functions, "AcqFunction")
      iwalk(acq_functions, function(acq_function, id) {
        if (!is.null(acq_function$surrogate)) {
          warningf(
            "Ignoring pre-set surrogate on acquisition function '%s'; use ALProposer$surrogate_id instead",
            id
          )
          acq_function$domain <- ParamSet$new()
          acq_function$codomain <- Codomain$new(list())
          private_acq <- get_private(acq_function)
          private_acq$.surrogate <- NULL
          private_acq$.archive <- NULL
        }
      })
      acq_functions
    },

    deep_clone = function(name, value) {
      switch(name,
        .proposer = value$clone(deep = TRUE),
        .init_sampler = if (!is.null(value)) value$clone(deep = TRUE),
        .surrogates = lapply(value, private$.clone_surrogate),
        .acq_functions = lapply(value, function(acq_function) acq_function$clone(deep = TRUE)),
        .own_param_set = value$clone(deep = TRUE),
        .param_set = {
          private$.param_set <- NULL  # required to keep clone identical to original, otherwise tests can get ugly
          NULL
        },
        .result_assigner = if (!is.null(value)) value$clone(deep = TRUE),
        value
      )
    }
  )
)

#' @include aaa.R
optimizers[["al"]] = OptimizerAL
