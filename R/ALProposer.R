#' @title Active Learning Proposer
#'
#' @include ALContext.R
#' @include SpaceSampler.R
#' @include ALScoreModifier.R
#'
#' @description
#' Base class for objects that propose the next active-learning batch.
#'
#' Proposers receive an [ALContext] from [OptimizerAL]. They should use the
#' context's accessor methods instead of touching surrogate and acquisition
#' registries directly, because those methods implement lazy update and
#' clone-on-write rules.
#'
#' Inheriting classes should implement the `private$.propose()` method.
#' It gets the context and the number of points to propose, should return a
#' `data.table` of the proposed points.
#'
#' @export
ALProposer <- R6Class("ALProposer",
  inherit = ConfigurableComponent,

  public = list(

    #' @description
    #' Creates a new proposer.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier.
    #' @param param_set ([paradox::ParamSet] | `list()`)\cr
    #'   Configuration parameter set or dynamic parameter-set source list.
    #' @param packages (`character()`)\cr
    #'   Required packages.
    #' @param label (`character(1)`)\cr
    #'   Label.
    #' @param man (`character(1)`)\cr
    #'   Help page reference.
    #' @param additional_phash_input (`character()`)\cr
    #'   Additional private/public fields used for persistent hashing.
    initialize = function(id,
        param_set = ps(),
        packages = character(0),
        label = NA_character_,
        man = NA_character_,
        additional_phash_input = character(0)) {

      private$.label <- assert_string(label, na.ok = TRUE)
      private$.man <- assert_string(man, na.ok = TRUE)
      private$.packages <- assert_character(packages, min.chars = 1L,
        any.missing = FALSE, unique = TRUE)

      super$initialize(
        id = id,
        param_set = param_set,
        additional_phash_input = c(".label", ".man", ".packages", additional_phash_input)
      )
    },

    #' @description
    #' Proposes up to `n` points for the next evaluation batch.
    #'
    #' @param context ([ALContext])\cr
    #'   Active-learning proposal context.
    #' @param n (`integer(1)`)\cr
    #'   Maximum number of points to propose.
    #'
    #' @return `data.table`.
    propose = function(context, n) {
      assert_r6(context, "ALContext")
      n <- assert_count(n, positive = TRUE, tol = 0)
      xdt <- private$.propose(context, n)
      assert_data_table(xdt, max.rows = n)
      feature_ids <- context$instance$search_space$ids()
      assert_names(names(xdt), must.include = feature_ids)
      xdt[, feature_ids, with = FALSE]
    }
  ),

  active = list(
    #' @field label (`character(1)`)
    #' Label.
    label = function(rhs) {
      assert_ro_binding(rhs)
      private$.label
    },

    #' @field man (`character(1)`)
    #' Help page reference.
    man = function(rhs) {
      assert_ro_binding(rhs)
      private$.man
    },

    #' @field packages (`character()`)
    #' Required packages.
    packages = function(rhs) {
      assert_ro_binding(rhs)
      private$.packages
    }
  ),

  private = list(
    .label = NULL,
    .man = NULL,
    .packages = NULL,

    .propose = function(context, n) {
      stop("Abstract.")
    },


    #' @description
    #' Calculates the scalar score to be maximized by the proposer.
    .acq_utility = function(acq_function, scores) {
      if (acq_function$codomain$target_length != 1L) {
        stop("ALProposer currently supports only single-objective acquisition functions")
      }

      y_id <- acq_function$codomain$target_ids[[1L]]
      direction <- acq_function$codomain$direction[[y_id]]
      -scores[[y_id]] * direction
    },

    #' @description
    #' Returns the indices of the top `n` candidates based on the utility score.
    .top_indices = function(utility, n) {
      if (!length(utility) || all(is.na(utility))) {
        return(integer(0))
      }
      idx <- order(utility, decreasing = TRUE, na.last = NA)
      idx[seq_len(min(n, length(idx)))]
    }
  )
)


#' @title Abstract Base Class for Score-Based Active Learning Proposers
#'
#' @description
#' Scores a finite candidate set once with an acquisition function and returns
#' the top candidates.
#'
#' @export
ALProposerScoreAbstract <- R6Class("ALProposerScoreAbstract",
  inherit = ALProposer,

  public = list(

    #' @description
    #' Creates a score-based proposer.
    #'
    #' @param acq_id (`character(1)`)\cr
    #'   Acquisition-function registry id.
    #' @param surrogate_id (`character(1)`)\cr
    #'   Surrogate registry id.
    #' @param candidate_sampler (`NULL` | [SpaceSampler])\cr
    #'   Optional sampler used to create the candidate set to score. `NULL`
    #'   means exhaustive scoring of the remaining finite pool.
    #' @param n_candidates (`integer(1)` | `Inf`)\cr
    #'   Number of candidates requested from `candidate_sampler`. Ignored when
    #'   `candidate_sampler` is `NULL`.
    #' @param acq_fit_scope (`character(1)`)\cr
    #'   `"global"` fits acquisition functions on the run's full finite pool;
    #'   `"candidate"` fits on the current candidate set; `"search_space"` fits
    #'   on the search space bounds.
    initialize = function(id, acq_id, surrogate_id,
        candidate_sampler = NULL,
        n_candidates = Inf,
        acq_fit_scope = "global",
        param_set = ps(),
        packages = character(0),
        label = NA_character_,
        man = NA_character_,
        additional_phash_input = character(0)) {

      private$.acq_id <- assert_string(acq_id, min.chars = 1L)
      private$.surrogate_id <- assert_string(surrogate_id, min.chars = 1L)
      private$.candidate_sampler <- assert_r6(candidate_sampler, "SpaceSampler", null.ok = TRUE)
      acq_fit_scope <- assert_choice(acq_fit_scope, al_proposer_acq_fit_scopes)

      private$.al_proposer_score_abstract_param_set <- ps(
        n_candidates = p_int(lower = 1L, special_vals = list(Inf), init = n_candidates, tags = "required"),
        acq_fit_scope = p_fct(levels = al_proposer_acq_fit_scopes, init = acq_fit_scope, tags = "required")
      )

      param_set_full <- alist(private$.al_proposer_score_abstract_param_set)
      if (!is.null(private$.candidate_sampler)) {
        param_set_full$candidate_sampler <- quote(private$.candidate_sampler$param_set)
      }
      if (inherits(param_set, "ParamSet")) {
        # param_set is a ParamSet object
        private$.al_proposer_score_abstract_param_set <- c(
          private$.al_proposer_score_abstract_param_set,
          param_set
        )
      } else {
        # param_set is a list of parameter set source expressions
        param_set_full <- c(param_set_full, param_set)
      }

      super$initialize(
        id = id,
        param_set = param_set_full,
        packages = unique(c(packages, private$.candidate_sampler$packages)),
        label = label,
        man = man,
        additional_phash_input = c(".acq_id", ".surrogate_id", ".candidate_sampler",
          additional_phash_input)
      )
    }
  ),

  active = list(
    #' @field acq_id (`character(1)`)
    #' Acquisition-function registry id.
    acq_id = function(rhs) {
      assert_ro_binding(rhs)
      private$.acq_id
    },

    #' @field surrogate_id (`character(1)`)
    #' Surrogate registry id.
    surrogate_id = function(rhs) {
      assert_ro_binding(rhs)
      private$.surrogate_id
    },

    #' @field candidate_sampler (`NULL` | [SpaceSampler])
    #' Optional candidate sampler.
    candidate_sampler = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.candidate_sampler)) {
        stop("candidate_sampler is read-only.")
      }
      private$.candidate_sampler
    }
  ),

  private = list(
    .acq_id = NULL,
    .surrogate_id = NULL,
    .candidate_sampler = NULL,
    .al_proposer_score_abstract_param_set = NULL,

    .propose = function(context, n) {
      pv <- private$.al_proposer_score_abstract_param_set$get_values()
      n_candidates <- pv$n_candidates
      candidate_sampler <- private$.candidate_sampler
      candidates <- context$proposable_xdt
      if (is.null(candidates) && (is.null(candidate_sampler) || n_candidates == Inf)) {
        stop("A candidate_sampler with finite n_candidates is required when not optimizing over a finite candidate pool")  # nolint
      }
      if (is.null(candidate_sampler) || (!is.null(candidates) && nrow(candidates) <= n_candidates)) {
        candidate_pool <- candidates
      } else {
        candidate_pool <- candidate_sampler$sample(
          n = n_candidates,
          search_space = context$instance$search_space,
          pool = candidates,
          known_pool = context$evaluated_and_pending_xdt
        )
      }

      if (nrow(candidate_pool) <= n) {
        return(candidate_pool)
      }

      private$.propose_pool_score_based(context, n, candidate_pool, private$.acq_id,
        private$.surrogate_id, pv$acq_fit_scope)
    },

    # at this point, n is guaranteed to be less than nrow(candidate_pool)
    .propose_pool_score_based = function(context, n, candidate_pool, acq_id, surrogate_id, acq_fit_scope) {
      stop("Abstract.")
    },
    deep_clone = function(name, value) {
      switch(name,
        .candidate_sampler = if (is.null(value)) NULL else value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)


#' @title Score-Based Active Learning Proposer
#'
#' @description
#' Scores a finite candidate set once with an acquisition function and returns
#' the top candidates.
#'
#' @export
ALProposerScore <- R6Class("ALProposerScore",
  inherit = ALProposerScoreAbstract,

  public = list(

    #' @description
    #' Creates a score-based proposer.
    #'
    #' @param acq_id (`character(1)`)\cr
    #'   Acquisition-function registry id.
    #' @param surrogate_id (`character(1)`)\cr
    #'   Surrogate registry id.
    #' @param candidate_sampler (`NULL` | [SpaceSampler])\cr
    #'   Optional sampler used to create the candidate set to score. `NULL`
    #'   means exhaustive scoring of the remaining finite pool.
    #' @param n_candidates (`integer(1)` | `Inf`)\cr
    #'   Number of candidates requested from `candidate_sampler`. Ignored when
    #'   `candidate_sampler` is `NULL`.
    #' @param acq_fit_scope (`character(1)`)\cr
    #'   `"global"` fits acquisition functions on the run's full finite pool;
    #'   `"candidate"` fits on the current candidate set; `"search_space"` fits
    #'   on the search space bounds.
    initialize = function(acq_id, surrogate_id,
        candidate_sampler = NULL,
        n_candidates = Inf,
        acq_fit_scope = "global") {

      super$initialize(
        id = sprintf("score_%s.%s", private$.acq_id, private$.surrogate_id),
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        candidate_sampler = candidate_sampler,
        n_candidates = n_candidates,
        acq_fit_scope = acq_fit_scope,
        label = "Score-Based AL Proposer",
        man = "celecx::ALProposerScore"
      )
    }
  ),

  private = list(
    .propose_pool_score_based = function(context, n, candidate_pool, acq_id, surrogate_id, acq_fit_scope) {
      acq_function <- context$get_acq(
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        fit_scope = acq_fit_scope,
        pool = candidate_pool
      )
      utility <- private$.acq_utility(acq_function, acq_function$eval_dt(candidate_pool))

      candidate_pool[private$.top_indices(utility, n)]
    }
  )
)


#' @title Sequential Score-Based Active Learning Proposer
#'
#' @description
#' Scores a candidate set once and then builds a batch sequentially by applying
#' a proposer-local [ALScoreModifier].
#'
#' @export
ALProposerSequentialScore <- R6Class("ALProposerSequentialScore",
  inherit = ALProposerScoreAbstract,

  public = list(

    #' @description
    #' Creates a sequential score-based proposer.
    #'
    #' @param acq_id (`character(1)`)\cr
    #'   Acquisition-function registry id.
    #' @param surrogate_id (`character(1)`)\cr
    #'   Surrogate registry id.
    #' @param score_modifier ([ALScoreModifier])\cr
    #'   Score modifier used between sequential selections.
    #' @param candidate_sampler (`NULL` | [SpaceSampler])\cr
    #'   Optional sampler used to create the candidate set to score.
    #' @param n_candidates (`integer(1)` | `Inf`)\cr
    #'   Number of candidates requested from `candidate_sampler`.
    #' @param acq_fit_scope (`character(1)`)\cr
    #'   Acquisition-function fit scope.
    initialize = function(acq_id, surrogate_id,
        score_modifier = ALScoreModifierNone$new(),
        candidate_sampler = NULL,
        n_candidates = Inf,
        acq_fit_scope = "global") {

      private$.score_modifier <- assert_r6(score_modifier, "ALScoreModifier")

      param_set <- alist(score_modifier = private$.score_modifier$param_set)

      super$initialize(
        id = sprintf("sequential_score_%s.%s.%s", acq_id, surrogate_id, score_modifier$id),
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        candidate_sampler = candidate_sampler,
        n_candidates = n_candidates,
        acq_fit_scope = acq_fit_scope,
        param_set = param_set,
        packages = score_modifier$packages,
        label = "Sequential Score-Based AL Proposer",
        man = "celecx::ALProposerSequentialScore",
        additional_phash_input = ".score_modifier"
      )
    }
  ),

  active = list(
    #' @field score_modifier ([ALScoreModifier])
    #' Score modifier.
    score_modifier = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.score_modifier)) {
        stop("score_modifier is read-only.")
      }
      private$.score_modifier
    }
  ),

  private = list(
    .score_modifier = NULL,

    .propose_pool_score_based = function(context, n, candidate_pool, acq_id, surrogate_id, acq_fit_scope) {
      acq_function <- context$get_acq(
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        fit_scope = acq_fit_scope,
        pool = candidate_pool
      )
      utility <- private$.acq_utility(acq_function, acq_function$eval_dt(candidate_pool))

      selected <- integer(0)
      available <- rep(TRUE, nrow(candidate_pool))
      pending_xdt <- context$pending_xdt
      selected_xdt <- candidate_pool[integer(0)]
      for (i in seq_len(n)) {
        in_batch_xdt <- rbind(pending_xdt, selected_xdt, use.names = TRUE)
        modified <- private$.score_modifier$modify(
          candidates = candidate_pool,
          utility = utility,
          selected = in_batch_xdt,
          search_space = context$instance$search_space,
          context = context
        )
        modified[!available] <- -Inf
        if (all(is.na(modified) | modified == -Inf)) {
          break
        }

        best <- which.max(modified)
        selected[[i]] <- best
        available[[best]] <- FALSE
        selected_xdt <- candidate_pool[selected]
      }

      selected_xdt
    },

    deep_clone = function(name, value) {
      switch(name,
        .score_modifier = value$clone(deep = TRUE),
        super$deep_clone(name, value)
      )
    }
  )
)


#' @title Sequential Reference Active Learning Proposer
#'
#' @description
#' Builds a batch by repeatedly rescoring after treating already selected batch
#' points as temporary distance references.
#'
#' This is exact for GSx-style acquisition functions whose scores only depend on
#' input-space references. It is a heuristic for acquisitions that also depend on
#' labels or model predictions.
#'
#' It may be necessary to use ALProposerSequentialReference even when proposing
#' only a single point with this proposer itself if the proposer is part of a
#' portfolio of proposers.
#'
#' @export
ALProposerSequentialReference <- R6Class("ALProposerSequentialReference",
  inherit = ALProposerScoreAbstract,

  public = list(
    #' @description
    #' Creates a sequential-reference proposer.
    initialize = function(acq_id, surrogate_id,
        candidate_sampler = NULL,
        n_candidates = Inf,
        acq_fit_scope = "global") {
      super$initialize(
        id = sprintf("sequential_reference_%s.%s", acq_id, surrogate_id),
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        candidate_sampler = candidate_sampler,
        n_candidates = n_candidates,
        acq_fit_scope = acq_fit_scope,
        label = "Sequential Reference AL Proposer",
        man = "celecx::ALProposerSequentialReference"
      )
    }
  ),

  private = list(
    .propose_pool_score_based = function(context, n, candidate_pool, acq_id, surrogate_id, acq_fit_scope) {
      acq_function <- context$get_acq(
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        fit_scope = acq_fit_scope,
        pool = candidate_pool,
        clone = TRUE
      )
      if (!inherits(acq_function, "AcqFunctionDist")) {
        stopf("ALProposerSequentialReference requires acquisition function '%s' to inherit from AcqFunctionDist", acq_function$id)
      }

      selected <- integer(0)
      available <- rep(TRUE, nrow(candidate_pool))
      evaluated_and_pending_xdt <- context$evaluated_and_pending_xdt
      selected_xdt <- candidate_pool[integer(0)]
      for (i in seq_len(n)) {
        references <- rbind(selected_xdt, evaluated_and_pending_xdt, use.names = TRUE)
        acq_function$al_distance$set_reference_points(references)
        utility <- private$.acq_utility(acq_function, acq_function$eval_dt(candidate_pool))
        utility[!available] <- -Inf
        if (all(is.na(utility) | utility == -Inf)) {
          break
        }

        best <- which.max(utility)
        selected[[i]] <- best
        available[[best]] <- FALSE
        selected_xdt <- candidate_pool[selected]
      }

      selected_xdt
    }
  )
)


#' @title Pseudo-Label Active Learning Proposer
#'
#' @description
#' Builds a batch sequentially by adding pseudo-labels to a temporary archive
#' between proposals.
#'
#' The pseudo-label values are computed by the `label_surrogate_id` surrogate.
#'
#' @export
ALProposerPseudoLabel <- R6Class("ALProposerPseudoLabel",
  inherit = ALProposerScoreAbstract,

  public = list(
    #' @description
    #' Creates a pseudo-label proposer.
    #'
    #' @param label_surrogate_id (`character(1)`)\cr
    #'   Label surrogate registry id.
    initialize = function(acq_id, surrogate_id, label_surrogate_id,
        candidate_sampler = NULL,
        n_candidates = Inf,
        acq_fit_scope = "global") {
      private$.label_surrogate_id <- assert_string(label_surrogate_id, min.chars = 1L)
      super$initialize(
        id = sprintf("pseudo_label_%s.%s.%s", acq_id, surrogate_id, label_surrogate_id),
        acq_id = acq_id,
        surrogate_id = surrogate_id,
        candidate_sampler = candidate_sampler,
        n_candidates = n_candidates,
        acq_fit_scope = acq_fit_scope,
        label = "Pseudo-Label AL Proposer",
        man = "celecx::ALProposerPseudoLabel"
      )
    }
  ),

  active = list(
    #' @field label_surrogate_id (`character(1)`)\cr
    #' Label surrogate registry id.
    label_surrogate_id = function(rhs) {
      assert_ro_binding(rhs)
      private$.label_surrogate_id
    }
  ),

  private = list(
    .label_surrogate_id = NULL,

    .propose_pool_score_based = function(context, n, candidate_pool, acq_id, surrogate_id, acq_fit_scope) {

      tmp_archive <- context$instance$archive$clone(deep = TRUE)
      surrogate <- context$get_surrogate(surrogate_id)$clone(deep = TRUE)
      surrogate$archive <- tmp_archive
      liar_surrogate <- context$get_surrogate(private$.label_surrogate_id)$clone(deep = TRUE)
      liar_surrogate$archive <- tmp_archive
      acq_function <- context$new_acq(
        acq_id = acq_id,
        surrogate = surrogate,
        fit_scope = acq_fit_scope,
        pool = candidate_pool
      )

      selected <- integer(0)
      available <- rep(TRUE, nrow(candidate_pool))

      for (i in seq_len(n)) {
        if (i > 1L) {
          surrogate$update()
          liar_surrogate$update()
          acq_function$update()
        }

        utility <- private$.acq_utility(acq_function, acq_function$eval_dt(candidate_pool))
        utility[!available] <- -Inf
        if (all(is.na(utility) | utility == -Inf)) {
          break
        }

        best <- which.max(utility)
        selected[[i]] <- best
        available[[best]] <- FALSE

        xdt_new <- candidate_pool[best]
        ydt <- as.data.table(set_names(
          list(liar_surrogate$predict(xdt_new)$mean),
          tmp_archive$cols_y
        ))

        tmp_archive$add_evals(
          xdt = xdt_new,
          xss_trafoed = transform_xdt_to_xss(xdt_new, tmp_archive$search_space),
          ydt = ydt
        )
      }

      candidate_pool[selected]
    }
  )
)


#' @title Portfolio Active Learning Proposer
#'
#' @description
#' Combines child proposers by querying them round-robin until enough unique
#' points have been proposed or all children fail to produce another point.
#'
#' @export
ALProposerPortfolio <- R6Class("ALProposerPortfolio",
  inherit = ALProposer,

  public = list(

    #' @description
    #' Creates a portfolio proposer.
    #'
    #' @param proposers (named `list()` of [ALProposer])\cr
    #'   Child proposers.
    initialize = function(proposers) {
      assert_list(proposers, min.len = 1L, names = "unique")
      walk(proposers, assert_r6, "ALProposer")
      private$.proposers <- proposers
      private$.own_param_set <- ps(n_per_proposer = p_int(lower = 1L, init = 1L, tags = "required"))

      param_set <- alist(private$.own_param_set)
      for (pname in names(private$.proposers)) {
        param_set[[pname]] <- substitute(
          private$.proposers[[pname]]$param_set,
          list(pname = pname)
        )
      }

      super$initialize(
        id = sprintf("portfolio_%s", paste(names(private$.proposers), collapse = ".")),
        param_set = param_set,
        packages = unique(unlist(map(private$.proposers, "packages"), use.names = FALSE)),
        label = "Portfolio AL Proposer",
        man = "celecx::ALProposerPortfolio",
        additional_phash_input = ".proposers"
      )
    }
  ),

  active = list(
    #' @field proposers (named `list()` of [ALProposer])
    #' Child proposers.
    proposers = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.proposers)) {
        stop("proposers is read-only.")
      }
      private$.proposers
    }
  ),

  private = list(
    .proposers = NULL,
    .own_param_set = NULL,

    .propose = function(context, n) {
      pv <- private$.own_param_set$get_values()
      n_per_proposer <- pv$n_per_proposer

      empty_streak <- 0L
      child_pos <- 1L
      n_remaining <- n
      selected_list <- list()

      while (n_remaining > 0L && empty_streak < length(private$.proposers)) {
        child <- private$.proposers[[child_pos]]
        proposal <- child$propose(context, min(n_per_proposer, n_remaining))
        n_remaining <- n_remaining - nrow(proposal)

        if (nrow(proposal)) {
          selected_list[[length(selected_list) + 1L]] <- proposal
          context$add_pending(proposal)
          empty_streak <- 0L
        } else {
          empty_streak <- empty_streak + 1L
        }

        child_pos <- child_pos + 1L
        if (child_pos > length(private$.proposers)) {
          child_pos <- 1L
        }
      }

      if (length(selected_list)) {
        rbindlist(selected_list, use.names = TRUE)
      } else {
        param_set_empty_dt(context$instance$search_space)
      }
    },

    deep_clone = function(name, value) {
      switch(name,
        .proposers = lapply(value, function(proposer) proposer$clone(deep = TRUE)),
        super$deep_clone(name, value)
      )
    }
  )
)
