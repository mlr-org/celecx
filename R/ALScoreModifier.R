#' @title Active Learning Score Modifier
#'
#' @include ConfigurableComponent.R
#' @include ALDistanceGower.R
#'
#' @description
#' Score modifiers adjust acquisition utilities while a batch is built
#' sequentially. Utilities use the convention that larger values are better.
#'
#' Score modifiers are proposer-local: they may consider points already selected
#' for the current batch, but they do not change the real archive, surrogate, or
#' acquisition function state.
#'
#' @export
ALScoreModifier <- R6Class("ALScoreModifier",
  inherit = ConfigurableComponent,

  public = list(

    #' @description
    #' Creates a new score modifier.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier.
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Configuration parameters.
    #' @param label (`character(1)`)\cr
    #'   Label.
    #' @param man (`character(1)`)\cr
    #'   Help page reference.
    initialize = function(id, param_set = ps(), label = NA_character_, man = NA_character_) {
      private$.label <- assert_string(label, na.ok = TRUE)
      private$.man <- assert_string(man, na.ok = TRUE)

      super$initialize(
        id = id,
        param_set = param_set,
        additional_phash_input = c(".label", ".man")
      )
    },

    #' @description
    #' Modifies acquisition utilities.
    #'
    #' @param candidates (`data.table`)\cr
    #'   Candidate points.
    #' @param utility (`numeric()`)\cr
    #'   Acquisition utility, larger is better.
    #' @param selected (`data.table`)\cr
    #'   Points already selected for the current batch or portfolio round.
    #'   The point of the score modifier is typically to penalize points close to
    #'   points proposed within the same batch (but to be indifferent of points
    #'   that were already evaluated -- it is expected that their value was
    #'   taken into account by the acquisition function).
    #' @param search_space ([paradox::ParamSet])\cr
    #'   Search space.
    #' @param context ([ALContext])\cr
    #'   Current active-learning context.
    #'
    #' @return `numeric()`.
    modify = function(candidates, utility, selected, search_space, context) {
      assert_data_table(candidates)
      assert_numeric(utility, len = nrow(candidates))
      assert_data_table(selected)
      assert_param_set(search_space)
      assert_r6(context, "ALContext")

      private$.modify(candidates, utility, selected, search_space, context)
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
    }
  ),

  private = list(
    .label = NULL,
    .man = NULL,

    .modify = function(candidates, utility, selected, search_space, context) {
      stop("Abstract.")
    }
  )
)


#' @title No-Op Active Learning Score Modifier
#'
#' @description
#' Leaves acquisition utilities unchanged.
#'
#' @export
ALScoreModifierNone <- R6Class("ALScoreModifierNone",
  inherit = ALScoreModifier,

  public = list(
    #' @description
    #' Creates a no-op score modifier.
    initialize = function() {
      super$initialize(
        id = "none",
        label = "No-Op AL Score Modifier",
        man = "celecx::ALScoreModifierNone"
      )
    }
  ),

  private = list(
    .modify = function(candidates, utility, selected, search_space, context) {
      utility
    }
  )
)


#' @title Local-Penalization Active Learning Score Modifier
#'
#' @description
#' Penalizes candidates close to points already selected for the current batch.
#'
#' @export
ALScoreModifierLocalPenalization <- R6Class("ALScoreModifierLocalPenalization",
  inherit = ALScoreModifier,

  public = list(
    #' @description
    #' Creates a local-penalization score modifier.
    #'
    #' @param bandwidth (`numeric(1)`)\cr
    #'   Gower-distance radius in which utilities are penalized.
    #' @param penalization (`numeric(1)`)\cr
    #'   Amount subtracted from utilities. `Inf` removes nearby points.
    initialize = function(bandwidth = 0.1, penalization = Inf) {
      param_set <- ps(
        bandwidth = p_dbl(lower = 0, upper = 1, init = bandwidth, tags = "required"),
        penalization = p_dbl(lower = 0, special_vals = list(Inf), init = penalization, tags = "required")
      )

      super$initialize(
        id = "local_penalization",
        param_set = param_set,
        label = "Local-Penalization AL Score Modifier",
        man = "celecx::ALScoreModifierLocalPenalization"
      )
    }
  ),

  private = list(
    .modify = function(candidates, utility, selected, search_space, context) {
      if (!nrow(selected)) {
        return(utility)
      }

      pv <- self$param_set$get_values()
      if (pv$bandwidth == 0) {
        return(utility)
      }

      distances <- al_distance_gower_search_space_distances(candidates, selected, search_space)
      nearby <- rowSums(distances <= pv$bandwidth) > 0

      if (is.infinite(pv$penalization)) {
        utility[nearby] <- -Inf
      } else {
        utility[nearby] <- utility[nearby] - pv$penalization
      }

      utility
    }
  )
)


#' @title Diversity Active Learning Score Modifier
#'
#' @description
#' Blends acquisition utility with distance to points already selected for the
#' current batch.
#'
#' @export
ALScoreModifierDiversity <- R6Class("ALScoreModifierDiversity",
  inherit = ALScoreModifier,

  public = list(
    #' @description
    #' Creates a diversity score modifier.
    #'
    #' @param diversity_weight (`numeric(1)`)\cr
    #'   Weight of the diversity term. `0` keeps pure acquisition utility, `1`
    #'   uses only distance to already-selected batch points.
    initialize = function(diversity_weight = 0.5) {
      param_set <- ps(
        diversity_weight = p_dbl(lower = 0, upper = 1, init = diversity_weight, tags = "required")
      )

      super$initialize(
        id = "diversity",
        param_set = param_set,
        label = "Diversity AL Score Modifier",
        man = "celecx::ALScoreModifierDiversity"
      )
    }
  ),

  private = list(
    .modify = function(candidates, utility, selected, search_space, context) {
      pv <- self$param_set$get_values()
      w <- pv$diversity_weight
      if (!nrow(selected) || w == 0) {
        return(utility)
      }

      utility_normalized <- rep(-Inf, length(utility))
      finite <- is.finite(utility)
      if (any(finite)) {
        utility_range <- max(utility[finite]) - min(utility[finite])
        utility_normalized[finite] <- if (utility_range == 0) {
          0
        } else {
          (utility[finite] - min(utility[finite])) / utility_range
        }
      }

      distances <- al_distance_gower_search_space_distances(candidates, selected, search_space)
      distance_min <- apply(distances, 1L, min)
      distance_range <- max(distance_min) - min(distance_min)
      distance_normalized <- if (distance_range == 0) {
        rep(0, length(distance_min))
      } else {
        (distance_min - min(distance_min)) / distance_range
      }
      if (w == 1) {
        return(distance_normalized)
      }

      (1 - w) * utility_normalized + w * distance_normalized
    }
  )
)
