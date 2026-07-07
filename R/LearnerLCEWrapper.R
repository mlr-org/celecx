#' @include LearnerLCE.R
#' @include utils_lce.R
NULL

# Abstract base class for LCE learners that wrap a base LearnerLCE (bootstrap,
# conformal). Owns the wrap-a-learner plumbing: the cloned base learner, the
# lazy ParamSetCollection exposing the wrapper's own parameters next to the
# base learner's parameters under the `base.` prefix, the `wrapped` binding,
# and a deep_clone that delegates to mlr3's Learner deep_clone (state$train_task,
# log, fallback) while cloning the wrapper-owned R6 objects.
#
# Subclasses implement `.train` / `.predict` (reading their own parameters
# from private$.own_param_set). They store plain learner states in their model
# (never learner objects) and use with_learner_state() on
# private$.base_learner_obj both for training (harvest the resulting state)
# and for prediction (inject a stored state). The stored states are shared,
# immutable data, so deep_clone needs no model handling.
#
# Not registered in mlr_learners and not exported; it is an implementation
# detail of the concrete wrapper learners.
LearnerLCEWrapper <- R6Class("LearnerLCEWrapper",
  inherit = LearnerLCE,
  public = list(
    initialize = function(learner, id_prefix, param_set, predict_types, label, man) {
      assert_r6(learner, "LearnerLCE")
      private$.base_learner_obj <- learner$clone(deep = TRUE)
      private$.own_param_set <- c(
        param_set,
        lce_predict_type_params(predict_types)
      )

      super$initialize(
        id = sprintf("%s.%s", id_prefix, learner$id),
        param_set = ps(),
        predict_types = predict_types,
        feature_types = learner$feature_types,
        properties = intersect(learner$properties,
          mlr_reflections$learner_properties$lce),
        packages = union("celecx", learner$packages),
        label = label,
        man = man
      )

      # LearnerLCE$initialize stored (and augmented) the placeholder ParamSet;
      # replace it with the lazy own + base collection built by the binding.
      private$.param_set <- NULL
    }
  ),

  active = list(
    #' @field wrapped ([LearnerLCE])\cr
    #' Read-only access to the wrapped base learner.
    wrapped = function(rhs) {
      assert_ro_binding(rhs)
      private$.base_learner_obj
    },

    #' @field param_set ([paradox::ParamSet])\cr
    #' Combined parameter set: the wrapper's own parameters plus the base
    #' learner's parameters under the `base.` prefix.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set <- ParamSetCollection$new(list(
          private$.own_param_set,
          base = private$.base_learner_obj$param_set
        ))
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .base_learner_obj = NULL,
    .own_param_set = NULL,

    deep_clone = function(name, value) {
      switch(name,
        .base_learner_obj = value$clone(deep = TRUE),
        .own_param_set = value$clone(deep = TRUE),
        # the clone rebuilds its collection lazily from its cloned components;
        # the original's cache stays untouched
        .param_set = NULL,
        super$deep_clone(name, value)
      )
    }
  )
)
