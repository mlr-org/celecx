NULL

# Abstract base class for regression learners that wrap a base LearnerRegr
# (bootstrap_se, quantile_se). The regr counterpart of LearnerLCEWrapper: owns
# the cloned base learner, the lazy ParamSetCollection exposing the wrapper's
# own parameters next to the base learner's parameters under the `base.`
# prefix, the `wrapped` binding, and a deep_clone that delegates to mlr3's
# Learner deep_clone while cloning the wrapper-owned R6 objects.
#
# Subclasses store plain learner states in their model (never learner
# objects) and use with_learner_state() on private$.base_learner_obj both for
# training (harvest the resulting state) and for prediction (inject a stored
# state). The stored states are shared, immutable data, so deep_clone needs
# no model handling.
#
# Not registered in mlr_learners and not exported; it is an implementation
# detail of the concrete wrapper learners.
LearnerRegrWrapper <- R6Class("LearnerRegrWrapper",
  inherit = LearnerRegr,
  public = list(
    initialize = function(learner, id_prefix, param_set, properties, label, man) {
      assert_learner(learner, task_type = "regr")
      private$.base_learner_obj <- learner$clone(deep = TRUE)
      private$.own_param_set <- param_set

      super$initialize(
        id = sprintf("%s.%s", id_prefix, learner$id),
        feature_types = learner$feature_types,
        predict_types = c("response", "se"),
        param_set = ps(),
        packages = union("celecx", learner$packages),
        # marshaling of the stored trained learners is handled by the
        # marshal_model methods in marshal.R, regardless of the base learner
        properties = union(properties, "marshal"),
        label = label,
        man = man
      )

      # replace the placeholder ParamSet with the lazy own + base collection
      # built by the binding
      private$.param_set <- NULL
    },

    # Marshal the model (see mlr3's marshaling contract).
    marshal = function(...) {
      learner_marshal(.learner = self, ...)
    },

    # Unmarshal the model.
    unmarshal = function(...) {
      learner_unmarshal(.learner = self, ...)
    }
  ),

  active = list(
    #' @field wrapped ([mlr3::LearnerRegr])\cr
    #' Read-only access to the wrapped base learner.
    wrapped = function(rhs) {
      assert_ro_binding(rhs)
      private$.base_learner_obj
    },

    #' @field marshaled (`logical(1)`)\cr
    #' Whether the model is marshaled.
    marshaled = function(rhs) {
      assert_ro_binding(rhs)
      learner_marshaled(self)
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
