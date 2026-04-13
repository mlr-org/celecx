#' @title Validate Learner Against Domain
#'
#' @description
#' Validates that a fitted learner is compatible with a given domain.
#' Checks that the learner is fitted, that domain parameters match trained features,
#' and validates type compatibility including factor level constraints.
#'
#' @param learner ([mlr3::LearnerRegr])\cr
#'   A fitted regression learner.
#' @param domain ([paradox::ParamSet])\cr
#'   Parameter set describing the input space.
#'
#' @return `TRUE` invisibly if validation succeeds.
#'
#' @details
#' Type compatibility:
#' * `ParamDbl` -> numeric feature in training task
#' * `ParamInt` -> integer or numeric feature in training task
#' * `ParamFct` -> factor feature in training task, and domain levels must be
#'   a subset of the trained factor levels
#' * `ParamLgl` -> logical feature in training task
#'
#' @keywords internal
assert_learner_domain <- function(learner, domain, learner_name = NULL) {
  assert_learner(learner, task_type = "regr")
  assert_string(learner_name, null.ok = TRUE)
  if (is.null(learner_name)) {
    learner_identifier <- ""
  } else {
    learner_identifier <- sprintf(" (%s)", learner_name)
  }
  # Check learner is fitted
  if (is.null(learner$state$model) && is.null(learner$state$fallback_state$model)) {
    stopf("Learner%s has not been trained yet", learner_identifier)
  }

  # Get train_task info
  train_task <- learner$state$train_task
  if (is.null(train_task)) {
    # train_task is always created on training, so this never happens when learner is not corrupted
    stopf("Corrupted Learner%s: state does not contain train_task", learner_identifier)
  }

  domain_ids <- domain$ids()

  # Get the features the learner was trained on
  trained_feature_names <- learner$state$feature_names
  if (is.null(trained_feature_names)) {
    stopf("Corrupted Learner%s: state does not contain feature_names", learner_identifier)
  }

  # Check that all domain parameters were features in the training task
  missing_features <- setdiff(domain_ids, trained_feature_names)
  if (length(missing_features)) {
    stopf("Domain parameters not found in learner%s's training features: %s",
      learner_identifier,
      str_collapse(missing_features))
  }

  # Check that domain covers all trained features
  missing_domain <- setdiff(trained_feature_names, domain_ids)
  if (length(missing_domain)) {
    stopf("Domain must include all learner%s training features. Missing: %s",
      learner_identifier,
      str_collapse(missing_domain))
  }

  # Get col_info from training task for type checking
  col_info <- train_task$col_info

  # Process each domain parameter to check type compatibility
  for (param_id in domain_ids) {
    param_class <- domain$class[[param_id]]

    # Get the training task's type and levels for this feature
    train_info <- col_info[col_info$id == param_id, ]
    if (nrow(train_info) == 0L) {
      stopf("Corrupted Learner%s: feature '%s' not found in training task col_info", learner_identifier, param_id)
    }
    train_type <- train_info$type

    # For learner, ParamFct must match exactly "factor" (no character conversion)
    if (param_class == "ParamFct" && train_type != "factor") {
      stopf("Domain parameter '%s' is ParamFct but training feature%s is '%s' (must also be factor)",
        param_id, learner_identifier, train_type)
    } else if (param_class != "ParamFct") {
      assert_param_type_compatible(param_id, param_class, train_type, sprintf("training feature%s", learner_identifier))
    }

    # For ParamFct: check that domain levels are a SUBSET of trained levels
    if (param_class == "ParamFct") {
      train_levels <- train_info$levels[[1L]]
      domain_levels <- domain$levels[[param_id]]
      extra_levels <- setdiff(domain_levels, train_levels)
      if (length(extra_levels)) {
        stopf("Domain parameter '%s' of Learner%s has levels not in training data: %s (trained levels: %s)",
          param_id, learner_identifier, str_collapse(extra_levels), str_collapse(train_levels))
      }
    }
  }

  invisible(TRUE)
}


#' @title Objective Function Based on a Fitted Learner
#'
#' @description
#' An [Objective] subclass where evaluation uses a fitted regression learner
#' to predict outcomes. This is useful for surrogate-based optimization where
#' a model has been trained on observed data and we want to optimize over
#' predicted outcomes.
#'
#' @details
#' The learner must be trained before creating the objective. The domain
#' parameters must correspond to features the learner was trained on.
#' For factor features, domain levels must be a subset of the levels
#' present in the training data (the learner cannot generalize to unseen
#' factor levels).
#'
#' Type compatibility:
#' * `ParamDbl` corresponds to numeric training features
#' * `ParamInt` corresponds to integer or numeric training features
#' * `ParamFct` corresponds to factor training features (domain levels must be subset)
#' * `ParamLgl` corresponds to logical training features
#'
#' @include utils_objective.R
#' @export
ObjectiveLearner <- R6Class("ObjectiveLearner",
  inherit = Objective,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr] | named `list` of [mlr3::LearnerRegr])\cr
    #'   A fitted regression learner or a named list of fitted regression learners. Must have been trained via `$train()`
    #'   before creating the objective.
    #'   If this is a named list, its names must correspond to codomain target IDs.
    #'   There must be one learner per codomain target.
    #'   If this is a single learner, `codomain` must only have one parameter.
    #' @param domain ([paradox::ParamSet])\cr
    #'   Parameter set describing the input space. All parameter IDs must
    #'   correspond to features the learner was trained on.
    #' @param codomain ([paradox::ParamSet])\cr
    #'   Parameter set describing the output space. Must contain at least one
    #'   target tagged with `"minimize"`, `"maximize"`, or `"learn"`.
    #' @param id (`character(1)`)\cr
    #'   Identifier for the objective.
    #' @param check_values (`logical(1)`)\cr
    #'   Whether to check validity of input configurations against the domain.
    initialize = function(learner, domain, codomain, id = "learner", check_values = TRUE) {

      # Validate learner against domain/codomain
      assert_domain_codomain(domain, codomain)
      codomain_ids <- codomain$ids()
      if (is.list(learner)) {
        assert_names(names(learner), permutation.of = codomain_ids)
        for (l in learner) {
          assert_learner_domain(l, domain)
        }
      } else {
        if (length(codomain_ids) != 1L) {
          stopf("If learner is a single learner, codomain must have exactly one parameter, got %s", length(codomain_ids))
        }
        assert_learner_domain(learner, domain)
        learner <- structure(list(learner), names = codomain_ids)
      }

      # Store the learner (clone it to avoid modifications affecting us)
      private$.learners <- lapply(learner, function(l) l$clone(deep = TRUE))

      # Call parent constructor
      super$initialize(
        id = id,
        domain = domain,
        codomain = codomain,
        properties = "deterministic",
        constants = ps(),
        check_values = check_values,
        label = "Objective from Fitted Learner",
        man = NA_character_
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
        # note that we allow a subset of domain columns, since predict_newdata() can handle missing cols.
        assert_data_table_param_set(
          xdt,
          self$domain,
          require_uniqueness = FALSE,
          min_rows = 0L,
          presence = "subset",
          allow_extra = FALSE,
          .param_set_name = "domain",
          .dt_name = "xdt"
        )
      }
      private$.predict(xdt)
    }
  ),

  active = list(
    #' @field learner ([mlr3::LearnerRegr])\cr
    #' Read-only access to the internal learner.
    #' If there are multiple learners, this is the first one.
    learner = function(rhs) {
      if (!missing(rhs)) stop("learner is read-only")
      private$.learners[[1L]]
    },
    #' @field learners (`list` of [mlr3::LearnerRegr])\cr
    #' Read-only access to the internal learners.
    #' The names of the list are the codomain target IDs.
    learners = function(rhs) {
      if (!missing(rhs)) stop("learners is read-only")
      private$.learners
    }
  ),

  private = list(
    .learners = NULL,

    # Make predictions using the learner(s)
    .predict = function(xdt) {
      # predict_newdata handles type conversions and missing columns for us
      as.data.table(lapply(private$.learners, function(l) {
        l$predict_newdata(xdt)$response
      }))
    },

    # Override private $.eval_many for list input
    .eval_many = function(xss, ...) {
      xdt <- rbindlist(xss, use.names = TRUE, fill = TRUE)
      private$.predict(xdt)
    },

    deep_clone = function(name, value) {
      switch(name,
        .learners = lapply(value, function(l) l$clone(deep = TRUE)),
        super$deep_clone(name, value)
      )
    }
  )
)
