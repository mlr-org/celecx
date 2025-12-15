#' @title Validate Learner Against Domain and Codomain
#'
#' @description
#' Validates that a fitted learner is compatible with a given domain and codomain.
#' Checks that the learner is fitted, that domain parameters match trained features,
#' and validates type compatibility including factor level constraints.
#'
#' @param learner ([mlr3::LearnerRegr])\cr
#'   A fitted regression learner.
#' @param domain ([paradox::ParamSet])\cr
#'   Parameter set describing the input space.
#' @param codomain ([paradox::ParamSet])\cr
#'   Parameter set describing the output space.
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
assert_learner_domain_codomain <- function(learner, domain, codomain) {
  assert_r6(learner, "Learner")

  # Check learner is a regression learner
  if (!inherits(learner, "LearnerRegr")) {
    stop("Learner must be a LearnerRegr")
  }

  # Check learner is fitted
  if (is.null(learner$state) || is.null(learner$model)) {
    stop("Learner has not been trained yet")
  }

  # Get train_task info
  train_task <- learner$state$train_task
  if (is.null(train_task)) {
    stop("Learner state does not contain train_task information")
  }

  # Basic domain/codomain structure validation
  ids <- assert_domain_codomain(domain, codomain)
  domain_ids <- ids$domain_ids

  # Get the features the learner was trained on
  trained_feature_names <- learner$state$feature_names
  if (is.null(trained_feature_names)) {
    trained_feature_names <- train_task$feature_names
  }

  # Check that all domain parameters were features in the training task
  missing_features <- setdiff(domain_ids, trained_feature_names)
  if (length(missing_features)) {
    stopf("Domain parameters not found in learner's training features: %s",
      str_collapse(missing_features))
  }

  # Check that domain covers all trained features (learner needs all features to predict)
  missing_domain <- setdiff(trained_feature_names, domain_ids)
  if (length(missing_domain)) {
    stopf("Domain must include all learner training features. Missing: %s",
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
      stopf("Feature '%s' not found in training task col_info", param_id)
    }
    train_type <- train_info$type

    # For learner, ParamFct must match exactly "factor" (no character conversion)
    if (param_class == "ParamFct" && train_type != "factor") {
      stopf("Domain parameter '%s' is ParamFct but training feature is '%s' (not factor)",
        param_id, train_type)
    } else if (param_class != "ParamFct") {
      assert_param_type_compatible(param_id, param_class, train_type, "training feature")
    }

    # For ParamFct: check that domain levels are a SUBSET of trained levels
    if (param_class == "ParamFct") {
      train_levels <- train_info$levels[[1L]]
      domain_levels <- domain$levels[[param_id]]
      extra_levels <- setdiff(domain_levels, train_levels)
      if (length(extra_levels)) {
        stopf("Domain parameter '%s' has levels not in training data: %s (trained levels: %s)",
          param_id, str_collapse(extra_levels), str_collapse(train_levels))
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
#' @export
ObjectiveLearner <- R6Class("ObjectiveLearner",
  inherit = Objective,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::LearnerRegr])\cr
    #'   A fitted regression learner. Must have been trained via `$train()`
    #'   before creating the objective.
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
      assert_r6(learner, "LearnerRegr")
      assert_param_set(domain)
      assert_param_set(codomain)
      assert_string(id)
      assert_flag(check_values)

      # Validate learner against domain/codomain
      assert_learner_domain_codomain(learner, domain, codomain)

      # Store the learner (clone it to avoid modifications affecting us)
      private$.learner <- learner$clone(deep = TRUE)

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
      if (self$check_values) self$domain$assert_dt(xdt)
      private$.predict(xdt)
    }
  ),

  active = list(
    #' @field learner ([mlr3::LearnerRegr])\cr
    #' Read-only access to the internal learner.
    learner = function(rhs) {
      if (!missing(rhs)) stop("learner is read-only")
      private$.learner
    },

    #' @field train_task ([mlr3::TaskRegr])\cr
    #' Returns the task the learner was trained on (without data backend).
    train_task = function(rhs) {
      if (!missing(rhs)) stop("train_task is read-only")
      private$.learner$state$train_task
    }
  ),

  private = list(
    .learner = NULL,

    # Make predictions using the learner
    .predict = function(xdt) {
      codomain_ids <- self$codomain$ids()
      codomain_target_ids <- codomain_optimize_ids(self$codomain)

      # Use predict_newdata to handle type conversions and missing columns
      prediction <- private$.learner$predict_newdata(xdt)

      # Get the response from the prediction
      response <- prediction$response

      # Build result data.table with codomain structure
      # The first target gets the prediction, other columns need handling
      result <- data.table(response)
      setnames(result, codomain_target_ids[1L])

      # If there are additional codomain columns, fill with NA
      # (multi-objective scenarios where learner only predicts one target)
      for (cid in setdiff(codomain_ids, codomain_target_ids[1L])) {
        result[[cid]] <- NA_real_
      }

      result[, codomain_ids, with = FALSE]
    },

    # Override private $.eval_many for list input
    .eval_many = function(xss, ...) {
      xdt <- rbindlist(xss, use.names = TRUE, fill = TRUE)
      private$.predict(xdt)
    },

    deep_clone = function(name, value) {
      if (name == ".learner") {
        return(value$clone(deep = TRUE))
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    }
  )
)
