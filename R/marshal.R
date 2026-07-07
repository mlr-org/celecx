#' @title Marshal Model for LearnerRegrBootstrapSE State
#'
#' @description
#' Marshals the model state of [LearnerRegrBootstrapSE], preparing it for
#' serialization. This marshals the model inside each stored bootstrap member
#' state (via [mlr3]'s `marshal_model.learner_state`).
#'
#' @param model (`learner_regr_bootstrap_se_state`)\cr
#'   The model to marshal.
#' @param inplace (`logical(1)`)\cr
#'   Whether to marshal in place.
#' @param ... (any)\cr
#'   Additional arguments passed to [mlr3::marshal_model()].
#'
#' @return Either the original model (if no marshaling was needed) or a
#'   marshaled version with class `learner_regr_bootstrap_se_state_marshaled`.
#'
#' @keywords internal
#' @export
marshal_model.learner_regr_bootstrap_se_state = function(model, inplace = FALSE, ...) {
  model$bootstrap_states = map(model$bootstrap_states, marshal_model, inplace = inplace, ...)

  # Only wrap in marshaled class if any member state was actually marshaled
  if (some(model$bootstrap_states, is_marshaled_model)) {
    model = structure(
      list(marshaled = model, packages = "celecx"),
      class = c("learner_regr_bootstrap_se_state_marshaled", "marshaled")
    )
  }
  model
}

#' @title Unmarshal Model for LearnerRegrBootstrapSE State
#'
#' @description
#' Unmarshals the model state of [LearnerRegrBootstrapSE], restoring it after
#' deserialization.
#'
#' @param model (`learner_regr_bootstrap_se_state_marshaled`)\cr
#'   The marshaled model to unmarshal.
#' @param inplace (`logical(1)`)\cr
#'   Whether to unmarshal in place.
#' @param ... (any)\cr
#'   Additional arguments passed to [mlr3::unmarshal_model()].
#'
#' @return The unmarshaled model state.
#'
#' @keywords internal
#' @export
unmarshal_model.learner_regr_bootstrap_se_state_marshaled = function(model, inplace = FALSE, ...) {
  state_marshaled = model$marshaled
  state_marshaled$bootstrap_states = map(state_marshaled$bootstrap_states,
    unmarshal_model, inplace = inplace, ...)
  state_marshaled
}

#' @title Marshal Model for LearnerRegrQuantileSE State
#'
#' @description
#' Marshals the model state of [LearnerRegrQuantileSE], preparing it for
#' serialization. This marshals the model inside the stored base learner state
#' (via [mlr3]'s `marshal_model.learner_state`).
#'
#' @param model (`learner_regr_quantile_se_state`)\cr
#'   The model to marshal.
#' @param inplace (`logical(1)`)\cr
#'   Whether to marshal in place.
#' @param ... (any)\cr
#'   Additional arguments passed to [mlr3::marshal_model()].
#'
#' @return Either the original model (if no marshaling was needed) or a
#'   marshaled version with class `learner_regr_quantile_se_state_marshaled`.
#'
#' @keywords internal
#' @export
marshal_model.learner_regr_quantile_se_state = function(model, inplace = FALSE, ...) {
  model$base_state = marshal_model(model$base_state, inplace = inplace, ...)

  # Only wrap in marshaled class if the base state was actually marshaled
  if (is_marshaled_model(model$base_state)) {
    model = structure(
      list(marshaled = model, packages = "celecx"),
      class = c("learner_regr_quantile_se_state_marshaled", "marshaled")
    )
  }
  model
}

#' @title Unmarshal Model for LearnerRegrQuantileSE State
#'
#' @description
#' Unmarshals the model state of [LearnerRegrQuantileSE], restoring it after
#' deserialization.
#'
#' @param model (`learner_regr_quantile_se_state_marshaled`)\cr
#'   The marshaled model to unmarshal.
#' @param inplace (`logical(1)`)\cr
#'   Whether to unmarshal in place.
#' @param ... (any)\cr
#'   Additional arguments passed to [mlr3::unmarshal_model()].
#'
#' @return The unmarshaled model state.
#'
#' @keywords internal
#' @export
unmarshal_model.learner_regr_quantile_se_state_marshaled = function(model, inplace = FALSE, ...) {
  state_marshaled = model$marshaled
  state_marshaled$base_state = unmarshal_model(state_marshaled$base_state,
    inplace = inplace, ...)
  state_marshaled
}
