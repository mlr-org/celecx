# Run `fn(learner)` on a shared, untrained base-learner object, optionally with
# a trained `state` injected and the prediction configuration (`predict_type`,
# `quantiles`, `quantile_response`) overridden. Afterwards the learner's state
# is cleared and its configuration restored, so the object stays untrained
# between uses (the PipeOpLearnerCV state-juggling pattern). This serves both
# training (`fn` trains the learner and returns the harvested `$state`) and
# prediction (`fn` queries the injected model), letting wrapper learners store
# plain learner states in their model instead of learner objects.
with_learner_state <- function(learner, fn, state = NULL, predict_type = NULL, quantiles = NULL, quantile_response = NULL) {
  prev_predict_type <- learner$predict_type
  prev_quantiles <- learner$quantiles
  prev_quantile_response <- learner$quantile_response
  on.exit({
    learner$state <- NULL
    learner$predict_type <- prev_predict_type
    # the quantiles / quantile_response setters reject NULL, so a previously
    # unset value cannot be un-set again and keeps the value assigned below
    if (!is.null(quantiles) && !is.null(prev_quantiles)) learner$quantiles <- prev_quantiles
    if (!is.null(quantile_response) && !is.null(prev_quantile_response)) learner$quantile_response <- prev_quantile_response
  })
  if (!is.null(predict_type)) learner$predict_type <- predict_type
  # order matters: the quantile_response setter unions the response probability
  # into $quantiles
  if (!is.null(quantiles)) learner$quantiles <- quantiles
  if (!is.null(quantile_response)) learner$quantile_response <- quantile_response
  if (!is.null(state)) learner$state <- state
  fn(learner)
}
