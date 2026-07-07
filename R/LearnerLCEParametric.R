#' @include LearnerLCE.R
#' @include utils_lce.R
NULL

# Abstract base class for the nonlinear parametric LCE learners (exponential,
# power-law, logistic). Owns the shared train/predict skeleton: per-batch
# aggregation on the link scale, box-constrained least-squares fitting via
# lce_fit_parametric(), the Gaussian-on-link prediction assembly, and the
# common rate_lower / maxit parameters (merged into the subclass ParamSet).
#
# Subclasses provide the curve family through private hooks:
#   .coef_names       character(); coefficient names, defines the parameter count
#   .curve(par, b)    numeric; curve values at batches `b` (positional `par`)
#   .grad(par, b)     matrix; row-wise gradient of the curve wrt `par`
#   .par_init(pb, value_vec, pv)  numeric; initial values (honoring *_init params)
#   .par_lower(pv)    numeric; lower box constraints
#   .check_batches(b) optional batch-domain validation (e.g. b > 0), default no-op
#
# Not registered in mlr_learners and not exported; it is an implementation
# detail of the concrete parametric learners.
LearnerLCEParametric <- R6Class("LearnerLCEParametric",
  inherit = LearnerLCE,
  public = list(
    initialize = function(id, param_set, label, man) {
      param_set <- c(param_set, ps(
        rate_lower = p_dbl(lower = 0, init = 1e-6, tags = c("train", "required")),
        maxit = p_int(lower = 1L, init = 500L, tags = c("train", "required"))
      ))

      super$initialize(
        id = id,
        param_set = param_set,
        predict_types = c("response", "se", "quantiles", "target_reached"),
        feature_types = "integer",
        label = label,
        man = man
      )
    }
  ),

  private = list(
    .coef_names = NULL,

    .curve = function(par, b) {
      stop("Abstract.")
    },

    .grad = function(par, b) {
      stop("Abstract.")
    },

    .par_init = function(pb, value_vec, pv) {
      stop("Abstract.")
    },

    .par_lower = function(pv) {
      stop("Abstract.")
    },

    .check_batches = function(b) {
      invisible(NULL)
    },

    .train = function(task) {
      pv <- self$param_set$get_values(tags = "train")
      link <- lce_link(task$link)
      pb <- lce_train_per_batch(task, link)
      if (pb$n_batches < 2L) {
        stopf("Need at least two distinct batches to fit '%s'", self$id)
      }
      private$.check_batches(pb$batch)

      batch_vec <- pb$batch
      value_vec <- link$transform(pb$value)
      par_init <- private$.par_init(pb, value_vec, pv)

      fit <- lce_fit_parametric(
        par_init = par_init,
        lower = private$.par_lower(pv),
        upper = rep(Inf, length(par_init)),
        fn = function(par) sum((value_vec - private$.curve(par, batch_vec))^2),
        maxit = pv$maxit,
        n_batches = pb$n_batches
      )

      list(
        coefficients = set_names(fit$coefficients, private$.coef_names),
        sigma2 = fit$sigma2,
        Sigma = fit$Sigma,
        n_batches = pb$n_batches,
        convergence = fit$convergence,
        link = task$link,
        minimize = lce_model_minimize(task),
        last_train_batch = max(as.numeric(task$batch_nrs))
      )
    },

    .predict = function(task) {
      m <- self$model
      link <- lce_link(m$link)
      bb <- lce_predict_batches(task)
      private$.check_batches(bb)
      mu <- private$.curve(m$coefficients, bb)
      if (self$predict_type == "response") {
        return(list(response = link$inverse(mu)))
      }
      se <- lce_se_components(private$.grad(m$coefficients, bb), m$Sigma, m$sigma2)
      pv <- self$param_set$get_values(tags = "predict")
      lce_distr_predict(self$predict_type, mu, se$se_total, se$se_epi, link,
        probs = pv$quantile_probs %??% lce_default_probs,
        reach_target = pv$reach_target, minimize = m$minimize)
    }
  )
)
