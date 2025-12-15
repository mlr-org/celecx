#' @title Codomain Helpers for Active Learning
#'
#' @description
#' Helper functions for working with codomains that include the "learn" tag
#' for active learning, in addition to bbotk's standard "minimize"/"maximize" tags.
#'
#' @details
#' As of bbotk >= 0.9.0, the [Codomain] class natively supports "learn" tags.
#' These helper functions provide convenient accessors for filtering targets
#' by their purpose (optimization vs learning).
#'
#' @section Tag Semantics:
#' - **"minimize"**: Lower values are better; we seek x that minimizes f(x)
#' - **"maximize"**: Higher values are better; we seek x that maximizes f(x)
#' - **"learn"**: We want to learn f(x) accurately across the domain; no
#'   optimization direction
#'
#' @name codomain_helpers
NULL


#' @title Get Target IDs from Codomain
#'
#' @description
#' Returns IDs of parameters tagged as targets (minimize, maximize, or learn).
#'
#' @param codomain ([paradox::ParamSet] or [Codomain])\cr
#'   The codomain to inspect.
#'
#' @return Character vector of target parameter IDs.
#'
#' @details
#' This is a convenience wrapper around `codomain$target_ids` that works with
#' both ParamSet and Codomain objects. If you know you have a Codomain object,
#' you can use `codomain$target_ids` directly.
#'
#' @export
codomain_target_ids <- function(codomain) {
  # If it's a Codomain object, use the active binding
  if (inherits(codomain, "Codomain")) {
    return(codomain$target_ids)
  }
  # Otherwise treat as ParamSet and filter by tags
  codomain$ids(any_tags = c("minimize", "maximize", "learn"))
}


#' @title Get Optimization Target IDs
#'
#' @description
#' Returns IDs of parameters tagged for optimization (minimize or maximize),
#' excluding "learn" targets.
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to inspect.
#'
#' @return Character vector of optimization target IDs.
#'
#' @export
codomain_optimize_ids <- function(codomain) {
  codomain$ids(any_tags = c("minimize", "maximize"))
}


#' @title Get Learning Target IDs
#'
#' @description
#' Returns IDs of parameters tagged for learning (not optimization).
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to inspect.
#'
#' @return Character vector of learn-tagged target IDs.
#'
#' @export
codomain_learn_ids <- function(codomain) {
  codomain$ids(tags = "learn")
}


#' @title Check if Codomain Has Learning Targets
#'
#' @description
#' Tests whether a codomain contains any "learn" tagged parameters.
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to check.
#'
#' @return `logical(1)` TRUE if any parameter has "learn" tag.
#'
#' @export
codomain_has_learn <- function(codomain) {
  length(codomain_learn_ids(codomain)) > 0L
}


#' @title Check if Codomain Has Optimization Targets
#'
#' @description
#' Tests whether a codomain contains any minimize/maximize tagged parameters.
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to check.
#'
#' @return `logical(1)` TRUE if any parameter has minimize or maximize tag.
#'
#' @export
codomain_has_optimize <- function(codomain) {
  length(codomain_optimize_ids(codomain)) > 0L
}


#' @title Determine Goal from Codomain
#'
#' @description
#' Infers the search goal based on codomain tags.
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to inspect.
#'
#' @return `character(1)`: "optimize", "learn", or "both".
#'
#' @export
codomain_goal <- function(codomain) {
  has_opt <- codomain_has_optimize(codomain)
  has_learn <- codomain_has_learn(codomain)

  if (has_opt && has_learn) {
    "both"
  } else if (has_opt) {
    "optimize"
  } else if (has_learn) {
    "learn"
  } else {
    stop("Codomain has no targets (no minimize, maximize, or learn tags)")
  }
}
