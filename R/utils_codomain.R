# Codomain Helpers for Active Learning
#
# Helper functions for working with codomains that include the "learn" tag
# for active learning, in addition to bbotk's standard "minimize"/"maximize" tags.
#
# As of bbotk >= 0.9.0, the [Codomain] class natively supports "learn" tags.
# These helper functions provide convenient accessors for filtering targets
# by their purpose (optimization vs learning).
#
# Tag Semantics:
# - **"minimize"**: Lower values are better; we seek x that minimizes f(x)
# - **"maximize"**: Higher values are better; we seek x that maximizes f(x)
# - **"learn"**: We want to learn f(x) accurately across the domain; no
#   optimization direction

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
#' @keywords internal
codomain_target_ids <- function(codomain) {
  codomain$ids(any_tags = c("minimize", "maximize", "learn"))
}

#' @title Determine Goals from Codomain
#'
#' @description
#' Lists all search goals based on codomain tags.
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   The codomain to inspect.
#'
#' @return `character`: subset of `"minimize"`, `"maximize"`, `"learn"`.
#' Future versions may add more tags.
#'
#' @keywords internal
codomain_goals <- function(codomain) {
  intersect(c("minimize", "maximize", "learn"), unlist(codomain$tags))
}
