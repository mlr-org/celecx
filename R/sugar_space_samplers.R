#' @title Syntactic Sugar Space Sampler Construction
#'
#' @include mlr_space_samplers.R
#'
#' @description
#' This function complements [mlr_space_samplers] with a construction helper in
#' the spirit of `mlr_sugar`.
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to [mlr_space_samplers].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_get()] for details.
#'
#' @return [SpaceSampler].
#'
#' @export
#' @examples
#' clx_sps("uniform")
clx_sps <- function(.key, ...) {
  dictionary_sugar_get(mlr_space_samplers, .key, ...)
}

#' @title Syntactic Sugar Space Samplers Construction
#'
#' @description
#' Retrieve multiple space samplers from [mlr_space_samplers].
#'
#' @param .keys (`character()`)\cr
#'   Keys passed to [mlr_space_samplers].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_mget()] for details.
#'
#' @return Named `list` of [SpaceSampler] objects.
#'
#' @export
#' @examples
#' clx_spss(c("uniform", "gsx"))
clx_spss <- function(.keys, ...) {
  dictionary_sugar_mget(mlr_space_samplers, .keys, ...)
}
