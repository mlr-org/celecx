#' @title Syntactic Sugar Active Learning Distance Construction
#'
#' @include mlr_al_distances.R
#'
#' @description
#' This function complements [mlr_al_distances] with a construction helper in
#' the spirit of `mlr_sugar`.
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to [mlr_al_distances].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_get()] for details.
#'
#' @return [ALDistance].
#'
#' @export
#' @examples
#' clx_ald("standardize")
clx_ald <- function(.key, ...) {
  dictionary_sugar_get(mlr_al_distances, .key, ...)
}

#' @title Syntactic Sugar Active Learning Distances Construction
#'
#' @description
#' Retrieve multiple active-learning distances from [mlr_al_distances].
#'
#' @param .keys (`character()`)\cr
#'   Keys passed to [mlr_al_distances].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_mget()] for details.
#'
#' @return Named `list` of [ALDistance] objects.
#'
#' @export
#' @examples
#' clx_alds(c("standardize", "affine"))
clx_alds <- function(.keys, ...) {
  dictionary_sugar_mget(mlr_al_distances, .keys, ...)
}
