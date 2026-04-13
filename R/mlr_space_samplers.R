#' @title Dictionary of Space Samplers
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [SpaceSampler].
#' Each sampler has an associated help page, see `mlr_space_samplers_[id]`.
#'
#' For a more convenient way to retrieve and construct a sampler, see
#' [clx_sps()] and [clx_spss()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family SpaceSampler
#' @seealso
#' Sugar functions: [clx_sps()], [clx_spss()]
#' @export
#' @examples
#' as.data.table(mlr_space_samplers)
#' clx_sps("uniform")
mlr_space_samplers <- R6Class("DictionarySpaceSampler",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionarySpaceSampler <- function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    sampler <- x$get(key)
    insert_named(
      list(
        key = key,
        label = sampler$label,
        man = sampler$man,
        deterministic = sampler$deterministic
      ),
      if (objects) list(object = list(sampler))
    )
  }, .fill = TRUE), "key")[]
}
