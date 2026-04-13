#' @title Dictionary of Active Learning Distances
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [ALDistance].
#' Each distance has an associated help page, see `mlr_al_distances_[id]`.
#'
#' For a more convenient way to retrieve and construct a distance, see
#' [clx_ald()] and [clx_alds()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @family ALDistance
#' @seealso
#' Sugar functions: [clx_ald()], [clx_alds()]
#' @export
#' @examples
#' as.data.table(mlr_al_distances)
#' clx_ald("standardize")
mlr_al_distances <- R6Class("DictionaryALDistance",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryALDistance <- function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    distance <- x$get(key)
    insert_named(
      list(
        key = key,
        label = distance$label,
        man = distance$man
      ),
      if (objects) list(object = list(distance))
    )
  }, .fill = TRUE), "key")[]
}
