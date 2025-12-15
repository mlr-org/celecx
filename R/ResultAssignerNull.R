#' @title Null Result Assigner
#'
#' @description
#' Result assigner that intentionally does nothing.
#'
#' This is useful for search runs where no single "best" point exists, e.g.
#' active learning tasks with codomain targets tagged `"learn"`. In these cases,
#' calling `archive$best()` is undefined and will error, so using the default
#' result assigner from mlr3mbo is not appropriate.
#'
#' @export
ResultAssignerNull <- R6Class("ResultAssignerNull",
  inherit = mlr3mbo::ResultAssigner,

  public = list(

    #' @description
    #' Creates a new ResultAssignerNull.
    initialize = function() {
      super$initialize(label = "Null", man = "celecx::ResultAssignerNull")
    },

    #' @description
    #' Assigns the result to the instance.
    #'
    #' This implementation does nothing (leaves `instance$result` untouched).
    #'
    #' @param instance ([bbotk::OptimInstance])\cr
    #'   The instance the result would be assigned to.
    assign_result = function(instance) {
      invisible(NULL)
    }
  ),

  active = list(

    #' @field packages (`character()`)\cr
    #' Required packages (none).
    packages = function(rhs) {
      assert_ro_binding(rhs)
      character(0)
    }
  )
)

