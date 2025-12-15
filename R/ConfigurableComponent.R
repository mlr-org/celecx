#' @title ConfigurableComponent
#' @description
#' Base class for objects that unifies configuration of state.
#' `ConfigurableComponent`-objects should always have active bindings that reflect the arguments given to their
#' constructor.
#' They also have a `$param_set` field containing a `paradox::ParamSet`, which are used for more fine-grained
#' configuration of the object.
#' Things that somehow determine how an object can be used (e.g. what kinds of things it is compatible with),
#' particularly things that can not be changed after constructions should likely be construction arguments;
#' others should likely be in `$param_set`.
#' @export
#' @family ConfigurableComponent
#' @family algorithms
#' @family ConfigurableComponent

ConfigurableComponent <- R6Class("ConfigurableComponent",
  public = list(
    #' @description
    #' Initialize a ConfigurableComponent.
    #' @param id (`character(1)`)
    #'   Identifier of the object. If left as `NULL`, the object does not get an ID.
    #' @param param_set ([paradox::ParamSet] | `NULL`)
    #'   Set of hyperparameters / configuration parameters.
    #' @param additional_configuration (`character(1)`)
    #'   Additional configuration settings.
    #'   Anything passed here must be a field or active binding of the object that is
    #'   not passed as an initialization argument to the (top-level) constructor.
    #' @param additional_phash_input (`character()`)
    #'   Names of fields (in `self` or `private`) whose values should influence the persistent hash (`phash`).
    #'   Useful for subclasses that expose additional configuration state outside of constructor arguments
    #'   or the parameter set.
    #'   Note that entries of `additional_configuration` are *not* automatically included (as they may be expensive to
    #'   compute).
    #'   `additional_phash_input` should reference the data from which `additional_configuration` fields are computed.
    #' @details
    #'   The state of the object should be completely determined by:
    #'    - arguments passed for initialization, all of which must be accessible
    #'      as fields or active bindings of the object.
    #'    - param_set configuration parameteres
    #'    - additional fields / active bindings, which must be listed as `additional_configuration`.
    #'    - there may be other active bindings, but these should not convey any relevant state.
    initialize = function(id = NULL, param_set = ps(), additional_configuration = character(0),
      additional_phash_input = character(0)
    ) {
      private$.id <- id
      private$.has_id <- !is.null(id)
      private$.additional_phash_input <- assert_character(additional_phash_input, any.missing = FALSE,
        min.chars = 1L, unique = TRUE)
      valid_phash_inputs <- c(names(self), names(private))
      assert_subset(additional_phash_input, valid_phash_inputs)

      # ParamSet is optional to keep this base class generic across components
      if (is.null(param_set)) {
        private$.param_set <- NULL
        private$.param_set_source <- NULL
      } else if (inherits(param_set, "ParamSet")) {
        private$.param_set <- paradox::assert_param_set(param_set)
        private$.param_set_source <- NULL
      } else {
        lapply(param_set, function(x) paradox::assert_param_set(eval(x, envir = topenv())))
        private$.param_set_source <- param_set
      }

      assert_character(additional_configuration, any.missing = FALSE, min.chars = 1L, unique = TRUE)
      assert_subset(additional_configuration, names(self))
      assert_disjunct(additional_configuration, c(
        # additional configuration can not be:
        # (1) a parameter, (2) a construction argument (these are captured automatically), (3) a standard field
        if (!is.null(self$param_set)) self$param_set$ids(),
        names(formals(self$initialize)),
        "id", "param_set", "format", "print", "configure", "hash", "phash"
      ))
      private$.additional_configuration <- additional_configuration
    },

    #' @description
    #' Helper for print outputs.
    #' @param ... (ignored).
    format = function(...) {
      if (private$.has_id) {
        sprintf("<%s:%s>", class(self)[1L], self$id)
      } else {
        sprintf("<%s>", class(self)[1L])
      }
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function(...) {
      if (private$.has_id) {
        cat(sprintf("<%s:%s>", class(self)[1L], self$id), "\n")
      } else {
        cat(sprintf("<%s>", class(self)[1L]), "\n")
      }
    },

    #' @description
    #' Set parameter values and fields in one step.
    #' @param ... (named `any`)
    #' @param .values (named `list()`)
    configure = function(..., .values = list()) {
      dots <- list(...)
      assert_list(dots, names = "unique")
      assert_list(.values, names = "unique")
      assert_disjunct(names(dots), names(.values))
      .values[names(dots)] <- dots
      if (!length(.values)) return(invisible(self))

      param_set <- self$param_set
      param_ids <- if (is.null(param_set)) character() else param_set$ids()

      available_names <- setdiff(names(self), ".__enclos_env__")
      param_assignments <- list()
      field_assignments <- list()
      invalid <- character()

      for (name in names(.values)) {
        value <- .values[[name]]
        if (name %in% param_ids) {
          param_assignments[[name]] <- value
        } else if (exists(name, envir = self, inherits = FALSE)) {
          field_assignments[[name]] <- value
        } else {
          invalid <- c(invalid, name)
        }
      }

      if (length(invalid)) {
        nn <- invalid[[1L]]
        stop(sprintf("Cannot set argument '%s' for '%s' (not a parameter, not a field).%s",
          nn, class(self)[1L], mlr3misc::did_you_mean(nn, available_names)))
      }

      if (length(param_assignments) && !is.null(param_set)) {
        param_set$values[names(param_assignments)] <- param_assignments
      }

      if (length(field_assignments)) {
        for (name in names(field_assignments)) {
          self[[name]] <- field_assignments[[name]]
        }
      }

      invisible(self)
    }
  ),

  active = list(
    #' @field id (`character(1)`)
    #' Identifier of the object.
    #' Used in tables, plot and text output.
    id = function(rhs) {
      if (!missing(rhs)) {
        if (!private$.has_id) {
          stop("id is read-only")
        }
        private$.id <- assert_string(rhs, min.chars = 1L)
      }
      private$.id
    },

    #' @field param_set ([paradox::ParamSet] | `NULL`)
    #' Set of hyperparameters.
    param_set = function(val) {
      if (is.null(private$.param_set) && !is.null(private$.param_set_source)) {
        sourcelist <- lapply(private$.param_set_source, function(x) eval(x, envir = topenv()))  # nolint
        if (length(sourcelist) > 1) {
          private$.param_set <- paradox::ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set <- sourcelist[[1]]
        }
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

    #' @field hash (`character(1)`)
    #' Stable hash that includes id, parameter values (if present) and additional configuration settings (from
    #' construction or class fields) but not state.
    #' Makes use of the `private$.additional_phash_input()` function to collect additional information, which must
    #' therefore be implemented by subclasses.
    hash = function(rhs) {
      if (!missing(rhs)) stop("hash is read-only")
      param_values <- list()
      if (!is.null(self$param_set)) {
        param_values <- self$param_set$values
      }
      hash_list(list(param_values, self$phash))
    },

    #' @field phash (`character(1)`)
    #' Hash that includes id and additional configuration settings (from construction or class fields) but not
    #' parameter values and no state.
    #' Makes use of the `private$.additional_phash_input()` function to collect additional information, which must
    #' therefore be implemented by subclasses.
    phash = function(rhs) {
      if (!missing(rhs)) stop("phash is read-only")
      inputs <- list(self$id, class(self))
      fields <- private$.additional_phash_input
      if (length(fields)) {
        inputs <- c(inputs, lapply(fields, private$.hash_item))
      }
      hash_list(inputs)
    }
  ),

  private = list(
    .has_id = NULL,
    .id = NULL,
    .param_set = NULL,
    .param_set_source = NULL,
    .additional_configuration = NULL,
    .additional_phash_input = NULL,

    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        private$.param_set <- NULL  # required to keep clone identical to original, otherwise tests get really ugly
        if (name == ".param_set_source") {
          value <- lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x
          })
        }
      }
      if (is.environment(value) && ".__enclos_env__" %in% names(value) && "clone" %in% names(value)) {
        return(value$clone(deep = TRUE))
      }
      value
    },

    .hash_item = function(id) {
      if (exists(id, envir = self, inherits = FALSE)) {
        return(self[[id]])
      }
      if (exists(id, envir = private, inherits = FALSE)) {
        return(private[[id]])
      }
      stop(sprintf("Cannot hash unknown field '%s'.", id))
    }
  )
)

hash_list <- function(x) {
  assert_list(x)
  digest::digest(lapply(x, hash_transform), algo = "xxhash64")
}

#' @export
hash_transform <- function(x) {
  UseMethod("hash_transform")
}

#' @export
hash_transform.default <- function(x) {
  x
}

#' @export
hash_transform.ConfigurableComponent <- function(x) {
  x$phash
}

#' @export
hash_transform.list <- function(x) {
  hash_list(x)
}

#' @export
hash_transform.R6 <- function(x) {
  if (!"phash" %in% names(x)) {
    stop("Cannot hash R6 object without phash field.")
  }
  x$phash
}
