prepare_pool_al_data <- function(pool_dt, domain_ids) {
  domain_dt <- pool_dt[, domain_ids, with = FALSE]
  representative_idx <- which(!duplicated(domain_dt))

  list(
    pool_dt = pool_dt[representative_idx],
    representative_idx = representative_idx
  )
}

remaining_search_capacity <- function(inst) {
  status <- tryCatch(
    inst$terminator$status(inst$archive),
    error = function(e) NULL
  )

  if (is.null(status)) {
    return(Inf)
  }

  if (!all(c("current_steps", "max_steps") %in% names(status))) {
    return(Inf)
  }

  current_steps <- as.integer(status[["current_steps"]])
  max_steps <- as.integer(status[["max_steps"]])
  if (is.na(current_steps) || is.na(max_steps) || !is.finite(max_steps)) {
    return(Inf)
  }

  max(0L, max_steps - current_steps)
}

clamp_batch_size <- function(batch_size, n_available, remaining_capacity = Inf) {
  if (is.finite(remaining_capacity)) {
    min(batch_size, n_available, remaining_capacity)
  } else {
    min(batch_size, n_available)
  }
}

#' Get the first target column ID from a codomain.
#'
#' Prefers "learn"-tagged targets, then falls back to "minimize"/"maximize".
#'
#' @param codomain ([paradox::ParamSet])\cr
#'   A codomain.
#' @return `character(1)` first target column name.
#' @keywords internal
get_first_target_id <- function(codomain) {
  learn_ids <- codomain_learn_ids(codomain)
  if (length(learn_ids) > 0L) return(learn_ids[1L])
  opt_ids <- codomain_optimize_ids(codomain)
  if (length(opt_ids) > 0L) return(opt_ids[1L])
  stopf("Codomain has no target columns")
}
