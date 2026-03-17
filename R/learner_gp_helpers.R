#' @title Internal Helpers for GP Learners
#'
#' @description
#' Utility helpers shared by the GP learner wrappers.
#'
#' @name learner_gp_helpers
#' @keywords internal
NULL

task_feature_matrix <- function(task) {
  as.matrix(task$data(cols = task$feature_names))
}

prediction_se_from_variance <- function(variance) {
  sqrt(pmax(as.numeric(variance), 0))
}

# Rename prefixed param values: strips `prefix` from names in `pv` according
# to `rename_map` (named character vector: prefixed name -> target name).
# Entries not in `rename_map` are kept as-is.
rename_prefixed_params <- function(pv, rename_map) {
  args <- remove_named(pv, names(rename_map))
  for (from in names(rename_map)) {
    if (!is.null(pv[[from]])) args[[rename_map[[from]]]] <- pv[[from]]
  }
  args
}

tgp_prediction_variance <- function(prediction, n) {
  variance <- prediction$ZZ.s2
  if (is.null(variance)) {
    variance <- prediction$ZZ.ks2
  }

  if (is.null(variance)) {
    stopf("tgp prediction did not return predictive variances.")
  }

  if (is.matrix(variance)) {
    return(diag(variance))
  }

  variance <- as.numeric(variance)

  if (length(variance) == n) {
    return(variance)
  }

  if (length(variance) == n^2) {
    return(diag(matrix(variance, nrow = n)))
  }

  if (!is.null(prediction$ZZ.ks2) && length(prediction$ZZ.ks2) == n) {
    return(as.numeric(prediction$ZZ.ks2))
  }

  stopf(
    "tgp prediction returned %i variance values for %i observations.",
    length(variance), n
  )
}

deepgp_prediction_variance <- function(prediction) {
  if (!is.null(prediction$s2_all) && !is.null(prediction$mean_all)) {
    s2_all <- as.matrix(prediction$s2_all)
    mean_all <- as.matrix(prediction$mean_all)

    s2_mean <- apply(s2_all, 2L, function(x) {
      if (all(is.na(x))) {
        NA_real_
      } else {
        mean(x, na.rm = TRUE)
      }
    })

    mean_var <- apply(mean_all, 2L, function(x) {
      x <- x[is.finite(x)]
      if (length(x) <= 1L) {
        0
      } else {
        stats::var(x)
      }
    })

    return(s2_mean + mean_var)
  }

  if (!is.null(prediction$s2)) {
    return(as.numeric(prediction$s2))
  }

  if (!is.null(prediction$Sigma)) {
    return(diag(prediction$Sigma))
  }

  stopf("deepgp prediction did not return predictive variances.")
}
