#' @title Distance-Based Space Sampler
#'
#' @include SpaceSampler.R
#' @include ALDistance.R
#'
#' @description
#' Abstract base class for pool samplers that are driven by an [ALDistance].
#'
#' The wrapped distance object is stored directly and its [paradox::ParamSet] is
#' forwarded as the sampler parameter set.
#'
#' @export
#' @family SpaceSampler
SpaceSamplerDistance <- R6Class("SpaceSamplerDistance",
  inherit = SpaceSampler,

  public = list(

    #' @description
    #' Creates a new distance-based space sampler.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the sampler.
    #' @param distance ([ALDistance])\cr
    #'   Distance object used by the sampler.
    initialize = function(id,
        distance,
        deterministic,
        packages = character(0),
        label = NA_character_,
        man = NA_character_) {

      private$.distance <- assert_r6(distance, "ALDistance")

      super$initialize(
        id = paste0(id, ".", distance$id),
        deterministic = deterministic,
        packages = unique(c(packages, private$.distance$packages)),
        param_set = alist(private$.distance$param_set),
        label = label,
        man = man,
        additional_phash_input = ".distance"
      )
    }
  ),

  active = list(

    #' @field distance ([ALDistance])
    #'   Wrapped distance object.
    distance = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.distance)) {
        stop("distance is read-only.")
      }
      private$.distance
    }
  ),

  private = list(
    .distance = NULL
  )
)

space_sampler_distance_fit_data <- function(pool, known_pool) {
  list(
    xdt = rbind(pool, known_pool, use.names = TRUE),
    pool_idx = seq_len(nrow(pool)),
    known_idx = nrow(pool) + seq_len(NROW(known_pool))
  )
}

# get distance matrix, made symmetric and zero on diagonal
space_sampler_self_distance_matrix <- function(distance, xdt, search_space) {
  on.exit(distance$clear())
  distance$fit_pool(xdt, search_space)
  distance$set_reference_points(xdt)

  distance_matrix <- distance$distances(xdt)
  assert_matrix(
    distance_matrix,
    mode = "numeric",
    nrows = nrow(xdt),
    ncols = nrow(xdt)
  )

  distance_matrix <- (distance_matrix + t(distance_matrix)) / 2
  diag(distance_matrix) <- 0
  distance_matrix
}

space_sampler_cluster_representatives <- function(cluster_distances) {
  n_clusters <- ncol(cluster_distances)
  selected <- integer(n_clusters)
  available <- rep(TRUE, nrow(cluster_distances))

  for (j in seq_len(n_clusters)) {
    candidate_distances <- cluster_distances[, j]
    candidate_distances[!available] <- Inf
    best <- which.min(candidate_distances)
    selected[j] <- best
    available[best] <- FALSE
  }

  selected
}
