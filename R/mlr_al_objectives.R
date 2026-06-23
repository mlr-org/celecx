#' @include ObjectivePoolRFun.R
NULL

#' @title Dictionary of Active-Learning Benchmark Objectives
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A [mlr3misc::Dictionary] storing constructors for pool-backed benchmark
#' objectives. The registered objectives cover the synthetic and real-world
#' regression benchmarks used by Bemporad's inverse-distance active-learning
#' paper.
#'
#' Objective data that require downloads or expensive preprocessing are cached
#' below `tools::R_user_dir("celecx", "cache")`. Pass `refresh = TRUE` to the
#' sugar constructor to rebuild a cached benchmark.
#'
#' @section Construction:
#' ```
#' alobj("scalar")
#' alobj("mpqp")
#' alobj("auto-mpg")
#' ```
#'
#' @section Duplicate feature rows:
#' The package's pool-backed objectives require a unique mapping from feature
#' values to responses. Some paper datasets contain repeated feature rows under
#' the paper's feature selection. Real-world benchmark constructors therefore
#' use a tiny deterministic jitter on duplicate rows by default, preserving the
#' paper row counts and feature counts. Use `duplicate_handling = "error"` to
#' fail instead.
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Dictionary
#' @seealso
#' Sugar functions: [alobj()], [alobjs()]
#' @export
#' @examples
#' as.data.table(mlr_al_objectives)
#' alobj("scalar")
mlr_al_objectives <- R6Class("DictionaryALObjective",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryALObjective <- function(x, ..., objects = FALSE) {
  assert_flag(objects)

  dt <- copy(al_objective_registry)
  dt <- dt[key %in% x$keys()]
  setkeyv(dt, "key")
  if (objects) {
    dt[, object := lapply(key, x$get)]
  }
  dt[]
}
registerS3method(
  "as.data.table",
  "DictionaryALObjective",
  as.data.table.DictionaryALObjective,
  envir = asNamespace("data.table")
)

#' @title Syntactic Sugar Active-Learning Benchmark Objective Construction
#'
#' @include mlr_al_objectives.R
#'
#' @description
#' Retrieves an objective from [mlr_al_objectives].
#'
#' @param .key (`character(1)`)\cr
#'   Key passed to [mlr_al_objectives].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_get()] for details.
#'
#' @return [bbotk::Objective].
#'
#' @export
#' @examples
#' alobj("scalar")
alobj <- function(.key, ...) {
  dictionary_sugar_get(mlr_al_objectives, .key, ...)
}

#' @title Syntactic Sugar Active-Learning Benchmark Objectives Construction
#'
#' @description
#' Retrieves multiple objectives from [mlr_al_objectives].
#'
#' @param .keys (`character()`)\cr
#'   Keys passed to [mlr_al_objectives].
#' @param ... (named `list()`)\cr
#'   Named arguments passed to the constructor, to the parameter set, or to
#'   public fields. See [mlr3misc::dictionary_sugar_mget()] for details.
#'
#' @return Named `list` of [bbotk::Objective] objects.
#'
#' @export
#' @examples
#' alobjs(c("scalar", "bell"))
alobjs <- function(.keys, ...) {
  dictionary_sugar_mget(mlr_al_objectives, .keys, ...)
}

al_objective_registry <- as.data.table(list(
  objective_key = c(
    "scalar",
    "scalar-noisy",
    "mpqp",
    "bell",
    "bell-constrained",
    "bell-feasible",
    "concrete-slump",
    "auto-mpg",
    "winequality-white",
    "yacht",
    "qsar-aquatic-toxicity",
    "bodyfat",
    "beer",
    "pm10"
  ),
  label = c(
    "Scalar synthetic function",
    "Noisy scalar synthetic function",
    "Random mpQP response function",
    "Bell synthetic function",
    "Bell synthetic function with unknown constraint",
    "Bell synthetic function on feasible pool",
    "Concrete slump test",
    "Auto MPG",
    "White wine quality",
    "Yacht hydrodynamics",
    "QSAR aquatic toxicity",
    "Body fat",
    "Beer consumption Sao Paulo",
    "PM10"
  ),
  source = c(
    rep("Bemporad synthetic", 6L),
    "UCI",
    "UCI",
    "UCI",
    "UCI",
    "UCI",
    "Kaggle",
    "Kaggle",
    "truncSP"
  ),
  pool_size = c(
    1000L,
    1000L,
    1000L,
    1000L,
    1000L,
    NA_integer_,
    103L,
    392L,
    4898L,
    308L,
    546L,
    252L,
    365L,
    500L
  ),
  feature_count = c(
    1L,
    1L,
    2L,
    2L,
    2L,
    2L,
    7L,
    6L,
    7L,
    6L,
    8L,
    14L,
    4L,
    7L
  ),
  n_init = c(
    10L,
    10L,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    rep(20L, 8L)
  ),
  n_max = c(
    30L,
    30L,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    NA_integer_,
    103L,
    100L,
    100L,
    100L,
    120L,
    120L,
    120L,
    120L
  ),
  learner_family = c(
    rep(NA_character_, 6L),
    "mlp",
    "mlp",
    "mlp",
    "mlp",
    "svr_rbf",
    "svr_rbf",
    "svr_rbf",
    "svr_rbf"
  ),
  requires = I(list(
    character(0),
    character(0),
    "quadprog",
    character(0),
    character(0),
    character(0),
    character(0),
    character(0),
    character(0),
    character(0),
    character(0),
    "kaggle CLI credentials",
    "kaggle CLI credentials",
    "truncSP"
  )),
  note = c(
    "Eq. 13 on 1000 equally spaced points in [-3, 3].",
    "Eq. 13 with fixed Gaussian observation noise.",
    "Randomly generated quadratic program response; default seeds make the instance reproducible.",
    "Eq. 23 on 1000 uniform points in [-2, 2]^2.",
    "Eq. 23 with infeasible points carrying missing responses and a feasible column in the pool.",
    "Eq. 23 restricted to feasible points from the default constrained pool.",
    "Seven material inputs and SLUMP(cm) target by default.",
    "Drops rows with missing horsepower and excludes origin/name to match n = 6.",
    "Uses the first seven attributes as features and the ninth as target.",
    "Six hull-design attributes and residuary resistance target.",
    "Eight molecular descriptors and LC50 target.",
    "Kaggle body-fat percentage target and remaining columns as features.",
    "Four weather features and beer consumption target.",
    "PM10 response with seven covariates from truncSP."
  )
))
setnames(al_objective_registry, "objective_key", "key")
setkeyv(al_objective_registry, "key")

al_objective_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    return(file.path(tools::R_user_dir("celecx", "cache"), "al_objectives"))
  }
  assert_string(cache_dir, min.chars = 1L)
  cache_dir
}

al_objective_file_slug <- function(x) {
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

al_objective_cache_path <- function(id, key, cache_dir = NULL) {
  assert_string(id, min.chars = 1L)
  root <- al_objective_cache_dir(cache_dir)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  hash <- digest::digest(
    list(cache_version = 1L, id = id, key = key),
    algo = "xxhash64"
  )
  file.path(root, sprintf("%s-%s.rds", al_objective_file_slug(id), hash))
}

al_objective_cached_value <- function(id, key, fun, cache = TRUE,
    cache_dir = NULL, refresh = FALSE) {
  assert_flag(cache)
  assert_flag(refresh)
  assert_function(fun)

  if (!cache) {
    return(fun())
  }

  path <- al_objective_cache_path(id, key, cache_dir = cache_dir)
  if (file.exists(path) && !refresh) {
    return(readRDS(path))
  }

  value <- fun()
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".rds")
  saveRDS(value, tmp, version = 2)
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  value
}

al_objective_download_url <- function(url, filename = basename(url),
    cache_dir = NULL, refresh = FALSE) {
  assert_string(url, min.chars = 1L)
  assert_string(filename, min.chars = 1L)
  assert_flag(refresh)

  root <- file.path(al_objective_cache_dir(cache_dir), "downloads")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  ext <- tools::file_ext(filename)
  suffix <- if (nzchar(ext)) paste0(".", ext) else ""
  path <- file.path(root, sprintf(
    "%s-%s%s",
    al_objective_file_slug(tools::file_path_sans_ext(filename)),
    digest::digest(url, algo = "xxhash64"),
    suffix
  ))

  if (file.exists(path) && file.info(path)$size > 0 && !refresh) {
    return(path)
  }

  tmp <- tempfile(tmpdir = root, fileext = suffix)
  on.exit(unlink(tmp), add = TRUE)
  status <- tryCatch(
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !identical(status, 0L)) {
    stopf("Could not download benchmark data from '%s'.", url)
  }
  if (!file.rename(tmp, path)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  path
}

al_objective_with_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }
  assert_int(seed, tol = 0)

  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (old_seed_exists) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}

al_objective_as_numeric <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }

  x <- trimws(as.character(x))
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

al_objective_prepare_regression_dt <- function(X, y, expected_rows = NULL,
    expected_features = NULL, dataset_id = "dataset") {
  assert_string(dataset_id, min.chars = 1L)
  assert_count(expected_rows, null.ok = TRUE)
  assert_count(expected_features, null.ok = TRUE)

  feature_dt <- as.data.table(X)
  feature_dt <- feature_dt[, lapply(.SD, al_objective_as_numeric)]
  target_y <- al_objective_as_numeric(y)

  keep <- stats::complete.cases(feature_dt) & is.finite(target_y)
  feature_dt <- feature_dt[keep]
  target_y <- target_y[keep]
  setnames(feature_dt, paste0("x", seq_len(ncol(feature_dt))))
  feature_dt[, y := target_y]

  if (!is.null(expected_rows) && nrow(feature_dt) != expected_rows) {
    stopf(
      "Dataset '%s' has %i rows after preprocessing, expected %i.",
      dataset_id,
      nrow(feature_dt),
      expected_rows
    )
  }
  if (!is.null(expected_features) && ncol(feature_dt) - 1L != expected_features) {
    stopf(
      "Dataset '%s' has %i feature columns after preprocessing, expected %i.",
      dataset_id,
      ncol(feature_dt) - 1L,
      expected_features
    )
  }

  feature_dt
}

al_objective_make_features_unique <- function(dt, feature_names,
    duplicate_handling = c("jitter", "error")) {
  row_index <- row_in_group <- group_size <- NULL

  duplicate_handling <- match.arg(duplicate_handling)
  assert_data_table(dt, min.rows = 1L)
  assert_character(feature_names, any.missing = FALSE, min.len = 1L, unique = TRUE)
  assert_subset(feature_names, names(dt))

  feature_dt <- dt[, feature_names, with = FALSE]
  duplicate_mask <- duplicated(feature_dt) | duplicated(feature_dt, fromLast = TRUE)
  if (!any(duplicate_mask)) {
    return(dt)
  }

  if (duplicate_handling == "error") {
    stopf(
      "Pool contains duplicate feature rows; use duplicate_handling = 'jitter' to preserve row count."
    )
  }

  jitter_candidates <- feature_names[vapply(feature_names, function(feature_name) {
    values <- dt[[feature_name]]
    diff(range(values, na.rm = TRUE)) > 0
  }, logical(1L))]
  if (!length(jitter_candidates)) {
    stop("Cannot jitter duplicate feature rows because all feature ranges are zero.")
  }

  jitter_id <- jitter_candidates[[length(jitter_candidates)]]
  jitter_scale <- sqrt(.Machine$double.eps) *
    max(1, diff(range(dt[[jitter_id]], na.rm = TRUE)))
  group_info <- dt[, {
    n_group <- .N
    if (n_group > 1L) {
      data.table(
        row_index = .I,
        row_in_group = seq_len(n_group),
        group_size = n_group
      )
    }
  }, by = feature_names]

  for (multiplier in 10^(0:6)) {
    candidate <- copy(dt)
    offsets <- (group_info$row_in_group - (group_info$group_size + 1) / 2) *
      jitter_scale * multiplier
    set(
      candidate,
      i = group_info$row_index,
      j = jitter_id,
      value = dt[[jitter_id]][group_info$row_index] + offsets
    )

    if (!any(duplicated(candidate[, feature_names, with = FALSE]))) {
      return(candidate)
    }
  }

  stop("Deterministic jitter did not resolve duplicate feature rows.")
}

al_objective_make_domain <- function(pool, feature_names) {
  do.call(ps, set_names(lapply(feature_names, function(feature_name) {
    values <- pool[[feature_name]]
    p_dbl(lower = min(values, na.rm = TRUE), upper = max(values, na.rm = TRUE))
  }), feature_names))
}

al_objective_make_pool_objective <- function(pool, id, properties = "deterministic",
    feature_names = NULL, duplicate_handling = c("jitter", "error"),
    check_values = TRUE) {
  duplicate_handling <- match.arg(duplicate_handling)
  assert_data_table(pool, min.rows = 1L)
  assert_string(id, min.chars = 1L)
  assert_subset("y", names(pool))
  assert_flag(check_values)

  pool <- copy(pool)
  if (is.null(feature_names)) {
    feature_names <- setdiff(names(pool), c("y", "row_id", "feasible"))
  }
  assert_character(feature_names, any.missing = FALSE, min.len = 1L, unique = TRUE)
  assert_subset(feature_names, names(pool))

  if (!"row_id" %in% names(pool)) {
    pool[, row_id := seq_len(nrow(pool))]
  }
  pool <- al_objective_make_features_unique(pool, feature_names, duplicate_handling)

  ObjectivePoolRFun$new(
    pool = pool,
    fun = function(matched_pool) matched_pool[, "y", with = FALSE],
    domain = al_objective_make_domain(pool, feature_names),
    codomain = ps(y = p_dbl(tags = "learn")),
    id = id,
    properties = properties,
    check_values = check_values
  )
}

al_objective_scalar_function <- function(x) {
  x <- as.numeric(x)
  x^4 * sin(x^2 / 3)^2
}

al_objective_make_scalar_pool <- function(M = 1000L, noise_sd = 0,
    seed = NULL) {
  M <- assert_count(M, positive = TRUE)
  assert_number(noise_sd, lower = 0)
  assert_int(seed, tol = 0, null.ok = TRUE)

  x <- seq(-3, 3, length.out = M)
  y <- al_objective_scalar_function(x)
  if (noise_sd > 0) {
    y <- al_objective_with_seed(seed, {
      y + stats::rnorm(length(y), mean = 0, sd = noise_sd)
    })
  }
  data.table(x1 = x, y = y)
}

al_objective_scalar <- function(M = 1000L, noise_sd = 0, seed = NULL,
    id = "scalar", cache = TRUE, cache_dir = NULL, refresh = FALSE,
    check_values = TRUE) {
  key <- list(M = M, noise_sd = noise_sd, seed = seed)
  pool <- al_objective_cached_value(
    id = paste0("synthetic-", id),
    key = key,
    fun = function() al_objective_make_scalar_pool(M = M, noise_sd = noise_sd, seed = seed),
    cache = cache,
    cache_dir = cache_dir,
    refresh = refresh
  )
  al_objective_make_pool_objective(
    pool = pool,
    id = id,
    properties = if (noise_sd > 0) "noisy" else "deterministic",
    duplicate_handling = "error",
    check_values = check_values
  )
}

al_objective_bell_function <- function(X) {
  X <- as.matrix(X)
  if (ncol(X) != 2L) {
    stop("X must have exactly two columns.")
  }

  x1 <- X[, 1L]
  x2 <- X[, 2L]
  exp(-(((1.5 * x1)^2 + (1.5 * x2)^2)^3))
}

al_objective_bell_feasible <- function(X) {
  X <- as.matrix(X)
  if (ncol(X) != 2L) {
    stop("X must have exactly two columns.")
  }

  3 * X[, 2L] <= sqrt(3) * abs(X[, 1L])
}

al_objective_make_bell_pool <- function(M = 1000L, seed = 1L,
    constrained = FALSE, feasible_only = FALSE) {
  M <- assert_count(M, positive = TRUE)
  assert_int(seed, tol = 0, null.ok = TRUE)
  assert_flag(constrained)
  assert_flag(feasible_only)

  X <- al_objective_with_seed(seed, {
    matrix(stats::runif(M * 2L, min = -2, max = 2), ncol = 2L)
  })
  y <- al_objective_bell_function(X)
  feasible <- al_objective_bell_feasible(X)
  if (constrained) {
    y[!feasible] <- NA_real_
  }

  pool <- data.table(x1 = X[, 1L], x2 = X[, 2L], y = y, feasible = feasible)
  if (feasible_only) {
    pool <- pool[feasible == TRUE]
    pool[, y := al_objective_bell_function(pool[, .(x1, x2)])]
  }
  pool
}

al_objective_bell <- function(M = 1000L, seed = 1L, constrained = FALSE,
    feasible_only = FALSE, id = "bell", cache = TRUE, cache_dir = NULL,
    refresh = FALSE, check_values = TRUE) {
  key <- list(
    M = M,
    seed = seed,
    constrained = constrained,
    feasible_only = feasible_only
  )
  pool <- al_objective_cached_value(
    id = paste0("synthetic-", id),
    key = key,
    fun = function() {
      al_objective_make_bell_pool(
        M = M,
        seed = seed,
        constrained = constrained,
        feasible_only = feasible_only
      )
    },
    cache = cache,
    cache_dir = cache_dir,
    refresh = refresh
  )
  al_objective_make_pool_objective(
    pool = pool,
    id = id,
    properties = "deterministic",
    duplicate_handling = "error",
    check_values = check_values
  )
}

al_objective_random_spd <- function(n, condition_number = 1e3) {
  assert_count(n, positive = TRUE)
  assert_number(condition_number, lower = 1)

  Z <- matrix(stats::rnorm(n * n), nrow = n)
  U <- qr.Q(qr(Z))
  eigvals <- exp(seq(0, log(condition_number), length.out = n))
  Q <- U %*% diag(eigvals, nrow = n) %*% t(U)
  (Q + t(Q)) / 2
}

al_objective_make_mpqp_problem <- function(seed = 1L, n = 2L, nz = 12L,
    m = 1L, q = 12L, condition_number = 1e3) {
  assert_int(seed, tol = 0, null.ok = TRUE)
  n <- assert_count(n, positive = TRUE)
  nz <- assert_count(nz, positive = TRUE)
  m <- assert_count(m, positive = TRUE)
  q <- assert_count(q, positive = TRUE)
  assert_number(condition_number, lower = 1)

  al_objective_with_seed(seed, {
    list(
      n = n,
      nz = nz,
      m = m,
      q = q,
      Q = al_objective_random_spd(nz, condition_number),
      A = matrix(stats::rnorm(q * nz), nrow = q, ncol = nz),
      F = matrix(stats::rnorm(nz * n), nrow = nz, ncol = n),
      b = stats::runif(q),
      S = matrix(0, nrow = q, ncol = n),
      ell = -stats::runif(nz),
      u = stats::runif(nz)
    )
  })
}

al_objective_mpqp_y <- function(X, problem) {
  if (!requireNamespace("quadprog", quietly = TRUE)) {
    stop("Package 'quadprog' is required for uncached mpQP benchmark construction.")
  }

  X <- as.matrix(X)
  if (ncol(X) != problem$n) {
    stopf("X must have %i columns.", problem$n)
  }

  Amat <- cbind(
    t(-problem$A),
    diag(problem$nz),
    -diag(problem$nz)
  )

  vals <- t(vapply(seq_len(nrow(X)), function(i) {
    x <- X[i, ]
    dvec <- -drop(problem$F %*% x)
    bvec <- c(
      -drop(problem$b + problem$S %*% x),
      problem$ell,
      -problem$u
    )
    sol <- quadprog::solve.QP(
      Dmat = problem$Q,
      dvec = dvec,
      Amat = Amat,
      bvec = bvec,
      meq = 0
    )$solution
    sol[seq_len(problem$m)]
  }, numeric(problem$m)))

  if (problem$m == 1L) {
    drop(vals)
  } else {
    vals
  }
}

al_objective_make_mpqp_pool <- function(M = 1000L, seed_problem = 1L,
    seed_pool = 2L, n = 2L, nz = 12L, m = 1L, q = 12L,
    condition_number = 1e3) {
  M <- assert_count(M, positive = TRUE)
  if (m != 1L) {
    stop("Only m = 1 is supported by the single-response mpQP benchmark objective.")
  }

  problem <- al_objective_make_mpqp_problem(
    seed = seed_problem,
    n = n,
    nz = nz,
    m = m,
    q = q,
    condition_number = condition_number
  )
  X <- al_objective_with_seed(seed_pool, {
    matrix(stats::runif(M * problem$n, min = -3, max = 3), ncol = problem$n)
  })
  dt <- as.data.table(X)
  setnames(dt, paste0("x", seq_len(problem$n)))
  dt[, y := as.numeric(al_objective_mpqp_y(X, problem))]
  dt
}

al_objective_mpqp <- function(M = 1000L, seed_problem = 1L,
    seed_pool = 2L, n = 2L, nz = 12L, m = 1L, q = 12L,
    condition_number = 1e3, id = "mpqp", cache = TRUE,
    cache_dir = NULL, refresh = FALSE, check_values = TRUE) {
  key <- list(
    M = M,
    seed_problem = seed_problem,
    seed_pool = seed_pool,
    n = n,
    nz = nz,
    m = m,
    q = q,
    condition_number = condition_number
  )
  pool <- al_objective_cached_value(
    id = paste0("synthetic-", id),
    key = key,
    fun = function() {
      al_objective_make_mpqp_pool(
        M = M,
        seed_problem = seed_problem,
        seed_pool = seed_pool,
        n = n,
        nz = nz,
        m = m,
        q = q,
        condition_number = condition_number
      )
    },
    cache = cache,
    cache_dir = cache_dir,
    refresh = refresh
  )
  al_objective_make_pool_objective(
    pool = pool,
    id = id,
    properties = "deterministic",
    duplicate_handling = "error",
    check_values = check_values
  )
}

al_objective_real_spec <- function(dataset_id) {
  assert_string(dataset_id, min.chars = 1L)
  spec <- al_objective_registry[list(dataset_id)]
  if (nrow(spec) != 1L) {
    stopf("Unknown active-learning benchmark dataset '%s'.", dataset_id)
  }
  spec
}

al_objective_read_concrete_slump <- function(target = c("slump", "flow", "strength"),
    cache_dir = NULL, refresh = FALSE) {
  target <- match.arg(target)
  path <- al_objective_download_url(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/slump/slump_test.data",
    filename = "slump_test.data",
    cache_dir = cache_dir,
    refresh = refresh
  )
  d <- fread(path)
  y_col <- switch(target,
    slump = 9L,
    flow = 10L,
    strength = 11L
  )
  al_objective_prepare_regression_dt(
    X = d[, 2:8, with = FALSE],
    y = d[[y_col]],
    expected_rows = 103L,
    expected_features = 7L,
    dataset_id = "concrete-slump"
  )
}

al_objective_read_auto_mpg <- function(cache_dir = NULL, refresh = FALSE) {
  path <- al_objective_download_url(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
    filename = "auto-mpg.data",
    cache_dir = cache_dir,
    refresh = refresh
  )
  d <- utils::read.table(
    path,
    header = FALSE,
    na.strings = "?",
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE,
    col.names = c(
      "mpg",
      "cylinders",
      "displacement",
      "horsepower",
      "weight",
      "acceleration",
      "model_year",
      "origin",
      "car_name"
    )
  )
  d <- d[stats::complete.cases(d[, c(
    "mpg",
    "cylinders",
    "displacement",
    "horsepower",
    "weight",
    "acceleration",
    "model_year"
  )]), ]
  al_objective_prepare_regression_dt(
    X = d[, c(
      "cylinders",
      "displacement",
      "horsepower",
      "weight",
      "acceleration",
      "model_year"
    ), drop = FALSE],
    y = d$mpg,
    expected_rows = 392L,
    expected_features = 6L,
    dataset_id = "auto-mpg"
  )
}

al_objective_read_winequality_white <- function(cache_dir = NULL,
    refresh = FALSE) {
  path <- al_objective_download_url(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
    filename = "winequality-white.csv",
    cache_dir = cache_dir,
    refresh = refresh
  )
  d <- fread(path, sep = ";")
  al_objective_prepare_regression_dt(
    X = d[, 1:7, with = FALSE],
    y = d[[9L]],
    expected_rows = 4898L,
    expected_features = 7L,
    dataset_id = "winequality-white"
  )
}

al_objective_read_yacht <- function(cache_dir = NULL, refresh = FALSE) {
  path <- al_objective_download_url(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data",
    filename = "yacht_hydrodynamics.data",
    cache_dir = cache_dir,
    refresh = refresh
  )
  d <- as.data.table(utils::read.table(path, header = FALSE))
  al_objective_prepare_regression_dt(
    X = d[, 1:6, with = FALSE],
    y = d[[7L]],
    expected_rows = 308L,
    expected_features = 6L,
    dataset_id = "yacht"
  )
}

al_objective_read_qsar_aquatic <- function(cache_dir = NULL,
    refresh = FALSE) {
  path <- al_objective_download_url(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/00505/qsar_aquatic_toxicity.csv",
    filename = "qsar_aquatic_toxicity.csv",
    cache_dir = cache_dir,
    refresh = refresh
  )
  d <- fread(path, sep = ";", header = FALSE)
  al_objective_prepare_regression_dt(
    X = d[, 1:8, with = FALSE],
    y = d[[9L]],
    expected_rows = 546L,
    expected_features = 8L,
    dataset_id = "qsar-aquatic-toxicity"
  )
}

al_objective_download_kaggle <- function(slug, cache_dir = NULL,
    refresh = FALSE, kaggle_cli = "kaggle") {
  assert_string(slug, min.chars = 1L)
  assert_string(kaggle_cli, min.chars = 1L)
  assert_flag(refresh)

  cli <- Sys.which(kaggle_cli)
  if (!nzchar(cli)) {
    stop("The Kaggle CLI is not on PATH. Install and configure it before constructing this benchmark.")
  }

  destdir <- file.path(
    al_objective_cache_dir(cache_dir),
    "kaggle",
    al_objective_file_slug(slug)
  )
  marker <- file.path(destdir, ".complete")
  if (file.exists(marker) && !refresh) {
    return(destdir)
  }

  dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  output <- system2(
    cli,
    args = c("datasets", "download", "-d", slug, "-p", destdir, "--unzip"),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stopf("Kaggle download failed for dataset slug '%s': %s", slug, paste(output, collapse = "\n"))
  }
  writeLines("complete", marker)
  destdir
}

al_objective_read_bodyfat <- function(cache_dir = NULL, refresh = FALSE,
    kaggle_cli = "kaggle") {
  path <- al_objective_download_kaggle(
    slug = "fedesoriano/body-fat-prediction-dataset",
    cache_dir = cache_dir,
    refresh = refresh,
    kaggle_cli = kaggle_cli
  )
  csvs <- list.files(path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csvs)) {
    stop("No CSV file found after Kaggle bodyfat download.")
  }

  d <- fread(csvs[[1L]])
  target_name <- grep("body.?fat", names(d), ignore.case = TRUE, value = TRUE)[[1L]]
  if (is.na(target_name)) {
    stop("Could not find BodyFat target column.")
  }
  feature_names <- setdiff(names(d), target_name)
  al_objective_prepare_regression_dt(
    X = d[, feature_names, with = FALSE],
    y = d[[target_name]],
    expected_rows = 252L,
    expected_features = 14L,
    dataset_id = "bodyfat"
  )
}

al_objective_read_beer <- function(cache_dir = NULL, refresh = FALSE,
    kaggle_cli = "kaggle") {
  path <- al_objective_download_kaggle(
    slug = "dongeorge/beer-consumption-sao-paulo",
    cache_dir = cache_dir,
    refresh = refresh,
    kaggle_cli = kaggle_cli
  )
  csvs <- list.files(path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csvs)) {
    stop("No CSV file found after Kaggle beer download.")
  }

  d <- fread(csvs[[1L]], encoding = "UTF-8")
  if (ncol(d) == 1L) {
    d <- fread(csvs[[1L]], sep = ";", dec = ",", encoding = "UTF-8")
  }

  column_names <- names(d)
  pick <- function(pattern) {
    hits <- grep(pattern, column_names, ignore.case = TRUE, value = TRUE)
    if (!length(hits)) {
      stopf("No beer dataset column matching pattern '%s'.", pattern)
    }
    hits[[1L]]
  }

  col_avg <- pick("Media|M.dia|Average")
  col_min <- pick("Minima|M.nima|Minimum")
  col_max <- pick("Maxima|M.xima|Maximum")
  col_prc <- pick("Precipit")
  col_y <- pick("Consumo|Consumption")

  X <- data.table(
    temperatura_media = al_objective_as_numeric(d[[col_avg]]),
    temperatura_minima = al_objective_as_numeric(d[[col_min]]),
    temperatura_maxima = al_objective_as_numeric(d[[col_max]]),
    precipitacao = al_objective_as_numeric(d[[col_prc]])
  )
  al_objective_prepare_regression_dt(
    X = X,
    y = al_objective_as_numeric(d[[col_y]]),
    expected_rows = 365L,
    expected_features = 4L,
    dataset_id = "beer"
  )
}

al_objective_read_pm10 <- function() {
  if (!requireNamespace("truncSP", quietly = TRUE)) {
    stop("Package 'truncSP' is required for uncached PM10 benchmark construction.")
  }

  env <- new.env(parent = emptyenv())
  utils::data("PM10", package = "truncSP", envir = env)
  d <- as.data.table(env$PM10)
  target_name <- grep("^PM10\\s*$", names(d), value = TRUE)[[1L]]
  if (is.na(target_name)) {
    stop("Could not find PM10 target column.")
  }
  feature_names <- setdiff(names(d), target_name)
  al_objective_prepare_regression_dt(
    X = d[, feature_names, with = FALSE],
    y = d[[target_name]],
    expected_rows = 500L,
    expected_features = 7L,
    dataset_id = "pm10"
  )
}

al_objective_load_real_dataset <- function(dataset_id, target = c("slump", "flow", "strength"),
    cache_dir = NULL, refresh = FALSE, kaggle_cli = "kaggle") {
  target <- match.arg(target)

  switch(dataset_id,
    "concrete-slump" = al_objective_read_concrete_slump(
      target = target,
      cache_dir = cache_dir,
      refresh = refresh
    ),
    "auto-mpg" = al_objective_read_auto_mpg(cache_dir = cache_dir, refresh = refresh),
    "winequality-white" = al_objective_read_winequality_white(cache_dir = cache_dir, refresh = refresh),
    "yacht" = al_objective_read_yacht(cache_dir = cache_dir, refresh = refresh),
    "qsar-aquatic-toxicity" = al_objective_read_qsar_aquatic(cache_dir = cache_dir, refresh = refresh),
    "bodyfat" = al_objective_read_bodyfat(cache_dir = cache_dir, refresh = refresh, kaggle_cli = kaggle_cli),
    "beer" = al_objective_read_beer(cache_dir = cache_dir, refresh = refresh, kaggle_cli = kaggle_cli),
    "pm10" = al_objective_read_pm10(),
    stopf("No loader implemented for real-world benchmark '%s'.", dataset_id)
  )
}

al_objective_real <- function(dataset_id, id = dataset_id,
    target = c("slump", "flow", "strength"), cache = TRUE,
    cache_dir = NULL, refresh = FALSE,
    duplicate_handling = c("jitter", "error"), check_values = TRUE,
    kaggle_cli = "kaggle") {
  target <- match.arg(target)
  duplicate_handling <- match.arg(duplicate_handling)
  spec <- al_objective_real_spec(dataset_id)

  key <- list(
    dataset_id = dataset_id,
    target = if (dataset_id == "concrete-slump") target else NULL,
    duplicate_handling = duplicate_handling
  )
  pool <- al_objective_cached_value(
    id = paste0("real-", dataset_id),
    key = key,
    fun = function() {
      dt <- al_objective_load_real_dataset(
        dataset_id = dataset_id,
        target = target,
        cache_dir = cache_dir,
        refresh = refresh,
        kaggle_cli = kaggle_cli
      )
      feature_names <- setdiff(names(dt), "y")
      al_objective_make_features_unique(
        dt,
        feature_names = feature_names,
        duplicate_handling = duplicate_handling
      )
    },
    cache = cache,
    cache_dir = cache_dir,
    refresh = refresh
  )

  feature_names <- setdiff(names(pool), c("y", "row_id", "feasible"))
  if (nrow(pool) != spec$pool_size[[1L]]) {
    stopf(
      "Benchmark '%s' has %i rows after preprocessing, expected %i.",
      dataset_id,
      nrow(pool),
      spec$pool_size[[1L]]
    )
  }
  if (length(feature_names) != spec$feature_count[[1L]]) {
    stopf(
      "Benchmark '%s' has %i feature columns after preprocessing, expected %i.",
      dataset_id,
      length(feature_names),
      spec$feature_count[[1L]]
    )
  }

  al_objective_make_pool_objective(
    pool = pool,
    id = id,
    properties = "deterministic",
    feature_names = feature_names,
    duplicate_handling = "error",
    check_values = check_values
  )
}

mlr_al_objectives$add("scalar", al_objective_scalar, id = "scalar")
mlr_al_objectives$add(
  "scalar-noisy",
  al_objective_scalar,
  id = "scalar-noisy",
  noise_sd = 1,
  seed = 1L
)
mlr_al_objectives$add("mpqp", al_objective_mpqp, id = "mpqp")
mlr_al_objectives$add("bell", al_objective_bell, id = "bell")
mlr_al_objectives$add(
  "bell-constrained",
  al_objective_bell,
  id = "bell-constrained",
  constrained = TRUE
)
mlr_al_objectives$add(
  "bell-feasible",
  al_objective_bell,
  id = "bell-feasible",
  constrained = TRUE,
  feasible_only = TRUE
)

for (dataset_id in c(
  "concrete-slump",
  "auto-mpg",
  "winequality-white",
  "yacht",
  "qsar-aquatic-toxicity",
  "bodyfat",
  "beer",
  "pm10"
)) {
  mlr_al_objectives$add(
    dataset_id,
    al_objective_real,
    dataset_id = dataset_id,
    id = dataset_id
  )
}
