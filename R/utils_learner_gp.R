# Internal Helpers for GP Learners
#
# Utility helpers shared by the GP learner wrappers.

task_feature_matrix <- function(task) {
  as.matrix(task$data(cols = task$feature_names))
}

prediction_se_from_variance <- function(variance) {
  variance <- as.numeric(variance)
  variance[variance < 0] <- 0
  sqrt(variance)
}

with_temp_workdir <- function(code) {
  code <- substitute(code)
  old_wd <- getwd()
  temp_wd <- tempfile("celecx-")
  dir.create(temp_wd)

  on.exit(setwd(old_wd), add = TRUE)
  on.exit(unlink(temp_wd, recursive = TRUE, force = TRUE), add = TRUE)

  setwd(temp_wd)
  eval(code, envir = parent.frame())
}
