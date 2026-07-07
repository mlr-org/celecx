# Load suggested packages needed for tests
library(mlr3learners)

# Search space / codomain matching the archive columns used by the LCE
# learner/measure/resampling test tasks (archive_x = x1, x2 ; archive_y = y).
# Both are optional replay provenance; tests that exercise replay (or that
# should stay closest to callback-produced tasks) attach them.
lce_search_space <- function() {
  ps(x1 = p_dbl(), x2 = p_dbl())
}

lce_codomain <- function() {
  Codomain$new(ps(y = p_dbl(tags = "learn"))$domains)
}

# Bounded variant of lce_search_space(): forward-simulation learners need finite
# bounds for samplers/terminators (the unbounded variant exercises the learner's
# bound-imputation path instead).
lce_search_space_bounded <- function() {
  ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 1))
}
