make_gp_task <- function(n = 12L, d = 1L) {
  x1 <- seq(0.05, 0.95, length.out = n)
  backend <- data.table(
    x1 = x1,
    y = sin(2 * pi * x1)
  )

  if (d >= 2L) {
    backend[, x2 := cos(2 * pi * x1)]
    backend[, y := y + 0.1 * x2]
  }

  TaskRegr$new(
    id = sprintf("gp_task_%i", d),
    backend = backend,
    target = "y"
  )
}
