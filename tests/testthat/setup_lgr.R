if (requireNamespace("lgr", quietly = TRUE)) {
  # root logger controls defaults for descendants
  lgr::get_logger("root")$set_threshold("warn")
  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")
  lgr::get_logger("mlr3mbo")$set_threshold("warn")
}
