.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0)
    return()

  crayon::num_colors(TRUE)
  MomX_attach()

  x <- MomX_conflicts()
  msg(MomX_conflict_message(x), startup = TRUE)
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
