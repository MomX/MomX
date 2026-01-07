install_momx_suggests <- function() {
  momx_packages <- c("Momocs2", "Momacs", "Momoshop", "Momdata", "Momstats")

  for (pkg in momx_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      tryCatch(
        remotes::install_github(sprintf("MomX/%s", pkg)),
        error = function(e) invisible()
      )
    }
  }
}
