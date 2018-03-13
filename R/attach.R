# Largely derived from tidyverse
# https://github.com/tidyverse/tidyverse

# Master list of core packages
# core <- c("Momocs", "Momacs", "Momecs", "Momit", "Momfarm")
core <- c("Momocs")

# Attach all core MomX packages
MomX_attach <- function() {
  to_load <- core_unloaded()
  if (length(to_load) == 0)
    return(invisible())

  # msg(
  #   cli::rule(
  #     left = crayon::bold("Attaching packages"),
  #     right = paste0("MomX ", package_version("MomX"))
  #   ),
  #   startup = TRUE
  # )

  versions <- vapply(to_load, package_version, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

#' List all packages in MomX
#'
#' @export
#' @examples
#' MomX_packages()
MomX_packages <- function() {
  raw <- utils::packageDescription("MomX")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))
  names <- c("MomX", names)
  names
}

