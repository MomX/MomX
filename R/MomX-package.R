#' @keywords internal
"_PACKAGE"

core <- c("Momocs", "Momecs")

.onAttach <- function(libname, pkgname) {
  needed <- as.list(core[!is_attached(core)])
  if (length(needed) == 0)
    return()

  # ipak <- function(pkg){
  #   new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  #   if (length(new.pkg))
  #     utils::install.packages(new.pkg, dependencies = TRUE)
  #   sapply(pkg, require, character.only = TRUE)
  # }

  # usage

  sapply(core, require, character.only = TRUE)


  # # crayon::num_colors(TRUE)
  # # MomX_attach()
  # for (i in seq_along(core))
  #   library(core[i],  character.only = TRUE)
  # lapply(core, library, character.only=TRUE)
  #
  # x <- MomX_conflicts()
  # msg(MomX_conflict_message(x), startup = TRUE)
}



