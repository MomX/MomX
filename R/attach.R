#
# # Attach all core MomX packages
# MomX_attach <- function() {
#   search <- paste0("package:", core)
#   to_load <- core[!search %in% search()]
#
#   if (length(to_load) == 0)
#     return(invisible())
#
#   message(
#     cli::rule(
#       left = crayon::bold("Attaching packages"),
#       right = paste0("MomX ", utils::packageVersion("MomX"))
#     ),
#     startup = TRUE
#   )
#
#   versions <- sapply(to_load, MomX_package_version)
#   packages <- paste0(
#     crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
#     crayon::col_align(versions, max(crayon::col_nchar(versions)))
#   )
#
#   if (length(packages) %% 2 == 1) {
#     packages <- append(packages, "")
#   }
#   col1 <- seq_len(length(packages) / 2)
#   info <- paste0(packages[col1], "     ", packages[-col1])
#
#   message(paste(info, collapse = "\n"), startup = TRUE)
#
#   lapply(to_load, function(.x)
#     suppressPackageStartupMessages(
#       library(.x, character.only = TRUE, warn.conflicts = FALSE)
#     )
#   )
#
#   invisible()
# }
#
