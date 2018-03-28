#
# # Grab a package version
# MomX_package_version <- function(x) {
#   version <- as.character(unclass(utils::packageVersion(x))[[1]])
#
#   if (length(version) > 3) {
#     version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
#   }
#   paste0(version, collapse = ".")
# }
#
