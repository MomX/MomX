# Core ------

#' @keywords internal
"_PACKAGE"

core <- c("Momocs", "Momecs")

.onAttach <- function(libname, pkgname) {
  MomX_attach()
}

# MomX_funs ------------

#' Attach, detach, install, update MomX packages
#'
#' Some MomX packages are still considered experimental.
#'
#' @name MomX_funs
#' @rdname MomX_funs
#' @export
MomX_attach <- function(){
  # check which are already attached
  needed <- as.list(core[!is_attached(core)])
  if (length(needed) == 0)
    return()

  # start rule
  cat(cli::rule(center = crayon::green(" Attaching MomX packages "),
                line_col="silver"), "\n")

  # load/install them all
  statuses <- suppressPackageStartupMessages(
    sapply(core, pkg_load_or_install_cran_then_github)
  )

  # ending rule with global status
  if (all(statuses))
    cat(cli::rule(center = crayon::green(cli::symbol$tick),
                  line_col="silver"))
  else
    cat(cli::rule(center = crayon::red(cli::symbol$cross),
                  line_col="silver"), "\n")

}

#' @name MomX_funs
#' @rdname MomX_funs
#' @export
MomX_detach <- function(){
  # detach them all
  status <- all(sapply(sapply(core, detach1), is.null))
  # report status
  if (status)
    cat(crayon::green(cli::symbol$cross), "Detached all MomX packages ")
  else
    cat(crayon::red(cli::symbol$tick), "Some of MomX packages still attached")
}

#' @name MomX_funs
#' @rdname MomX_funs
#' @export
MomX_update_cran <- function(){

  # detach them all
  status <- unlist(sapply(core, cran1))

  # report status
  if (all(status)){
    cat(crayon::green(cli::symbol$tick),
        "Installed the last CRAN versions of: ", paste(core, collapse=", "))
  } else {
    cat(crayon::green(cli::symbol$tick),
        "Installed the last CRAN versions of: ", paste(core[status], sep=", "), "\n")
    cat(crayon::red(cli::symbol$cross),
        "Failed to install from CRAN: ", paste(core[!status], sep=", "), "; ")
    cat(cli::symbol$arrow_right, " Try", crayon::cyan("MomX_update_github()"), "\n")
  }
}

#' @name MomX_funs
#' @rdname MomX_funs
#' @export
MomX_update_github <- function(){

  # detach them all
  status <- unlist(sapply(core, github1))

  # report status
  if (all(status)){
    cat(crayon::green(cli::symbol$tick),
        "Installed the very last GitHub versions of: ", paste(core, collapse=", "))
  } else {
    cat(crayon::green(cli::symbol$tick),
        "Installed the very last GitHub versions of: ", paste(core[status], sep=", "), "\n")
    cat(crayon::red(cli::symbol$cross),
        "Failed to install from GitHub: ", paste(core[!status], sep=", "), "; ")
  }
}

# helpers ------

# utils --------
is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

pkg_load_or_install_cran_then_github <- function(pkg){
  # pre loading
  cat(cli::symbol$arrow_right, pkg, "\t")
  loaded <- require(pkg, character.only = TRUE)

  # Uncomment below when everything will be ok
  # # if not already succeed, try to install from CRAN
  # if (!loaded){
  #   cat(crayon::blue("trying to install from CRAN\t"))
  #   utils::install.packages(pkg, dependencies =TRUE)
  #   loaded <- require(pkg, character.only = TRUE)
  # }

  # if not already succeed, try to install from GitHub
  if (!loaded){
    gh_url <- paste0("MomX/", pkg)
    cat(crayon::blue("trying to install from GitHub\t"))
    devtools::install_github(gh_url)
    loaded <- require(pkg, character.only = TRUE)
  }

  # if succeed, then report it
  if (loaded){
    cat(crayon::silver(as.character(utils::packageVersion(pkg))), "\t",
        # crayon::green(cli::symbol$tick),
        "\n")
    return(TRUE)
  } else {
    # otherwise, report it too
    cat(crayon::red("failed", cli::symbol$cross), "\n")
    return(FALSE)
  }
}

# adapted from https://stackoverflow.com/questions/9341635
detach1 <- function(pkg){
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search()){
    suppressWarnings(
      detach(search_item, unload = TRUE, character.only = TRUE)
    )
  }
}

cran1 <- function(pkg){
  # pre install message
  cat(cli::symbol$arrow_right, pkg, "\t")
  cat(crayon::blue("trying to install from CRAN\t"))

  # try to install and load
  suppressMessages(
    utils::install.packages(pkg, dependencies=TRUE)
  )
  loaded <- suppressPackageStartupMessages(
    require(pkg, character.only = TRUE)
  )

  # message status and return value
  if (loaded){
    cat(crayon::green(cli::symbol$tick), "\n")
    return(TRUE)
  } else {
    cat(crayon::red(cli::symbol$cross), "\n")
    return(FALSE)
  }
}

github1 <- function(pkg){
  # pre install message
  cat(cli::symbol$arrow_right, pkg, "\t")
  cat(crayon::blue("trying to install from http://github.com/MomX\t"))

  # try to install and load
  suppressMessages(
    devtools::install_github(paste0("MomX/", pkg))
  )
  loaded <- suppressPackageStartupMessages(
    require(pkg, character.only = TRUE)
  )

  # message status and return value
  if (loaded){
    cat(crayon::green(cli::symbol$tick), "\n")
    return(TRUE)
  } else {
    cat(crayon::red(cli::symbol$cross), "\n")
    return(FALSE)
  }
}
