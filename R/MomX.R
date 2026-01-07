# package list --------

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# Define core and extended packages
core <- c(
  "Momocs2",    # Core morphometric methods and data structures
  "Momacs",     # Interactive digitization
  "Momoshop"    # Image processing pipelines
)

extended <- c(
  "Momdata",    # Example datasets
  "Momstats"    # Advanced statistical methods
  # "MomViz",     # Specialized visualizations
  # "MomBot",     # Botanical morphometrics
  # "MomArch"     # Archaeological applications
)

#' List all packages in the MomX ecosystem
#'
#' @param include_self Include MomX package itself? Default is FALSE.
#' @return Character vector of package names
#' @export
#' @examples
#' momx_packages()
momx_packages <- function(include_self = FALSE) {
  raw <- utils::packageDescription("MomX")$Package %>%
    strsplit(",") %>%
    unlist() %>%
    trimws()

  if (include_self) {
    return(c(raw, "MomX"))
  }
  raw
}

#' List core MomX packages
#' @export
momx_packages_core <- function() {
  core
}

#' List extended MomX packages
#' @export
momx_packages_extended <- function() {
  extended
}

#' List all MomX packages (core + extended)
#' @export
momx_packages_all <- function() {
  c(core, extended)
}

# status ----

#' List available MomX packages with status
#'
#' Show which MomX packages are installed and their versions.
#' Includes information about packages that don't exist yet.
#'
#' @export
#' @examples
#' momx_status()
momx_status <- function() {
  all_pkgs <- c(
    momx_packages_core(),
    momx_packages_extended()
  )

  pkg_type <- c(
    rep("core", length(momx_packages_core())),
    rep("extended", length(momx_packages_extended()))
  )

  # Check installation status
  installed <- all_pkgs %in% rownames(utils::installed.packages())

  # Get versions for installed packages
  versions <- vapply(all_pkgs, function(pkg) {
    if (pkg %in% rownames(utils::installed.packages())) {
      as.character(utils::packageVersion(pkg))
    } else {
      NA_character_
    }
  }, character(1))

  # Check if loaded
  loaded <- all_pkgs %in% loadedNamespaces()

  # Create status tibble
  status <- tibble::tibble(
    package = all_pkgs,
    type = pkg_type,
    installed = installed,
    version = versions,
    loaded = loaded
  )

  # Print nicely
  cli::cli_h1("MomX Package Status")

  cli::cli_h2("Core Packages")
  print_status_section(status[status$type == "core", ])

  cli::cli_h2("Extended Packages")
  print_status_section(status[status$type == "extended", ])

  # Summary
  n_installed <- sum(status$installed)
  n_total <- nrow(status)
  cli::cli_inform("")
  cli::cli_inform(c(
    "i" = "{n_installed} of {n_total} packages installed",
    "i" = "Install missing packages with: {.code momx_install('all')}"
  ))

  invisible(status)
}

print_status_section <- function(df) {
  if (nrow(df) == 0) return(invisible())

  for (i in seq_len(nrow(df))) {
    pkg <- df$package[i]

    if (df$installed[i]) {
      status_icon <- crayon::green(cli::symbol$tick)
      version_str <- crayon::silver(paste0("v", df$version[i]))
      loaded_str <- if (df$loaded[i]) {
        crayon::blue(" [loaded]")
      } else {
        ""
      }

      cli::cli_inform(paste0(
        status_icon, " ", pkg, " ", version_str, loaded_str
      ))
    } else {
      status_icon <- crayon::silver(cli::symbol$circle_dotted)
      cli::cli_inform(paste0(
        status_icon, " ", pkg, " ", crayon::silver("[not installed]")
      ))
    }
  }
}

# install -----
#' Install MomX packages
#'
#' Install core or extended MomX packages from CRAN or GitHub.
#' Gracefully handles packages that don't exist yet.
#'
#' @param which Which packages to install? "core" (default), "extended", or "all"
#' @param from Source to install from: "cran" (default) or "github"
#' @param force Force reinstallation even if packages are up to date?
#' @export
#' @examples
#' \dontrun{
#' # Install core packages from CRAN
#' momx_install()
#'
#' # Install all packages from GitHub (development versions)
#' momx_install("all", from = "github")
#'
#' # Force reinstall core packages
#' momx_install(force = TRUE)
#' }
momx_install <- function(which = c("core", "extended", "all"),
                         from = c("cran", "github"),
                         force = FALSE) {
  which <- match.arg(which)
  from <- match.arg(from)

  pkgs <- switch(which,
                 core = momx_packages_core(),
                 extended = momx_packages_extended(),
                 all = momx_packages_all()
  )

  # Filter out packages that are already installed (unless force = TRUE)
  if (!force) {
    installed <- pkgs %in% rownames(utils::installed.packages())
    pkgs <- pkgs[!installed]

    if (length(pkgs) == 0) {
      cli::cli_inform("All requested packages are already installed.")
      return(invisible())
    }
  }

  if (from == "cran") {
    cli::cli_inform("Installing {length(pkgs)} package{?s} from CRAN...")

    # Try to install, but capture which ones fail
    results <- vapply(pkgs, function(pkg) {
      tryCatch({
        utils::install.packages(pkg, quiet = TRUE)
        TRUE
      }, error = function(e) {
        cli::cli_warn("Package {.pkg {pkg}} not available on CRAN")
        FALSE
      }, warning = function(w) {
        cli::cli_warn("Package {.pkg {pkg}} not available on CRAN")
        FALSE
      })
    }, logical(1))

    if (any(!results)) {
      failed <- pkgs[!results]
      cli::cli_inform(c(
        "i" = "Some packages are not yet on CRAN: {.pkg {failed}}",
        "i" = "Try installing from GitHub with: {.code momx_install(from = 'github')}"
      ))
    }

  } else {
    # GitHub installation
    if (!requireNamespace("remotes", quietly = TRUE)) {
      cli::cli_inform("Installing {.pkg remotes} first...")
      utils::install.packages("remotes")
    }

    github_repos <- paste0("MomX/", pkgs)
    cli::cli_inform("Installing {length(pkgs)} package{?s} from GitHub...")

    for (repo in github_repos) {
      tryCatch({
        cli::cli_inform("Installing {.pkg {repo}}...")
        remotes::install_github(repo, force = force, upgrade = "never")
      }, error = function(e) {
        cli::cli_warn(c(
          "x" = "Failed to install {.pkg {repo}}",
          "i" = "Repository may not exist yet"
        ))
      })
    }
  }

  # Check what actually got installed
  now_installed <- pkgs[pkgs %in% rownames(utils::installed.packages())]

  if (length(now_installed) > 0) {
    cli::cli_inform(c(
      "v" = "Successfully installed: {.pkg {now_installed}}",
      "i" = "Load packages with {.code library(MomX)}"
    ))
  }

  invisible()
}


# update ----
#' Update MomX packages
#'
#' This will check CRAN and GitHub for updates to MomX packages,
#' and install them if available. Gracefully handles packages that
#' don't exist yet.
#'
#' @param which Which packages to update? "core" (default), "extended", or "all"
#' @param from Source to update from: "cran" (default) or "github"
#' @export
#' @examples
#' \dontrun{
#' momx_update()
#' momx_update("all")
#' momx_update(from = "github")
#' }
momx_update <- function(which = c("core", "extended", "all"),
                        from = c("cran", "github")) {
  which <- match.arg(which)
  from <- match.arg(from)

  pkgs <- switch(which,
                 core = momx_packages_core(),
                 extended = momx_packages_extended(),
                 all = momx_packages_all()
  )

  # Only try to update packages that are actually installed
  installed <- pkgs[pkgs %in% rownames(utils::installed.packages())]

  if (length(installed) == 0) {
    cli::cli_inform(c(
      "i" = "No MomX packages installed yet.",
      "i" = "Install packages with: {.code momx_install()}"
    ))
    return(invisible())
  }

  if (from == "cran") {
    cli::cli_inform("Checking CRAN for updates to {length(installed)} package{?s}...")
    utils::update.packages(oldPkgs = installed, ask = FALSE)
  } else {
    # GitHub installation
    if (!requireNamespace("remotes", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg remotes} is required for GitHub installation.")
    }

    github_repos <- paste0("MomX/", installed)
    cli::cli_inform("Updating {length(installed)} package{?s} from GitHub...")

    for (repo in github_repos) {
      tryCatch({
        cli::cli_inform("Updating {.pkg {repo}}...")
        remotes::install_github(repo, upgrade = "never")
      }, error = function(e) {
        cli::cli_warn("Failed to update {.pkg {repo}}: {e$message}")
      })
    }
  }

  invisible()
}


# attach ------

# Attach all MomX packages
# Heavily inspired by tidyverse::tidyverse_attach()

momx_attach <- function() {
  to_load <- momx_packages_core()

  # Check which packages are actually available
  available <- vapply(to_load, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  # Warn about missing packages (but don't error)
  if (!all(available)) {
    missing <- to_load[!available]
    cli::cli_warn(c(
      "!" = "Some core MomX packages are not installed:",
      "x" = paste0(missing, collapse = ", "),
      "i" = "Install them with: {.code momx_install()}"
    ))
    to_load <- to_load[available]
  }

  # If no packages available, just return silently
  if (length(to_load) == 0) {
    return(invisible(character(0)))
  }

  # Check which packages are already loaded
  loaded <- to_load %in% loadedNamespaces()

  # Load packages that aren't loaded yet
  if (any(!loaded)) {
    suppressPackageStartupMessages({
      lapply(to_load[!loaded], function(pkg) {
        tryCatch(
          library(pkg, character.only = TRUE, warn.conflicts = FALSE),
          error = function(e) {
            cli::cli_warn("Failed to load {.pkg {pkg}}: {e$message}")
            NULL
          }
        )
      })
    })
  }

  # Create invisible list of package versions
  invisible(momx_attach_message(to_load))
}

momx_attach_message <- function(to_load) {
  # Get versions of all loaded packages
  versions <- vapply(to_load, function(pkg) {
    tryCatch(
      package_version_h(pkg),
      error = function(e) "?"
    )
  }, character(1))

  # Format package names and versions
  packages <- paste0(
    crayon::blue(cli::symbol$tick), " ",
    format(to_load), "  ",
    crayon::silver(versions)
  )

  # Check if all packages loaded successfully
  loaded <- to_load %in% loadedNamespaces()

  if (all(loaded)) {
    cli::cli_inform(c(
      "v" = "MomX packages loaded:",
      packages
    ))
  } else {
    failed <- to_load[!loaded]
    if (length(failed) > 0) {
      cli::cli_warn(c(
        "!" = "Some MomX packages failed to load:",
        paste0("x ", failed)
      ))
    }
  }

  # Show conflicts only if packages actually loaded
  if (any(loaded)) {
    conflicts <- momx_conflicts()
    if (length(conflicts) > 0) {
      cli::cli_inform("")
      momx_conflict_message(conflicts)
    }
  }

  invisible(to_load)
}

package_version_h <- function(pkg) {
  version <- utils::packageVersion(pkg)
  paste0(crayon::silver("v"), version)
}

#' @importFrom rlang .data
.onAttach <- function(...) {
  needed <- momx_packages_core()

  # Check which packages are actually available
  available <- vapply(needed, function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }, logical(1))

  # If none are available (like during R CMD check), silently return
  if (!any(available)) {
    return(invisible())
  }

  # Check which are already loaded
  loaded <- needed %in% loadedNamespaces()

  # If all available packages are already loaded, return silently
  if (all(loaded[available])) {
    return(invisible())
  }

  # Otherwise attach packages
  momx_attach()
}

# conflicts -----

#' Conflicts between MomX packages and other packages
#'
#' This function lists all conflicts between packages in the MomX ecosystem
#' and other packages that you have loaded.
#'
#' @return A named list of character vectors giving the conflicts
#' @export
#' @examples
#' momx_conflicts()
momx_conflicts <- function() {
  # Get list of core packages that are actually loaded
  core_loaded <- momx_packages_core()[momx_packages_core() %in% loadedNamespaces()]

  # If no packages loaded, return empty list
  if (length(core_loaded) == 0) {
    return(list())
  }

  envs <- purrr::map(core_loaded, ~ asNamespace(.))
  names(envs) <- core_loaded

  # Get only EXPORTED objects (not internal ones like server/ui from Shiny apps)
  objs <- purrr::map(envs, ls_env_exported)

  # Find functions that appear in multiple packages
  conflicts <- purrr::map(objs, confirm_conflict, envs)
  purrr::compact(conflicts)
}

# Helper to get only exported objects from a namespace
ls_env_exported <- function(env) {
  # Get namespace exports
  exports <- tryCatch(
    getNamespaceExports(env),
    error = function(e) character(0)
  )

  # Filter to only functions (not data)
  is_function <- vapply(exports, function(obj) {
    tryCatch(
      is.function(get(obj, envir = env)),
      error = function(e) FALSE
    )
  }, logical(1))

  exports[is_function]
}

confirm_conflict <- function(obj_names, envs) {
  if (length(obj_names) == 0) return(character(0))

  # For each object, check if it exists in multiple packages
  conflicts <- purrr::map_lgl(obj_names, function(obj) {
    # Get all packages that have this object
    pkgs <- purrr::map_lgl(envs, ~ exists(obj, envir = ., inherits = FALSE))
    sum(pkgs) > 1
  })

  obj_names[conflicts]
}

momx_conflict_message <- function(conflicts) {
  if (length(conflicts) == 0) {
    return(invisible())
  }

  msgs <- purrr::imap(conflicts, format_conflict)

  cli::cli_inform(c(
    "x" = "Conflicts:",
    msgs
  ))
}

format_conflict <- function(val, pkg) {
  others <- val[val != pkg]
  paste0(
    crayon::blue(pkg), "::", crayon::blue(names(val)[1]),
    " masks ",
    paste0(crayon::silver(others), "::", crayon::silver(names(val)[1]),
           collapse = ", ")
  )
}

# go to web ------
#' Open MomX package websites
#'
#' Open the pkgdown website for MomX packages in your browser
#'
#' @param which Which package website to open? Use "MomX" for the main site,
#'   or any package name like "Momocs2", "Momacs", etc.
#' @export
#' @examples
#' \dontrun{
#' # Open main MomX website
#' momx_website()
#'
#' # Open Momocs2 documentation
#' momx_website("Momocs2")
#' }
momx_website <- function(which = "MomX") {
  url <- sprintf("https://momx.github.io/%s/", which)
  cli::cli_inform("Opening {.url {url}}")
  utils::browseURL(url)
  invisible()
}

#' Open MomX package GitHub repositories
#'
#' Open the GitHub repository for MomX packages in your browser
#'
#' @param which Which package repository to open? Use "MomX" for the main repo,
#'   or any package name like "Momocs2", "Momacs", etc.
#' @export
#' @examples
#' \dontrun{
#' # Open main MomX repository
#' momx_repo()
#'
#' # Open Momocs2 repository
#' momx_repo("Momocs2")
#' }
momx_repo <- function(which = "MomX") {
  url <- sprintf("https://github.com/MomX/%s", which)
  cli::cli_inform("Opening {.url {url}}")
  utils::browseURL(url)
  invisible()
}
