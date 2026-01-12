#' List functions defined in a package
#'
#' Returns only functions actually defined in the package, excluding imports and re-exports
#'
#' @param package Character. Package name
#'
#' @return Character vector of function names defined in this package
#'
#' @examples
#' list_functions("MomX")
#'
#' @export
list_functions <- function(package) {
  # Get package namespace
  ns <- getNamespace(package)

  # Get all exported objects
  exports <- getNamespaceExports(ns)

  # Filter to functions only
  funs <- exports[sapply(exports, function(x) {
    obj <- get(x, envir = ns)
    is.function(obj)
  })]

  # Filter out re-exports (functions from other packages)
  own_funs <- funs[sapply(funs, function(x) {
    fun <- get(x, envir = ns)
    env <- environment(fun)
    # Check if function's environment is this package's namespace
    identical(env, ns) || is.null(env)  # NULL for primitive functions
  })]

  sort(own_funs)
}

# rinse ----

#' Clear the environment
#'
#' Remove all objects from the current environment.
#'
#' @param env Environment to clear. Default is the global environment.
#'
#' @return Invisibly returns NULL.
#'
#' @details
#' Removes all user-created objects from the specified environment.
#' Useful for cleaning up workspace between analyses or starting fresh.
#'
#' Use with caution: removed objects cannot be recovered.
#'
#' @note I know J Bryan and my old friend T Poisot said it was anti-social in
#' a famous tweet but I still find it useful.
#'
#' @examples
#' \dontrun{
#' # Create some objects
#' x <- 1
#' y <- 2
#' z <- data.frame(a = 1:10)
#'
#' # Check what's in environment
#' ls()
#'
#' # Clean it all up
#' rinse()
#'
#' # Environment is now empty
#' ls()
#' }
#'
#' @keywords internal
#' @export
rinse <- function(env = .GlobalEnv) {
  rm(list = ls(envir = env, all.names = TRUE), envir = env)
  invisible(NULL)
}

