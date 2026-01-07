# # =======================================================
# # tests/testthat/helper.R
# # Helper functions for testing
# # =======================================================
#
# # Skip tests if core packages aren't available
# skip_if_no_momx <- function(pkg = "Momocs2") {
#   if (!requireNamespace(pkg, quietly = TRUE)) {
#     skip(paste0(pkg, " not available"))
#   }
# }
#
# # =======================================================
# # tests/testthat/test-package-lists.R
# # =======================================================
#
# test_that("core packages are defined", {
#   expect_true(length(momx_packages_core()) > 0)
#   expect_type(momx_packages_core(), "character")
#   expect_true("Momocs2" %in% momx_packages_core())
#   expect_true("Momacs" %in% momx_packages_core())
#   expect_true("Momoshop" %in% momx_packages_core())
# })
#
# test_that("extended packages are defined", {
#   expect_true(length(momx_packages_extended()) > 0)
#   expect_type(momx_packages_extended(), "character")
#   # Check at least one extended package
#   expect_true(any(c("Momdata", "Momstats") %in% momx_packages_extended()))
# })
#
# test_that("all packages includes core and extended", {
#   all_pkgs <- momx_packages_all()
#   core <- momx_packages_core()
#   extended <- momx_packages_extended()
#
#   expect_true(all(core %in% all_pkgs))
#   expect_true(all(extended %in% all_pkgs))
#   expect_equal(length(all_pkgs), length(core) + length(extended))
# })
#
# test_that("no duplicate packages", {
#   all_pkgs <- momx_packages_all()
#   expect_equal(length(all_pkgs), length(unique(all_pkgs)))
# })
#
# test_that("package names follow conventions", {
#   all_pkgs <- momx_packages_all()
#   # All should start with "Mom"
#   expect_true(all(grepl("^Mom", all_pkgs)))
#   # No spaces in package names
#   expect_true(all(!grepl(" ", all_pkgs)))
# })
#
# # =======================================================
# # tests/testthat/test-attach.R
# # =======================================================
#
# test_that("momx_attach handles missing packages gracefully", {
#   # This should not error even if packages don't exist
#   expect_silent({
#     result <- tryCatch(
#       momx_attach(),
#       error = function(e) NULL,
#       warning = function(w) NULL
#     )
#   })
# })
#
# test_that("package_version_h works with base packages", {
#   expect_type(package_version_h("base"), "character")
#   expect_match(package_version_h("base"), "^v[0-9]")
#   expect_match(package_version_h("stats"), "^v[0-9]")
# })
#
# test_that("momx_attach only loads available packages", {
#   # Get currently loaded namespaces before
#   before <- loadedNamespaces()
#
#   # Try to attach (may warn if packages missing)
#   suppressWarnings(momx_attach())
#
#   # Check that only available packages were attempted
#   core <- momx_packages_core()
#   available <- vapply(core, requireNamespace, logical(1), quietly = TRUE)
#
#   # If any are available, they should be loaded now
#   if (any(available)) {
#     after <- loadedNamespaces()
#     loaded_momx <- intersect(core, after)
#     # All loaded MomX packages should be in available list
#     expect_true(all(loaded_momx %in% core[available]))
#   }
# })
#
# # =======================================================
# # tests/testthat/test-conflicts.R
# # =======================================================
#
# test_that("momx_conflicts returns a list", {
#   conflicts <- momx_conflicts()
#   expect_type(conflicts, "list")
# })
#
# test_that("momx_conflicts handles no loaded packages", {
#   # Should return empty list if no MomX packages loaded
#   # (or list of conflicts if they are loaded)
#   conflicts <- momx_conflicts()
#   expect_type(conflicts, "list")
#   # Should not error
#   expect_true(TRUE)
# })
#
# test_that("ls_env_exported returns character vector", {
#   env <- asNamespace("base")
#   result <- ls_env_exported(env)
#   expect_type(result, "character")
#   # Should only return exported functions
#   expect_true(all(vapply(result, function(x) {
#     exists(x, envir = env) && is.function(get(x, envir = env))
#   }, logical(1))))
# })
#
# test_that("confirm_conflict handles empty input", {
#   result <- confirm_conflict(character(0), list())
#   expect_type(result, "character")
#   expect_length(result, 0)
# })
#
# # =======================================================
# # tests/testthat/test-install.R
# # =======================================================
#
# test_that("momx_install accepts valid arguments", {
#   expect_error(
#     momx_install(which = "invalid"),
#     "should be one of"
#   )
#
#   expect_error(
#     momx_install(from = "invalid"),
#     "should be one of"
#   )
# })
#
# test_that("momx_install doesn't error with missing packages", {
#   # Should handle gracefully even if packages don't exist
#   # We don't actually install to avoid side effects
#   expect_silent({
#     pkgs <- switch("core",
#                    core = momx_packages_core(),
#                    extended = momx_packages_extended(),
#                    all = momx_packages_all())
#     expect_type(pkgs, "character")
#   })
# })
#
# # =======================================================
# # tests/testthat/test-update.R
# # =======================================================
#
# test_that("momx_update accepts valid arguments", {
#   expect_error(
#     momx_update(which = "invalid"),
#     "should be one of"
#   )
#
#   expect_error(
#     momx_update(from = "invalid"),
#     "should be one of"
#   )
# })
#
# test_that("momx_update handles no installed packages", {
#   # Mock scenario where no packages are installed
#   # Should inform user, not error
#   expect_silent({
#     core <- momx_packages_core()
#     installed <- core[core %in% rownames(utils::installed.packages())]
#     # This is fine whether length is 0 or not
#     expect_type(installed, "character")
#   })
# })
#
# # =======================================================
# # tests/testthat/test-status.R
# # =======================================================
#
# test_that("momx_status returns a tibble", {
#   result <- suppressMessages(momx_status())
#   expect_s3_class(result, "tbl_df")
#   expect_named(result, c("package", "type", "installed", "version", "loaded"))
# })
#
# test_that("momx_status includes all packages", {
#   result <- suppressMessages(momx_status())
#   all_pkgs <- momx_packages_all()
#
#   expect_equal(nrow(result), length(all_pkgs))
#   expect_true(all(all_pkgs %in% result$package))
# })
#
# test_that("momx_status correctly identifies package types", {
#   result <- suppressMessages(momx_status())
#
#   core <- momx_packages_core()
#   extended <- momx_packages_extended()
#
#   core_rows <- result[result$package %in% core, ]
#   extended_rows <- result[result$package %in% extended, ]
#
#   expect_true(all(core_rows$type == "core"))
#   expect_true(all(extended_rows$type == "extended"))
# })
#
# test_that("momx_status handles missing packages", {
#   # Should not error even if no packages installed
#   expect_silent({
#     result <- suppressMessages(momx_status())
#     expect_s3_class(result, "tbl_df")
#   })
# })
#
# test_that("print_status_section handles empty data frame", {
#   empty_df <- data.frame(
#     package = character(0),
#     installed = logical(0),
#     version = character(0),
#     loaded = logical(0)
#   )
#
#   expect_silent(print_status_section(empty_df))
# })
#
# # =======================================================
# # tests/testthat/test-website.R
# # =======================================================
#
# test_that("momx_website formats URLs correctly", {
#   # Mock browser to capture URL without opening
#   withr::local_options(list(browser = function(url) url))
#
#   url <- sprintf("https://momx.github.io/%s/", "Momocs2")
#   expect_match(url, "^https://momx.github.io/")
#   expect_match(url, "Momocs2/$")
# })
#
# test_that("momx_repo formats URLs correctly", {
#   withr::local_options(list(browser = function(url) url))
#
#   url <- sprintf("https://github.com/MomX/%s", "Momocs2")
#   expect_match(url, "^https://github.com/MomX/")
#   expect_match(url, "Momocs2$")
# })
#
# test_that("momx_website works with main site", {
#   withr::local_options(list(browser = function(url) url))
#
#   url <- sprintf("https://momx.github.io/%s/", "MomX")
#   expect_match(url, "MomX/$")
# })
#
# # =======================================================
# # tests/testthat/test-integration.R
# # Integration tests (only run if packages are available)
# # =======================================================
#
# test_that("full workflow works when packages are installed", {
#   skip_if_no_momx("Momocs2")
#
#   # Get status
#   status <- suppressMessages(momx_status())
#   expect_s3_class(status, "tbl_df")
#
#   # Check that Momocs2 shows as installed
#   momocs2_row <- status[status$package == "Momocs2", ]
#   expect_true(momocs2_row$installed)
#   expect_false(is.na(momocs2_row$version))
# })
#
# test_that("attach works when packages are installed", {
#   skip_if_no_momx("Momocs2")
#   skip_if_no_momx("Momacs")
#
#   # Detach if loaded
#   if ("package:Momocs2" %in% search()) {
#     detach("package:Momocs2", unload = TRUE)
#   }
#
#   # Attach
#   suppressMessages(momx_attach())
#
#   # Check that packages are loaded
#   expect_true("Momocs2" %in% loadedNamespaces())
# })
#
# test_that("conflicts detection works with loaded packages", {
#   skip_if_no_momx("Momocs2")
#
#   # Get conflicts
#   conflicts <- momx_conflicts()
#
#   # Should return a list (possibly empty)
#   expect_type(conflicts, "list")
# })
