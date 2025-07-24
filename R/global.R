# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.
#' @include utils_helpers.R
Sys.setenv(TZ = "America/New_York")

clean_null <- function(files) {
  .rds <- stringr::str_subset(files, "rds$")
  .sizes <- file.size(.rds)
  file.remove(.rds[.sizes == 44])
  files[!files %in% .rds[.sizes == 44]]
}

# accessor_create <- function(.x, do_update) rlang::new_function(args =
#                                                                  rlang::pairlist2(
#                                                                    path = rlang::expr(!!.x),
#                                                                    dep_update = maleta::update_dropbox,
#                                                                    do_update = rlang::expr(!!do_update),
#                                                                    ... = ,
#                                                                  ),
#                                                                body = base::quote({
#                                                                  if (do_update)
#                                                                    dep_update(path)
#                                                                  UU::file_fn(path)(path, ...)
#                                                                }))

# accessor_create <- function(.x, do_update) {
#   cat("Creating accessor for path:", .x, "\n")
#   
#   rlang::new_function(
#     args = rlang::pairlist2(
#       path = rlang::expr(!!.x),
#       dep_update = maleta::update_dropbox,
#       do_update = rlang::expr(!!do_update),
#       ... = ,
#     ),
#     body = rlang::expr({
#       cat("Accessor function called with path:", path, "\n")
#       
#       if (is.null(path) || length(path) == 0 || path == "") {
#         stop("Invalid path in accessor function: ", path)
#       }
#       
#       if (do_update)
#         dep_update(path)
#       
#       file_func <- UU::file_fn(path)
#       file_func(path, ...)
#     })
#   )
# }

accessor_create <- function(.x, do_update) {
  cat("Creating accessor for path:", .x, "\n")
  
  rlang::new_function(
    args = rlang::pairlist2(
      path = rlang::expr(!!.x),
      dep_update = maleta::update_dropbox,
      do_update = rlang::expr(!!do_update),
      ... = ,
    ),
    body = rlang::expr({
      cat("Accessor function called with path:", path, "\n")
      
      if (is.null(path) || length(path) == 0 || path == "") {
        stop("Invalid path in accessor function: ", path)
      }
      
      if (do_update)
        dep_update(path)
      
      # Check file extension and handle differently
      if (tools::file_ext(path) == "rds") {
        # For .rds files, maybe use readRDS directly?
        readRDS(path)
      } else {
        file_func <- UU::file_fn(path)
        file_func(path, ...)
      }
    })
  )
}

create_accessors <- function(dep_dir = "data", deps = NULL, dep_update = maleta::dep_update_dropbox, update_all = TRUE) {
  if (is.null(deps))
    deps <- clean_null(UU::list.files2(dep_dir)) |>
      stringr::str_subset("\\.png$", negate = TRUE)
  deps <- fs::path_abs(deps)
  UU::mkpath(dep_dir)
  if (update_all)
    dep_update(deps)
  accessor_funs <- purrr::map(rlang::set_names(deps, fs::path_ext_remove(basename(deps))), accessor_create, do_update = !update_all)
  do_assignment(accessor_funs)
}

do_assignment <- function(funs, ns = pkgload::pkg_name()) {
  if (UU::is_legit(try(ns, silent = TRUE))) {
    namespace <- rlang::ns_env(ns)
    
    purrr::iwalk(funs, ~{
      # Unlock the specific binding if it exists
      if (exists(.y, envir = namespace, inherits = FALSE))
        rlang::env_binding_unlock(namespace, .y)
      
      # Assign to namespace
      assign(.y, .x, envir = namespace)
      assignInNamespace(.y, .x, ns, envir = namespace)
      
      # Lock the binding again
      rlang::env_binding_lock(namespace, .y)
    })
  } else
    funs
}

# Debug: Check what files are found
files <- UU::list.files2("data")
print(paste("Found files:", paste(files, collapse = ", ")))

.time <- system.time({
  create_accessors("data")
})
# Create accessor functions

# rm_dates() <- HMISprep::load_dates()

if (exists("validation")) {
  programs <- validation() |>
    dplyr::distinct(ProjectID, ProjectName) |>
    dplyr::arrange(ProjectName) |> 
      {\(x) {rlang::set_names(x$ProjectID, x$ProjectName)}}()
  
    
  
}
  
if (exists("Regions")) {
  regions <- Regions() |> 
    dplyr::distinct(Region, RegionName) |> 
    {\(x) {rlang::set_names(x$Region, x$RegionName)}}()
  counties <- sort(Regions()$County)
  qpr_tab_choices <- regions |>
    {\(x) {list(
      community_need_ph = list(
        choices = x
      ),
      community_need_lh = list(
        choices = x
      ),
      length_of_stay = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0, 1, 2, 8, 13)])
      ),
      permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(0:4, 8:9, 12:13)])
      ),
      temp_permanent_housing = list(
        choices = unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(4)])
      ),
      noncash_benefits = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      health_insurance = list(
        choices = unique(qpr_benefits()$ProjectName)
      ),
      income_growth = list(
        choices = unique(qpr_income()$ProjectName)
      ),
      rrh_placement = list(
        choices = unique(sort(
          qpr_rrh_enterers()$ProjectName
        ))
      ),
      reentries = list(
        choices = unique(sort(
          qpr_reentries()$ExitingHP
        ))
      ),
      rrh_spending = list(
        choices = unique(sort(
          qpr_spending()$OrganizationName
        ))
      )
    )}}()
  
}



