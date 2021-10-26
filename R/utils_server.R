server_header <- function(title, project, date_range) {
  out <- list()
  out$header <- shiny::h2(title)
  if (!missing(project))
    out$project <- shiny::h4(names(projects)[project == projects])
  if (!missing(date_range))
    out$dr <- shiny::h4(paste0(date_range[1]," - ", date_range[2]))
  do.call(tagList, out)
}