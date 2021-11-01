#' @title Create a default header block
#'
#' @param title \code{(character)} Title to be wrapped in \link[shiny]{h2}
#' @param project \code{(character)} Project name to be wrapped in \link[shiny]{h4}
#' @param date_range \code{(Date)} vector to be collapsed with `" - "` in \link[shiny]{h4}
#'
#' @return \code{shiny.tag.list}
#' @export
#'
#' @examples
server_header <- function(title, project, date_range, ...) {
  out <- list()
  out$header <- shiny::h2(title)
  if (!missing(project))
    out$project <- shiny::h4(names(projects)[project == projects])
  if (!missing(date_range)) {
    out$dr <- purrr::when(date_range, length(.) > 1 ~ shiny::h4(paste0(.[1]," - ", .[2])),
                          ~ shiny::h4(.))
  }
  .dots <- rlang::dots_list(...)
  if (UU::is_legit(.dots))
    out <- append(out, .dots)
  do.call(tagList, out)
}


#' @title DT Datatable with some helpful defaults
#'
#' @inheritParams DT::datatable
#' @inheritDotParams DT::datatable
#'
#' @return \code{(shiny.tag)}
#' @export

datatable_default <- function(data,
                              rownames = FALSE,
                              options = list(dom = 'Bfrtip',
                                             buttons = c('copy', 'excel', 'csvHtml5'),
                                             responsive = TRUE,
                                             lengthMenu = c(10, 25, 50, 75, 100),
                                             lengthChange = TRUE,
                                             pageLength = 100),
                              filter = list(position = 'top',
                                            clear = TRUE,
                                            plain = FALSE),
                              extensions = "Buttons",
                              style = "bootstrap4",
                              elementId = NULL,
                              ...) {
  DT::datatable(
    data,
    rownames = rownames,
    filter = filter,
    options = options,
    extensions = extensions,
    style = style,
    elementId = elementId,
    ...
  )
}
