#' Shiny Link
#'
#' Create a link to an internal tab panel
#'
#' @param to page to navigate to
#' @param label text that describes the link
#'
#' @importFrom shiny tags
#' @export
shinyLink <- function(to, label) {
    tags$a(
        class = "shiny__link",
        href = to,
        label
    )
}
