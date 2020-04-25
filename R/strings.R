#' URL-ify a string
#'
#' @param string
#'
#' @return an URL-formatted string
#' @importFrom magrittr '%>%'
#' @export
urlify <- function(string) {
  stringi::stri_trans_general(string, "latin-ascii") %>%
    gsub(" ", "-", .) %>%
    gsub("[\\. ,'\\(\\)]", "", .) %>%
    gsub("-{2,}", "-", .) %>%
    tolower
}

#' Translate string to ASCII
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
asciify <- function(string) {
  stringi::stri_trans_general(string, "latin-ascii")
}
