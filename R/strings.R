#' URL-ify a string
#'
#' @param string
#'
#' @return an URL-formatted string
#' @export
urlify <- function(string) {
  s <- asciify(string)
  s <- gsub(" ", "-", s)
  s <- gsub("[\\. ,'\\(\\)]", "", s)
  s <- gsub("-{2,}", "-", s)
  tolower(s)
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

#' Generate random string
#'
#' @param n number of characters
#'
#' @return
#' @export
#'
#' @examples
randStr <- function(n) {
  str <- vector(mode = 'character', length = n)
  pool <- c(letters, LETTERS, c(0:9))
  for (i in 1:n)
    str[i] <- sample(pool, 1)
  paste(str, collapse = '')
}
