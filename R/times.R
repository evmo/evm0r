#' HH:MM:SS to Seconds
#'
#' @param hms a time (HH:MM:SS)
#'
#' @return seconds (integer)
#' @export
#'
#' @examples
hms2sec <- function(hms) {
  H <- as.integer(strsplit(hms, ":")[[1]][1])
  M <- as.integer(strsplit(hms, ":")[[1]][2])
  S <- as.integer(strsplit(hms, ":")[[1]][3])
  H*3600 + M*60 + S
}

#' Seconds to c(H, M, S)
#'
#' @param seconds
#'
#' @return vector c(H, M, S)
#' @export
#'
#' @examples
sec2hms_v <- function(seconds) {
  hrs <- seconds %/% 3600
  min <- (seconds - hrs * 3600) %/% 60
  sec <- seconds - (min * 60 + hrs * 3600)
  c(hrs, min, sec)
}

#' Seconds to HH:MM:SS
#'
#' @param seconds integer
#'
#' @return time (HH:MM:SS) string
#' @export
#'
#' @examples
sec2hms <- function(seconds) {
  hms <- sec2hms_v(seconds)
  sprintf("%02.f:%02.f:%02.f", hms[1], hms[2], hms[3])
}

#' Sum some elapsed times
#'
#' @param ...
#'
#' @return string HH:MM:SS
#' @export
#'
#' @examples
sum_HMS <- function(...) {
  sec2hms(sum(sapply(list(...), function(x) hms2sec(x))))
}
