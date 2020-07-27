#' Test if a string is a valid date
#'
#' @param a_date
#'
#' @return boolean
#' @export
#'
#' @examples
is_date <- function(a_date) {
  tryCatch(!is.na(as.Date(a_date)),
           error = function(err) {FALSE})
}

#' Format date
#'
#' @param Y
#' @param M
#' @param D
#'
#' @return string, e.g. 1980 Feb 11
#' @export
#'
#' @examples
date_fmt <- function(Y, M, D) {
  paste(Y, substr(month.name[M], 1, 3), D)
}

#' Subset valid dates
#'
#' @param dates vector of dates
#'
#' @return
#' @export
#'
#' @examples
valid_dates <- function(dates) {
  as.Date(dates[grepl("\\d{4}-\\d{2}-\\d{2}", dates)])
}

#' Find closest 3 dates from 3 lists
#'
#' @param dates1 vector of dates
#' @param dates2 vector of dates
#' @param dates3 vector of dates
#'
#' @return list: 3 dates and the range (days)
#' @export
#'
#' @examples
closest3dates <- function(dates1, dates2, dates3) {
  dates1 <- valid_dates(dates1)
  dates2 <- valid_dates(dates2)
  dates3 <- valid_dates(dates3)

  # get num. dates in each set
  len1 <- length(dates1)
  len2 <- length(dates2)
  len3 <- length(dates3)

  # sort dates
  dates1 <- sort(dates1)
  dates2 <- sort(dates2)
  dates3 <- sort(dates3)

  theDiff <- 99999999999

  index1 <- 1
  index2 <- 1
  index3 <- 1
  res1 <- 1
  res2 <- 1
  res3 <- 1

  while (index1 <= len1 && index2 <= len2 && index3 <= len3) {
    theMin <- min(dates1[index1], min(dates2[index2], dates3[index3]))
    theMax <- max(dates1[index1], max(dates2[index2], dates3[index3]))

    if (theMax - theMin < theDiff) {
      res1 <- index1
      res2 <- index2
      res3 <- index3
      theDiff <- theMax - theMin
    }

    if (theDiff == 0) break

    if (dates1[index1] == theMin)
      index1 <- index1 + 1
    else if (dates2[index2] == theMin)
      index2 <- index2 + 1
    else
      index3 <- index3 + 1
  }

  ret1 <- dates1[res1]
  ret2 <- dates2[res2]
  ret3 <- dates3[res3]

  list(
    ev1 = ret1,
    ev2 = ret2,
    ev3 = ret3,
    days = max(ret1, ret2, ret3) - min(ret1, ret2, ret3)
  )
}

#' Find furthest 3 dates from 3 lists
#'
#' @param dates1 vector of dates
#' @param dates2 vector of dates
#' @param dates3 vector of dates
#'
#' @return list: 3 dates and the range (days)
#' @export
#'
#' @examples
furthest3dates <- function(dates1, dates2, dates3) {
  dates1 <- valid_dates(dates1)
  dates2 <- valid_dates(dates2)
  dates3 <- valid_dates(dates3)

  # get num. dates in each set
  len1 <- length(dates1)
  len2 <- length(dates2)
  len3 <- length(dates3)

  # sort dates
  dates1 <- sort(dates1)
  dates2 <- sort(dates2)
  dates3 <- sort(dates3)

  theDiff <- 99999999999

  index1 <- 1
  index2 <- 1
  index3 <- 1
  res1 <- 1
  res2 <- 1
  res3 <- 1

  while (index1 <= len1 && index2 <= len2 && index3 <= len3) {
    theMin <- min(dates1[index1], min(dates2[index2], dates3[index3]))
    theMax <- max(dates1[index1], max(dates2[index2], dates3[index3]))

    if (theMax - theMin > theDiff) {
      res1 <- index1
      res2 <- index2
      res3 <- index3
      theDiff <- theMax - theMin
    }

    if (theDiff == 0) break

    if (dates1[index1] == theMin)
      index1 <- index1 + 1
    else if (dates2[index2] == theMin)
      index2 <- index2 + 1
    else
      index3 <- index3 + 1
  }

  ret1 <- dates1[res1]
  ret2 <- dates2[res2]
  ret3 <- dates3[res3]

  list(
    ev1 = ret1,
    ev2 = ret2,
    ev3 = ret3,
    days = max(ret1, ret2, ret3) - min(ret1, ret2, ret3)
  )
}
