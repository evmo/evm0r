#' Create File Request via Dropbox API
#'
#' @param token
#' @param folder_title
#' @param folder_path
#'
#' @return URL string
#' @importFrom httr content POST add_headers
#' @export
#'
#' @examples
dbx_create_file_req <- function(folder_title, folder_path, token) {
  resp <- content(POST(
    url = "https://api.dropboxapi.com/2/file_requests/create",
    add_headers(
      Authorization = token,
      `User-Agent` = "R"
    ),
    body = list(
      title = folder_title,
      destination = folder_path
    ),
    encode = "json"
  ))

  resp$url
}
