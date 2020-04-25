#' Parse RSS feed, write titles & links to file
#'
#' @param url RSS feed url
#' @param out_path file path to write
#'
#' @return
#' @importFrom xml2 read_xml xml_find_all xml_text '%>%'
#' @export
#'
#' @examples
parseRSS <- function(url, out_path) {
    feed 	<- read_xml(url)
    titles 	<- feed %>%
      xml_find_all('//title') %>%
      xml_text() %>%
      as.character

    links <- feed %>%
      xml_find_all('//link') %>%
      xml_text() %>%
      as.character

    df <- data.frame(titles, links)[3:7, ]
    df$text <- sprintf("<li><a href='%s'>%s</a></li>", df$links, df$titles)

    writeLines(df$text, out_path)
}
