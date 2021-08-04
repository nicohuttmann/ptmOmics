#' Imports list of kinases from kinhub
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
import_kinhub_kinase_list <- function(
  url = "http://www.kinhub.org/kinases.html") {

  kinase_table <- RCurl::getURL(url = url,
                                .opts = list(ssl.verifypeer = FALSE)) %>%
    XML::readHTMLTable() %>%
    purrr::pluck(1)

  if (is.null) {
    message("Table could not be imported. Please check the url.")
    return(invisible(FALSE))
  }

  else {
    names(kinase_table) <- keep_first(names(kinase_table), sep = "\n")
    add_database(id = "Kinases", type = "Kinhub")
    return(invisible(TRUE))
  }


}
