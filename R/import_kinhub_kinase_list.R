#' Imports list of kinases from kinhub
#'
#' @param url url of KinHub kinase table
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
import_KinHub_kinase_list <- function(
  url = "http://www.kinhub.org/kinases.html") {

  kinase_table <- RCurl::getURL(url = url,
                                .opts = list(ssl.verifypeer = FALSE)) %>%
    XML::readHTMLTable() %>%
    purrr::pluck(1)

  if (is.null(kinase_table)) {
    message("Table could not be imported. Please check the url.")
    return(invisible(FALSE))
  }

  else {
    names(kinase_table) <- keep_first(names(kinase_table), split = "\n")
    add_database(database = kinase_table,
                 id = "Kinases",
                 type = "KinHub")
    return(invisible(TRUE))
  }


}
