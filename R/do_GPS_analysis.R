#' Title
#'
#' @param pSites
#' @param name
#'
#' @return
#' @export
#'
#'
do_GPS_analysis <- function(pSites, name) {



  pSites_ <- intersect(pSites, unique(GPS_results[["pSite"]]))


  # Collapse kinases for each pSite in list
  kinase_list <- list()

  for (i in pSites_) {
    kinase_list[[i]] <- GPS_results %>%
      dplyr::filter(pSite == i) %>%
      dplyr::pull(Kinase) %>%
      strsplit_(split = "/") %>%
      unique()
    print(i)
  }


  groups <- kinase.list %>%
    lapply(FUN = function(x) x[x %in% kinase_table[["Group"]]])

  groups <- kinase.list %>%
    lapply(FUN = function(x) x[x %in% kinase_table[["HGNC Name"]]])


}
