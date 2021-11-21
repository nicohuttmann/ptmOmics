#' Extract phosphosite in standard format from PhospoSitePlus data frame
#'
#' @param x data frame from PhosphoSitePlus
#'
#' @return
#' @export
#'
#'
extract_pSites <- function(x) {

x %>%
    dplyr::filter(grepl("-p", MOD_RSD)) %>%
    dplyr::mutate(pSites = paste(ACC_ID,
                                 substring(MOD_RSD, 1, 1),
                                 keep_first(substring(MOD_RSD, 2), split = "-"),
                                 sep = "_")) %>%
    dplyr::pull()


}
