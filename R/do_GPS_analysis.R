#' Combines GPS prediction with given set of pSites
#'
#' @param pSites vector of phosphorylation sites
#' @param name if of GPS results in databases
#'
#' @return
#' @export
#'
#'
do_GPS_analysis <- function(pSites, name = "table") {

  data_ <- list()

  check_database(id = "Kinases", type = "KinHub")
  kinase_table <- get_database(id = "Kinases", type = "KinHub")


  GPS_results <- get_database(id = name, type = "GPS_results") %>%
    dplyr::filter(pSite %in% pSites)


  # Collapse kinases for each pSite in list
  kinase_list <- list()

  for (i in unique(GPS_results[["pSite"]])) {
    kinase_list[[i]] <- GPS_results %>%
      dplyr::filter(pSite == i) %>%
      dplyr::pull(Kinase) %>%
      strsplit_(split = "/") %>%
      unique()
    #print(i)
  }




  # Kinases
  data_[["kinase"]] <- kinase_list %>%
    lapply(FUN = function(x) x[x %in% kinase_table[["xName"]]])

  data_[["kinase_table"]] <- data_[["kinase"]] %>%
    unlist() %>%
    table() %>%
    sort(decreasing = T)

  data_[["kinase_table_p"]] <- data_[["kinase_table"]] /
    length(kinase_list)


  # Kinase groups
  data_[["kinase_group"]] <- data_[["kinase"]] %>%
    lapply(FUN = function(x)
      unique(kinase_table[match(x, kinase_table$xName), "Group"]))

  data_[["kinase_group_table"]] <- data_[["kinase_group"]] %>%
    unlist() %>%
    table() %>%
    sort(decreasing = T)

  data_[["kinase_group_table_p"]] <- data_[["kinase_group_table"]] /
    length(kinase_list)

  # Kinase families
  data_[["kinase_family"]] <- data_[["kinase"]] %>%
    lapply(FUN = function(x)
      unique(kinase_table[match(x, kinase_table$xName), "Family"]))

  data_[["kinase_family_table"]] <- data_[["kinase_family"]] %>%
    unlist() %>%
    table() %>%
    sort(decreasing = T)

  data_[["kinase_family_table_p"]] <- data_[["kinase_family_table"]] /
    length(kinase_list)


  return(data_)


  # Weighted sum
  #
  # kinase.w <- kinase %>%
  #   lapply(FUN = function(x) {
  #     w <- rep(1/length(x), length(x))
  #     names(w) <- x
  #     return(w)
  #   })
  #
  # a <- kinase.w %>%
  #   unname() %>%
  #   unlist()
  #
  # a <- dplyr::tibble(kinase = names(a),
  #               w = a)
  #
  # a %>% collapse_rows(FUN = sum, by = "kinase") %>%
  #   data2vector() %>%
  #   sort(decreasing = T) %>%
  #   barplot()


  # groups <- kinase_list %>%
  #   lapply(FUN = function(x) x[x %in% kinase_table[["Group"]]]) %>%
  #   unlist() %>%
  #   table() %>%
  #   sort(decreasing = T) %>%
  #   plot()
  #
  # families <- kinase_list %>%
  #   lapply(FUN = function(x) x[x %in% kinase_table[["Family"]]]) %>%
  #   unlist() %>%
  #   table() %>%
  #   sort(decreasing = T) %>%
  #   plot()


}
