#' Combines and exports GPS analysis tables
#'
#' @param tables.list list of GPS analysis tables
#' @param output.dir
#'
#' @return
#' @export
#'
#'
export_GPS_analysis <- function(tables.list, output.dir) {


  # Kinase
  kinase <- c()
  for (i in names(tables)) {
    kinase <- unique(c(kinase, names(tables[[i]][["kinase_table"]])))
  }

  kinase_table <- dplyr::tibble(Kinase = kinase)

  for (i in names(tables)) {

    kinase_table <- kinase_table %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_table"]]),
                                         Kinase = ".",
                                         !!i := Freq),
                       by = "Kinase") %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_table_p"]]),
                                         Kinase = ".",
                                         !!paste0(i, "(%)") := Freq),
                       by = "Kinase")

  }


  # Kinase group
  kinase_group <- c()
  for (i in names(tables)) {
    kinase_group <- unique(c(kinase_group, names(tables[[i]][["kinase_group_table"]])))
  }

  kinase_group_table <- dplyr::tibble(Group = kinase_group)

  for (i in names(tables)) {

    kinase_group_table <- kinase_group_table %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_group_table"]]),
                                         Group = ".",
                                         !!i := Freq),
                       by = "Group") %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_group_table_p"]]),
                                         Group = ".",
                                         !!paste0(i, "(%)") := Freq),
                       by = "Group")

  }

  # Kinase family
  kinase_family <- c()
  for (i in names(tables)) {
    kinase_family <- unique(c(kinase_family, names(tables[[i]][["kinase_family_table"]])))
  }

  kinase_family_table <- dplyr::tibble(Family = kinase_family)

  for (i in names(tables)) {

    kinase_family_table <- kinase_family_table %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_family_table"]]),
                                         Family = ".",
                                         !!i := Freq),
                       by = "Family") %>%
      dplyr::left_join(y = dplyr::rename(data.frame(tables[[i]][["kinase_family_table_p"]]),
                                         Family = ".",
                                         !!paste0(i, "(%)") := Freq),
                       by = "Family")

  }


  summary <- list(kinase = kinase_table,
                  kinase_group = kinase_group_table,
                  kinase_family = kinase_family_table)


  if (hasArg(output.dir)) {

    dir.create(output.dir, recursive = T)
    for (i in names(summary)) {
      openxlsx::write.xlsx(summary[[i]], paste0(output.dir, "/", i, ".xlsx"),
                           overwrite = T)
    }

  }

  return(invisible(summary))

}
