#' Imports GPS 5.0 results file
#'
#' @param file path to GPS 5.0 results file
#' @param name (optional) identifier for imported data
#'
#' @return
#' @export
#'
#'
import_GPS_results <- function(file, name = "table") {

  # Choose file path of not given
  if (!hasArg(file)) file <- file.choose()

  # Read file
  GPS_results <- read.delim(file)

  # Find locations of protein breaks
  loc <- c(grep(">", GPS_results$Position), nrow(GPS_results) + 1)

  # vector to constrct protein column for table
  proteins <- c()

  # Add each proteins times number of pSites
  for (i in 1:(length(loc) - 1)) {
    proteins <- c(proteins,
                  rep(substring(GPS_results$Position[[loc[i]]], 2),
                      loc[i + 1] - loc[i]))
  }


  # Prepare dataframe and add pSite column
  GPS_results <- GPS_results %>%
    dplyr::mutate(Protein = proteins, .before = 1) %>%
    dplyr::filter(!grepl(">", Position)) %>%
    dplyr::arrange(Protein, Code, Position, -Score) %>%
    data2tibble() %>%
    dplyr::mutate(pSite = paste(Protein, Code, Position, sep = "_"),
                  .before = 1)



  # Save results
  add_database(GPS_results, id = name, type = "GPS_results")

}
