#' Subsets a given fasta file by given proteins for GPS 5.0 PTM site prediction
#'
#' @param proteins vector of proteins to export
#' @param fasta.file path to fasta file containing all sequences
#' @param output.dir path to folder where fasta file should be saved
#'
#' @return
#' @export
#'
#'
export_GPS_fasta <- function(proteins, fasta.file,
                             output.dir = "Data/GPS") {

  # Test protein input
  if (!hasArg(proteins)) {
    message("Please provide identifiers of the subset of proteins to use.")
    return(invisible(FALSE))
  }

  # Select fasta file manually if not given
  if (!hasArg(fasta.file)) {
    message("No fasta file given, please select manually.")
    fasta.file <- file.choose()
  }

  # import fasta file
  fasta <- Biostrings::readAAStringSet(
    filepath = fasta.file)

  # Match given proteins to fasta headers
  entries <- sapply(X = names(fasta),
                    FUN = function(x) strsplit_(x, "\\|")[2], USE.NAMES = F)

  # Subset fasta file
  fasta.export <- fasta[match(proteins, entries)]


  # Test found proteins
  if (length(fasta.export) == 0) {
    message("No given proteins found in fasta file.")
    return(invisible(FALSE))
  } else if (length(fasta.export) < length(proteins)) {
    message("Not all proteins found in fasta file. Proceeding with export.")
  }

  # Name fasta headers (simplifying to given identifier)
  names(fasta.export) <- proteins

  # Create output directory
  if (!dir.exists(output.dir)) {
    dir.create(output.dir, recursive = TRUE)
  }

  # Write fasta file
  Biostrings::writeXStringSet(x = fasta.export,
                              filepath = paste0(output.dir,
                                                "/GPS_sequences.fasta"))
  # Test export
  if (file.exists(paste0(output.dir,
                         "/GPS_sequences.fasta"))) {
    message(paste0("Protein sequences sucessfully exported to ", output.dir,
                   "/GPS_sequences.fasta."))
            return(invisible(TRUE))
  # Error
  } else {
    message(paste0("Something went wrong. Please check fasta file, ",
                   "proteins or output directory."))
    return(invisible(TRUE))
  }

}
