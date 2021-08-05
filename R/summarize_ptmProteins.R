#' Summarize modified peptides and modification sites per proteinGroup
#'
#' @param proteinGroups proteinGroups
#' @param modificationSpecificPeptides modificationSpecificPeptides
#' @param sites sites
#' @param observations observations
#' @param group.by observatrions data to groups by
#' @param replicates observations data for biological replicates etc.
#' @param replicates.min min fraction to be considered identified
#' @param modification name of modification site dataset
#' @param modification.abbr abbreviation to use for modification (e.g. 'p')
#' @param min.localization.prob localization probability threshold (e.g. 0.75)
#'
#' @return
#' @export
#'
#'
summarize_ptmProteins <- function(proteinGroups,
                                  modificationSpecificPeptides,
                                  sites,
                                  observations,
                                  replicates = "groups",
                                  replicates.min = 2/3,
                                  group.by = "cell.line",
                                  modification = "Phospho (STY)",
                                  modification.abbr = "p",
                                  min.localization.prob = 0.75,
                                  add.info = c("Gene.names", "Protein.names")) {


  # Modification specific dataset
  sites_dataset <- paste0(modification, "Sites")



  # Get all proteins to be included
  proteinGroups <- get_variables(variables = proteinGroups,
                                 dataset = "proteinGroups")

  # Add protein information
  genes <- get_variables_data(which = "Gene.names",
                              variables = proteinGroups,
                              dataset = "proteinGroups")
  protein.names <- get_variables_data(which = "Protein.names",
                                      variables = proteinGroups,
                                      dataset = "proteinGroups")



  # Prepare main dataframe
  summary.proteins <- tibble::tibble(
    variables = proteinGroups)
    #!!paste0(modification.abbr, "Peptides_total") := NA_integer_,
    #!!paste0(modification.abbr, "Sites_total") := NA_integer_)


  # Get observations to include
  observations <- get_observations(observations = observations,
                                   observations.set = ,
                                   dataset = "proteinGroups")




  # Get all modification specific peptides
  modificationSpecificPeptides_ <-
    get_data(which = "Experiment",
             variables = modificationSpecificPeptides,
             observations = observations,
             dataset = "modificationSpecificPeptides") %>%
    # Evaluate Experiment data by is.na()
    eval_data(ifelse(is.na(x), F, T)) %>%
    # Include replicates information if given
    {if (hasArg(replicates))
         include_observations_data(data_ = .,
                                   which = replicates,
                                   dataset = "modificationSpecificPeptides")
         else .} %>%
    # Include group information if given
    {if (hasArg(group.by))
         include_observations_data(data_ = .,
                                   which = group.by,
                                   dataset = "modificationSpecificPeptides")
         else .} %>%
    # Combine replicates if given
    {if (hasArg(replicates))
         collapse_rows(FUN = function(x) mean(x) >= replicates.min,
                       by = replicates)
         else .} %>%
    # Combine groups if given
    {if (hasArg(group.by))
         collapse_rows(FUN = any, by = group.by)
         else .} %>%
    # Transpose data frame and add proteinGroups information to sum peptides
    transpose_data() %>%
    include_variables_data(which = "Protein.Groups",
                           dataset = "modificationSpecificPeptides") %>%
    collapse_rows(FUN = sum, by = "Protein.Groups") %>%
    do_something(dplyr::rename, id = observations)







  # Get all modification sites
  sites_ <-
    get_data(which = "Localization.prob",
             variables = sites,
             observations = observations,
             dataset = sites_dataset) %>%
    # Replace na with 0
    eval_data(ifelse(is.na(x), 0, x)) %>%
    # Test localization probability
    eval_data(x >= min.localization.prob) %>%
    # Include replicates information if given
    {if (hasArg(replicates))
         include_observations_data(data_ = .,
                                   which = replicates,
                                   dataset = sites_dataset)
         else .} %>%
    # Include group information if given
    {if (hasArg(group.by))
         include_observations_data(data_ = .,
                                   which = group.by,
                                   dataset = sites_dataset)
         else .} %>%
    # Combine replicates if given
    {if (hasArg(replicates))
         collapse_rows(FUN = function(x) mean(x) >= replicates.min,
                       by = replicates)
         else .} %>%
    # Combine groups if given
    {if (hasArg(group.by))
         collapse_rows(FUN = any, by = group.by)
         else .} %>%
    # Transpose data frame and add proteinGroups information to sum mod sites
    transpose_data() %>%
    include_variables_data(which = "Protein.group.IDs",
                           dataset = sites_dataset) %>%
    collapse_rows(FUN = sum, by = "Protein.group.IDs") %>%
    do_something(dplyr::rename, id = observations)


  # Combine dataframes
  summary.proteins <- summary.proteins %>%
    include_variables_data("id", dataset = "proteinGroups") %>%
    dplyr::left_join(modificationSpecificPeptides_[["Experiment"]],
                     by = "id") %>%
    dplyr::rename_with(function(x) paste0(modification.abbr, "Peptides_", x),
                       dplyr::all_of(names(sites_[[1]])[-1])) %>%
    dplyr::left_join(sites_[["Localization.prob"]],
                     by = "id") %>%
    dplyr::rename_with(function(x) paste0(modification.abbr, "Sites_", x),
                       dplyr::all_of(names(sites_[[1]])[-1])) %>%
    dplyr::select(-id)



  # Add protein information
  genes <- get_variables_data(which = "Gene.names",
                              variables = proteinGroups,
                              dataset = "proteinGroups")
  protein.names <- get_variables_data(which = "Protein.names",
                                      variables = proteinGroups,
                                      dataset = "proteinGroups")



  # Prepare main dataframe
  for (i in add.info) {
    summary.proteins <- summary.proteins %>%
      dplyr::mutate(!!i := get_variables_data(which = i,
                                              variables = summary.proteins[[1]],
                                              dataset = "proteinGroups"))
  }

  # Rename variables to Proteins
  summary.proteins <- summary.proteins %>%
    dplyr::rename(Proteins = variables)

  # Return
  return(summary.proteins)

}
