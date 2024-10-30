#' Convert pathway names to Kegg IDs
#'
#' @param path_names A vector of pathway names
#' @param keep_all Boolean: TRUE keeps rows that were not found in the path to names data. FALSE removes these.
#' @param return_df Boolean: TRUE returns a data frame with 2 columns (i.e. the input vector and the matching KEGG.IDs). FALSE just returns the matching KEGG.ID vector
#'
#' @export
namesToKeggMaps <- function(
    path_names,
    keep_all = TRUE,
    return_df = FALSE
    ) {

  # read the path to map df
  data("pathToMap")

  paths <- data.frame(Pathway = path_names)
  paths <- merge(paths, pathToMap, by = "Pathway", all.x = TRUE)

  # Inform user if their paths are invalid
  if (nrow(paths[is.na(paths$KEGG.ID), ]) > 0) {
    print(paste0("Warning: Could not find '", paste(paths[is.na(paths$KEGG.ID), "Pathway"], collapse = ", "), "' in KEGG pathways."))
  }

  # Select only pathway names
  if(!keep_all) {
    paths <- paths[!is.na(paths$Pathway), ]
  }

  if(!return_df) {
    paths <- paths$KEGG.ID
  }

  return(paths)

}
