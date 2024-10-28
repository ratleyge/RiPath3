#' Convert KEGG IDs to pathway names
#'
#' @param map_ids A vector of KEGG IDs
#' @param keep_all Boolean: TRUE keeps rows that were not found in the path to names data. FALSE removes these.
#' @param return_df Boolean: TRUE returns a data frame with 2 columns (i.e. the input vector and the matching pathway name). FALSE just returns the matching pathway vector
#'
#' @export
keggMapsToNames <- function(
    map_ids,
    keep_all = TRUE,
    return_df = FALSE
) {

  # read the path to map df
  load("data/pathToMap.Rda")

  if (length(map_ids[grep("map", map_ids)]) == 0) {
    stop(
      print("The function was not able find any KEGG maps in your input data. KEGG pathway IDs always start with 'map'. This function does not convert KEGG compound IDs.")
    )
  }

  paths <- data.frame(KEGG.ID = map_ids[grep("map", map_ids)])
  paths <- merge(paths, pathToMap, by = "KEGG.ID", all.x = TRUE)

  # Inform user if their paths are invalid
  if (nrow(paths[is.na(paths$Pathway), ]) > 0) {
    print(paste("Warning: Could not find", paste(paths[is.na(paths$Pathway), "KEGG.ID"], collapse = ", "), "in KEGG pathways."))
  }

  # Select only pathway names
  if(!keep_all) {
    paths <- paths[!is.na(paths$Pathway), ]
  }

  if(!return_df) {
    paths <- paths$Pathway
  }

  return(paths)

}
