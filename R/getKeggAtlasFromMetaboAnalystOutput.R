#' Make a Kegg Atlas Heatmap from a MetaboAnalyst directory
#'
#' @description
#' This function will generate the KEGG atlas heatmap from a MetaboAnalyst Pathways enrichment analysis.
#'
#' @import scales
#' @param metaboanalyst_directory When you run MetaboAnalyst, download the entire zip folder and put the path here
#' @param paths_color_by Select the output you would like to color by.
#' @param paths_color_pallette Set color palette from RColorBrewer. For example: brewer.pal(7, "YlOrRd").
#' @param paths_significant_only Only map the significant paths and select a variable to filter by.
#' @param paths_significant_cutoff Choose a p-value cutoff.
#' @param paths_weight Add weight to the pathway lines in the format "W10".
#' @param paths_opacity Set opacity of highlighted lines. Value between 0 and 1.
#' @param include_compounds Map compounds in addition to pathways. Can be only significant or all.
#' @param compounds_color Put one color in hex form here for the compounds.
#' @param compounds_weight Add weight to the highlighted compounds in the format "W10"
#' @param compounds_opacity Set opacity of highlighted compounds. Value between 0 and 1.
#' @param generate_legend Boolean generates a legend in the R graphics window.
#' @param df A data frame with 1-4 columns. The first column should be 'KEGG.ID' (can be compounds "C00001" or paths "map00012"), then 'Color' in hex form, then 'Line.Width' in the from "W12", then 'Opacity' between 0 and 1.
#' @param highlight_path_names Boolean to bold the highlighted KEGG pathways.
#' @param module_names_font_color Color of the module labels (i.e. Carbohydrate metabolism, Vitamins and co-factors).
#' @param module_names_font_weight Weight of the module labels. Options are 'bold', 'normal', 'bolder', 'lighter', or a numeric between 1 and 1000.
#' @param module_names_background_color Change the background colors of the module labels. iPath defaults to colors of the rainbow. This allows you to insert a color in hex or color names (e.g. "grey") to change all the module fill colors at the same time.
#' @param module_names_background_stroke This allows you to put a boarder around the module label background. Default is no stroke.
#'
#' @export
#'
getKeggAtlasFromMetaboAnalystOutput <- function(
    metaboanalyst_directory,
    paths_color_by = "Enrichment", #c("Enrichment", "P.Fisher.", "P.EASE.", "P.Gamma.","AdjP.Fisher", "AdjP.EASE", "AdjP.Gamma"),
    paths_color_pallette = brewer.pal(7, "YlOrRd"),
    paths_significant_only = "all", # c("all", "P.Fisher.", "P.EASE.", "P.Gamma.","AdjP.Fisher", "AdjP.EASE", "AdjP.Gamma"),
    paths_significant_cutoff = 0.05,
    paths_weight = "W10",
    paths_opacity = 1, # between 1 and 0
    include_compounds = "significant", # c("none", "significant", "all"),
    compounds_color = "#00ff00",
    compounds_weight = "W10",
    compounds_opacity = 1,
    download_file_path = getwd(),
    download_file_name = "RiPath3 Kegg Atlas.png",
    highlight_path_names = TRUE,
    module_names_font_color = "#ffffff",
    module_names_font_weight = "bold",
    module_names_background_color = "Default",
    module_names_background_stroke = "Default",
    generate_legend = TRUE
) {

  # Read in paths
  df <- read.csv(paste0(metaboanalyst_directory, "/mummichog_pathway_enrichment_mummichog.csv"))



  # Get enrichment values ----

  # If enrichment then parse the scattermum.json file to get enrichment numbers append to df
  if (paths_color_by == "Enrichment") {

    # Read the json just the enrichment line and the pathways line
    myJson <- read.csv(
      paste0(metaboanalyst_directory, "/scattermum.json"),
      sep = ",",
      skip = 1,
      nrows = 2
      )

    # remove json artifacts
    myJson <- data.frame(
      lapply(myJson, function(x) gsub("(pathnames:)|(enr:)|\\[|\\]", "", x)),
      stringsAsFactors = FALSE
    )

    myJson <- t(myJson) # Transpose
    myJson <- as.data.frame(trimws(myJson)) # Trim white space
    colnames(myJson) <- c("Enrichment", "X") # Set column names
    myJson$Enrichment <- as.numeric(myJson$Enrichment)
    df <- merge(df, myJson, by = "X") # merge with the df

  }

  # If the user wants to plot only the significantly different pathways
  if (paths_significant_only != "all") {
    df <- df[which(df[,paths_significant_only] <= paths_significant_cutoff), ]
  }

  if (paths_color_by == "Enrichment") {
    # Set the color range
    color_gradient <- col_numeric(palette = paths_color_pallette, domain = range(df[, paths_color_by]))
  } else {
    # Reverse the color palette if using pvalue
    color_gradient <- col_numeric(palette = rev(paths_color_pallette), domain = range(df[, paths_color_by]))
  }

  # Create the data frame
  iPathDf <- data.frame(
    KEGG.ID = namesToKeggMaps(df$X),
    color = color_gradient(df[, paths_color_by]),
    weight = paths_weight,
    opacity = paths_opacity
    )



  # Get compound information ----
  # if compounds should be included, then rbind them to the iPathDf
  if (include_compounds == "significant") {

    # if significant only, then read from the pathway file
    compounds <- data.frame(
      KEGG.ID = unique(unlist(strsplit(paste0(df$cpd.hits, collapse = ";"), ";"))),
      color = compounds_color,
      weight = compounds_weight,
      opacity = compounds_opacity
      )

    iPathDf <- rbind(iPathDf, compounds)

  } else if (include_compounds == "all") {

    # if all then read from the mummichog compounds file
    compounds <- read.csv(paste0(metaboanalyst_directory, "/mummichog_matched_compound_all.csv"))
    compounds <- data.frame(
      KEGG.ID = unique(compounds$Matched.Compound),
      color = compounds_color,
      weight = compounds_weight,
      opacity = compounds_opacity
    )

    iPathDf <- rbind(iPathDf, compounds)

  }

  # Then run the getKeggMap function
  getKeggAtlas(
    iPathDf,
    download_file_path,
    download_file_name,
    highlight_path_names,
    module_names_font_color,
    module_names_font_weight,
    module_names_background_color,
    module_names_background_stroke
    )

  if (generate_legend) {
    if(paths_color_by == "Enrichment") {

      pathway_colors = colorRamp2(seq(min(df[, paths_color_by]), max(df[, paths_color_by]), length = length(paths_color_pallette)), paths_color_pallette)
      pathway_labels = signif(c(min(df[, paths_color_by]), ((max(df[, paths_color_by])-min(df[, paths_color_by]))/2) + min(df[, paths_color_by]), max(df[, paths_color_by])), 2)

    } else {

      pathway_colors = colorRamp2(seq(max(df[, paths_color_by]), min(df[, paths_color_by]), length = length(paths_color_pallette)), paths_color_pallette)
      pathway_labels = signif(c(max(df[, paths_color_by]), (max(df[, paths_color_by])-min(df[, paths_color_by]))/2, min(df[, paths_color_by])), 2)

    }

    generateKeggAtlasHeatmapLegend(
      compound_legend = ifelse(include_compounds == "none", "none", "discrete"),
      compound_legend_title = "Compounds",
      compound_legend_title_cex = 2,
      compound_colors = compounds_color,
      compound_labels = ifelse(include_compounds == "significant", "Significant", "Matched"),
      compound_labels_cex = 1.5,
      pathway_legend = 'continuous',
      pathway_legend_title = "Pathways",
      pathway_legend_title_cex = 2,
      pathway_colors = pathway_colors,
      pathway_labels = pathway_labels,
      pathway_labels_cex = 1.5
    )

  }
}

