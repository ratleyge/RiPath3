#' Make a Kegg Atlas Heatmap from a MetaboAnalyst directory
#'
#' @description
#' This function will generate the KEGG atlas heatmap from a MetaboAnalyst Pathways enrichment analysis.
#'
#' @import scales
#' @param metaboanalyst_directory When you run MetaboAnalyst, download the entire zip folder and put the path here.
#' @param paths_color_by Select the output you would like to color by.
#' @param paths_color_pallette Set color palette from RColorBrewer. For example: brewer.pal(7, "YlOrRd").
#' @param paths_significant_only Only map the significant paths and select a variable to filter by.
#' @param paths_significant_cutoff Choose a p-value cutoff.
#' @param paths_weight Add weight to the pathway lines in the format "W10".
#' @param paths_opacity Set opacity of highlighted lines. Value between 0 and 1.
#' @param include_compounds Map compounds in addition to pathways. Can be only significant or all.
#' @param compounds_color Put one color in hex form here for the compounds.
#' @param compounds_weight Add weight to the highlighted compounds in the format "W10".
#' @param compounds_opacity Set opacity of highlighted compounds. Value between 0 and 1.
#' @param generate_legend Boolean generates a legend in the R graphics window.
#' @param highlight_path_names Boolean to bold the highlighted KEGG pathways.
#' @param module_styles_default A list of style properties to apply to all modules at once.
#'   Valid fields are \code{bg}, \code{font}, \code{stroke}, and \code{font_weight}.
#'   Example: \code{list(bg = "grey", font = "black")}.
#' @param module_styles A named list of per-module style overrides, applied on top of
#'   \code{module_styles_default}. Names must match KEGG subcategory names. Each entry
#'   is a list with any of the following optional fields: \code{bg}, \code{font},
#'   \code{stroke}, \code{font_weight}.
#'   Example: \code{list("Amino acid metabolism" = list(bg = "#00234b", font = "white"))}.
#'
#' @export
#'
getKeggAtlasFromMetaboAnalystOutput <- function(
    metaboanalyst_directory,
    paths_color_by = "Enrichment",
    paths_color_pallette = brewer.pal(7, "YlOrRd"),
    paths_significant_only = "all",
    paths_significant_cutoff = 0.05,
    paths_weight = "W10",
    paths_opacity = 1,
    include_compounds = "significant",
    compounds_color = "#00ff00",
    compounds_weight = "W10",
    compounds_opacity = 1,
    download_file_path = getwd(),
    download_file_name = "RiPath3 Kegg Atlas.png",
    highlight_path_names = TRUE,
    module_styles_default = list(),
    module_styles = list(),
    generate_legend = TRUE,
    compound_legend_title = "MetaboAnalyst \nCompounds",
    pathway_legend_title = ifelse(paths_color_by == "Enrichment", "Pathway \nEnrichment", "Pathway Significance")
) {

  # Read in paths
  df <- read.csv(paste0(metaboanalyst_directory, "/mummichog_pathway_enrichment_mummichog.csv"))


  # Get enrichment values ----
  if (paths_color_by == "Enrichment") {

    myJson <- read.csv(
      paste0(metaboanalyst_directory, "/scattermum.json"),
      sep = ",",
      skip = 1,
      nrows = 2
    )

    myJson <- data.frame(
      lapply(myJson, function(x) gsub("(pathnames:)|(enr:)|\\[|\\]", "", x)),
      stringsAsFactors = FALSE
    )

    myJson <- t(myJson)
    myJson <- as.data.frame(trimws(myJson))
    colnames(myJson) <- c("Enrichment", "X")
    myJson$Enrichment <- as.numeric(myJson$Enrichment)
    df <- merge(df, myJson, by = "X")

  }

  if (paths_significant_only != "all") {
    df <- df[which(df[, paths_significant_only] <= paths_significant_cutoff), ]
  }

  if (paths_color_by == "Enrichment") {
    color_gradient <- col_numeric(palette = paths_color_pallette, domain = range(df[, paths_color_by]))
  } else {
    color_gradient <- col_numeric(palette = rev(paths_color_pallette), domain = range(df[, paths_color_by]))
  }

  iPathDf <- data.frame(
    KEGG.ID = namesToKeggMaps(df$X),
    color   = color_gradient(df[, paths_color_by]),
    weight  = paths_weight,
    opacity = paths_opacity
  )


  # Get compound information ----
  if (include_compounds == "significant") {

    compounds <- data.frame(
      KEGG.ID = unique(unlist(strsplit(paste0(df$cpd.hits, collapse = ";"), ";"))),
      color   = compounds_color,
      weight  = compounds_weight,
      opacity = compounds_opacity
    )
    iPathDf <- rbind(iPathDf, compounds)

  } else if (include_compounds == "all") {

    compounds <- read.csv(paste0(metaboanalyst_directory, "/mummichog_matched_compound_all.csv"))
    compounds <- data.frame(
      KEGG.ID = unique(compounds$Matched.Compound),
      color   = compounds_color,
      weight  = compounds_weight,
      opacity = compounds_opacity
    )
    iPathDf <- rbind(iPathDf, compounds)

  }


  # Run getKeggAtlas ----
  getKeggAtlas(
    iPathDf,
    download_file_path    = download_file_path,
    download_file_name    = download_file_name,
    highlight_path_names  = highlight_path_names,
    module_styles_default = module_styles_default,
    module_styles         = module_styles
  )


  # Generate legend ----
  if (generate_legend) {

    if (paths_color_by == "Enrichment") {
      pathway_colors <- colorRamp2(seq(min(df[, paths_color_by]), max(df[, paths_color_by]), length = length(paths_color_pallette)), paths_color_pallette)
      pathway_labels <- signif(c(min(df[, paths_color_by]), ((max(df[, paths_color_by]) - min(df[, paths_color_by])) / 2) + min(df[, paths_color_by]), max(df[, paths_color_by])), 2)
    } else {
      pathway_colors <- colorRamp2(seq(max(df[, paths_color_by]), min(df[, paths_color_by]), length = length(paths_color_pallette)), paths_color_pallette)
      pathway_labels <- signif(c(max(df[, paths_color_by]), ((max(df[, paths_color_by]) - min(df[, paths_color_by])) / 2) + min(df[, paths_color_by]), min(df[, paths_color_by])), 2)
    }

    Sys.sleep(1)

    generateKeggAtlasHeatmapLegend(
      compound_legend       = ifelse(include_compounds == "none", "none", "discrete"),
      compound_legend_title = compound_legend_title,
      compound_legend_title_cex = 1.8,
      compound_colors       = compounds_color,
      compound_labels       = ifelse(include_compounds == "significant", "Significant", "Matched"),
      compound_labels_cex   = 1.5,
      pathway_legend        = "continuous",
      pathway_legend_title  = pathway_legend_title,
      pathway_legend_title_cex = 1.8,
      pathway_colors        = pathway_colors,
      pathway_labels        = pathway_labels,
      pathway_labels_cex    = 1.5
    )

  }
}
