#' Make a legend for the KEGG Atlas heatmap
#'
#' @import ComplexHeatmap
#' @import RColorBrewer
#' @import circlize
#' @description
#' Draws a legend that you can add to the figure. If the legend is getting clipped, expand your window.
#' @param compound_legend Draws a legend for compounds. Can be continuous or discrete.
#' @param compound_legend_title Legend title as string.
#' @param compound_colors For discrete, this takes a vector list of colors. For continuous, this takes a colorRamp2 object (e.g. colorRamp2(c(1:7), brewer.pal(7, "YlOrRd")))
#' @param compound_labels For discrete, this takes a vector of the same length and order as the colors. For continuous, this takes a vector of the labels you want.
#' @param pathway_legend Draws a legend for pathways. Can be continuous or discrete.
#' @param pathway_legend_title Legend title as string
#' @param pathway_colors For discrete, this takes a vector list of colors. For continuous, this takes a colorRamp2 object (e.g. colorRamp2(c(1:7), brewer.pal(7, "YlOrRd")))
#' @param pathway_labels For discrete, this takes a vector of the same length and order as the colors. For continuous, this takes a vector of the labels you want.
#' @examples
#' generateKeggAtlasHeatmapLegend(pathway_legend = "continuous", compound_legend = "discrete")
#'
#' generateKeggAtlasHeatmapLegend(
#'   pathway_legend = "discrete",
#'   pathway_colors = c("red", "blue"),
#'   pathway_labels = c("Up", "Down"),
#'   compound_legend = "continuous",
#'   compound_colors = colorRamp2(c(1:7), brewer.pal(7, "YlOrRd")),
#'   compound_labels = c(1, 4, 7)
#' )
#' @export
generateKeggAtlasHeatmapLegend <- function(
    compound_legend = 'none', # options are 'none', 'continuous', and 'discrete'
    compound_legend_title = "Compounds",
    compound_legend_title_cex = 2,
    compound_colors = c("#00cc33", "#0018A9", "#333333"), # takes a vector of colors
    compound_labels = c("Upregulated", "Downregulated", "Conflicting"), # must match colors if discrete
    compound_labels_cex = 1.5,
    pathway_legend = 'none', # options are 'none', 'continuous', and 'discrete'
    pathway_legend_title = "Pathways",
    pathway_legend_title_cex = 2,
    pathway_colors = colorRamp2(c(1:7), brewer.pal(7, "YlOrRd")), # takes a vector of colors
    pathway_labels = c(1, 3, 5, 7), # must match colors if discrete
    pathway_labels_cex = 1.5
) {

  plot.new()

  if (pathway_legend == "continuous") {
    pathwayLegend <- Legend(
      title = pathway_legend_title,
      col_fun = pathway_colors,
      at = pathway_labels,
      grid_width = unit(1, "cm"),
      legend_height = unit(4, "cm"),
      labels_gp = gpar(cex = pathway_labels_cex),
      title_gp = gpar(cex = pathway_legend_title_cex),
      title_gap = unit(4, "mm")
    )

    draw(
      pathwayLegend,
      y = unit(55, "mm"),
      x = unit(40, "mm"),
      just = "left"
    )

  } else if (pathway_legend == "discrete") {

    if (length(pathway_colors) != length(pathway_labels)){
      stop(paste0(
        "pathway_colors (input: ",
        length(pathway_colors),
        " observations) must be the same length as pathway_labels (input: ",
        length(pathway_labels),
        " observations)."
      ))
    }

    pathwayLegend <- Legend(
      title = pathway_legend_title,
      legend_gp = gpar(fill = pathway_colors),
      at = pathway_labels,
      grid_width = unit(1, "cm"),
      labels_gp = gpar(cex = pathway_labels_cex),
      title_gp = gpar(cex = pathway_legend_title_cex),
      title_gap = unit(3, "mm")
    )

    draw(
      pathwayLegend,
      y = unit(55, "mm"),
      x = unit(40, "mm"),
      just = "left"
    )
  }

  if (compound_legend == "discrete") {

    if (length(compound_colors) != length(compound_labels)){
      stop(paste0(
        "compound_colors (input: ",
        length(compound_colors),
        " observations) must be the same length as compound_labels (input: ",
        length(compound_labels),
        " observations)."
      ))
    }

    compoundLegend <- Legend(
      title = compound_legend_title,
      legend_gp = gpar(fill = compound_colors),
      at = compound_labels,
      grid_width = unit(1, "cm"),
      labels_gp = gpar(cex = compound_labels_cex),
      title_gp = gpar(cex = compound_legend_title_cex),
      title_gap = unit(3, "mm"),
    )

    draw(compoundLegend,
         y = unit(55, "mm"),
         x = unit(90, "mm"),
         just = "left",
    )

  } else if (compound_legend == "continuous") {

    compoundLegend <- Legend(
      title = compound_legend_title,
      col_fun = compound_colors,
      at = compound_labels,
      grid_width = unit(1, "cm"),
      legend_height = unit(4, "cm"),
      labels_gp = gpar(cex = compound_labels_cex),
      title_gp = gpar(cex = compound_legend_title_cex),
      title_gap = unit(4, "mm")
    )

    draw(compoundLegend,
         y = unit(55, "mm"),
         x = unit(90, "mm"),
         just = "left",
    )

  }

  print("If you notice clipping of the legend, try exapnding your R graphics panel.")

}

