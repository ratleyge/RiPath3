# RiPath3 - Customizable KEGG Heatmaps in R
RiPath3 is an R package that allows users to generate and customize KEGG Atlas heatmaps. It does so by running a headless browser using `chromote`, uploading the data, adding customizations beyond what is available on the website, and then downloading the result to your computer. 

**The use of this package requires internet access and a working [installation of Google Chrome](https://support.google.com/chrome/answer/95346?hl=en&co=GENIE.Platform%3DDesktop).**

## Installation
```
devtools::install_github("ratleyeg/RiPath3")
```

## Usage
### Basic usage
```
# Define data frame
df <- data.frame(
  Pathway = c(
    "Arginine biosynthesis",
    "Caffeine metabolism",
    "Glycine, serine and threonine metabolism",
    "Lysine degradation",
    "Arginine and proline metabolism",
    "beta-Alanine metabolism",
    "Glutathione metabolism"),
  value = c(
    0.77393471,
    0.42070720,
    0.12953268,
    -1.74740108,
    -0.58241999,
    -1.30774442,
    -1.01095672)
  )

# Convert names to IDs
df$KEGG.ID <- namesToKeggMaps(df$Pathway)

# Give a color palette
my_cols <- brewer.pal(7, "YlOrRd") # From RColorBrewer
color_gradient <- col_numeric(palette = my_cols, domain = range(df$value))
df$color <- color_gradient(df$value)

# Add width
df$width <- "W12"

getKeggAtlas(
  df[, c("KEGG.ID", "color", "width")],
  download_file_path = getwd(),
  highlight_path_names = TRUE,
  module_names_font_color = "black",
  module_names_font_weight = "bolder",
  module_names_background_color = "#dddddd",
  module_names_background_stroke = "black"
 )
```
This code will download an image called RiPath3 KEGG Atlas.png which should look like this:
![Example - RiPath3 KEGG Atlas.png](https://github.com/ratleyge/RiPath3/blob/main/Example%20-%20RiPath3%20Kegg%20Atlas.png)

### Creating a vizualization from MetaboAnalyst Output
```
# You can download the `Example - MetaboAnalyst Output` file from this repository
getKeggAtlasFromMetaboAnalystOutput(
    metaboanalyst_directory = "C:/<YOUR PATH GOES HERE>/Example - MetaboAnalyst Output/",
    paths_color_by = "Enrichment",
    paths_color_pallette = brewer.pal(7, "YlOrRd"),
    paths_significant_only = "P.Gamma.",
    paths_significant_cutoff = 0.05,
    include_compounds = "significant",
    compounds_color = "#00ff00",
    download_file_path = getwd(),
    download_file_name = "Example - RiPath3 Kegg Atlas From MetaboAnalyst.png",
    highlight_path_names = TRUE,
    module_names_font_color = "black",
    module_names_font_weight = "bolder",
    module_names_background_color = "#dddddd",
    module_names_background_stroke = "black",
    generate_legend = TRUE
)
```
This generates the following image: 

## Citation
I am not a contributor to the iPath3 project, but if you use this package you should cite the original creators. 

[Darzi Y et al.](https://doi.org/10.1093/nar/gky299) (2018) Nucleic Acids Res. 46(W1): W510-W513 iPath3.0: interactive pathways explorer v3
