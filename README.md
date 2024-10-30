# RiPath3 - Customizable KEGG Heatmaps in R


## Installation
```
devtools::install_github("ratleyeg/RiPath3")
```

## Usage
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


## Citation
I am not a contributor to the iPath3 project, but if you use this package you should cite the original creators of the iPath3 package. 
[Darzi Y et al.](https://doi.org/10.1093/nar/gky299) (2018) Nucleic Acids Res. 46(W1): W510-W513 iPath3.0: interactive pathways explorer v3
