# has functions to make a color gradient from a numeric vector
library(scales)

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
