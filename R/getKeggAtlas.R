#' Generate a Kegg Atlas Heatmap
#'
#' @import chromote
#' @param df A data frame with 1-4 columns. The first column should be 'KEGG.ID'
#'   (can be compounds "C00001" or paths "map00012"), then 'Color' in hex form,
#'   then 'Line.Width' in the form "W12", then 'Opacity' between 0 and 1.
#' @param highlight_path_names Boolean to bold the highlighted KEGG pathways.
#' @param module_styles_default A list with any of the following optional fields:
#'   \itemize{
#'     \item \code{bg} - Background fill color of module label boxes (hex or color name).
#'     \item \code{font} - Font color of module label (hex or color name).
#'     \item \code{stroke} - Border color of module label boxes (hex or color name).
#'     \item \code{font_weight} - Font weight; options are 'bold', 'normal', 'bolder',
#'       'lighter', or a numeric between 1 and 1000.
#'   }
#'   Any field omitted will fall back to the defaults defined in \code{subcategoryCoordinates}.
#' @param module_styles A named list of per-module style overrides. Names must
#'   match KEGG subcategory names (e.g. "Amino acid metabolism"). Each entry is
#'   a list with any of the following optional fields:
#'   \itemize{
#'     \item \code{bg} - Background fill color of the module label box (hex or color name).
#'     \item \code{font} - Font color of the module label (hex or color name).
#'     \item \code{stroke} - Border color of the module label box (hex or color name).
#'     \item \code{font_weight} - Font weight; options are 'bold', 'normal', 'bolder',
#'       'lighter', or a numeric between 1 and 1000.
#'   }
#'   Any field omitted will fall back to the defaults defined in \code{subcategoryCoordinates}.
#'   Example:
#'   \code{list("Amino acid metabolism" = list(bg = "#999999", font = "black",
#'   stroke = "darkgrey", font_weight = "normal"))}
#' @description
#'   This function runs a headless browser using 'chromote' and opens the iPath3
#'   metabolic pathways website: 'https://pathways.embl.de/ipath3.cgi?map=metabolic'.
#'   It will submit your data frame with pathways or compounds, apply per-module
#'   style customizations, and download a PNG to the specified path.
#'   IMPORTANT: This function requires internet access and Google Chrome to be installed.
#' @example inst/Example.R
#'
#' @export
getKeggAtlas <- function(df,
                         download_file_path = getwd(),
                         download_file_name = "RiPath3 Kegg Atlas.png",
                         highlight_path_names = FALSE,
                         module_styles_default = list(),
                         module_styles = list()
) {

  # Create payload ----
  js_payload <- paste0(lapply(1:nrow(df), function(i) {
    paste0(
      "document.getElementById('selection').value += '",
      paste(df[i, ], collapse = "\t"),
      "\\n';"
    )
  }), collapse = "")


  # Highlight path names ----
  if (highlight_path_names) {
    data("pathToMap")

    if (nrow(df[grep("map", df$KEGG.ID), ]) == 0) {
      stop(
        print("The function was not able find any KEGG maps in your input data. KEGG pathway IDs always start with 'map'. If you do not have any pathways in your input, set 'highlight_path_names' to FALSE")
      )
    }

    paths <- df[grep("map", df$KEGG.ID), ]
    paths <- merge(paths, pathToMap, by = "KEGG.ID", all.x = TRUE)

    if (nrow(paths[is.na(paths$Pathway), ]) > 0) {
      print(paste("Warning: Could not find", paste(paths[is.na(paths$Pathway), "KEGG.ID"], collapse = ", "), "in KEGG."))
    }

    paths <- paths[!is.na(paths$Pathway), "Pathway"]

    paths_js <- paste0(
      "var paths = ['",
      paste(paths, collapse = "','"),
      "'];"
    )

    pathways <- sprintf(
      "%s\npaths.forEach(function(pathway) {
        var path = \"/html/body/div[2]/*[local-name()='svg']/*[local-name()='g'][1]/*[local-name()='g']/*[local-name()='g'][4]/*[local-name()='text'][text()[contains(., '\" +
                  pathway.substring(0, 6) +
                \"')]]\";

        var elements = getElementByXpath(path);

        elements.forEach(function(ele) {
          var yVal = ele.y.animVal[0].value;
          var innerPath = \"/html/body/div[2]/*[local-name()='svg']/*[local-name()='g'][1]/*[local-name()='g']/*[local-name()='g'][4]/*[local-name()='text'][@x='\" +
                          ele.x.animVal[0].value +
                        \"']\";

          var eleText = ele.innerHTML;
          getElementByXpath(innerPath).forEach(function(e) {
            if (yVal + 15 == e.y.animVal[0].value) {
              eleText = eleText + e.innerHTML;
              if (pathway.includes(eleText)) {
                ele.style.fontSize = '20px';
                ele.style.fontWeight = 'bold';
                ele.style.fill = 'black';
                e.style.fontSize = '20px';
                e.style.fontWeight = 'bold';
                e.style.fill = 'black';
                var testThisY = yVal + 30;

                if (getElementByXpath(\"/html/body/div[2]/*[local-name()='svg']/*[local-name()='g'][1]/*[local-name()='g']/*[local-name()='g'][4]/*[local-name()='text'][@y='\" + testThisY + \"']\").length > 0) {
                  getElementByXpath(\"/html/body/div[2]/*[local-name()='svg']/*[local-name()='g'][1]/*[local-name()='g']/*[local-name()='g'][4]/*[local-name()='text'][@y='\" + testThisY + \"']\").forEach(function(thisOneToo) {
                    if (thisOneToo.x.animVal[0].value == ele.x.animVal[0].value) {
                      thisOneToo.style.fontSize = '20px';
                      thisOneToo.style.fontWeight = 'bold';
                      thisOneToo.style.fill = 'black';
                    }
                  });
                }
              }
            }
          });
        });
      });", paths_js
    )

  } else {
    pathways <- ""
  }


  # Build module styles ----
  data("subcategoryCoordinates")

  # Validate user-supplied module names
  invalid_mods <- setdiff(names(module_styles), subcategoryCoordinates$Subcategory)
  if (length(invalid_mods) > 0) {
    warning(paste0(
      "The following module names were not recognised and will be ignored: ",
      paste(invalid_mods, collapse = ", "),
      ".\n\nValid names are:\n",
      paste(subcategoryCoordinates$Subcategory, collapse = ", "), "."
    ), call. = FALSE)
    module_styles <- module_styles[!names(module_styles) %in% invalid_mods]
  }


  # Start from the defaults stored in subcategoryCoordinates
  styles_df <- subcategoryCoordinates

  # Apply global defaults first
  for (field in names(module_styles_default)) {
    col <- paste0("default_", field)
    if (col %in% names(styles_df)) {
      styles_df[, col] <- module_styles_default[[field]]
    } else {
      warning(paste0("'", field, "' is not a valid style field. Valid fields are: bg, font, stroke, font_weight."), call. = FALSE)
    }
  }

  # Apply user overrides
  for (mod in names(module_styles)) {
    idx <- which(styles_df$Subcategory == mod)
    if (!is.null(module_styles[[mod]]$bg))          styles_df[idx, "default_bg"]          <- module_styles[[mod]]$bg
    if (!is.null(module_styles[[mod]]$font))        styles_df[idx, "default_font"]        <- module_styles[[mod]]$font
    if (!is.null(module_styles[[mod]]$stroke))      styles_df[idx, "default_stroke"]      <- module_styles[[mod]]$stroke
    if (!is.null(module_styles[[mod]]$font_weight)) styles_df[idx, "default_font_weight"] <- module_styles[[mod]]$font_weight
  }

  # Build JS lookup maps (x coordinate -> style value)
  build_js_map <- function(df, col, key_col = "x") {
    paste0(
      "{",
      paste0(sapply(1:nrow(df), function(i) {
        paste0('"', df[i, key_col], '": "', df[i, col], '"')
      }), collapse = ", "),
      "}"
    )
  }

  js_bg_map          <- build_js_map(styles_df, "default_bg",          key_col = "x")
  js_stroke_map      <- build_js_map(styles_df, "default_stroke",      key_col = "x")
  js_font_map        <- build_js_map(styles_df, "default_font",        key_col = "y")
  js_font_weight_map <- build_js_map(styles_df, "default_font_weight", key_col = "y")


  js_module_styles <- paste0("
    var bgMap         = ", js_bg_map, ";
    var fontMap       = ", js_font_map, ";
    var strokeMap     = ", js_stroke_map, ";
    var fontWeightMap = ", js_font_weight_map, ";

    getElementByXpath(
      '/html/body/div[2]/*[local-name()=\"svg\"]/*[local-name()=\"g\"][1]/*[local-name()=\"g\"]/*[local-name()=\"g\"][5]/*[local-name()=\"rect\"]'
    ).forEach(function(el) {
      var x = el.getAttribute('x');
      if (bgMap[x] && bgMap[x] !== 'Default')     el.style.fill   = bgMap[x];
      if (strokeMap[x] && strokeMap[x] !== 'Default') el.style.stroke = strokeMap[x];
    });

    getElementByXpath(
      '/html/body/div[2]/*[local-name()=\"svg\"]/*[local-name()=\"g\"][1]/*[local-name()=\"g\"]/*[local-name()=\"g\"][6]/*[local-name()=\"text\"]'
    ).forEach(function(el) {
      var x = el.getAttribute('x');
      if (fontMap[x] && fontMap[x] !== 'Default')             el.style.fill       = fontMap[x];
      if (fontWeightMap[x] && fontWeightMap[x] !== 'Default') el.style.fontWeight = fontWeightMap[x];
    });
  ")


  # Set up the browser ----
  b <- ChromoteSession$new()

  tempDir <- paste0(tempdir(), "/iPath3")
  tempDir <- gsub("/", "\\\\", tempDir)
  if (substr(tempDir, nchar(tempDir), nchar(tempDir)) != "\\") {
    tempDir <- paste0(tempDir, "\\")
  }
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = tempDir)

  b$Page$navigate("https://pathways.embl.de/ipath3.cgi?map=metabolic", wait_ = TRUE)
  Sys.sleep(3)


  # Send to the browser ----
  b$Runtime$evaluate(
    paste0(
      "
      function getElementByXpath(path) {
        let results = [];
        var query = document.evaluate(
          path,
          document,
          null,
          XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,
          null
        );
        for (let p = 0, length = query.snapshotLength; p < length; ++p) {
          results.push(query.snapshotItem(p));
        }
        return results;
      }

      (async function() {
          ", js_payload, "
          await new Promise(resolve => setTimeout(resolve, 500));

          doCustomize();
          await new Promise(resolve => setTimeout(resolve, 500));

          document.getElementById('exportFormat').value = 'png';
          setExportOptions();
          await new Promise(resolve => setTimeout(resolve, 500));

          document.getElementById('export_dpi').value = '500';

          ", pathways, "

          ", js_module_styles, "

          await new Promise(resolve => setTimeout(resolve, 500));
          await exportMap();
      })();
    "
    )
  )

  i <- 0
  while (length(list.files(tempDir)) == 0) {
    Sys.sleep(1)
    i <- i + 1
    if (i == 60) {
      stop(print("Timed out while trying to download from iPath3"))
    }
  }

  Sys.sleep(5)

  file.rename(
    from = list.files(tempDir, full.names = TRUE)[1],
    to   = paste0(download_file_path, "/", download_file_name)[1]
  )

  b$close()
}
