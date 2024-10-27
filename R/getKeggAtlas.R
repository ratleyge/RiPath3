#' Generate a Kegg Atlas Heatmap
#'
#' @import chromote
#' @param df A data frame with 1-4 columns. The first column should be 'KEGG.ID' (can be compounds "C00001" or paths "map00012"), then 'Color' in hex form, then 'Line.Width' in the from "W12", then 'Opacity' between 0 and 1.
#' @param highlight_path_names Boolean to bold the highlighted KEGG pathways.
#' @param module_names_font_color Color of the module labels (i.e. Carbohydrate metabolism, Vitamins and co-factors).
#' @param module_names_font_weight Weight of the module labels. Options are 'bold', 'normal', 'bolder', 'lighter', or a numeric between 1 and 1000.
#' @param module_names_background_color Change the background colors of the module labels. iPath defaults to colors of the rainbow. This allows you to insert a color in hex or color names (e.g. "grey") to change all the module fill colors at the same time.
#' @param module_names_background_stroke This allows you to put a boarder around the module label background. Default is no stroke.
#' @description
#' This function runs a headless browser using 'chromote' and opens the iPath3 metabolic pathways website: 'https://pathways.embl.de/ipath3.cgi?map=metabolic'.
#' It will submit your data frame with pathways or compounds and allow you to do minor customizations.
#' It will then download a png of your data to a specified path name.
#' IMPORTANT: This function requires internet access and requires that you have Google Chrome installed.
#' @examples
#' df <- data.frame(
#'   ID = c("C00025", "C00022"),
#'   Color = c("#00cc33", "#ff3333"),
#'   Width = c("W14", "W10")
#' )
#' getKeggAtlas(
#'   df,
#'   download_file_path = getwd(),
#'   highlight_path_names = TRUE,
#'   module_names_font_color = "black",
#'   module_names_font_weight = "bolder",
#'   module_names_background_color = "#dddddd",
#'   module_names_background_stroke = "black"
#'  )
#'
getKeggAtlas <- function(df,
                         download_file_path = getwd(),
                         highlight_path_names = FALSE,
                         module_names_font_color = "#ffffff", # color
                         module_names_font_weight = "bold", # also normal
                         module_names_background_color = "Default", # color
                         module_names_background_stroke = "Default" # color
                         ) {



  # Create payload ----
  # Create JavaScript for each line in df
  js_payload <- paste0(lapply(1:nrow(df), function(i) {
    paste0(
      "document.getElementById('selection').value += '",
      paste(df[i, ], collapse = "\t"),
      "\\n';"
    )
  }), collapse = "")





  # Highligh path names ----
  # If user wants to highlight path names run this
  if (highlight_path_names) {
    # read the path to map df
    pathToMap <- read.csv("R/Pathway Name to KEGG Map.csv")

    # TO DO - Throw error if there are no paths with map
    paths <- df[grep("map", df$KEGG.ID), ]
    paths <- merge(paths, pathToMap, by = "KEGG.ID", all.x = TRUE)

    # Inform user if their paths are invalid
    if (nrow(paths[is.na(paths$Pathway), ]) > 0) {
      print("Could not find", paste(paths[is.na(paths$Pathway), "KEGG.ID"], collapse = ", "), "in KEGG.")
    }

    # Select only pathway names
    paths <- paths[!is.na(paths$Pathway), "Pathway"]

    # Build the JavaScript array of paths
    paths_js <- paste0(
      "var paths = ['",
      paste(paths, collapse = "','"),
      "'];"
    )

    # Define the JavaScript code as a single string
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




  # Set up the browser ----
  # Use chromate to open a new page in a browser
  b <- ChromoteSession$new()

  # Set download path
  download_file_path <- gsub("/", "\\\\", download_file_path)
  if (substr(download_file_path,
             nchar(download_file_path),
             nchar(download_file_path)) != "\\") {
    download_file_path <- paste0(download_file_path, "\\")
  }
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = download_file_path)

  # Open iPath3
  b$Page$navigate("https://pathways.embl.de/ipath3.cgi?map=metabolic",
                  wait_ = TRUE)
  Sys.sleep(3)  # Allow page to load


  # Send to the browser ----
  # JavaScript to insert payload line by line and export
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
          await new Promise(resolve => setTimeout(resolve, 500));  // wait after setting value

          doCustomize();
          await new Promise(resolve => setTimeout(resolve, 500));

          document.getElementById('exportFormat').value = 'png';
          setExportOptions();
          await new Promise(resolve => setTimeout(resolve, 500));

          document.getElementById('export_dpi').value = '500';", pathways, "

          getElementByXpath(
            '/html/body/div[2]/*[local-name()=\"svg\"]/*[local-name()=\"g\"][1]/*[local-name()=\"g\"]/*[local-name()=\"g\"][6]/*[local-name()=\"text\"]'
          ).forEach(function (element) {
            ", paste0("element.style.fill = '", module_names_font_color, "';"), "
            ", paste0("element.style.fontWeight = '", module_names_font_weight, "';"), "
          });

          getElementByXpath(
            '/html/body/div[2]/*[local-name()=\"svg\"]/*[local-name()=\"g\"][1]/*[local-name()=\"g\"]/*[local-name()=\"g\"][5]/*[local-name()=\"rect\"]'
          ).forEach(function (element) {
            ", if (module_names_background_color != "Default") {paste0("element.style.fill = '", module_names_background_color, "';")}, "
            ", if (module_names_background_stroke != "Default") {paste0("element.style.stroke = '", module_names_background_stroke, "';")}, "
          });

          await new Promise(resolve => setTimeout(resolve, 500));
          await exportMap();
      })();
    "
    )
  )

  # b$view()
  Sys.sleep(20)  # Allow time for download
  b$close()

}

