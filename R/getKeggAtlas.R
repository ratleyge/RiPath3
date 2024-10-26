# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# install.packages("chromote")
library(chromote)

getKeggAtlas <- function(df,
                         download_file_path = getwd(),
                         highlight_path_names = FALSE,
                         color_modules = "Default") {



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
                ",
                js_payload,
                "
                await new Promise(resolve => setTimeout(resolve, 500));  // wait after setting value

                doCustomize();
                await new Promise(resolve => setTimeout(resolve, 500));

                document.getElementById('exportFormat').value = 'png';
                setExportOptions();
                await new Promise(resolve => setTimeout(resolve, 500));

                document.getElementById('export_dpi').value = '500';",
                pathways,
                "
                await new Promise(resolve => setTimeout(resolve, 500));
                await exportMap();
            })();
        "
    )
  )

  #b$view()
  Sys.sleep(20)  # Allow time for download
  b$close()

}



df <- read.csv(
  "C:/Users/grarat/OneDrive - Karolinska Institutet/Desktop/Kegg Asthma Review/iPath3 compounds.csv"
)

getKeggAtlas(df, highlight_path_names = TRUE)
