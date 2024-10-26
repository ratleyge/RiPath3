# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
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

getKeggAtlas <- function(payload_df, download_file_path = getwd()) {

  # Use chromate to open a new page in a browser
  b <- ChromoteSession$new()

  # Set download path
  download_file_path <- gsub("/", "\\\\", download_file_path)
  if (substr(download_file_path, nchar(download_file_path), nchar(download_file_path)) != "\\") {
    download_file_path <- paste0(download_file_path, "\\")
  }
  b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = download_file_path)

  # Open iPath3
  b$Page$navigate("https://pathways.embl.de/ipath3.cgi?map=metabolic", wait_ = TRUE)
  Sys.sleep(3)  # Allow page to load

  # Create JavaScript for each line in payload_df
  js_payload <- paste0(
    lapply(1:nrow(payload_df), function(i) {
      paste0(
        "document.getElementById('selection').value += '",
        paste(payload_df[i, ], collapse = "\t"),
        "\\n';"
      )
    }),
    collapse = ""
  )

  # JavaScript to insert payload line by line and export
  b$Runtime$evaluate(
    paste0("
            (async function() {
                ", js_payload, "
                await new Promise(resolve => setTimeout(resolve, 500));  // wait after setting value

                doCustomize();
                await new Promise(resolve => setTimeout(resolve, 500));

                document.getElementById('exportFormat').value = 'png';
                setExportOptions();
                await new Promise(resolve => setTimeout(resolve, 500));

                document.getElementById('export_dpi').value = '500';




                await exportMap();
            })();
        ")
  )

  Sys.sleep(15)  # Allow time for download
  b$close()
}

# Example usage
df <- read.csv("C:/Users/grarat/OneDrive - Karolinska Institutet/Desktop/Kegg Asthma Review/iPath3 compounds.csv")

getKeggAtlas(df)


