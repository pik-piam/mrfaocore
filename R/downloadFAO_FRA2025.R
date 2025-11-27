#' Download FAO FRA data
#' 
#' Downloads the FRA 2025 bulk data from the FAO-FRA website available at 
#' https://fra-data.fao.org/assessments/fra/2025/WO/data-download/
#' Removes month and date from the names of three main files
#' (FRA_Years_2025, Intervals_2025 and Annual_2025.csv). 
#' 
downloadFRA2025 <- function() {
  # Download the bulk data and unzip 
  url_bulk <- "https://fra-data.fao.org/api/file/bulk-download?assessmentName=fra&cycleName=2025&countryIso=WO"
  exdir <- "FRA2025_data"
  zipfile <- file.path(exdir, "FRA2025_bulk.zip")
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  message("Downloading FRA 2025 bulk zip...")
  utils::download.file(url_bulk, zipfile, mode = "wb")
  utils::unzip(zipfile, exdir = exdir)
  
  # Rename the three main CSVs by removing _MM_DD for the use in read function
  patterns <- c("FRA_Years_2025_", "Intervals_2025_", "Annual_2025_")
  
  for (p in patterns) {
    files <- list.files(exdir, pattern = paste0("^", p, ".*\\.csv$"), full.names = TRUE)
    for (f in files) {
      newname <- sub("_\\d+_\\d+\\.csv$", ".csv", basename(f))
      file.rename(f, file.path(exdir, newname))
    }
  }
  
  message("Download and renaming of three main tables complete. Files are in: ", normalizePath(exdir))

  return(list(url          = url_bulk,
              doi          = "not available",
              title        = "FAO Global Forest Resources Assessment 2025",
              author       = "FAO",
              version      = "2025",
              release_date = "not available",
              description  = "not available",
              license      = "Creative Commons Attribution-4.0 International licence",
              reference    = "not available"))
}

