#' Read Fodder2010
#'
#' Read in new FAO fodder data (2010-2023) downloaded from the FAO SWS system.
#' This dataset uses CPC item codes and M49 country codes.
#' Contains elements: Area Harvested (5312, ha) and Production (5510, tonnes).
#'
#' @return FAO fodder data as MAgPIE object
#' @author David Chen
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("Fodder2010")
#' }
#' @importFrom data.table fread
#' @importFrom magclass as.magpie magpiesort

readFodder2010 <- function() {

  file <- "Fodder2010.csv"

  fao <- fread(input = file, header = TRUE, sep = ",",
               colClasses = list(character = c("measuredItemCPC", "measuredElement")),
               quote = "\"", encoding = "Latin-1", showProgress = FALSE)
  fao <- as.data.frame(fao)

  # ---- Assign ISO codes from country names ----

  faoIsoFaoCodeMapping <- toolGetMapping("FAOiso_faocode_online.csv", where = "mrfaocore")
  faoIsoFaoCode <- as.character(faoIsoFaoCodeMapping$ISO)
  names(faoIsoFaoCode) <- as.character(faoIsoFaoCodeMapping$Country)

  ignoreRegions <- c("Africa", "Americas", "Asia", "Europe", "Oceania", "World",
                     "European Union (27)", "European Union (28)")

  fao$ISO <- toolCountry2isocode(fao[["Geographic Area"]], mapping = faoIsoFaoCode,
                                 ignoreCountries = ignoreRegions)

  # remove entries with missing ISO code
  fao <- fao[!is.na(fao$ISO), ]

  # ---- Map element codes to ElementShort ----

  fao$measuredElement <- as.character(fao$measuredElement)
  elementMap <- data.frame(
    measuredElement = c("5312", "5510"),
    ElementShort    = c("area_harvested", "production"),
    stringsAsFactors = FALSE
  )

  fao <- merge(fao, elementMap, by = "measuredElement", all.x = TRUE)

  # ---- Create ItemCodeItem ----
  # Remove dots from Item names (following readFAO_online convention)
  fao$Item <- gsub("\\.", "", fao$Item, perl = TRUE)
  # Remove dots from CPC codes to avoid magclass dimension separator conflicts
  fao$measuredItemCPC <- gsub("\\.", "", fao$measuredItemCPC)
  fao$ItemCodeItem <- paste0(fao$measuredItemCPC, "|", fao$Item)

  # ---- Convert to magpie object ----

  faoMag <- as.magpie(fao[, c("Year", "ISO", "ItemCodeItem", "ElementShort", "Value")],
                      temporal = 1, spatial = 2, datacol = 5)

  faoMag <- magpiesort(faoMag)

  return(faoMag)
}
