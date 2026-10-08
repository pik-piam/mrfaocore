#' Convert Fodder2010 data
#'
#' Converts new FAO Fodder2010 data to fit to the common country list.
#' Data starts in 2010 so no historical country transitions are needed,
#' but standard ones are included defensively. Units are kept as tonnes and ha.
#'
#' @param x MAgPIE object containing original values
#' @return Data as MAgPIE object with common country list
#' @author David Chen
#' @seealso [readFodder2010()], [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("Fodder2010", convert = TRUE)
#' }
#' @importFrom magclass magpiesort getItems

convertFodder2010 <- function(x) {

  x[is.na(x)] <- 0

  # ---- Country-specific treatment ----

  additionalMapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if (all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XET", "ETH", "y1992"), c("XET", "ERI", "y1992")))
  }

  # Belgium-Luxemburg
  if (all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XBL", "BEL", "y1999"), c("XBL", "LUX", "y1999")))
  } else if (("XBL" %in% getItems(x, dim = 1.1)) && !("BEL" %in% getItems(x, dim = 1.1))) {
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan
  if (all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XSD", "SSD", "y2011"), c("XSD", "SDN", "y2011")))
  } else if ("XSD" %in% getItems(x, dim = 1.1) && !any(c("SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XSD"] <- "SDN"
  }

  # China mainland
  if ("XCN" %in% getItems(x, dim = 1.1)) {
    if ("CHN" %in% getItems(x, dim = 1.1)) x <- x["CHN", , , invert = TRUE]
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XCN"] <- "CHN"
  }

  # Netherlands Antilles
  if (any(getItems(x, dim = 1.1) == "ANT")) {
    x <- x["ANT", , , invert = TRUE]
  }

  # ---- Historical transitions and country fill ----

  if (length(additionalMapping) > 0) {
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
  }

  # units stay in tonnes and ha, consistent with readSource("FAO", "Fodder")
  x <- toolCountryFill(x, fill = 0, verbosity = 2)

  return(x)
}
