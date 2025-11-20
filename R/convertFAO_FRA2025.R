#' Convert FRA2025 data
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO FRA 2025 subtype.
#' @return Data as MAgPIE object with common country list
#' @author Simin Yu
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FRA2025", "growing_stock", convert = TRUE)
#' }
#'
#' @importFrom madrat toolCountryFill
#' @export
convertFRA2025 <- function(x, subtype) {
  subtypeList <- c("forest_area", "deforestation", "growing_stock", "biomass_stock","carbon_stock", "management", 
                   "disturbance", "forest_fire", "expansion", "afforestation", "net_change")
  
  if (subtype %in% c("forest_area", "deforestation", "growing_stock", "management",
                     "disturbance", "forest_fire", "expansion", "afforestation", "net_change")) {
    x <- toolCountryFill(x, fill = 0)
    if (any(getNames(x) %in% grep(pattern = "gs_ha", x = getNames(x), value = TRUE))) {
      # gs_ha_ variables are already in m3/ha, no need for conversion
      out <- x
    } else {
      # Conversion from thousand to million units
      out <- x / 1000 
    }
    return(out)
  } else if (subtype %in% c("biomass_stock", "carbon_stock")) {
    x <- toolCountryFill(x, fill = 0)
    out <- x
    return(out)
  } else {
    stop("Invalid or unsupported subtype ", subtype, ". Accepted subtypes are ",
         paste(subtypeList, collapse = ", "), ". Choose one of the accepted subtype.")
  }
}
