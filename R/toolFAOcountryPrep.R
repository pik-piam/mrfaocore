#' toolFAOcountryPrep
#'
#' Prepares the country dimension of FAO data for [madrat::toolISOhistorical()]: adds
#' missing successor countries, collects the FAO-specific country transitions, replaces
#' FAO's China total by China mainland and removes former countries without successor data.
#'
#' @param x MAgPIE object with FAO data, including FAO's codes for former countries
#' @return list with the prepared MAgPIE object `x` and the `additionalMapping` to pass
#' to [madrat::toolISOhistorical()]
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein, David Chen, Edna Molina Bacca, Bin Lin
#' @seealso [convertFAO_online()]
#' @examples
#' \dontrun{
#' prep <- toolFAOcountryPrep(x)
#' x <- toolISOhistorical(prep$x, overwrite = TRUE, additional_mapping = prep$additionalMapping)
#' }
#' @export
toolFAOcountryPrep <- function(x) {
  ## data for Eritrea ERI and South Sudan SSD added with 0 if not existing after the split
  ## to make toolISOhistorical work
  splits <- list(c(former = "XET", existing = "ETH", added = "ERI"),
                 c(former = "XSD", existing = "SDN", added = "SSD"))
  for (s in splits) {
    countries <- getItems(x, dim = 1.1)
    if (all(s[c("former", "existing")] %in% countries) && !(s[["added"]] %in% countries)) {
      xAdded <- x[s[["existing"]], , ]
      xAdded[, , ] <- 0
      getItems(xAdded, dim = 1) <- s[["added"]]
      x <- magpiesort(mbind(x, xAdded))
    }
  }

  additionalMapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if (all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XET", "ETH", "y1992"), c("XET", "ERI", "y1992")))
  }

  # Belgium-Luxemburg
  if (all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XBL", "BEL", "y1999"), c("XBL", "LUX", "y1999")))
  } else if (("XBL" %in% getItems(x, dim = 1.1)) && !("BEL" %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan. If non of the latter two is in the data make Sudan (former) to Sudan
  if (all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("XSD", "SSD", "y2011"), c("XSD", "SDN", "y2011")))
  } else if ("XSD" %in% getItems(x, dim = 1.1) && !any(c("SSD", "SDN") %in% getItems(x, dim = 1.1))) {
    getCells(x)[getItems(x, dim = 1.1) == "XSD"] <- "SDN"
  }

  ## if XCN exists, replace CHN with XCN.
  if ("XCN" %in% getItems(x, dim = 1.1)) {
    if ("CHN" %in% getItems(x, dim = 1.1)) {
      x <- x["CHN", , , invert = TRUE]
    }
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XCN"] <- "CHN"
  }

  ## data for the Netherlands Antilles is currently removed because currently no
  ## information for its successors SXM, CUW, ABW is available as input for toolISOhistorical
  if (any(getItems(x, dim = 1.1) == "ANT")) {
    x <- x["ANT", , , invert = TRUE]
  }

  ## data for PCI split up into:
  # Marshall Islands (MH, MHL, 584)
  # Micronesia, Federated States of (FM, FSM, 583)
  # Northern Mariana Islands (MP, MNP, 580)
  # Palau (PW, PLW, 585)
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getItems(x, dim = 1.1))) {
    additionalMapping <- append(additionalMapping, list(c("PCI", "MHL", "y1991"),
                                                        c("PCI", "FSM", "y1991"),
                                                        c("PCI", "MNP", "y1991"),
                                                        c("PCI", "PLW", "y1991")))
  } else if ("PCI" %in% getItems(x, dim = 1.1)) {
    x <- x["PCI", , invert = TRUE]
  }

  return(list(x = x, additionalMapping = additionalMapping))
}
