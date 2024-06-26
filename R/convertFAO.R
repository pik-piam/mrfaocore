#' Convert FAO data
#'
#' Converts FAO data to fit to the common country list and removes or converts
#' relative values where possible. Yields (Hg/ha) are for instance removed
#' since they can later easily be calculated from production and area but might
#' be problematic in the spatial aggregation. Per capita demand values are
#' transformed into absolute values using population estimates from the
#' calcPopulationPast function.
#'
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO file type, e.g.: CBCrop
#' @return Data as MAgPIE object with common country list
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic
#' @seealso [readFAO()], [readSource()],
#' @examples
#' \dontrun{
#' a <- readSource("FAO", "Crop", convert = TRUE)
#' }
#' @importFrom magclass magpiesort getItems dimExists
#'

## check why LivePrim has such strange Units such as (0_1Gr/An) and "Yield_(Hg)"

convertFAO <- function(x, subtype) { # nolint: cyclocomp_linter.
  ## datasets that have only absolute values
  absolute <- c("CBCrop", "CBLive", "CropProc", "Fertilizer", "Land", "LiveHead",
                "LiveProc", "Pop", "ValueOfProd", "ForestProdTrade", "Fbs")



  ## datasets that contain relative values that can be deleted because they can
  ## be calculated again at a later point in time
  ## and the dimensions that can be deleted
  relativeDelete <- list()
  relativeDelete[["Crop"]] <- "Yield_(Hg/Ha)"
  relativeDelete[["Fodder"]] <- "Yield_(Hg/Ha)"
  relativeDelete[["LivePrim"]] <- c("Yield_Carcass_Weight_(Hg/An)",
                                    "Yield_(100Mg/An)",
                                    "Yield_Carcass_Weight_(0_1Gr/An)",
                                    "Yield_(Hg/An)",
                                    "Yield_(Hg)")

  ## datasets that contain relative values: and define these dimensions
  relative <- list()
  relative[["FSCrop"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")

  relative[["FSLive"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")


  ### Section for country specific treatment ###

  ## data for Eritrea ERI added with 0 if not existing in the dimensionality of
  ## Ethiopia, to make toolISOhistorical work
  if (all((c("XET", "ETH", "ERI") %in% getItems(x, dim = 1)) == c(TRUE, TRUE, FALSE))) {
    xERI <- x["ETH", , ]
    xERI[, , ] <- 0
    getItems(xERI, dim = 1) <- "ERI"
    x <- magpiesort(mbind(x, xERI))
  }

  ## add additional mappings
  additionalMapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if (all(c("XET", "ETH", "ERI") %in% getItems(x, dim = 1))) {
    additionalMapping <- append(additionalMapping, list(c("XET", "ETH", "y1992"), c("XET", "ERI", "y1992")))
  }

  # Belgium-Luxemburg
  if (all(c("XBL", "BEL", "LUX") %in% getItems(x, dim = 1))) {
    additionalMapping <- append(additionalMapping, list(c("XBL", "BEL", "y1999"), c("XBL", "LUX", "y1999")))
  } else if (("XBL" %in% getItems(x, dim = 1)) && !("BEL" %in% getItems(x, dim = 1))) {
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan. If non of the latter two is in the data make Sudan (former) to Sudan
  if (all(c("XSD", "SSD", "SDN") %in% getItems(x, dim = 1))) {
    additionalMapping <- append(additionalMapping, list(c("XSD", "SSD", "y2010"), c("XSD", "SDN", "y2010")))
  } else if ("XSD" %in% getItems(x, dim = 1) && !any(c("SDD", "SDN") %in% getItems(x, dim = 1))) {
    getItems(x, dim = 1)[getItems(x, dim = 1) == "XSD"] <- "SDN"
  }

  ## if there is information for CHN: China, XCN: China, mainland and at least one of the regions
  ## HKG: China, Hong Kong SAR, TWN: China, Taiwan Province of, MAC: China, Macao SAR
  ## then replace CHN information by XCN, otherwise discard XCN
  if (all(c("CHN", "XCN") %in% getItems(x, dim = 1)) && any(getItems(x, dim = 1) %in% c("HKG", "TWN", "MAC"))) {
    chinaMainland <- x["XCN", , ]
    getItems(chinaMainland, dim = 1) <- "CHN"
    x["CHN", , ] <- chinaMainland
    x <- x["XCN", , , invert = TRUE]
  } else if (any(getItems(x, dim = 1) == "XCN")) {
    x <- x["XCN", , , invert = TRUE]
  }

  ## data for the Netherlands Antilles is currently removed because currently no
  ## information for its successors SXM, CUW, ABW is available as input for toolISOhistorical
  if ("ANT" %in% getItems(x, dim = 1)) {
    x <- x["ANT", , , invert = TRUE]
  }


  ## data for PCI split up into:
  # Marshall Islands (MH, MHL, 584)
  # Micronesia, Federated States of (FM, FSM, 583)
  # Northern Mariana Islands (MP, MNP, 580)
  # Palau (PW, PLW, 585)
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getItems(x, dim = 1))) {
    additionalMapping <- append(additionalMapping, list(c("PCI", "MHL", "y1991"), c("PCI", "FSM", "y1991"),
                                                        c("PCI", "MNP", "y1991"), c("PCI", "PLW", "y1991")))
  } else if ("PCI" %in% getItems(x, dim = 1)) {
    x <- x["PCI", , invert = TRUE]
  }


  ### in the dataset EmisAgRiceCult certain follow up states of the Soviet Union are missing. Add them with values of 0
  if (subtype == "EmisAgRiceCult") {
    isoHistorical <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"),
                               stringsAsFactors = FALSE)
    former <- isoHistorical[isoHistorical$fromISO %in% c("SUN", "YUG", "SCG"), "toISO"]
    missing <- former[!former %in% getItems(x, dim = 1)]
    x2 <- new.magpie(cells_and_regions = missing, years = getYears(x), names = getNames(x))
    x2[, getYears(x2)[getYears(x2, as.integer = TRUE) >= 1992], ] <- 0
    x <- mbind(x, x2)
    vcat(2, "Added the countries", missing, "with value of 0 from 1992 onwards")
  }



  if (any(subtype == absolute)) {
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
      warning("The following elements could be relative: \n",
              paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
              "\n", "and would need a different treatment of NAs in convertFAO")
    }

  } else if (any(subtype == names(relativeDelete))) {
    x[is.na(x)] <- 0
    x <- x[, , relativeDelete[[subtype]], invert = TRUE]
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
      warning("The following elements could be relative: \n",
              paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
              "\n", "and would need a different treatment of NAs in convertFAO")
    }
  } else if (any(subtype == c("FSCrop", "FSLive"))) {


    xabs <- x[, , relative[[subtype]], invert = TRUE]
    xrel <- x[, , relative[[subtype]], invert = FALSE]


    # handling of relative values
    # replaced toolISOhistorical by the following approach for disaggregation
    mapping <- read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"), stringsAsFactors = FALSE)
    for (elem in additionalMapping) {
      mapping <- rbind(mapping, elem)
    }

    adoptAggregatedAverage <- function(country, data, mapping) {
      if (length(country) > 1) {
        stop("only one transition per function call")
      }
      toISO <- mapping$toISO[mapping$fromISO == country]
      lastyear <- unique(mapping$lastYear[mapping$fromISO == country])
      if (length(lastyear) > 1) {
        stop("strange transition mapping")
      }
      allyears <- getYears(data, as.integer = TRUE)
      years <- allyears[allyears <= as.integer(substring(lastyear, 2, 5))]
      data[toISO, years, ] <- magclass::colSums(data[country, years])
      data <- data[country, , , invert = TRUE]
      return(data)
    }
    xrel <- adoptAggregatedAverage(country = "SUN", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "YUG", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "CSK", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "XET", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "XBL", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "SCG", data = xrel, mapping = mapping)
    xrel <- adoptAggregatedAverage(country = "XSD", data = xrel, mapping = mapping)

    # transforming relative values into absolute values
    pop <- calcOutput("PopulationPast", aggregate = FALSE)
    xrel <- toolCountryFill(xrel, fill = 0, verbosity = 2)
    commonyears <- intersect(getYears(pop), getYears(x))
    xrelpop <- collapseNames(complete_magpie(pop[, commonyears, ]) * complete_magpie(xrel[, commonyears, ]))
    xrelpop <- xrelpop[, , c("food_supply_kcal/cap/day", "protein_supply_g/cap/day", "fat_supply_g/cap/day")] * 365
    getNames(xrelpop, dim = 2) <- c("food_supply_kcal", "protein_supply", "fat_supply")
    xrelpop[is.na(xrelpop)] <- 0

    # absolute values
    xabs[is.na(xabs)] <- 0
    xabs[xabs < 0] <- 0
    xabs <- toolISOhistorical(xabs, overwrite = TRUE, additional_mapping = additionalMapping)
    xabs <- toolCountryFill(xabs, fill = 0, verbosity = 2)

    x <- mbind(xabs, xrelpop)
    x <- complete_magpie(x)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]]))) {
      warning("The following elements could be relative: \n",
              paste(grep(pattern = "yield|Yield|/", getNames(x, fulldim = TRUE)[[2]], value = TRUE), collapse = " "),
              "\n", "and would need a different treatment of NAs in convertFAO")
    }
    # automatically delete the "Implied emissions factor XXX" dimension for Emission datasets
  } else if (substring(subtype, 1, 6) == "EmisAg" || substring(subtype, 1, 6) == "EmisLu") {
    if (any(grepl("Implied_emission_factor", getItems(x, dim = 3.2)))) {
      x <- x[, , "Implied_emission_factor", pmatch = TRUE, invert = TRUE]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)

    # Producer Prices Annual
  } else if (subtype == "PricesProducerAnnual") {
    x <- collapseNames(x[, , "Producer_Price_(US_$_tonne)_(USD)"])
    ## Serbia and Montenegro split
    if (all(c("SCG", "SRB") %in% getItems(x, dim = 1)) && !("MNE" %in% getItems(x, dim = 1))) {
      mne <- x["SRB", , ]
      dimnames(mne)[[1]] <- "MNE"
      x <- mbind(x, mne)
    }
    ## Adjust prices of live animal weight to the carcass weith
    mapping <- toolGetMapping("FAO_livestock_carcass_price_factor.csv", type = "sectoral", where = "mrfaocore")
    for (item in mapping$FAO_carcass) {
      litem <- mapping$FAO_live_weigth[grep(item, mapping$FAO_carcass)]
      countries <- unique(rownames(which(!is.na(x[, , item]), arr.ind = TRUE)))
      countries <- setdiff(getItems(x, dim = 1), countries)
      x[countries, , item] <- x[countries, , litem] / mapping$Price_factor[grep(item, mapping$FAO_carcass)]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
  } else if (subtype == "PricesProducerAnnualLCU") {
    x <- collapseNames(x[, , "Producer_Price_(Standard_local_Currency_tonne)_(SLC)"])
    ## Serbia and Montenegro split
    if (all(c("SCG", "SRB") %in% getItems(x, dim = 1)) && !"MNE" %in% getItems(x, dim = 1)) {
      mne <- x["SRB", , ]
      dimnames(mne)[[1]] <- "MNE"
      x <- mbind(x, mne)
    }
    ## Adjust prices of live animal weight to the carcass weith
    mapping <- toolGetMapping("FAO_livestock_carcass_price_factor.csv", type = "sectoral", where = "mrfaocore")
    for (item in mapping$FAO_carcass) {
      litem <- mapping$FAO_live_weigth[grep(item, mapping$FAO_carcass)]
      countries <- unique(rownames(which(!is.na(x[, , item]), arr.ind = TRUE)))
      countries <- setdiff(getItems(x, dim = 1), countries)
      x[countries, , item] <- x[countries, , litem] / mapping$Price_factor[grep(item, mapping$FAO_carcass)]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additionalMapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
  } else {
    cat("Specify whether dataset contains absolute or relative values in convertFAO")
  }


  ### set negative values (except stock variation) to 0

  if (dimExists(3.2, x)) {
    novar <- setdiff(getItems(x, dim = 3.2), "stock_variation")
    x[, , novar][x[, , novar] < 0] <- 0
  }

  ## Unit conversion in case of FAO Forestry Trade and Production Data:

  if (subtype == "ForestProdTrade") {
    x[, , "Import_Value_(1000_US$)"] <- x[, , "Import_Value_(1000_US$)"] / 1000
    x[, , "Export_Value_(1000_US$)"] <- x[, , "Export_Value_(1000_US$)"] / 1000
    x[, , "Production_(tonnes)"] <- x[, , "Production_(tonnes)"] / 1000000
    x[, , "Export_Quantity_(tonnes)"] <- x[, , "Export_Quantity_(tonnes)"] / 1000000
    x[, , "Import_Quantity_(tonnes)"] <- x[, , "Import_Quantity_(tonnes)"] / 1000000

    getNames(x, dim = 2)[3] <- "Import_Value_(Mio_US$)"
    getNames(x, dim = 2)[5] <- "Export_Value_(Mio_US$)"
    getNames(x, dim = 2)[6] <- "Production_(Mio_tonnes)"
    getNames(x, dim = 2)[7] <- "Import_Quantity_(Mio_tonnes)"
    getNames(x, dim = 2)[8] <- "Export_Quantity_(Mio_tonnes)"

    getNames(x) <- gsub("^\\|", "", getNames(x))
  }
  return(x)
}
