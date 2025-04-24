#' @title calcFAOmassbalance_pre
#' @description Calculates an extended version of the Food Balance Sheets. Makes explicit the conversion processes that
#' convert one type of product into another. Includes processes like milling, distilling, extraction etc. Adds certain
#'  byproducts like distillers grains or ethanol.
#'
#' @param years years to be estimated, if null, then all years in FAOharmonized are returned
#' @param version whether to return the new post-2010 massbalance "post2010", the "pre2010" older sheets, or
#' or join them at 2010 (replace old with new at 2010) "join2010"
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' This is an intermediary result, which is used e.g. for estimating the feed baskets. For most uses, it is more
#' appropriate to use the FAOmasbalance instead of the FAOmassbalance_pre.
#' @author Benjamin Leon Bodirsky, David M Chen
#' @seealso
#' [calcFAOmassbalance()]
#' @examples
#' \dontrun{
#' calcOutput("FAOmassbalance_pre")
#' }
#' @importFrom graphics plot
#' @importFrom magclass getSets as.magpie complete_magpie
#' @importFrom utils read.csv
#' @importFrom withr local_options

calcFAOmassbalance_pre <- function(version = "join2010", years = NULL) { # nolint

  if (version == "join2010") {

    pre2010 <- calcOutput("FAOmassbalance_pre", version = "pre2010", aggregate = FALSE)
    post2010 <- calcOutput("FAOmassbalance_pre", version = "post2010", aggregate = FALSE)

    # add new dims
    newdims <- setdiff(getItems(post2010, dim = 3.2), getItems(pre2010, dim = 3.2))
    pre2010 <- add_columns(pre2010, dim = 3.2, addnm = newdims, fill = 0)

    # we will make a hard break of using the new data in 2010.
    pre2010 <- pre2010[, c(2010:2013), , invert = TRUE]
    massbalance <- mbind(pre2010, post2010)

  } else if (version == "post2010") {

    fb <-  calcOutput("FAOharmonized", source = "post2010", return = "FB", aggregate = FALSE)
    sua <-  calcOutput("FAOharmonized", source = "post2010", return = "SUA", aggregate = FALSE)
    relationmatrix <-  toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")
    # give opening stocks to FB from SUA , not in data
    openingStocks <- toolAggregate(sua[, , "opening_stocks"], rel = relationmatrix,
                                   from = "post2010_SupplyUtilizationItem", to = "post2010_FoodBalanceItem",
                                   partrel = TRUE, dim = 3.1)
    fb <- mbind(fb, openingStocks)

    ### FAO Commodity Balance
    getSets(fb) <- c("region", "year", "ItemCodeItem.ElementShort")
    getSets(sua) <- c("region", "year", "ItemCodeItem.ElementShort")

    if (any(duplicated(dimnames(fb)[[3]]) == TRUE)) {
      stop("The folowing dimnames are duplicated: ",
           paste(getNames(cbc)[duplicated(getNames(cbc))], collapse = "\", \""))
    }

    if (any(duplicated(dimnames(sua)[[3]]) == TRUE)) {
      stop("The folowing dimnames are duplicated: ",
           paste(getNames(cbc)[duplicated(getNames(cbc))], collapse = "\", \""))
    }

    # determine years
    if (is.null(years)) {
      years <- getYears(fb)
    }
    if (nchar(years[[1]]) < 5) {
      years <- paste0("y", years)
    }

    fb <- fb[, years, ]
    sua <- sua[, years, ]

    # remove double counting and add missing products
    removethem <- c(
      # crop commodity balance and Food Supply items aggregated
      "2924|Alcoholic Beverages",
      "2905|Cereals - Excluding Beer",
      "2919|Fruits - Excluding Wine",
      "2928|Miscellaneous",
      "2913|Oilcrops",
      "2911|Pulses",
      "2923|Spices",
      "2907|Starchy Roots",
      "2922|Stimulants",
      "2909|Sugar & Sweeteners",
      "2908|Sugar Crops",
      "2912|Treenuts",
      "2914|Vegetable Oils",
      "2918|Vegetables",
      "2903|Vegetal Products",
      "2901|Grand Total",
      # livestock commodity balance and Food Supply items aggregated
      "2941|Animal Products",
      "2946|Animal fats",
      "2961|Aquatic Products, Other",
      "2949|Eggs",
      "2960|Fish, Seafood",
      "2943|Meat",
      "2948|Milk - Excluding Butter",
      "2945|Offals",
      # others and equivalents
      "2562|Palm kernels",
      "2960|Fish, Seafood"
    )

    fb <- fb[, , removethem, invert = TRUE]

    fb <- complete_magpie(fb, fill = 0)

    # there are some products and processes in teh SUA we don't cover.
    # here we first add them to the original product, in terms of demand categories,
    # while subtracting them from the processing accounting
    otherSecMapping <- toolGetMapping("FBSSUA_otherSecondary.csv", type = "sectoral",
                                      where = "mrfaocore")
    secMissing <- intersect(getItems(sua, dim = 3.1), otherSecMapping$Sec)
    suaSec <- toolAggregate(sua[, , secMissing], dim = 3.1, rel = otherSecMapping,
                            from = "Sec", to = "Prim", partrel = TRUE)
    # remove same amount  from processing, assuming 1:1 conversion, we will add these on at the end!
    sua[, , getNames(suaSec, dim = 1)][, , "processed"] <-   sua[, , getNames(suaSec, dim = 1)][, , "processed"] -
      dimSums(suaSec[, , c("food", "feed", "other_util", "waste")],
              dim = 3.2)
    sua[sua < 0] <- 0
    # tehre are negatives due to trade

    # As current mass balance stands we ned flours, brans, oilcakes, Oilcrops Other, molasses
    # from the more disaggregated SUA categories

    # helper function for SUA
    .getFAOitemsSUA <- function(magpieItems) {
      return(relationmatrix[relationmatrix$k %in% magpieItems, "post2010_SupplyUtilizationItem"])
    }

    # helper function for FB
    .getFAOitems <- function(magpieItems) {
      return(relationmatrix[relationmatrix$k %in% magpieItems, "post2010_FoodBalanceItem"])
    }



    # the following are the items required from SUA to disaggregate the large FB products,
    # everything else is too specific for our purposes here


    oilcakes <-  .getFAOitemsSUA("oilcakes")
    oilCrops <- .getFAOitemsSUA(c("soybean", "maiz", "groundnut", "rapeseed", "sunflower", "cottn_pro"))
    oilCrops <- oilCrops[grep("Maize (corn)|Cotton|Rape|Mustard|Coco|Sesame|Soya|
                               Groundnut|Kapok|inseed|Sunflower|Hemp|Other oil seeds|Oliv", oilCrops)]
    # other oil crops more complicated, restrict to those which have cakes, others accounted for via "others"
    oils <- .getFAOitemsSUA("oils")
    oils <- oils[grep("Soy|maize|Rapeseed|Mustard|sesame|Coconut|palm kernel|Palm|Sunflower|
                      Groundnut|inseed|Cottonseed|hemp|kapok|poppy|Safflower|rice bran|Other oil|Olive", oils)]
    otherOilCrops <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in% # nolint
                                                                           relationmatrix[grep("2570", relationmatrix$post2010_FoodBalanceItem), # nolint
                                                                                          "post2010_SupplyUtilizationItem"])] #nolint
    otherOilCrops <- otherOilCrops[grep("copra|Other|Hemp|Safflower|Kapok|Poppy", otherOilCrops)]
    otherOils <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in% # nolint
                                                                       relationmatrix[grep("2586", relationmatrix$post2010_FoodBalanceItem), # nolint
                                                                                      "post2010_SupplyUtilizationItem"])] #nolint
    otherOils <- otherOils[which(otherOils %in% relationmatrix[grep("oils", relationmatrix$k),
                                                               "post2010_SupplyUtilizationItem"])]
    otherOils <- otherOils[otherOils != ""]
    oilpalm <-  .getFAOitemsSUA("oilpalm")
    oilpalm <- oilpalm[oilpalm != ""]

    # need particular maize products
    maizeGluten <-  relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in% #nolint
                                                                          relationmatrix[grep("Maize gluten",
                                                                                              relationmatrix$post2010_SupplyUtilizationItem), # nolint
                                                                                         "post2010_SupplyUtilizationItem"])] #nolint
    maizeGerm <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in%
                                                                       relationmatrix[grep("Germ of maize",
                                                                                           relationmatrix$post2010_SupplyUtilizationItem), # nolint
                                                                                      "post2010_SupplyUtilizationItem"])] #nolint

    sugarCane <- .getFAOitemsSUA("sugr_cane")
    sugarBeet <- .getFAOitemsSUA("sugr_beet")
    potato <- .getFAOitemsSUA("potato")

    starches <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in%
                                                                      relationmatrix[grep("Starch of ",
                                                                                          relationmatrix$post2010_SupplyUtilizationItem), # nolint
                                                                                     "post2010_SupplyUtilizationItem"])]
    sugar <- .getFAOitemsSUA("sugar")
    sugar <- sugar[sugar != ""]

    molasses <- .getFAOitemsSUA("molasses")
    cereals <- .getFAOitemsSUA(c("tece", "maiz", "rice_pro", "trce"))
    cereals <- cereals[cereals != ""]


    brans <- .getFAOitemsSUA("brans")
    brans <- brans[brans != ""]

    beers <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in%
                                                                   relationmatrix[grep("Beer|-ferm",
                                                                                       relationmatrix$post2010_SupplyUtilizationItem), # nolint
                                                                                  "post2010_SupplyUtilizationItem"])]
    distillersBrewersG <- .getFAOitemsSUA(c("distillers_grain", "brewers_grain"))

    cassava <- relationmatrix$post2010_SupplyUtilizationItem[which(relationmatrix$post2010_SupplyUtilizationItem %in%
                                                                     relationmatrix[grep("Cassava, fresh",
                                                                                         relationmatrix$post2010_SupplyUtilizationItem), # nolint
                                                                                    "post2010_SupplyUtilizationItem"])]
    others <- .getFAOitemsSUA("others")
    others <- others[others != ""]

    alcohol <- .getFAOitemsSUA("alcohol")


    # keep the specific ones and all that are mapped
    keep <- unique(c(oilCrops, oilcakes, oils, otherOilCrops, otherOils, oilpalm, sugarCane, sugarBeet,
                     cereals, brans, beers, distillersBrewersG, maizeGluten,
                     sugar, molasses, potato, cassava,  starches, maizeGerm, others, alcohol))


    sua <- sua[, , intersect(getItems(sua, dim = 3.1), keep)]


    # add these products not existent in FAOSTAT to SUA for disaggregated process accounting
    missingproducts <- c("X001|Ethanol",
                         "X002|Distillers_grain",
                         "X003|Palmoil_Kerneloil_Kernelcake",
                         "X004|Brewers_grain")
    sua <- add_columns(sua, addnm = missingproducts, dim = 3.1)
    fb <- add_columns(fb, addnm = missingproducts, dim = 3.1)

    #### Product Attributes
    prodAttributes <- calcOutput("Attributes", aggregate = FALSE)
    attributeTypes <- getNames(prodAttributes, dim  = 1)
    removeProd     <- c("betr", "begr", "pasture", "scp", "res_cereals",
                        "res_fibrous", "res_nonfibrous", "wood", "woodfuel")
    prodAttributes <- prodAttributes[, , removeProd, invert = TRUE]

    # Map production attributes to FAO items
    relationmatrixF <-  relationmatrix[which(relationmatrix$post2010_FoodBalanceItem %in% getItems(fb, dim = 3.1)), ]
    relationmatrixF <- relationmatrixF[-which(relationmatrixF$post2010_FoodBalanceItem == ""), ]
    prodAttributesFB      <- toolAggregate(x = prodAttributes, rel = relationmatrixF,
                                           dim = 3.2, from = "k",
                                           to = "post2010_FoodBalanceItem", partrel = TRUE)

    # reduce the mapping to those in SUA, as there are too many SUA items
    relationmatrixS <- relationmatrix[which(relationmatrix$post2010_SupplyUtilizationItem %in%
                                              getItems(sua, dim = 3.1)), ]
    prodAttributesSUA      <- toolAggregate(x = prodAttributes, rel = relationmatrixS, dim = 3.2, from = "k",
                                            to = "post2010_SupplyUtilizationItem", partrel = TRUE)

    prodAttributes <- mbind(prodAttributesFB, prodAttributesSUA[, , setdiff(getItems(prodAttributesSUA, dim = 3.2),
                                                                            getItems(prodAttributesFB, dim = 3.2))])

    getSets(prodAttributes) <- c("region", "year", "attributes", "ItemCodeItem")
    getSets(prodAttributesFB) <- c("region", "year", "attributes", "ItemCodeItem")
    getSets(prodAttributesSUA) <- c("region", "year", "attributes", "ItemCodeItem")


    # change prod attributes from share of dm to share of wm
    attributesWM <- (prodAttributes / dimSums(prodAttributes[, , "wm"], dim = "attributes"))

    itemNames <- c(getNames(fb, dim = "ItemCodeItem"), getNames(sua, dim = "ItemCodeItem"))
    itemNamesAttributes <- getNames(attributesWM, dim = 2)
    if (!(all(itemNames %in% itemNamesAttributes))) {
      vcat(verbosity = 2, "The following items were removed from the dataset because of missing prodAttributes: ",
           paste(itemNames[!(itemNames %in% itemNamesAttributes)], collapse = "\", \""))
      fb <- fb[, ,  intersect(getNames(fb, dim = 1), itemNamesAttributes)]
      sua <- sua[, , intersect(getNames(sua, dim = 1), itemNamesAttributes)]
    }
    if (!all(itemNamesAttributes %in% itemNames)) {
      vcat(verbosity = 2, "For the following items there were entries in prodAttributes but no respective data,
                         removed from prodAttributes ",
           paste(itemNamesAttributes[!(itemNamesAttributes %in% itemNames)], collapse = "\", \""))
      prodAttributes <- prodAttributes[, , itemNamesAttributes[itemNamesAttributes %in% itemNames]]
    }

    ### FAO items not relevant to processing, and processing dimensions that need to be added
    noProcessing <- c("livst_rum", "livst_pig", "livst_milk", "livst_egg", "livst_chick", "foddr", "fish", "fibres")
    noProcessingFAO <- .getFAOitems(noProcessing)
    noProcessingFAO <- unique(noProcessingFAO[noProcessingFAO %in% getNames(fb, dim = 1)])

    namesProcessing <- c("production_estimated", "process_estimated",
                         "milling", "brans1", "branoil1", "flour1",
                         "refining", "sugar1", "sugar2", "sugar3", "molasses1", "refiningloss",
                         "extracting", "oil1", "oil2", "oilcakes1", "extractionloss",
                         "fermentation", "alcohol1", "alcohol2", "alcohol3", "alcohol4", "brewers_grain1",
                         "alcoholloss",
                         "distilling", "ethanol1", "distillers_grain1", "distillingloss",
                         "intermediate",
                         "households")

    #### Definition of subfunctions #####

    # run massbalance checks and clear processed positions after calculating process
    .checkAndClear2 <- function(object,
                                objectO = NULL,
                                goodsIn,
                                from,
                                process,
                                reportAs,
                                residual,
                                relevantAttributes = attributeTypes,
                                goodsOut = NULL,
                                extractionBasis = "input",
                                threshold = 1e-5) {
      # perform massbalance tests:

      if (extractionBasis == "input") {
        # 1) input goods balanced?
        if (is.null(objectO)) {
          diff <- (dimSums(object[, , list(goodsIn, c(reportAs, residual))], dim = c("ElementShort"))
                   - dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort")))
        } else {
          diff <- (dimSums(object[, , list(goodsIn, c(reportAs, residual))], dim = c("ElementShort"))
                   - dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort")) -
                     dimSums(objectO[, , list(goodsIn, residual)], dim = c("ElementShort")))
        }
        if (any(abs(diff) > threshold)) {
          stop("NAs in dataset or function corrupt: process not balanced for ",
               paste(goodsIn, collapse = ", "), " reported as ", paste(reportAs, collapse = ", "))
        }

        # 2) output goods balanced...
        if (!is.null(goodsOut)) {
          # ... with input goods?
          if (is.null(objectO)) {
            diff <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort", "ItemCodeItem"))
              - dimSums(object[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
              - dimSums(object[, , list(goodsOut, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
            )
          } else {
            diff <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort", "ItemCodeItem"))
              - dimSums(object[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
              - dimSums(object[, , list(goodsOut, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
              + dimSums(objectO[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
            )
          }
          if (any(abs(diff) > threshold)) {
            stop("NAs in dataset or function corrupt: goods not balanced for ",
                 paste(goodsOut, collapse = ", "), " from ", paste(goodsIn, collapse = ", "))
          }

          # ... in production?
          diff <- (sum(object[, , list(goodsOut, "production_estimated")])
                   - sum(object[, , list(goodsOut, "production")]))
          if (any(abs(diff) > threshold)) {
            stop("Global estimated production does not meet global production for ",
                 paste(goodsOut, collapse = ", "))
          }
        }

        # negative value check
        relValues <- object[, , list(goodsIn, c(reportAs, from, residual), relevantAttributes)]
        if (any(relValues < -threshold)) {
          warning("Massbalancing failed, negative values for ",
                  paste(unique(unname(where(relValues < -threshold)[[1]]$individual[, 3])), collapse = ", "))
        }


        # move from "from" to "process" and clear "from"
        # this is done in .extractGoodfromFlow in the "output" case
        if (from != process) {
          object[, , list(goodsIn, process)] <- dimSums(object[, , list(goodsIn, from)], dim = 3.2) +
            object[, , list(goodsIn, process)]
        }
        object[, , list(goodsIn, from)] <- 0  # if from == process it is "intermediate" which is to be cleared as well



      } else if (extractionBasis == "output") {
        # 1) output goods balanced...
        if (!is.null(goodsOut)) {
          # ... with input goods?
          diff <- (dimSums(object[, , list(goodsIn, "process_estimated")], dim = c("ElementShort", "ItemCodeItem"))
            - dimSums(object[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
            - dimSums(object[, , list(goodsOut, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
          )
          if (any(abs(diff) > threshold)) {
            stop("NAs in dataset or function corrupt: goods not balanced for ",
                 paste(goodsOut, collapse = ", "), " from ", paste(goodsIn, collapse = ", "))
          }

          # ... in production?
          diff <- (sum(object[, , list(goodsOut, "production_estimated")])
                   - sum(object[, , list(goodsOut, "production")]))
          if (any(abs(diff) > threshold)) {
            # check here instead if less than 1% as it is sum over time and space
            stop("Global estimated production does not meet global production for ",
                 paste(goodsOut, collapse = ", "))
          }
        }

        # negative value check
        relValues <- object[, , list(goodsIn, c(reportAs, from, residual), relevantAttributes)]
        if (any(relValues < -threshold)) {
          warning("Massbalancing failed, negative values for ",
                  paste(unique(unname(where(relValues < -threshold)[[1]]$individual[, 3])), collapse = ", "))
        }


      }
      gc()



      return(object)
    }

    # Different processes, e.g. ethanol production from cereals, are not specified
    # in cbc (instead the general categories "other_util" and "processed" are used),
    # but are required within MAgPIE.
    # This function calculates the maximum amount out of a given product "goodIn.from"
    # that can be used to produce a given output product "goodOut", depending on
    # the "extractionQuantity" and the "extractionAttribute". This quantity is
    # reported as "goodIn.reportAs", while any remaining quantity in "goodIn.from"
    # is reported as "goodIn.residual". The full amount in "goodIn.from" is then
    # moved to "goodIn.process" (i.e. "goodIn.from" will be empty after the function
    # call). The variable "process" specifies the process leading to the output product
    # "reportAs".
    # The calculated production quantity is also added to "goodOut.production_estimated".
    .extractGoodFromFlow2 <- function(object,
                                      goodIn,                        # FAO-defined input product, e.g. "2536|Sugar cane"
                                      from,                          # FAO-defined process, e.g. "other_util"
                                      process,                       # MAgPIE-defined process, e.g. "distilling"
                                      goodOut,                       # FAO-defined output product, e.g. "X001|Ethanol"
                                      reportAs,                      # MAgPIE-defined output product, e.g. "ethanol1"
                                      residual,                      # MAgPIE-defined residual, e.g. "distillingloss"
                                      extractionQuantity,  # e.g. 0.516006
                                      extractionAttribute, # e.g. "dm"
                                      prodAttributes) {

      if (length(from) > 1 || length(reportAs) > 1 || length(goodIn) > 1 || length(goodOut) > 1) {
        stop("please only use one item each for \"from\", \"reportAs\", \"goodIn\", and \"goodOut\"")
      }
      if (any(object[, , list(goodIn, c(reportAs, residual))] != 0)) {
        warning("Output flows already exist!")
      }

      # relevant attributes for extraction quantity
      attrNoWM <- setdiff(attributeTypes, "wm")

      # calculating possible extraction quantity per attribute
      attributesFrom   <- dimSums(object[, , list(goodIn, from), drop = TRUE], dim = "region") /
        dimSums(object[, , list(goodIn, from, extractionAttribute), drop = TRUE], dim = c("region"))
      attributesTo     <- prodAttributes[, , goodOut, drop = TRUE] /
        prodAttributes[, , list(goodOut, extractionAttribute), drop = TRUE]
      extractionFactor <- attributesFrom[, , attrNoWM] / attributesTo[, , attrNoWM]
      extractionFactor[is.na(extractionFactor)] <- 1


      # maximum extraction quantity as minimum over the possible quantity per attribute
      maxextract <- as.magpie(apply(X = extractionFactor, MARGIN = 2, FUN = min))
      if (extractionQuantity == "max") {
        extractionQuantity <- maxextract
      } else if (any(extractionQuantity > maxextract)) {
        stop("too high extraction quantity")
      }

      # calculate outputs
      extracted <- object[, , list(goodIn, from, extractionAttribute), drop = TRUE] * extractionQuantity * attributesTo
      losses    <- dimSums(object[, , list(goodIn, from)], dim = "ElementShort") - extracted

      object[, , list(goodIn, reportAs)] <- extracted
      object[, , list(goodIn, residual)] <- losses

      object[, , list(goodOut, "production_estimated")] <- object[, , list(goodOut, "production_estimated")] + extracted

      # check results and clear processed position
      object <- .checkAndClear2(object, objectO = NULL, goodIn, from, process, reportAs, residual, attrNoWM)

      return(object)
    }

    # This function is similar to .extractGoodFromFlow, with the difference that
    # multiple input goods can be given (which will then be added up before calculating
    # the amount of "goodsOut" that can be produced), and that multiple output goods
    # (and corresponding items in reportAs) can be given. The order of FAO categories
    # in "goodsOut" and corresponding MAgPIE categories in "reportAs" needs to match!
    # In contrast to .extractGoodFromFlow, this function calculates global
    # conversion factors per attribute instead of using an "extractionQuantity"
    # and "extractionAttribute" for calculations.
    .processingGlobal2 <- function(object,
      objectO = NULL, # for cases where there is some residual already we need the original object
      goodsIn,  # e.g. c("2536|Sugar cane", "2537|Sugar beet")
                                  from,     # e.g. "processed" #nolint
                                  process,  # e.g. "refining"
                                  goodsOut, # e.g. c("2818|Sugar, Refined Equiv", "2544|Molasses")(the order matters!)
                                  reportAs, # e.g. c("sugar1", "molasses1") - (the order matters!)
                                  extractionBasis = "input", # Base quantity extracted on input good (default),
                                  # or output good in the case of one input good multiple processes
                                  extractionFactor = NULL, # only relevant for extractionBasis output, when we have a factor for outputs #nolint
                                  extractionAttribute = "wm", # only relevant with extractionFactor
                                  residual  # e.g. "refiningloss"
    ) {
      if (residual != "food" && residual != "alcoholloss" &&
         any(object[, , list(goodsIn, c(reportAs, residual))] != 0)) { # nolint
        stop("Output flows already exist.")
      }

      if (any(object[, , list(goodsOut, "production_estimated")] != 0)) { # nolint
        stop("Output flows already exist.")
      }


      # attributes relevant for checking massbalance and convFactor
      relevantAttributes <- setdiff(attributeTypes, "wm")

      # calculate global conversion factor per attributes
      convFactor <- (dimSums(object[, , list(goodsOut, "production")], dim = c("region", "ElementShort"))
                     / dimSums(object[, , list(goodsIn, from)], dim = c("region", "ItemCodeItem", "ElementShort")))

      factors <- dimSums(convFactor[, , list(goodsOut, relevantAttributes)], dim = "ItemCodeItem")

      if (extractionBasis == "input") {

        if (any(factors > 1)) {
          stop("conversion factors exceed 1. not suitable for a global conversion factor.",
               paste(unique(unname(where(factors > 1)[[1]]$individual)), collapse = ", "))
        }


        # estimate outputs
        for (j in seq_along(goodsOut)) {
          object[, , list(goodsIn, reportAs[j])] <- dimSums(object[, , list(goodsIn, from)], dim = "ElementShort") *
            convFactor[, , goodsOut[j], drop = TRUE]

          object[, , list(goodsOut[j], "production_estimated")] <- dimSums(object[, , list(goodsIn, reportAs[j])],
                                                                           dim = c("ElementShort", "ItemCodeItem"))
        }

        if (from %in% c("processed", "other_util")) {
          object[, , list(goodsIn, "process_estimated")] <- dimSums(object[, , list(goodsIn, from)],
                                                                    dim = "ElementShort") +
            object[, , list(goodsIn, "process_estimated")]
        }

        if (residual == "food") {    # special case for milling which takes all the residual as food
          # calculate refining losses as mass balance difference
          object[, , list(goodsIn, "flour1")] <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort"))
                                                  - dimSums(object[, , list(goodsIn, reportAs)],
                                                            dim = c("ElementShort")))
        }
        # calculate refining losses as mass balance difference
        object[, , list(goodsIn, residual)] <-  object[, , list(goodsIn, residual)] +
          (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort"))
           - dimSums(object[, , list(goodsIn, reportAs)], dim = c("ElementShort")))

      } else if (extractionBasis == "output") {

        if (any(factors > 1)) {
          vcat(verbosity = 2, "The following items and attributes had summed conversion factor greater than 1,
                            some more inputs processed than attributes would suggest: ",
               paste0(c(getItems(object[, , c(goodsIn, goodsOut)], dim = 3.1),
                        unique(as.vector(where(factors > 1)$true$individual)))))
        }

        # we can't know how much of each input goes into each out so we distribute them proportionally
        ratioIns <- object[, , list(goodsIn, from)] / dimSums(object[, , list(goodsIn, from)], dim = "ItemCodeItem")
        # just in case there is production but no goodsIn we give it equal shares so it shows up in production estimated
        ratioIns[is.na(ratioIns)] <- 1 / length(goodsIn)
        # estimate outputs

        if (!is.null(extractionFactor)) {

          attributesTo <- attributesWM[, , goodsIn]
          extractionConversion <- attributesTo / extractionFactor


          object[, , list(goodsIn, process)] <- (dimSums(ratioIns, dim = "ElementShort") *
                                                   dimSums(object[, , list(goodsOut,
                                                                           "production")][, , extractionAttribute],
                                                           dim = c("ElementShort", "ItemCodeItem")) *
                                                   extractionConversion) +
            object[, , list(goodsIn, process)]
        } else {

          object[, , list(goodsIn, process)] <- (dimSums(ratioIns, dim = "ElementShort") *
                                                   dimSums(object[, , list(goodsOut, "production")],
                                                           dim = c("ElementShort", "ItemCodeItem"))) +
            object[, , list(goodsIn, process)]

        }

        for (j in seq_along(goodsOut)) {

          object[, , list(goodsIn, reportAs[j])] <- ratioIns * dimSums(object[, , list(goodsOut[j], "production")],
                                                                       dim = c("ElementShort", "ItemCodeItem")) +
            object[, , list(goodsIn, reportAs[j])]

          object[, , list(goodsOut[j], "production_estimated")] <- dimSums(object[, , list(goodsOut[j], "production")],
                                                                           dim = "ElementShort")
        }

        object[, , list(goodsIn, residual)] <- (dimSums(object[, , list(goodsIn, process)],
                                                        dim = c("ElementShort", "ItemCodeItem"))
                                                - (dimSums(object[, , list(goodsIn, reportAs)],
                                                           dim = c("ElementShort", "ItemCodeItem"))))

        if (from == "processed") {
          object[, , list(goodsIn, "process_estimated")] <- dimSums(object[, , list(goodsIn, process)],
                                                                    dim = c("ElementShort"))  +
            object[, , list(goodsIn, "process_estimated")]
        }
        # remove the amount processed
        object[, , list(goodsIn, from)] <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort"))
                                            - (dimSums(object[, , list(goodsIn, process)], dim = c("ElementShort"))))

        # if less than zero this is tracked in difference between production and process_estimated, remove 0's here
        object[, , list(goodsIn, from)][which(object[, , list(goodsIn, from)] < 0)] <- 0

      }


      # check results and clear processed position
      object <- .checkAndClear2(object, objectO, goodsIn, from,
                                process, reportAs, residual,
                                extractionBasis = extractionBasis,
                                relevantAttributes, goodsOut)
      return(object)
    }

    ### process-specific functions

    # processing of maiz and sugar cane  (other_util) to ethanol, distillers grain and distilling loss
    .ethanolProcessing <- function(object) {
      # subtract the starch and keep it in the other_util, afterwards add IEA residual to other_util (stays as is)
      "
    ethanol and maize:  https://doi.org/10.3390/fermentation7040268 #nolint
    Production of Bioethanol - A Review of Factors Affecting Ethanol Yield
    corn: 400 l/t
    sugarcane: 72.5 l/t
    ethanol weight per l:  789g
    "

      # subtract the starch and keep it in the other_util, afterwards add IEA residual to other_util (stays as is)

      prodIn <- "56|Maize (corn)"
      # liter yield multiplied by product attributes
      ethanolYieldLiterPerTonMaize <- 400 / attributesWM[, , prodIn][, , "dm"]


      # liter yield converted to dm (-> extraction factor)
      extractionQuantityMaize <- 0.789 * ethanolYieldLiterPerTonMaize / 1000

      # ethanol processing from tece and maize (ethanol1, distillers_grain, and distillingloss)
      object[, , c(prodIn, "X001|Ethanol")] <- .extractGoodFromFlow2(
          object = object[, , c(prodIn, "X001|Ethanol")], # nolint
          goodIn = prodIn,
          from = "other_util",
          process = "distilling",
          goodOut = "X001|Ethanol",
          reportAs = "ethanol1",
          residual = "intermediate",
          extractionQuantity = extractionQuantityMaize,
          extractionAttribute = "dm",
          prodAttributes = prodAttributes)

      object[, , c(prodIn, "X002|Distillers_grain")] <- .extractGoodFromFlow2(
          object = object[, , c(prodIn, "X002|Distillers_grain")], # nolint
          goodIn = prodIn,
          from = "intermediate",
          process = "intermediate",
          goodOut = "X002|Distillers_grain",
          reportAs = "distillers_grain1",
          residual = "distillingloss",
          extractionQuantity = "max",
          extractionAttribute = "nr",
          prodAttributes = prodAttributes)


      prodIn <- "156|Sugar cane"

      ethanolYieldLiterPerTonSugarcane <- 72.5 / attributesWM[, , prodIn][, , "dm"]

      # liter yield converted to dm (-> extraction factor)
      extractionQuantitySugarcane <- 0.789 * ethanolYieldLiterPerTonSugarcane / 1000

      # ethanol processing from sugarcane (only ethanol1 and distillingloss)
      object[, , c(prodIn, "X001|Ethanol")] <- .extractGoodFromFlow2(
        object = object[, , c(prodIn, "X001|Ethanol")], # nolint
        goodIn = prodIn,
        from = "other_util",
        process = "distilling",
        goodOut = "X001|Ethanol",
        reportAs = "ethanol1",
        residual = "distillingloss",
        extractionQuantity = extractionQuantitySugarcane,
        extractionAttribute = "dm",
        prodAttributes = prodAttributes)


      # liter yield for non-centrifugal sugar
      ethanolYieldLiterPerTonSugar <- 0.5

      # liter yield converted to dm (-> extraction factor)
      extractionQuantitySugar <- 0.789 * ethanolYieldLiterPerTonSugar / 1000

      # ethanol processing from sugarcane (only ethanol1 and distillingloss)
      object[, , c("163|Cane sugar, non-centrifugal", "X001|Ethanol")] <- .extractGoodFromFlow2(
        object = object[, , c("163|Cane sugar, non-centrifugal", "X001|Ethanol")], # nolint
        goodIn = "163|Cane sugar, non-centrifugal",
        from = "other_util",
        process = "distilling",
        goodOut = "X001|Ethanol",
        reportAs = "ethanol1",
        residual = "distillingloss",
        extractionQuantity = extractionQuantitySugar,
        extractionAttribute = "dm",
        prodAttributes = prodAttributes)


      return(object)
    }

    # processing of sugar cane and sugar beet (processed) to sugar1, molasses1 and refiningloss,
    # processing of starches to glucose and fructose
    .sugarProcessing <- function(object) {

      goodsIn <- c("156|Sugar cane", "157|Sugar beet")
      goodsOut <- c("164|Refined sugar", "165|Molasses", "163|Cane sugar, non-centrifugal")
      object[, , c(goodsIn, goodsOut)] <- .processingGlobal2(object = object[, , c(goodsIn, goodsOut)],
                                                             goodsIn = goodsIn,
                                                             from = "processed",
                                                             process = "refining",
                                                             goodsOut = goodsOut,
                                                             reportAs = c("sugar1", "molasses1", "sugar3"),
                                                             residual = "refiningloss")

      goodsIn <- c(starches)
      goodsOut <- c("166|Other fructose and syrup", "172|Glucose and dextrose")
      object[, , c(starches, goodsOut)] <- .processingGlobal2(object = object[, , c(starches, goodsOut)],
                                                              goodsIn = goodsIn,
                                                              from = "processed",
                                                              process = "refining",
                                                              goodsOut = goodsOut,
                                                              reportAs = c("sugar1", "sugar2"),
                                                              extractionBasis = "output",
                                                              residual = "refiningloss")

      # we use the starches as these alllow us to know how much is going into processed,
      # remove these from the amount of processed in the main crop,
      # as well as assign the amount refining (along with demand etc to the main crop)

      names(starches) <- c("125|Cassava, fresh", "15|Wheat", "27|Rice", "56|Maize (corn)", "116|Potatoes")
      for (i in seq_along(starches)) {
        # take away this amount of processing from the total, as starches have assumed same attributes as sugars,
        # the residual remains for food use
        object[, , list(names(starches)[[i]], "processed")] <-  object[, , list(names(starches)[[i]], "processed")] -
          dimSums(object[, , list(starches[[i]], c("sugar1", "sugar2"))], dim = 3.2)
        object[, , list(names(starches)[[i]], "refining")]  <-  dimSums(object[, , list(starches[[i]],
                                                                                        c("sugar1", "sugar2"))],
                                                                        dim = 3.2)
        object[, , list(names(starches)[[i]], c("sugar1", "sugar2"))] <- object[, , list(starches[[i]],
                                                                                         c("sugar1", "sugar2"))]

        # if process_estimated already exists (i.e. maize from the fermenting step before, add it on)
        object[, , list(names(starches)[[i]], "process_estimated")] <-  object[, , list(names(starches)[[i]],
                                                                                        "process_estimated")] +
          object[, , list(starches[[i]], "process_estimated"), drop = TRUE]
      }

      # sometimes there are starches that are imported, or appear out of thin air,
      # or glucose production appears without starch processing
      # here we need to make negatives 0, still tracked in process_estimated
      object[object < 0] <- 0


      return(object)
    }


    # processing of tece (processed) to alcohol1 and alcoholloss
    .beerProcessing <- function(object) {  ### do it product specific

      beerCereals <- c("44|Barley", "27|Rice",  "56|Maize (corn)", "15|Wheat", "79|Millet", "83|Sorghum")
      beersOut <- c("51|Beer of barley, malted", "39|Rice-fermented beverages", "66|Beer of maize, malted",
                    "26|Wheat-fermented beverages", "82|Beer of millet, malted",
                    "86|Beer of sorghum, malted")

      for (j in seq_along(beerCereals)) {

        object[, , c(beerCereals[j], beersOut[j])] <- .processingGlobal2(object = object[, ,
                                                                                         c(beerCereals[j],
                                                                                           beersOut[j])],
                                                                         goodsIn = beerCereals[j],
                                                                         from = "processed",
                                                                         process = "fermentation",
                                                                         goodsOut = beersOut[j],
                                                                         reportAs = "alcohol1",
                                                                         residual = "intermediate",
                                                                         extractionBasis = "output",
                                                                         extractionFactor = 5)

        object[, , c(beerCereals[j], "X004|Brewers_grain")] <- .extractGoodFromFlow2(object = object[, , c(beerCereals[j], "X004|Brewers_grain")], # nolint
                                                                                     goodIn = beerCereals[j],
                                                                                     from = "intermediate",
                                                                                     process = "intermediate",
                                                                                     goodOut = "X004|Brewers_grain",
                                                                                     reportAs = "brewers_grain1",
                                                                                     residual = "alcoholloss",
                                                                                     extractionQuantity = "max",
                                                                                     extractionAttribute = "dm",
                                                                                     prodAttributes = prodAttributes)
      }

      return(object)
    }

    .cerealMilling <- function(object) {

      cropsIn <- c("15|Wheat",
                   "44|Barley",
                   "71|Rye",
                   "75|Oats",
                   "103|Mixed grain",
                   "89|Buckwheat",
                   "94|Fonio",
                   "97|Triticale",
                   "79|Millet",
                   "83|Sorghum",
                   "108|Cereals nec")


      bransOut <- c("17|Bran of wheat",
                    "47|Bran of barley",
                    "73|Bran of rye",
                    "77|Bran of oats",
                    "105|Bran of mixed grain",
                    "91|Bran of buckwheat",
                    "96|Bran of fonio",
                    "99|Bran of triticale",
                    "81|Bran of millet",
                    "85|Bran of sorghum",
                    "112|Bran of cereals nec")

      whtGerm <- "19|Germ of wheat"

      # add wheat germ to brans
      object[, , "17|Bran of wheat"] <- object[, , "17|Bran of wheat"] + object[, , whtGerm, drop = TRUE]

      # main crops
      for (j in seq_along(cropsIn)) {

        object[, , c(cropsIn[j],  bransOut[j])] <-  .processingGlobal2(object = object[, , c(cropsIn[j], bransOut[j])],
                                                                       objectO = object[, , c(cropsIn[j], bransOut[j])],
                                                                       goodsIn = cropsIn[j],
                                                                       from = "processed",
                                                                       process = "milling",
                                                                       goodsOut = bransOut[j],
                                                                       reportAs = c("brans1"),
                                                                       residual = "food",
                                                                       extractionBasis = "input")
      }
      # milling of maize makes brans germoil and germcakes

      # rice makes brancakes - assigned to brans due to its attributes, and branoils
      riceIn <-  c("27|Rice")
      roOut <- c("36|Oil of rice bran")
      rcOut <- c("37|Cake of rice bran")

      object[, , c(riceIn, roOut, rcOut)] <-  .processingGlobal2(object = object[, , c(riceIn,
                                                                                       roOut, rcOut)],
                                                                 objectO = object[, , c(riceIn,
                                                                                        roOut, rcOut)],
                                                                 goodsIn = riceIn,
                                                                 from = "processed",
                                                                 process = "milling",
                                                                 goodsOut = c(rcOut, roOut),
                                                                 reportAs = c("oilcakes1", "branoil1"),
                                                                 residual = "food",
                                                                 extractionBasis = "input")
      maizIn <-  c("56|Maize (corn)")
      brOut <- c("59|Bran of maize")
      moOut <- c("60|Oil of maize")
      mcOut <- c("61|Cake of maize")



      object[, , c(maizIn, brOut, moOut, mcOut)] <-  .processingGlobal2(object = object[, , c(maizIn, brOut,
                                                                                              moOut, mcOut)],
                                                                        objectO = object[, , c(maizIn, brOut,
                                                                                               moOut, mcOut)],
                                                                        goodsIn = maizIn,
                                                                        from = "processed",
                                                                        process = "milling",
                                                                        goodsOut = c(brOut, moOut, mcOut),
                                                                        reportAs = c("brans1", "branoil1", "oilcakes1"),
                                                                        residual = "food",
                                                                        extractionBasis = "input")


      return(object)
    }


    # extraction of oil and oilcakes from oilcrops (processed)
    .oilProcessing <- function(object) {
      # we combine olives with rapeseed here as they have no cakes,
      # leading to imbalcnce in processing conversion later on when the rapeseed k category
      # gets harmonized. Mostly Spain now produces very expensive rapsoil
      object[, , "270|Rape or colza seed"] <-  object[, , "270|Rape or colza seed"] +
        dimSums(object[, , "260|Olives"], dim = 3.1)
      object[, , "271|Rapeseed or canola oil, crude"] <-  object[, , "271|Rapeseed or canola oil, crude"] +
        dimSums(object[, , "261|Olive oil"], dim = 3.1)


      # do the oilcrops with cakes
      # orders must match!
      cropsIn <- c("236|Soya beans", "242|Groundnuts, excluding shelled", "267|Sunflower seed",
                   "329|Cotton seed", "270|Rape or colza seed", "292|Mustard seed",
                   "249|Coconuts, in shell", "289|Sesame seed", "311|Kapokseed in shell",
                   "333|Linseed", "336|Hempseed",
                   "339|Other oil seeds, nec")

      oilOut <- c("237|Soya bean oil", "244|Groundnut oil", "268|Sunflower-seed oil, crude",
                  "331|Cottonseed oil", "271|Rapeseed or canola oil, crude", "293|Mustard seed oil, crude",
                  "252|Coconut oil", "290|Oil of sesame seed", "313|Oil of kapok",
                  "334|Oil of linseed", "337|Oil of hempseed",
                  "340|Other oil of vegetable origin, crude nec")

      cakeOut <- c("238|Cake of  soya beans", "245|Cake of groundnuts", "269|Cake of sunflower seed",
                   "332|Cake of cottonseed", "272|Cake of rapeseed", "294|Cake of mustard seed",
                   "253|Cake of copra", "291|Cake of sesame seed", "314|Cake of kapok",
                   "335|Cake of  linseed",  "338|Cake of hempseed",
                   "341|Cake, oilseeds nes")


      # oils processed -> food

      # main oil crops
      for (j in seq_along(cropsIn)) {
        object[, , c(cropsIn[j], oilOut[j])] <- .processingGlobal2(object = object[, , c(cropsIn[j], oilOut[j])],
                                                                   goodsIn = cropsIn[j],
                                                                   from = "processed",
                                                                   process = "extracting",
                                                                   goodsOut = oilOut[j],
                                                                   reportAs = "oil1",
                                                                   residual = "intermediate")
        object[, , c(cropsIn[j], cakeOut[j])] <- .extractGoodFromFlow2(object = object[, , c(cropsIn[j], cakeOut[j])],
                                                                       goodIn = cropsIn[j],
                                                                       from = "intermediate",
                                                                       process = "intermediate",
                                                                       goodOut = cakeOut[j],
                                                                       reportAs = "oilcakes1",
                                                                       residual = "extractionloss",
                                                                       extractionQuantity = "max",
                                                                       extractionAttribute = "dm",
                                                                       prodAttributes = prodAttributes)
      }


      return(object)
    }

    # processing of oil and oilcake from palm/palmkernel (processed)
    .oilpalmProcessing <- function(object) {
      # aggregate FAO products relating to oilpalm to a single raw product
      faoProductsOilpalm <- c("257|Palm oil", "258|Oil of palm kernel", "259|Cake of palm kernel")
      newproduct <- dimSums(object[, , list("production", faoProductsOilpalm, "dm")],
                            dim = c("ItemCodeItem", "ElementShort", "attributes"))
      newproduct <- prodAttributes[, , "X003|Palmoil_Kerneloil_Kernelcake"] * newproduct
      object[, , list("X003|Palmoil_Kerneloil_Kernelcake",
                      c("production", "domestic_supply", "processed"))] <- newproduct

      # extract oil
      goodIn <- "X003|Palmoil_Kerneloil_Kernelcake"
      goodsOut1 <- c("257|Palm oil", "258|Oil of palm kernel")
      goodsOut2 <- "259|Cake of palm kernel"

      object[, , c(goodIn, goodsOut1)] <- .processingGlobal2(object = object[, , c(goodIn, goodsOut1)],
                                                             goodsIn = goodIn,
                                                             from = "processed",
                                                             process = "extracting",
                                                             goodsOut = goodsOut1,
                                                             reportAs = c("oil1", "oil2"),
                                                             residual = "intermediate")

      object[, , c(goodIn, goodsOut2)] <- .extractGoodFromFlow2(object = object[, , c(goodIn, goodsOut2)],
                                                                goodIn = goodIn,
                                                                from = "intermediate",
                                                                process = "intermediate",
                                                                goodOut = goodsOut2,
                                                                reportAs = "oilcakes1",
                                                                residual = "extractionloss",
                                                                extractionQuantity = "max",
                                                                extractionAttribute = "dm",
                                                                prodAttributes = prodAttributes)

      return(object)
    }



    # main function combining all processing functions
    .massbalanceProcessing <- function(years) {
      # preparing dataset for given years
      .prepareDataset <- function(data) {

        cells <- getCells(data)
        s1 <- getNames(data, dim = 1)
        s2 <- c(getNames(data, dim = 2), namesProcessing)
        s3 <- attributeTypes
        flows <- array(dim = c(length(cells), length(years), length(s1), length(s2), length(s3)),
                       dimnames = list(cells, years, s1, s2, s3))
        flows <- as.magpie(flows)
        getSets(flows) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")
        flows[, , getNames(data, dim = 2)] <- data[, years, ] * attributesWM[, ,  getItems(data, dim = 3.1)]
        gc()

        # conversion from 10^12 kcal to PJ
        flows[, , list("households", "ge")] <- data[, years, "food_supply_kcal"] * 4.184
        # conversion of protein to nitrogen using average N content
        flows[, , list("households", "nr")] <- data[, years, "protein_supply"] / 6.25
        flows[, , list("households", "wm")] <- data[, years, "food"] # this was changed from food_supply

        flows <- flows[, , setdiff(getNames(flows, dim = "ElementShort"),
                                   c("food_supply_kcal", "protein_supply", "food_supply", "fat_supply"))]
        flows[is.na(flows)]  <- 0
        flows[is.nan(flows)] <- 0
        gc()
        return(flows)
      }

      fbFlows <- .prepareDataset(fb)
      suaFlows <- .prepareDataset(sua)
      # relevant processing dimensions
      millingDimensions <- c("production", "production_estimated", "process_estimated", "intermediate",
                             "extractionloss", "milling", "processed", "brans1",
                             "branoil1", "oilcakes1", "flour1", "food", "households")
      distillingDimensions <- c("production", "production_estimated", "process_estimated", "other_util", "distilling",
                                "ethanol1", "intermediate", "distillers_grain1", "distillingloss")
      fermentationDimensions <- c("production", "production_estimated", "process_estimated", "processed",
                                  "fermentation", "alcohol1", "intermediate", "brewers_grain1", "alcoholloss")
      refiningDimensions <- c("production", "production_estimated", "process_estimated", "processed",
                              "sugar1", "sugar2", "sugar3", "molasses1", "refining", "refiningloss")
      extractingDimensions <- c("production", "production_estimated", "process_estimated", "domestic_supply",
                                "processed", "extracting", "oil1", "oil2",
                                "intermediate", "oilcakes1", "extractionloss")

      # relevant processing products
      millingSUA <- c(.getFAOitemsSUA(c("tece", "maiz", "rice_pro", "trce", "brans")),
                      "61|Cake of maize", "37|Cake of rice bran",
                      "60|Oil of maize", "36|Oil of rice bran")
      millingSUA <- millingSUA[millingSUA != ""]

      distillingSUA <- c(.getFAOitemsSUA(c("maiz")), .getFAOitemsSUA(c("sugr_cane", "ethanol")),
                         "163|Cane sugar, non-centrifugal",
                         "X002|Distillers_grain")
      distillingSUA <- distillingSUA[-grep("gluten|lour|Germ", distillingSUA)]

      fermentationSUA <- c(.getFAOitemsSUA(c("tece", "rice_pro", "trce", "maiz")), beers,  "X004|Brewers_grain")
      fermentationSUA <- fermentationSUA[-grep("lour|Oat|Triticale|Fonio|Buckwheat|Mixed|Rye|nec",
                                               fermentationSUA)]
      fermentationSUA <- fermentationSUA[fermentationSUA != ""]

      refiningSUA <- c(.getFAOitemsSUA(c("sugr_cane", "sugr_beet", "potato", "maiz", "tece", "rice_pro", "sugar")),
                       molasses, cassava)
      refiningSUA <- refiningSUA[-grep("gluten|lour|Germ|Barley|grain|Buckwheat|Oats|Fonio|Triticale|Rye|nec",
                                       refiningSUA)]
      refiningSUA <- refiningSUA[refiningSUA != ""]


      extracting1SUA <- c("257|Palm oil", "258|Oil of palm kernel",
                          "259|Cake of palm kernel", "X003|Palmoil_Kerneloil_Kernelcake")
      extracting2SUA <- c(oils, oilCrops, oilcakes)

      flowsCBC <- suaFlows

      flowsCBC[, , list(distillingSUA, distillingDimensions)] <-
        .ethanolProcessing(flowsCBC[, , list(distillingSUA, distillingDimensions)])
      flowsCBC[, , list(fermentationSUA, fermentationDimensions)] <-
        .beerProcessing(flowsCBC[, , list(fermentationSUA, fermentationDimensions)])
      flowsCBC[, , list(refiningSUA, refiningDimensions)] <-
        .sugarProcessing(flowsCBC[, , list(refiningSUA, refiningDimensions)])
      # remove starches as we assume all strrches are used for glucose and fructose
      flowsCBC <- flowsCBC[, , starches, invert = TRUE]
      flowsCBC[, , list(millingSUA, millingDimensions)] <-
        .cerealMilling(flowsCBC[, , list(millingSUA, millingDimensions)])
      # add germ of wheat to the wheat bran for all those not added in the processing
      flowsCBC[, , setdiff(getItems(flowsCBC, dim = 3.2), millingDimensions)][, , "17|Bran of wheat"] <-
        flowsCBC[, , setdiff(getItems(flowsCBC, dim = 3.2), millingDimensions)][, , "17|Bran of wheat"] +
        flowsCBC[, , setdiff(getItems(flowsCBC, dim = 3.2), millingDimensions)][, , "19|Germ of wheat", drop = TRUE]
      # we combined wheat germ with brans in the above, so remove here
      flowsCBC <- flowsCBC[, , "19|Germ of wheat", invert = TRUE]
      # FAO accoutns for brans as brans --> processing -- > gluten feed an meal --> feed
      # so we assign brans --> processing directly to feed
      bransNoGerm <- brans[-which(brans == "19|Germ of wheat")]
      flowsCBC[, , bransNoGerm][, , "feed"] <- flowsCBC[, , bransNoGerm][, , "feed"] +
        flowsCBC[, , bransNoGerm][, , "processed", drop = TRUE]
      flowsCBC[, , bransNoGerm][, , "processed"]  <- 0

      # we need to do some harmonizing of the tece conversion factors
      teceIn <- c("15|Wheat", "44|Barley", "71|Rye",
                  "75|Oats", "103|Mixed grain", "89|Buckwheat",
                  "94|Fonio", "97|Triticale", "108|Cereals nec")
      bransOut <- c("17|Bran of wheat", "47|Bran of barley",
                    "73|Bran of rye", "77|Bran of oats",
                    "105|Bran of mixed grain", "91|Bran of buckwheat",
                    "96|Bran of fonio", "99|Bran of triticale",
                    "112|Bran of cereals nec")

      factor <- dimSums(flowsCBC[, , list(unlist(teceIn), "brans1")], dim = c(1, 3.1, 3.2)) /
        dimSums(flowsCBC[, , list(teceIn, "milling")], dim = c(1, 3.1, 3.2))
      flowsCBC[, , list(teceIn, "brans1")] <- factor * dimSums(flowsCBC[, , list(teceIn, "milling")],
                                                               dim = 3.2)
      flowsCBC[, , list(teceIn, "food")] <- dimSums(flowsCBC[, , list(teceIn, "milling")], dim = 3.2) -
        dimSums(flowsCBC[, , list(teceIn, "brans1")], dim = 3.2)
      gc()

      for (j in seq_along(bransOut)) {
        flowsCBC[, , list(bransOut[j], "production_estimated")] <- dimSums(flowsCBC[, , list(teceIn[j], "brans1")],
                                                                           dim = c(3.1, 3.2))
      }

      gc()

      # as well as trce conversion factors
      trceIn <- c("79|Millet", "83|Sorghum")
      bransOut <- c("81|Bran of millet", "85|Bran of sorghum")

      factor <- dimSums(flowsCBC[, , list(unlist(trceIn), "brans1")], dim = c(1, 3.1, 3.2)) /
        dimSums(flowsCBC[, , list(trceIn, "milling")], dim = c(1, 3.1, 3.2))
      flowsCBC[, , list(trceIn, "brans1")] <- factor * dimSums(flowsCBC[, , list(trceIn, "milling")],
                                                               dim = 3.2)
      flowsCBC[, , list(trceIn, "food")] <- dimSums(flowsCBC[, , list(trceIn, "milling")], dim = 3.2) -
        dimSums(flowsCBC[, , list(trceIn, "brans1")], dim = 3.2)
      gc()

      for (j in seq_along(bransOut)) {
        flowsCBC[, , list(bransOut[j], "production_estimated")] <- dimSums(flowsCBC[, , list(trceIn[j], "brans1")],
                                                                           dim = c(3.1, 3.2))
      }

      gc()

      flowsCBC[, , list(extracting1SUA, extractingDimensions)] <-
        .oilpalmProcessing(flowsCBC[, , list(extracting1SUA, extractingDimensions)])
      flowsCBC[, , list(extracting2SUA, extractingDimensions)] <-
        .oilProcessing(flowsCBC[, , list(extracting2SUA, extractingDimensions)])
      # remove olives as we added them to raps
      flowsCBC <- flowsCBC[, , c("260|Olives", "261|Olive oil"), invert = TRUE]
      # harmonizing conversion factors within the rapeseed group
      goodsIn  <- list("270|Rape or colza seed", "292|Mustard seed",
                       "249|Coconuts, in shell", "289|Sesame seed", "311|Kapokseed in shell",
                       "333|Linseed", "336|Hempseed",
                       "339|Other oil seeds, nec")
      oilsOut <- list("271|Rapeseed or canola oil, crude",  "293|Mustard seed oil, crude",
                      "252|Coconut oil", "290|Oil of sesame seed",  "313|Oil of kapok",
                      "334|Oil of linseed", "337|Oil of hempseed",
                      "340|Other oil of vegetable origin, crude nec")
      cakesOut <- list("272|Cake of rapeseed",  "294|Cake of mustard seed",
                       "253|Cake of copra", "291|Cake of sesame seed", "314|Cake of kapok",
                       "335|Cake of  linseed", "338|Cake of hempseed",
                       "341|Cake, oilseeds nes")

      for (from in c("oil1", "oilcakes1", "extractionloss")) {
        factor <- dimSums(flowsCBC[, , list(unlist(goodsIn), from)], dim = c(1, 3.1, 3.2)) /
          dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")], dim = c(1, 3.1, 3.2))
        flowsCBC[, , list(unlist(goodsIn), from)] <- factor * dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")],
                                                                      dim = 3.2)
        gc()
      }

      for (j in seq_along(oilsOut)) {
        flowsCBC[, , list(oilsOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]], "oil1")],
                                                                            dim = c(3.1, 3.2))
      }
      for (j in seq_along(cakesOut)) {
        flowsCBC[, , list(cakesOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]],
                                                                                               "oilcakes1")],
                                                                             dim = c(3.1, 3.2))
      }
      gc()
      # Alcohol production
      fruitsAlcohol <- c("560|Grapes", "515|Apples", "521|Pears", "526|Apricots",
                         "530|Sour cherries", "531|Cherries", "534|Peaches and nectarines",
                         "536|Plums and sloes", "541|Other stone fruits",
                         "542|Other pome fruits", "544|Strawberries", "547|Raspberries",
                         "549|Gooseberries", "550|Currants", "552|Blueberries",
                         "554|Cranberries", "558|Other berries and fruits of the genus vaccinium nec",
                         "569|Figs", "577|Dates",
                         "603|Other tropical fruits, nec", "619|Other fruits, nec")

      grainsAlcohol <-  .getFAOitemsSUA(c("tece", "trce", "rice_pro", "potato", "cassav_sp", "sugar", "molasses"))
      cropsAlcohol <- c(fruitsAlcohol, grainsAlcohol)
      cropsAlcohol <- intersect(getItems(flowsCBC, dim = 3.1), cropsAlcohol)
      fermentationDimensions <- c("production", "production_estimated", "processed", "process_estimated",
                                  "fermentation", "alcohol2",
                                  "alcohol3", "alcohol4", "intermediate", "brewers_grain1", "alcoholloss")
      fermentationProducts <- .getFAOitemsSUA(c("tece", "others", "trce", "rice_pro", "potato", "cassav_sp", "sugar",
                                                "molasses", "alcohol", "distillers_grain"))
      fermentationProducts <- intersect(getItems(flowsCBC, dim = 3.1), fermentationProducts)

      flowsCBC[, , list(fermentationProducts, fermentationDimensions)] <- .processingGlobal2(
        flowsCBC[, , list(fermentationProducts, fermentationDimensions)], # nolint
        objectO = flowsCBC[, , list(fermentationProducts, fermentationDimensions)],
        goodsIn  = cropsAlcohol,
        from      = "processed",
        process   = "fermentation",
        goodsOut = c("564|Wine",
                     "634|Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80% vol; spirits, liqueurs and other spirituous beverages", # nolint
                     "632|Undenatured ethyl alcohol of an alcoholic strength by volume of 80% vol or higher"),
        reportAs = c("alcohol2", "alcohol3", "alcohol4"), # nolint
        residual  = "alcoholloss")

      # Define use of products that are not existing in FAOSTAT
      goods <- c("X002|Distillers_grain", "X004|Brewers_grain")
      flowsCBC[, , list(goods, c("production", "domestic_supply", "feed"))]  <-
        flowsCBC[, , list(goods, "production_estimated"), drop = TRUE]
      flowsCBC[, , list("X001|Ethanol", c("production", "domestic_supply", "other_util"))] <-
        flowsCBC[, , list("X001|Ethanol", "production_estimated"), drop = TRUE]
      gc()

      # add food, feed, other_util, waste of the secondary ones we don't cover
      # multiply by attributes
      suaSec <- .prepareDataset(suaSec)

      flowsCBC[, ,  getNames(suaSec, dim = 1)][, , c("food", "feed", "other_util", "waste")] <-
        flowsCBC[, , getNames(suaSec, dim = 1)][, , c("food", "feed", "other_util", "waste")] +
        suaSec[, , c("food", "feed", "other_util", "waste")]
      # add remaining 'processed' to 'other_util' and remove obsolete dimensions
      flowsCBC[, , "other_util"] <- dimSums(flowsCBC[, , c("other_util", "processed")], dim = 3.2)
      flowsCBC <- flowsCBC[, , c("processed", "intermediate"), invert = TRUE]
      gc()

      # first map to food balance sheet
      processedFB <- toolAggregate(x = flowsCBC,
                                   rel = relationmatrix,
                                   dim = 3.1,
                                   from = "post2010_SupplyUtilizationItem",
                                   to = "post2010_FoodBalanceItem",
                                   partrel = TRUE)

      # get processed values
      proc <- c("food", "feed", "production_estimated", "process_estimated",
                "milling", "brans1", "branoil1", "flour1",
                "refining", "sugar1", "sugar2", "sugar3", "molasses1", "refiningloss",
                "extracting", "oil1", "oil2", "oilcakes1", "extractionloss",
                "fermentation", "alcohol1", "alcohol2", "alcohol3", "alcohol4", "brewers_grain1", "alcoholloss",
                "distilling", "ethanol1", "distillers_grain1", "distillingloss")

      # use explicit processed values from SUA, remove from FB
      fbFlows <- fbFlows[, , c("processed", "intermediate"), invert = TRUE]
      cprods <-    intersect(getItems(fbFlows, dim = 3.1), getItems(processedFB, dim = 3.1))

      fbFlows[, , proc][, , cprods] <- processedFB[, , proc][, , cprods]

      # oilpalm,  oilcakes, brans, and molasses are not part of FB so add all categories here
      replace <-  processedFB[, , c(oilcakes, missingproducts, molasses, "2600|Brans")][, , proc, invert = TRUE]
      fbFlows[, , c(oilcakes, missingproducts, molasses, "2600|Brans")][, , getItems(replace, dim = 3)] <- replace

      # map to magpie categories
      massbalanceProcessing <- toolAggregate(x = fbFlows,
                                             rel = relationmatrix,
                                             dim = 3.1,
                                             from = "post2010_FoodBalanceItem",
                                             to = "k",
                                             partrel = TRUE)
      gc()
      massbalanceProcessing <- massbalanceProcessing[, , "", invert = TRUE]
      return(massbalanceProcessing)
    }

    # function dealing with the non-processing aspects of cbc
    .massbalanceNoProcessing <- function(years) {
      # initializing magpie object
      cells <- getCells(fb)
      s2 <- c(getNames(fb, dim = 2), namesProcessing)
      s3 <- attributeTypes
      noProcessingFB <- array(dim = c(length(cells), length(years), length(noProcessingFAO), length(s2),
                                      length(s3)), dimnames = list(cells, years, noProcessingFAO, s2, s3))
      noProcessingFB <- as.magpie(noProcessingFB)
      getSets(noProcessingFB) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")

      # adding attributes and filling household dimension
      noProcessingFB[, , getNames(fb, dim = 2)] <- fb[, years, noProcessingFAO] * attributesWM[, , noProcessingFAO]
      # conversion from 10^12 kcal to PJ
      noProcessingFB[, , list("households", "ge")] <- noProcessingFB[, , list("food_supply_kcal", "wm")] * 4.184
      # conversion of protein to nitrogen using average N content
      noProcessingFB[, , list("households", "nr")] <- noProcessingFB[, , list("protein_supply", "wm")] / 6.25
      noProcessingFB[, , list("households", "wm")] <- noProcessingFB[, , list("food", "wm")]
      noProcessingFB <- noProcessingFB[, , setdiff(getNames(noProcessingFB, dim = "ElementShort"),
                                                   c("food_supply_kcal", "protein_supply",
                                                     "fat_supply"))]

      # fill NAs and NaNs
      noProcessingFB[is.na(noProcessingFB)]  <- 0
      noProcessingFB[is.nan(noProcessingFB)] <- 0

      # add 'processed' to 'other_util' and remove obsolete dimensions
      noProcessingFB[, , "other_util"] <- dimSums(noProcessingFB[, , c("other_util", "processed")], dim = 3.2)
      noProcessingFB <- noProcessingFB[, , c("processed", "intermediate"), invert = TRUE]

      # map to magpie categories
      massbalanceNoProcessing <- toolAggregate(x = noProcessingFB,
                                               rel = relationmatrix,
                                               dim = 3.1,
                                               from = "post2010_FoodBalanceItem",
                                               to = "k",
                                               partrel = TRUE)
      gc()
      massbalanceNoProcessing <- massbalanceNoProcessing[, , "", invert = TRUE]

    }


    #### Calculations ####

    # increase magclass sizelimit
    local_options(magclass_sizeLimit = 2e9)

    # option without splitting years (in case of memory issues this can be done in year chunks)
    massbalanceNoProcessing <- .massbalanceNoProcessing(years)
    fb <- fb[, , noProcessingFAO, invert = TRUE]
    massbalanceProcessing <- .massbalanceProcessing(years)

    # put results together
    massbalance <- mbind(massbalanceProcessing, massbalanceNoProcessing)





  } else if (version == "pre2010") {
    #### Data input ####

    ### FAO Commodity Balance
    cbc          <- calcOutput(type = "FAOharmonized", aggregate = FALSE)
    getSets(cbc) <- c("region", "year", "ItemCodeItem.ElementShort")

    if (any(duplicated(dimnames(cbc)[[3]]) == TRUE)) {
      stop("The folowing dimnames are duplicated: ",
           paste(getNames(cbc)[duplicated(getNames(cbc))], collapse = "\", \""))
    }

    # determine years
    if (is.null(years)) {
      years <- getYears(cbc)
    }
    if (nchar(years[[1]]) < 5) {
      years <- paste0("y", years)
    }

    cbc <- cbc[, years, ]

    # remove double counting and add missing products
    removethem <- c(
      # crop commodity balance and Food Supply items aggregated
      "2924|Alcoholic Beverages",
      "2905|Cereals - Excluding Beer",
      "2919|Fruits - Excluding Wine",
      "2928|Miscellaneous",
      "2913|Oilcrops",
      "2911|Pulses",
      "2923|Spices",
      "2907|Starchy Roots",
      "2922|Stimulants",
      "2909|Sugar & Sweeteners",
      "2908|Sugar Crops",
      "2912|Treenuts",
      "2914|Vegetable Oils",
      "2918|Vegetables",
      "2903|Vegetal Products",
      "2901|Grand Total",
      # livestock commodity balance and Food Supply items aggregated
      "2941|Animal Products",
      "2946|Animal fats",
      "2961|Aquatic Products, Other",
      "2949|Eggs",
      "2960|Fish, Seafood",
      "2943|Meat",
      "2948|Milk - Excluding Butter",
      "2738|Milk, Whole",
      "2739|Milk, Skimmed",
      "2945|Offals",
      # others and equivalents
      "2741|Cheese",
      "2556|Groundnuts (Shelled Eq)",
      "2562|Palm kernels",
      "2805|Rice (Milled Equivalent)",
      "2815|Roots & Tuber Dry Equiv",
      "2672|Rubber",
      "2747|Silk",
      "2827|Sugar, Raw Equivalent",
      "2542|Sugar (Raw Equivalent)",
      "2671|Tobacco",
      "2742|Whey",
      "2960|Fish, Seafood"
    )

    cbc <- cbc[, , removethem, invert = TRUE]

    cbc <- complete_magpie(cbc, fill = 0)

    missingproducts <- c("X001|Ethanol",
                         "X002|Distillers_grain",
                         "X003|Palmoil_Kerneloil_Kernelcake",
                         "X004|Brewers_grain")

    cbc <- add_columns(cbc, addnm = missingproducts, dim = 3.1)


    #### Product Attributes
    prodAttributes <- calcOutput("Attributes", aggregate = FALSE)
    attributeTypes <- getNames(prodAttributes, dim  = 1)
    removeProd     <- c("betr", "begr", "pasture", "scp", "res_cereals",
                        "res_fibrous", "res_nonfibrous", "wood", "woodfuel")
    prodAttributes <- prodAttributes[, , removeProd, invert = TRUE]

    # Sectoral mapping for FAO items
    relationmatrix <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")
    relationmatrix <- relationmatrix[, c("FoodBalanceItem", "k")]
    relationmatrix <- relationmatrix[!duplicated(relationmatrix[, "FoodBalanceItem"]), ]

    .getFAOitems <- function(magpieItems) {
      return(relationmatrix[relationmatrix$k %in% magpieItems, "FoodBalanceItem"])
    }

    # Map production attributes to FAO items
    prodAttributes          <- toolAggregate(x = prodAttributes, rel = relationmatrix, dim = 3.2, from = "k",
                                             to = "FoodBalanceItem", partrel = TRUE)
    getSets(prodAttributes) <- c("region", "year", "attributes", "ItemCodeItem")

    # change prod attributes from share of dm to share of wm
    attributesWM <- (prodAttributes / dimSums(prodAttributes[, , "wm"], dim = "attributes"))

    cbcItemnames <- getNames(cbc, dim = "ItemCodeItem")
    itemnamesAttributes <- getNames(attributesWM, dim = "ItemCodeItem")
    if (!(all(cbcItemnames %in% itemnamesAttributes))) {
      vcat(verbosity = 2, "The following items were removed from the dataset because of missing prodAttributes: ",
           paste(cbcItemnames[!(cbcItemnames %in% itemnamesAttributes)], collapse = "\", \""))
      cbc <- cbc[, , cbcItemnames[cbcItemnames %in% itemnamesAttributes]]
      relationmatrix <- relationmatrix[relationmatrix[, "FoodBalanceItem"] %in%
                                         intersect(cbcItemnames, itemnamesAttributes), ]
    }
    if (!all(itemnamesAttributes %in% cbcItemnames)) {
      stop("For the following items there were entries in prodAttributes but no respective data: ",
           paste(itemnamesAttributes[!(itemnamesAttributes %in% cbcItemnames)], collapse = "\", \""))
    }


    ### FAO items not relevant to processing, and processing dimensions that need to be added
    noProcessing <- c("livst_rum", "livst_pig", "livst_milk", "livst_egg", "livst_chick", "foddr", "fish", "fibres")
    noProcessingFAO <- .getFAOitems(noProcessing)
    noProcessingFAO <- noProcessingFAO[noProcessingFAO %in% getNames(cbc, dim = 1)]

    namesProcessing <- c("production_estimated",
                         "milling", "brans1", "branoil1", "flour1",
                         "refining", "sugar1", "molasses1", "refiningloss",
                         "extracting", "oil1", "oil2", "oilcakes1", "extractionloss",
                         "fermentation", "alcohol1", "alcohol2", "alcohol3", "alcohol4",
                         "brewers_grain1", "alcoholloss",
                         "distilling", "ethanol1", "distillers_grain1", "distillingloss",
                         "intermediate",
                         "households")

    #### Definition of subfunctions #####

    # run massbalance checks and clear processed positions after calculating process
    .checkAndClear <- function(object,
                               goodsIn,
                               from,
                               process,
                               reportAs,
                               residual,
                               relevantAttributes = attributeTypes,
                               goodsOut = NULL,
                               threshold = 1e-5) {
      # perform massbalance tests:
      # 1) input goods balanced?
      diff <- (dimSums(object[, , list(goodsIn, c(reportAs, residual))], dim = c("ElementShort"))
               - dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort")))
      if (any(abs(diff) > threshold)) {
        stop("NAs in dataset or function corrupt: process not balanced for ",
             paste(goodsIn, collapse = ", "), " reported as ", paste(reportAs, collapse = ", "))
      }

      # 2) output goods balanced...
      if (!is.null(goodsOut)) {
        # ... with input goods?
        diff <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort", "ItemCodeItem"))
          - dimSums(object[, , list(goodsIn, residual)], dim = c("ElementShort", "ItemCodeItem"))
          - dimSums(object[, , list(goodsOut, "production_estimated")], dim = c("ElementShort", "ItemCodeItem"))
        )
        if (any(abs(diff) > threshold)) {
          stop("NAs in dataset or function corrupt: goods not balanced for ",
               paste(goodsOut, collapse = ", "), " from ", paste(goodsIn, collapse = ", "))
        }

        # ... in production?
        diff <- (sum(object[, , list(goodsOut, "production_estimated")])
                 - sum(object[, , list(goodsOut, "production")]))
        if (any(abs(diff) > threshold)) {
          stop("Global estimated production does not meet global production for ",
               paste(goodsOut, collapse = ", "))
        }
      }

      # 4) special case for cereal milling: branoils balanced?
      if (residual == "flour1") {
        diff <- (dimSums(object[, , list(goodsIn, "branoil1")], dim = c("ElementShort", "ItemCodeItem"))
                 - dimSums(object[, , list(c("2581|Ricebran Oil", "2582|Maize Germ Oil"), "production_estimated")],
                           dim = c("ElementShort", "ItemCodeItem")))
        if (any(abs(diff) > threshold)) {
          stop("NAs in dataset or function corrupt: branoil1 not balanced")
        }
      }

      # negative value check
      relValues <- object[, , list(goodsIn, c(reportAs, from, residual), relevantAttributes)]
      if (any(relValues < -threshold)) {
        warning("Massbalancing failed, negative values for ",
                paste(unique(unname(where(relValues < -threshold)[[1]]$individual[, 3])), collapse = ", "))
      }

      # move from "from" to "process" and clear "from"
      if (from != process) {
        object[, , list(goodsIn, process)] <- object[, , list(goodsIn, from)]
      }
      object[, , list(goodsIn, from)] <- 0  # if from == process it is "intermediate" which is to be cleared as well

      gc()
      return(object)
    }

    # Different processes, e.g. ethanol production from cereals, are not specified
    # in cbc (instead the general categories "other_util" and "processed" are used),
    # but are required within MAgPIE.
    # This function calculates the maximum amount out of a given product "goodIn.from"
    # that can be used to produce a given output product "goodOut", depending on
    # the "extractionQuantity" and the "extractionAttribute". This quantity is
    # reported as "goodIn.reportAs", while any remaining quantity in "goodIn.from"
    # is reported as "goodIn.residual". The full amount in "goodIn.from" is then
    # moved to "goodIn.process" (i.e. "goodIn.from" will be empty after the function
    # call). The variable "process" specifies the process leading to the output product
    # "reportAs".
    # The calculated production quantity is also added to "goodOut.production_estimated".
    .extractGoodFromFlow <- function(object,
                                     goodIn,              # FAO-defined input product, e.g. "2536|Sugar cane"
                                     from,                # FAO-defined process, e.g. "other_util"
                                     process,             # MAgPIE-defined process, e.g. "distilling"
                                     goodOut,             # FAO-defined output product, e.g. "X001|Ethanol"
                                     reportAs,            # MAgPIE-defined output product, e.g. "ethanol1"
                                     residual,            # MAgPIE-defined residual, e.g. "distillingloss"
                                     extractionQuantity,  # e.g. 0.516006
                                     extractionAttribute, # e.g. "dm"
                                     prodAttributes) {

      if (length(from) > 1 || length(reportAs) > 1 || length(goodIn) > 1 || length(goodOut) > 1) {
        stop("please only use one item each for \"from\", \"reportAs\", \"goodIn\", and \"goodOut\"")
      }
      if (any(object[, , list(goodIn, c(reportAs, residual))] != 0)) {
        warning("Output flows already exist!")
      }

      # relevant attributes for extraction quantity
      attrNoWM <- setdiff(attributeTypes, "wm")

      # calculating possible extraction quantity per attribute
      attributesFrom   <- dimSums(object[, , list(goodIn, from), drop = TRUE], dim = "region") /
        dimSums(object[, , list(goodIn, from, extractionAttribute), drop = TRUE], dim = c("region"))
      attributesTo     <- prodAttributes[, , goodOut, drop = TRUE] /
        prodAttributes[, , list(goodOut, extractionAttribute), drop = TRUE]
      extractionFactor <- attributesFrom[, , attrNoWM] / attributesTo[, , attrNoWM]

      # maximum extraction quantity as minimum over the possible quantity per attribute
      maxextract <- as.magpie(apply(X = extractionFactor, MARGIN = 2, FUN = min))
      if (extractionQuantity == "max") {
        extractionQuantity <- maxextract
      } else if (any(extractionQuantity > maxextract)) {
        stop("too high extraction quantity")
      }

      # calculate outputs
      extracted <- object[, , list(goodIn, from, extractionAttribute), drop = TRUE] * extractionQuantity * attributesTo
      losses    <- dimSums(object[, , list(goodIn, from)], dim = "ElementShort") - extracted

      object[, , list(goodIn, reportAs)] <- extracted
      object[, , list(goodIn, residual)] <- losses

      object[, , list(goodOut, "production_estimated")] <- object[, , list(goodOut, "production_estimated")] + extracted

      # check results and clear processed position
      object <- .checkAndClear(object, goodIn, from, process, reportAs, residual, attrNoWM)

      return(object)
    }

    # This function is similar to .extractGoodFromFlow, with the difference that
    # multiple input goods can be given (which will then be added up before calculating
    # the amount of "goodsOut" that can be produced), and that multiple output goods
    # (and corresponding items in reportAs) can be given. The order of FAO categories
    # in "goodsOut" and corresponding MAgPIE categories in "reportAs" needs to match!
    # In contrast to .extractGoodFromFlow, this function calculates global
    # conversion factors per attribute instead of using an "extractionQuantity"
    # and "extractionAttribute" for calculations.
    .processingGlobal <- function(object,
      goodsIn,  # e.g. c("2536|Sugar cane", "2537|Sugar beet")
                                  from,     # e.g. "processed" #nolint
                                  process,  # e.g. "refining"
                                  goodsOut, # e.g. c("2818|Sugar, Refined Equiv", "2544|Molasses") (the order matters!)
                                  reportAs, # e.g. c("sugar1", "molasses1") - (the order matters!)
                                  residual  # e.g. "refiningloss"
    ) {
      if (any(object[, , list(goodsIn, c(reportAs, residual))] != 0)) {
        stop("Output flows already exist.")
      }
      if (any(object[, , list(goodsOut, "production_estimated")] != 0)) {
        stop("Output flows already exist.")
      }

      # attributes relevant for checking massbalance and convFactor
      relevantAttributes <- setdiff(attributeTypes, "wm")

      # calculate global conversion factor per attributes
      convFactor <- (dimSums(object[, , list(goodsOut, "production")], dim = c("region", "ElementShort"))
                     / dimSums(object[, , list(goodsIn, from)], dim = c("region", "ItemCodeItem", "ElementShort")))

      factors <- dimSums(convFactor[, , list(goodsOut, relevantAttributes)], dim = "ItemCodeItem")
      if (any(factors > 1)) {
        stop("conversion factors exceed 1. not suitable for a global conversion factor.",
             paste(unique(unname(where(factors > 1)[[1]]$individual)), collapse = ", "))
      }

      # estimate outputs
      for (j in seq_along(goodsOut)) {
        object[, , list(goodsIn, reportAs[j])] <- dimSums(object[, , list(goodsIn, from)], dim = "ElementShort") *
          convFactor[, , goodsOut[j], drop = TRUE]
        object[, , list(goodsOut[j], "production_estimated")] <- dimSums(object[, , list(goodsIn, reportAs[j])],
                                                                         dim = c("ElementShort", "ItemCodeItem"))
      }

      # calculate refining losses as mass balance difference
      object[, , list(goodsIn, residual)] <- (dimSums(object[, , list(goodsIn, from)], dim = c("ElementShort"))
                                              - dimSums(object[, , list(goodsIn, reportAs)], dim = c("ElementShort")))

      # check results and clear processed position
      object <- .checkAndClear(object, goodsIn, from, process, reportAs, residual, relevantAttributes, goodsOut)

      return(object)
    }


    # processing of cereals (milled) to bran and flour: this is the only process that does
    # not use the functions .extractGoodFromFlow and/or .processingGlobal,
    # as the extraction quantity of bran is calculated in a specific way, using the
    # ratio of bran to full cereal as given by Feedipedia.
    .cerealMillingGlobal <- function(object) {

      cereals <- c("2511|Wheat and products",
                   "2513|Barley and products",
                   "2514|Maize and products",
                   "2515|Rye and products",
                   "2516|Oats",
                   "2517|Millet and products",
                   "2518|Sorghum and products",
                   "2520|Cereals, Other",
                   "2804|Rice (Paddy Equivalent)")

      brans   <- .getFAOitems("brans")
      milled  <- "food"
      flour   <- "flour1"
      process <- "milling"

      if (any(object[, , list(cereals, c(flour, "brans1", "branoil1"))] != 0)) {
        stop("Output flows already exist.")
      }

      milledGlobal   <- dimSums(object[, , list(cereals, milled)], dim = c("region", "ElementShort"))
      bransGlobal    <- dimSums(object[, , list(brans, "production")],
                                dim = c("region", "ElementShort", "ItemCodeItem"))

      # as share of dm instaed of wm
      branAttributes <- bransGlobal / dimSums(bransGlobal[, , "dm"], dim = "attributes")

      # estimating bran based on simple factors (Feedipedia)
      # rice: 10%, wheat: 25%
      # we use 20% for wheat to account for some wholegrain meal
      # own estimates to not violate massbalance: corn and trce get only 5%
      branRatio <- new.magpie("GLO", getYears(object), cereals, fill = 0.20)
      getSets(branRatio) <- c("region", "year", "ItemCodeItem")
      branRatio[, , c("2804|Rice (Paddy Equivalent)", "2514|Maize and products")] <- 0.1
      branRatio[, , c("2518|Sorghum and products", "2517|Millet and products")]   <- 0.05
      bransUncalibrated <- dimSums(branRatio * milledGlobal[, , "dm"], dim = "ItemCodeItem")
      branRatio <- branRatio * dimSums(bransGlobal[, , "dm"], dim = "attributes") / bransUncalibrated

      # bran estimation
      branEstimated <- branRatio * branAttributes * dimSums(object[, , list(cereals, milled)][, , "dm"],
                                                            dim = "attributes")
      object[, , list(cereals, "brans1")]             <- dimSums(branEstimated[, , cereals], dim = c("ElementShort"))
      object[, , list(brans, "production_estimated")] <- dimSums(branEstimated[, , cereals],
                                                                 dim = c("ItemCodeItem", "ElementShort"))
      object[, , list(cereals, flour)]                <- object[, , list(cereals, milled)] - branEstimated

      # branoil estimation
      .branoil1Production <- function(object, branoilItem, cropItem) {
        branoilRatio <- (dimSums(object[, , list(branoilItem, "production")],
                                 dim = c("region", "ItemCodeItem", "ElementShort")) /
                           dimSums(milledGlobal[, , cropItem], dim = "ItemCodeItem"))
        estimatedBranoil <- object[, , list(cropItem, milled)] * branoilRatio
        object[, , list(cropItem, "branoil1")]                <- dimSums(estimatedBranoil[, , cropItem],
                                                                         dim = c("ElementShort"))
        object[, , list(branoilItem, "production_estimated")] <- dimSums(estimatedBranoil[, , cropItem],
                                                                         dim = c("ItemCodeItem", "ElementShort"))
        object[, , list(cropItem, flour)]                     <- object[, , list(cropItem, flour)] - estimatedBranoil
        return(object)
      }

      object <- .branoil1Production(object, "2582|Maize Germ Oil", "2514|Maize and products")
      object <- .branoil1Production(object, "2581|Ricebran Oil", "2804|Rice (Paddy Equivalent)")

      # check results and clear processed position
      object <- .checkAndClear(object, goodsIn = cereals, from = milled, process = process,
                               reportAs = c("brans1", "branoil1"), residual = flour)

      ### Fooduse in brans is included in the commodity balance sheets, but not reflected in calories.
      # We subtract bran consumption from cereal consumption in the respective countries.
      # For simplicity, we distribute brans proportional to all cereal fooduse.
      relAttributes <- c("wm", "ge", "nr")
      branshr <- (dimSums(object[, , list(brans, milled, relAttributes)], dim = c(3.1, 3.2))
                  / dimSums(object[, , list(cereals, "households", relAttributes)], dim = c(3.1, 3.2)))
      branshr[is.nan(branshr)] <- 0
      if (any(branshr < 0)) {
        vcat(1, "branshr should not be smaller than zero.")
      }
      object[, , list(cereals, "households", relAttributes)] <- (1 - branshr) *
        object[, , list(cereals, "households", relAttributes)]
      object[, , list(brans, "households", relAttributes)]   <- object[, , list(brans,
                                                                                milled, relAttributes)]

      return(object)
    }

    # processing of tece and maiz (other_util) to ethanol, distillers grain and distilling loss
    .ethanolProcessing <- function(object) {
      "
    ethanol and maize:  https://doi.org/10.3390/fermentation7040268 #nolint
    Production of Bioethanol - A Review of Factors Affecting Ethanol Yield
    corn: 400 l/t
    sugarcane: 72.5 l/t
    ethanol weight per l:  789g
    "
      # Wheat instead of tece would be more correct, but we need to have homogeneous products
      tece <- .getFAOitems("tece")
      teceMaize <- c(tece,  "2514|Maize and products")

      # liter yield for different sources
      ethanolYieldLiterPerTonTece <- 340
      ethanolYieldLiterPerTonMaize <- 400
      ethanolYieldLiterPerTonSugarcane <- 72.5

      # liter yield converted to dm (-> extraction factor)
      ethanolYieldLiterPerTonTeceMaize <- c(rep(ethanolYieldLiterPerTonTece, length(tece)),
                                            ethanolYieldLiterPerTonMaize)
      extractionQuantityTeceMaize <- 0.789 * ethanolYieldLiterPerTonTeceMaize / 1000 /
        attributesWM[, , "2514|Maize and products"][, , "dm"]
      extractionQuantitySugarcane <- 0.789 * ethanolYieldLiterPerTonSugarcane / 1000 /
        attributesWM[, , "2536|Sugar cane"][, , "dm"]

      # ethanol processing from tece and maize (ethanol1, distillers_grain, and distillingloss)
      for (j in seq_along(teceMaize)) {
        object[, , c(teceMaize[j], "X001|Ethanol")] <- .extractGoodFromFlow(
          object = object[, , c(teceMaize[j], "X001|Ethanol")], # nolint
          goodIn = teceMaize[j],
          from = "other_util",
          process = "distilling",
          goodOut = "X001|Ethanol",
          reportAs = "ethanol1",
          residual = "intermediate",
          extractionQuantity = extractionQuantityTeceMaize[j],
          extractionAttribute = "dm",
          prodAttributes = prodAttributes)

        object[, , c(teceMaize[j], "X002|Distillers_grain")] <- .extractGoodFromFlow(
          object = object[, , c(teceMaize[j], "X002|Distillers_grain")], # nolint
          goodIn = teceMaize[j],
          from = "intermediate",
          process = "intermediate",
          goodOut = "X002|Distillers_grain",
          reportAs = "distillers_grain1",
          residual = "distillingloss",
          extractionQuantity = "max",
          extractionAttribute = "nr",
          prodAttributes = prodAttributes)
      }

      # ethanol processing from sugarcane (only ethanol1 and distillingloss)
      object[, , c("2536|Sugar cane", "X001|Ethanol")] <- .extractGoodFromFlow(
        object = object[, , c("2536|Sugar cane", "X001|Ethanol")], # nolint
        goodIn = "2536|Sugar cane",
        from = "other_util",
        process = "distilling",
        goodOut = "X001|Ethanol",
        reportAs = "ethanol1",
        residual = "distillingloss",
        extractionQuantity = extractionQuantitySugarcane,
        extractionAttribute = "dm",
        prodAttributes = prodAttributes)

      return(object)
    }

    # processing of tece (processed) to alcohol1 and alcoholloss
    .beerProcessing <- function(object) {
      # Barley would be more correct, but we need to have homogenous products
      beercereals <- .getFAOitems("tece")

      object[, , c(beercereals, "2656|Beer")] <- .processingGlobal(object = object[, , c(beercereals, "2656|Beer")],
                                                                   goodsIn = beercereals,
                                                                   from = "processed",
                                                                   process = "fermentation",
                                                                   goodsOut = "2656|Beer",
                                                                   reportAs = "alcohol1",
                                                                   residual = "intermediate")

      for (x in beercereals) {
        object[, , c(x, "X004|Brewers_grain")] <- .extractGoodFromFlow(object = object[, , c(x, "X004|Brewers_grain")],
                                                                       goodIn = x,
                                                                       from = "intermediate",
                                                                       process = "intermediate",
                                                                       goodOut = "X004|Brewers_grain",
                                                                       reportAs = "brewers_grain1",
                                                                       residual = "alcoholloss",
                                                                       extractionQuantity = "max",
                                                                       extractionAttribute = "dm",
                                                                       prodAttributes = prodAttributes)
      }

      return(object)
    }

    # processing of sugar cane and sugar beet (processed) to sugar1, molasses1 and refiningloss
    .sugarProcessing <- function(object) {

      goodsIn <- c("2536|Sugar cane", "2537|Sugar beet")
      goodsOut <- c("2818|Sugar, Refined Equiv", "2544|Molasses")
      object[, , c(goodsIn, goodsOut)] <- .processingGlobal(object = object[, , c(goodsIn, goodsOut)],
                                                            goodsIn = goodsIn,
                                                            from = "processed",
                                                            process = "refining",
                                                            goodsOut = goodsOut,
                                                            reportAs = c("sugar1", "molasses1"),
                                                            residual = "refiningloss")

      goodsIn <- c("2514|Maize and products")
      goodsOut <- c("2543|Sweeteners, Other")
      object[, , c(goodsIn, goodsOut)] <- .processingGlobal(object = object[, , c(goodsIn, goodsOut)],
                                                            goodsIn = goodsIn,
                                                            from = "processed",
                                                            process = "refining",
                                                            goodsOut = goodsOut,
                                                            reportAs = c("sugar1"),
                                                            residual = "refiningloss")

      return(object)
    }

    # processing of oil and oilcake from palm/palmkernel (processed)
    .oilpalmProcessing <- function(object) {
      # aggregate FAO products relating to oilpalm to a single raw product
      faoProductsOilpalm <- c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake")
      newproduct <- dimSums(object[, , list("production", faoProductsOilpalm, "dm")],
                            dim = c("ItemCodeItem", "ElementShort", "attributes"))
      newproduct <- prodAttributes[, , "X003|Palmoil_Kerneloil_Kernelcake"] * newproduct
      object[, , list("X003|Palmoil_Kerneloil_Kernelcake",
                      c("production", "domestic_supply", "processed"))] <- newproduct

      # extract oil
      goodIn <- "X003|Palmoil_Kerneloil_Kernelcake"
      goodsOut1 <- c("2577|Palm Oil", "2576|Palmkernel Oil")
      goodsOut2 <- "2595|Palmkernel Cake"

      object[, , c(goodIn, goodsOut1)] <- .processingGlobal(object = object[, , c(goodIn, goodsOut1)],
                                                            goodsIn = goodIn,
                                                            from = "processed",
                                                            process = "extracting",
                                                            goodsOut = goodsOut1,
                                                            reportAs = c("oil1", "oil2"),
                                                            residual = "intermediate")

      object[, , c(goodIn, goodsOut2)] <- .extractGoodFromFlow(object = object[, , c(goodIn, goodsOut2)],
                                                               goodIn = goodIn,
                                                               from = "intermediate",
                                                               process = "intermediate",
                                                               goodOut = goodsOut2,
                                                               reportAs = "oilcakes1",
                                                               residual = "extractionloss",
                                                               extractionQuantity = "max",
                                                               extractionAttribute = "dm",
                                                               prodAttributes = prodAttributes)

      return(object)
    }

    # extraction of oil and oilcakes from oilcrops (processed)
    .oilProcessing <- function(object) {
      # orders must match!
      cropsIn <- c("2555|Soyabeans", "2820|Groundnuts (in Shell Eq)", "2557|Sunflower seed",
                   "2559|Cottonseed", "2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra",
                   "2561|Sesame seed")
      oilOut <- c("2571|Soyabean Oil", "2572|Groundnut Oil", "2573|Sunflowerseed Oil",
                  "2575|Cottonseed Oil", "2574|Rape and Mustard Oil", "2578|Coconut Oil",
                  "2579|Sesameseed Oil")
      cakeOut <- c("2590|Soyabean Cake", "2591|Groundnut Cake", "2592|Sunflowerseed Cake",
                   "2594|Cottonseed Cake", "2593|Rape and Mustard Cake", "2596|Copra Cake",
                   "2597|Sesameseed Cake")

      otherCropsIn <- c("2570|Oilcrops, Other", "2563|Olives (including preserved)")
      otherOilOut <- "2586|Oilcrops Oil, Other"
      otherCakeOut <- "2598|Oilseed Cakes, Other"

      # main oil crops
      for (j in seq_along(cropsIn)) {
        object[, , c(cropsIn[j], oilOut[j])] <- .processingGlobal(object = object[, , c(cropsIn[j], oilOut[j])],
                                                                  goodsIn = cropsIn[j],
                                                                  from = "processed",
                                                                  process = "extracting",
                                                                  goodsOut = oilOut[j],
                                                                  reportAs = "oil1",
                                                                  residual = "intermediate")
        object[, , c(cropsIn[j], cakeOut[j])] <- .extractGoodFromFlow(object = object[, , c(cropsIn[j], cakeOut[j])],
                                                                      goodIn = cropsIn[j],
                                                                      from = "intermediate",
                                                                      process = "intermediate",
                                                                      goodOut = cakeOut[j],
                                                                      reportAs = "oilcakes1",
                                                                      residual = "extractionloss",
                                                                      extractionQuantity = "max",
                                                                      extractionAttribute = "dm",
                                                                      prodAttributes = prodAttributes)
      }


      # other oil crops
      object[, , c(otherCropsIn, otherOilOut)] <- .processingGlobal(object = object[, , c(otherCropsIn, otherOilOut)],
                                                                    goodsIn = otherCropsIn,
                                                                    from = "processed",
                                                                    process = "extracting",
                                                                    goodsOut = otherOilOut,
                                                                    reportAs = "oil1",
                                                                    residual = "intermediate")

      for (goodIn in otherCropsIn) {
        object[, , c(goodIn, otherCakeOut)] <- .extractGoodFromFlow(object = object[, , c(goodIn, otherCakeOut)],
                                                                    goodIn = goodIn,
                                                                    from = "intermediate",
                                                                    process = "intermediate",
                                                                    goodOut = otherCakeOut,
                                                                    reportAs = "oilcakes1",
                                                                    residual = "extractionloss",
                                                                    extractionQuantity = "max",
                                                                    extractionAttribute = "dm",
                                                                    prodAttributes = prodAttributes)
      }

      return(object)
    }

    # main function combining all processing functions
    .massbalanceProcessing <- function(years) {
      # preparing dataset for given years
      cells <- getCells(cbc)
      s1 <- getNames(cbc, dim = 1)
      s2 <- c(getNames(cbc, dim = 2), namesProcessing)
      s3 <- attributeTypes
      flowsCBC <- array(dim = c(length(cells), length(years), length(s1), length(s2), length(s3)),
                        dimnames = list(cells, years, s1, s2, s3))
      flowsCBC <- as.magpie(flowsCBC)
      getSets(flowsCBC) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")
      flowsCBC[, , getNames(cbc, dim = 2)] <- cbc[, years, ] * attributesWM[, , getNames(cbc, dim = 1)]
      gc()

      # conversion from 10^12 kcal to PJ
      flowsCBC[, , list("households", "ge")] <- cbc[, years, "food_supply_kcal"] * 4.184
      # conversion of protein to nitrogen using average N content
      flowsCBC[, , list("households", "nr")] <- cbc[, years, "protein_supply"] / 6.25
      flowsCBC[, , list("households", "wm")] <- cbc[, years, "food_supply"]

      flowsCBC <- flowsCBC[, , setdiff(getNames(flowsCBC, dim = "ElementShort"),
                                       c("food_supply_kcal", "protein_supply", "food_supply", "fat_supply"))]
      flowsCBC[is.na(flowsCBC)]  <- 0
      flowsCBC[is.nan(flowsCBC)] <- 0
      gc()

      # relevant processing dimensions
      millingDimensions <- c("production", "production_estimated", "milling", "brans1",
                             "branoil1", "flour1", "food", "households")
      distillingDimensions <- c("production", "production_estimated", "other_util", "distilling",
                                "ethanol1", "intermediate", "distillers_grain1", "distillingloss")
      fermentationDimensions <- c("production", "production_estimated", "processed", "fermentation",
                                  "alcohol1", "intermediate", "brewers_grain1", "alcoholloss")
      refiningDimensions <- c("production", "production_estimated", "processed", "sugar1",
                              "molasses1", "refining", "refiningloss")
      extractingDimensions <- c("production", "production_estimated", "domestic_supply", "processed",
                                "extracting", "oil1", "oil2", "intermediate", "oilcakes1", "extractionloss")

      # relevant processing products
      millingProducts <- c(.getFAOitems(c("tece", "maiz", "rice_pro", "trce", "brans")),
                           "2582|Maize Germ Oil", "2581|Ricebran Oil")
      distillingProducts <- c(.getFAOitems(c("tece", "maiz", "sugr_cane", "ethanol")), "X002|Distillers_grain")
      fermentationProducts <- c(.getFAOitems("tece"), "2656|Beer", "X004|Brewers_grain")
      refiningProducts <- c(.getFAOitems(c("sugr_cane", "sugr_beet", "maiz", "molasses")),
                            "2818|Sugar, Refined Equiv", "2543|Sweeteners, Other")
      extractingProducts1 <- c("2577|Palm Oil", "2576|Palmkernel Oil", "2595|Palmkernel Cake",
                               "X003|Palmoil_Kerneloil_Kernelcake")
      extractingProducts2 <- setdiff(.getFAOitems(c("soybean", "groundnut", "sunflower", "cottn_pro",
                                                    "rapeseed", "oils", "oilcakes")),
                                     c(extractingProducts1, "2580|Olive Oil", "2581|Ricebran Oil",
                                       "2582|Maize Germ Oil"))


      # Food processing calculations
      flowsCBC[, , list(millingProducts, millingDimensions)] <-
        .cerealMillingGlobal(flowsCBC[, , list(millingProducts, millingDimensions)])
      flowsCBC[, , list(distillingProducts, distillingDimensions)] <-
        .ethanolProcessing(flowsCBC[, , list(distillingProducts, distillingDimensions)])
      flowsCBC[, , list(fermentationProducts, fermentationDimensions)] <-
        .beerProcessing(flowsCBC[, , list(fermentationProducts, fermentationDimensions)])
      flowsCBC[, , list(refiningProducts, refiningDimensions)] <-
        .sugarProcessing(flowsCBC[, , list(refiningProducts, refiningDimensions)])
      flowsCBC[, , list(extractingProducts1, extractingDimensions)] <-
        .oilpalmProcessing(flowsCBC[, , list(extractingProducts1, extractingDimensions)])
      flowsCBC[, , list(extractingProducts2, extractingDimensions)] <-
        .oilProcessing(flowsCBC[, , list(extractingProducts2, extractingDimensions)])

      # harmonizing conversion factors within the rapeseed group
      goodsIn  <- list("2558|Rape and Mustardseed", "2560|Coconuts - Incl Copra", "2561|Sesame seed",
                       c("2570|Oilcrops, Other", "2563|Olives (including preserved)"))
      oilsOut <- list("2574|Rape and Mustard Oil", "2578|Coconut Oil",
                      "2579|Sesameseed Oil", "2586|Oilcrops Oil, Other")
      cakesOut <- list("2593|Rape and Mustard Cake", "2596|Copra Cake", "2597|Sesameseed Cake",
                       "2598|Oilseed Cakes, Other")

      for (from in c("oil1", "oilcakes1", "extractionloss")) {
        factor <- dimSums(flowsCBC[, , list(unlist(goodsIn), from)], dim = c(1, 3.1, 3.2)) /
          dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")], dim = c(1, 3.1, 3.2))
        flowsCBC[, , list(unlist(goodsIn), from)] <- factor * dimSums(flowsCBC[, , list(unlist(goodsIn), "extracting")],
                                                                      dim = 3.2)
        gc()
      }

      for (j in seq_along(oilsOut)) {
        flowsCBC[, , list(oilsOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]], "oil1")],
                                                                            dim = c(3.1, 3.2))
        flowsCBC[, , list(cakesOut[[j]], "production_estimated")] <- dimSums(flowsCBC[, , list(goodsIn[[j]],
                                                                                               "oilcakes1")],
                                                                             dim = c(3.1, 3.2))
        gc()
      }

      # Alcohol production
      cropsAlcohol <- .getFAOitems(c("others", "trce", "rice_pro", "potato", "cassav_sp", "sugar", "molasses", "brans"))
      fermentationDimensions <- c("production", "production_estimated", "processed", "fermentation", "alcohol1",
                                  "alcohol2", "alcohol3", "alcohol4", "intermediate", "brewers_grain1", "alcoholloss")
      fermentationProducts <- .getFAOitems(c("tece", "others", "trce", "rice_pro", "potato", "cassav_sp", "sugar",
                                             "molasses", "brans", "alcohol", "distillers_grain"))
      flowsCBC[, , list(fermentationProducts, fermentationDimensions)] <- .processingGlobal(
        flowsCBC[, , list(fermentationProducts, fermentationDimensions)], # nolint
        goodsIn  = cropsAlcohol,
        from      = "processed",
        process   = "fermentation",
        goodsOut = c("2655|Wine", "2657|Beverages, Fermented",
                     "2658|Beverages, Alcoholic", "2659|Alcohol, Non-Food"),
        reportAs = c("alcohol1", "alcohol2", "alcohol3", "alcohol4"), # nolint
        residual  = "alcoholloss")

      # Define use of products that are not existing in FAOSTAT
      goods <- c("X002|Distillers_grain", "X004|Brewers_grain")
      flowsCBC[, , list(goods, c("production", "domestic_supply", "feed"))]  <-
        flowsCBC[, , list(goods, "production_estimated"), drop = TRUE]
      flowsCBC[, , list("X001|Ethanol", c("production", "domestic_supply", "other_util"))] <-
        flowsCBC[, , list("X001|Ethanol", "production_estimated"), drop = TRUE]
      gc()

      # add remaining 'processed' to 'other_util' and remove obsolete dimensions
      flowsCBC[, , "other_util"] <- dimSums(flowsCBC[, , c("other_util", "processed")], dim = 3.2)
      flowsCBC <- flowsCBC[, , c("processed", "intermediate"), invert = TRUE]
      gc()

      # map to magpie categories
      massbalanceProcessing <- toolAggregate(x = flowsCBC,
                                             rel = relationmatrix,
                                             dim = 3.1,
                                             from = "FoodBalanceItem",
                                             to = "k",
                                             partrel = TRUE)
      gc()
      return(massbalanceProcessing)
    }

    # function dealing with the non-processing aspects of cbc
    .massbalanceNoProcessing <- function(years) {
      # initializing magpie object
      cells <- getCells(cbc)
      s2 <- c(getNames(cbc, dim = 2), namesProcessing)
      s3 <- attributeTypes
      noProcessingCBC <- array(dim = c(length(cells), length(years), length(noProcessingFAO), length(s2),
                                       length(s3)), dimnames = list(cells, years, noProcessingFAO, s2, s3))
      noProcessingCBC <- as.magpie(noProcessingCBC)
      getSets(noProcessingCBC) <- c("region", "year", "ItemCodeItem", "ElementShort", "attributes")

      # adding attributes and filling household dimension
      noProcessingCBC[, , getNames(cbc, dim = 2)] <- cbc[, years, noProcessingFAO] * attributesWM[, , noProcessingFAO]
      # conversion from 10^12 kcal to PJ
      noProcessingCBC[, , list("households", "ge")] <- noProcessingCBC[, , list("food_supply_kcal", "wm")] * 4.184
      # conversion of protein to nitrogen using average N content
      noProcessingCBC[, , list("households", "nr")] <- noProcessingCBC[, , list("protein_supply", "wm")] / 6.25
      noProcessingCBC[, , list("households", "wm")] <- noProcessingCBC[, , list("food_supply", "wm")]
      noProcessingCBC <- noProcessingCBC[, , setdiff(getNames(noProcessingCBC, dim = "ElementShort"),
                                                     c("food_supply_kcal", "protein_supply",
                                                       "food_supply", "fat_supply"))]

      # fill NAs and NaNs
      noProcessingCBC[is.na(noProcessingCBC)]  <- 0
      noProcessingCBC[is.nan(noProcessingCBC)] <- 0

      # add 'processed' to 'other_util' and remove obsolete dimensions
      noProcessingCBC[, , "other_util"] <- dimSums(noProcessingCBC[, , c("other_util", "processed")], dim = 3.2)
      noProcessingCBC <- noProcessingCBC[, , c("processed", "intermediate"), invert = TRUE]

      # map to magpie categories
      massbalanceNoProcessing <- toolAggregate(x = noProcessingCBC,
                                               rel = relationmatrix,
                                               dim = 3.1,
                                               from = "FoodBalanceItem",
                                               to = "k",
                                               partrel = TRUE)
      gc()
      return(massbalanceNoProcessing)
    }


    #### Calculations ####

    # increase magclass sizelimit
    local_options(magclass_sizeLimit = 2e8)

    # option without splitting years (in case of memory issues this can be done in year chunks)
    massbalanceNoProcessing <- .massbalanceNoProcessing(years)
    cbc <- cbc[, , noProcessingFAO, invert = TRUE]
    massbalanceProcessing <- .massbalanceProcessing(years)

    # put results together
    massbalance <- mbind(massbalanceProcessing, massbalanceNoProcessing)
  }

  return(list(x = massbalance,
              weight = NULL,
              unit = "MT C, Mt DM, PJ, Mt K, Mt Nr, Mt P, Mt WM",
              description = paste("FAO massbalance calculates all conversion processes within the FAO CBS/FBS and",
                                  "makes them explict. More complete version can be found in calcFAOmassbalance")))
}
