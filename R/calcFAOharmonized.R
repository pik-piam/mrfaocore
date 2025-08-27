#' @title calcFAOharmonized
#' @description Calculate harmonized FAO Commodity Balance and Food Supply data based on CB, only harvested areas
#'              are taken from ProdSTAT. This functions adds the CBCrop, CBLive, FSCrop and FSLive data together.
#'
#' @return FAO harmonized data, weight as NULL, and a description as as a list of MAgPIE objects
#' @author Ulrich Kreidenweis, David Chen, Kristine Karstens
#' @param src source "pre2010" or "post2010" "pre2010" returns the FAO sheet that runs
#' until 2013 (actually 2013, but gets effectively chopped to 2010 by massbalance). "post2010" combines
#' the new FAO FB, SUA, and CB sheets that were re-done for 2010 onwards.
#' @param output whether to return FB (Food balance sheet) or SUA (SUpply Utilization Accounts)
#' @examples
#' \dontrun{
#' a <- calcOutput("FAOharmonized")
#' }
#' @importFrom utils read.csv

calcFAOharmonized <- function(src = "pre2010", output = "FB") {

  if (src == "join2010") { # nolint
    # take new values from 2010 onwards
    pre <- calcOutput("FAOharmonized", src = "pre2010", aggregate = FALSE)
    post <- calcOutput("FAOharmonized", src = "post2010", output = "FB", aggregate = FALSE)

    mapping <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")
    pre <- toolAggregate(pre, rel = mapping, from = "pre2010_FoodBalanceItem", to = "post2010_FoodBalanceItem",
                         partrel = TRUE, dim = 3.1)
    pre <- pre[, c(2010:2013), invert = TRUE]

    # create residuals with 0 in the old harmonized
    res <- pre[, , "production"]
    res[] <- 0
    getNames(res, dim = 2) <- "residuals"
    pre <- mbind(pre, res)

    # create food_supply same as food in the new
    fs <- post[, , "food"]
    getNames(fs, dim = 2) <- "food_supply"
    post <- mbind(post, fs)


    pre <- complete_magpie(pre)
    post <- complete_magpie(post)
    names <- intersect(getNames(pre, dim = 1), getNames(post, dim = 1))


    faoData <- mbind(pre[, , names], post[, , names])

    faoData[is.na(faoData)] <- 0


  } else if (src == "pre2010") { # nolint
    # input data: Commodity Balance (Crops Primary + Livestock Primary), Food Supply (Crops Primary + Livestock Primary)
    cbCrop <- readSource("FAO_online", "CBCrop")
    cbLive <- readSource("FAO_online", "CBLive")
    fsCrop <- readSource("FAO_online", "FSCrop")
    fsLive <- readSource("FAO_online", "FSLive")

    cb <- toolFAOcombine(cbLive, cbCrop, combine = "Item")
    fs <- toolFAOcombine(fsLive, fsCrop, combine = "Item")
    rm(cbCrop, cbLive, fsCrop, fsLive)

    faoData <- mbind(cb, fs)

    ## in addition harvested area from Crops Primary

    prod <- readSource("FAO_online", "Crop", convert = TRUE)

    ## aggregate Prod to CB units
    aggregation <- toolGetMapping("FAOitems_online.csv", type = "sectoral", where = "mappingfolder")

    # remove  aggregate categories
    remove <- setdiff(getNames(prod, dim = 1), aggregation$ProductionItem)
    prod <- prod[, , remove, invert = TRUE]

    areaHarvested <- toolAggregate(prod, rel = aggregation, from = "ProductionItem", to = "FoodBalanceItem",
                                   dim = 3.1, partrel = TRUE)[, , "area_harvested"]

    commonyears <- intersect(getYears(areaHarvested), getYears(faoData))

    faoData <- mbind(faoData[, commonyears, ], areaHarvested[, commonyears, ])

    rm(areaHarvested)

    ### add Fodder data ###

    fodder <- readSource("FAO", "Fodder")
    fodder <- toolExtrapolateFodder(fodder, endyear = max(getYears(faoData, as.integer = TRUE)))
    fodder <- add_columns(x = fodder, addnm = "domestic_supply", dim = 3.2)
    fodder[, , "domestic_supply"] <- fodder[, , "feed"]
    fodderAggregated <- toolAggregate(fodder, rel = aggregation, from = "ProductionItem",
                                      to = "FoodBalanceItem", dim = 3.1, partrel = TRUE)
    cyears <- intersect(getYears(faoData), getYears(fodderAggregated))
    faoData <- mbind(faoData[, cyears, ], fodderAggregated[, cyears, ])
    rm(fodder, fodderAggregated)
    gc()

    faoData[is.na(faoData)] <- 0

    ## check if there is data without an element name

    ## what to do? In case there is data these dimensions should not be deleted

    if (any(getItems(faoData, dim = 3.1) == "")) {
      if (sum(faoData[, , ""]) == 0) {
        faoData <- faoData[, , "", invert = TRUE]
      } else  {
        vcat(1, 'Aggregation created entries without name (""), ',
             'but containing information. This should not be the case.')
      }
    }

    if (any(getNames(faoData) == "remaining.production")) {
      remainProd <- mean(dimSums(faoData[, , "remaining.production"], dim = 1) /
                           dimSums(dimSums(faoData[, , "production"], dim = 3), dim = 1))
      if (remainProd > 0.02) {
        vcat(1, "Aggregation created a 'remaining' category. Production is ",
             round(remainProd, digits = 3) * 100, "% of total")
      }
    }
    if (any(getNames(faoData) == "remaining.area_harvested")) {
      remainArea <- mean(dimSums(faoData[, , "remaining.area_harvested"], dim = 1) /
                           dimSums(dimSums(faoData[, , "area_harvested"], dim = 3), dim = 1))
      if (remainArea > 0.02) {
        vcat(1, "Aggregation created a 'remaining' category. The area harvested is ",
             round(remainArea, digits = 3) * 100, "% of total")
      }
    }

    # conversion from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.
    faoData <- faoData / 10^6

  } else if (src == "post2010") { # nolint

    if (output == "FB") {
      # input data: Food Balance, Supply Utilization Account (more disaggregated), Commodity Balance (non-food)
      fb <- readSource("FAO_online", subtype = "FB2010")
      fb[is.na(fb)] <- 0
      cb <- readSource("FAO_online", subtype = "CB2010")

      # create domestic supply quantity for CB
      cbd <- dimSums(cb[, , c("Other_uses_(non_food)_(t)", "Processed_(t)")], dim = 3.2)
      cbd <- add_dimension(cbd, dim = 3.2, add = "ElementShort", nm = "Domestic_supply_quantity_(t)")
      cb <- mbind(cbd, cb)

      mapping <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")

      # rename and create columns so identical between fb and SUA
      fb <- add_columns(fb, addnm = "Processed_(t)", dim = 3.2)
      fb[, , "Processed_(t)"] <- fb[, , "Processing_(t)"]
      fb <- fb[, , "Processing_(t)", invert = TRUE]
      out <- complete_magpie(mbind(fb, cb), fill = 0)

      # remove the relative food supplys kcal/cap/d and kg/cap
      out <- out[, , c("Food_supply_quantity_(kg_capita_yr)_(kg/cap)",
                       "Food_supply_(kcal_capita_day)_(kcal/cap/d)",
                       "Fat_supply_quantity_(g_capita_day)_(g/cap/d)",
                       "Protein_supply_quantity_(g_capita_day)_(g/cap/d)"),
                 invert = TRUE]

    } else if (output == "SUA") {

      sua <- readSource("FAO_online", subtype = "SUA2010")
      sua[is.na(sua)] <- 0

      sua <- add_columns(sua, addnm = "Food_supply_(kcal)_(Kcal)", dim = 3.2)
      sua[, , "Food_supply_(kcal)_(Kcal)"] <- sua[, , "Calories_Year_(Kcal)"]

      sua <- add_columns(sua, addnm = "Protein_supply_quantity_(t)_(t)", dim = 3.2)
      sua[, , "Protein_supply_quantity_(t)_(t)"] <- sua[, , "Proteins_Year_(t)"]

      sua <- add_columns(sua, addnm = "Fat_supply_quantity_(t)_(t)", dim = 3.2)
      sua[, , "Fat_supply_quantity_(t)_(t)"] <- sua[, , "Fats_Year_(t)"]

      sua <- add_columns(sua, addnm = "Losses_(t)", dim = 3.2)
      sua[, , "Losses_(t)"] <- sua[, , "Loss_(t)"]

      sua <- add_columns(sua, addnm = "Food_(t)", dim = 3.2)
      sua[, , "Food_(t)"] <- sua[, , "Food_supply_quantity_(tonnes)_(t)"]

      sua <- sua[, , c("Food_supply_quantity_(tonnes)_(t)", "Fats_Year_(t)",
                       "Loss_(t)", "Proteins_Year_(t)", "Calories_Year_(Kcal)"),
                 invert = TRUE]

      # remove the relative food supplys kcal/cap/d and kg/cap
      sua <- sua[, , c("Food_supply_quantity_(g_capita_day)_(g/cap/d)",
                       "Food_supply_(kcal_capita_day)_(kcal/cap/d)",
                       "Fat_supply_quantity_(g_capita_day)_(g/cap/d)",
                       "Protein_supply_quantity_(g_capita_day)_(g/cap/d)"),
                 invert = TRUE]

      # create domestic supply quantity
      suad <- dimSums(sua[, , c("Food_(t)", "Feed_(t)", "Other_uses_(non_food)_(t)",
                                "Processed_(t)", "Seed_(t)", "Tourist_consumption_(t)", "Losses_(t)")], dim = 3.2)

      suad <- add_dimension(suad, dim = 3.2, add = "ElementShort", nm = "Domestic_supply_quantity_(t)")
      out <- mbind(suad, sua)
    }


    ## in addition harvested area from Crops Primary

    prod <- readSource("FAO_online", "CropLive2010", convert = TRUE)

    ## aggregate Prod to CB units
    aggregation <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")

    # remove  aggregate categories
    remove <- setdiff(getNames(prod, dim = 1), aggregation$post2010_ProductionItem)
    prod <- prod[, , remove, invert = TRUE]

    if (output == "FB") {
      toCol <- "post2010_FoodBalanceItem"
    } else if (output == "SUA") {
      toCol <- "post2010_SupplyUtilizationItem"
    }
    areaHarvested <- toolAggregate(prod, rel = aggregation, from = "post2010_ProductionItem", to = toCol,
                                   dim = 3.1, partrel = TRUE)[, , "area_harvested"]

    commonyears <- intersect(getYears(areaHarvested), getYears(out))

    faoData <- mbind(out[, commonyears, ], areaHarvested[, commonyears, ])

    rm(areaHarvested)

    # change names to old convention

    getNames(faoData, dim = 2) <- tolower(gsub("_\\(.*", "", getNames(faoData, dim  = 2)))
    getNames(faoData, dim = 2) <- tolower(gsub("_quantity", "", getNames(faoData, dim  = 2)))
    # change units from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.
    faoData <- faoData / 1e6
    faoData <- complete_magpie(faoData)

    kcal <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                       years = getItems(faoData, dim = 2),
                       names = paste0(getItems(faoData, dim = 3.1), ".food_supply_kcal"))
    kcal[, ,  "food_supply_kcal"] <- faoData[, ,  "food_supply"]
    faoData <- mbind(faoData, kcal)
    # remove
    faoData <- faoData[, , "food_supply", invert = TRUE]

    other <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                        years = getItems(faoData, dim = 2),
                        names = paste0(getItems(faoData, dim = 3.1), ".other_util"))
    other[, ,  "other_util"] <- faoData[, ,  "other_uses"]
    faoData <- mbind(faoData, other)
    # remove
    faoData <- faoData[, , "other_uses", invert = TRUE]


    waste <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                        years = getItems(faoData, dim = 2),
                        names = paste0(getItems(faoData, dim = 3.1), ".waste"))
    waste[, , "waste"] <- faoData[, , "losses"]
    faoData <- mbind(faoData, waste)
    # remove
    faoData <- faoData[, , "losses", invert = TRUE]


    # add tourist consumption to food - but note that this creates a small mismatch in food_supply_kcal,
    # to live with for now.
    faoData[is.na(faoData)] <- 0
    faoData[, , "food"] <-  faoData[, , "food"] + faoData[, , "tourist_consumption"]
    faoData <- faoData[, , "tourist_consumption", invert = TRUE]

    ### add Fodder data and add brans, oilcakes, and molasses (not in FB but in SUA) if at FB level ###

    if (output == "FB") {
      fodder <- readSource("FAO", "Fodder")
      fodder <- toolExtrapolateFodder(fodder, endyear = max(getYears(faoData, as.integer = TRUE)))
      fodder <- add_columns(x = fodder, addnm = "domestic_supply", dim = 3.2)
      fodder[, , "domestic_supply"] <- fodder[, , "feed"]
      fodderAggregated <- toolAggregate(fodder, rel = aggregation, from = "post2010_ProductionItem",
                                        to = "post2010_FoodBalanceItem", dim = 3.1, partrel = TRUE)
      cyears <- intersect(getYears(faoData), getYears(fodderAggregated))
      fodderAggregated <- fodderAggregated / 1e6

      faoData <- mbind(faoData[, cyears, ], fodderAggregated[, cyears, ])
      rm(fodder, fodderAggregated)
      gc()



      # get the brans, oilcakes, molasses  post 2010 from SUA
      sua <- calcOutput("FAOharmonized", src = "post2010", output = "SUA",
                        aggregate = FALSE)[, , "opening_stocks", invert = TRUE]
      brans <- c("59|Bran of maize",
                 "17|Bran of wheat",
                 "47|Bran of barley",
                 "73|Bran of rye",
                 "77|Bran of oats",
                 "105|Bran of mixed grain",
                 "91|Bran of buckwheat",
                 "96|Bran of fonio",
                 "81|Bran of millet",
                 "85|Bran of sorghum",
                 "112|Bran of cereals nec",
                 "35|Bran of rice")

      # subtract the bran  kcal and protein from the respective products, a few countries report
      brancrop <- c("2514|Maize and products", "2511|Wheat and products", "2513|Barley and products",
                    "2515|Rye and products", "2516|Oats", "2520|Cereals, Other",
                    "2520|Cereals, Other", "2520|Cereals, Other", "2517|Millet and products",
                    "2518|Sorghum and products", "2520|Cereals, Other", "2807|Rice and products")
      branmap <- data.frame("bran" = brans, "crop" = brancrop)
      # rename via toolAggregate to sum the "cereals, other"
      foodbrans <- toolAggregate(sua[, , brans][, , c("protein_supply", "food_supply_kcal")],
                                 rel = branmap, from = "bran", to = "crop", dim = 3.1)
      faoData[, , c("protein_supply", "food_supply_kcal")][, , unique(brancrop)] <-
        faoData[, , c("protein_supply", "food_supply_kcal")][, , unique(brancrop)] -
        foodbrans

      cakes <- c("238|Cake of  soya beans", "245|Cake of groundnuts", "269|Cake of sunflower seed",
                 "332|Cake of cottonseed", "272|Cake of rapeseed", "294|Cake of mustard seed",
                 "253|Cake of copra", "291|Cake of sesame seed", "314|Cake of kapok",
                 "335|Cake of  linseed",  "338|Cake of hempseed",
                 "341|Cake, oilseeds nes", "61|Cake of maize", "259|Cake of palm kernel", "37|Cake of rice bran")
      # re add pop for adding
      sua <- add_columns(sua, dim = 3.2, addnm = "total_population_both_sexes", fill = 0)

      suab <- sua[, , brans]
      suab <- dimSums(suab, dim = 3.1)
      suab <- add_dimension(suab, dim = 3.1, add = "ItemCodeItem", nm = "2600|Brans")

      faoData <- mbind(faoData, suab[, , getNames(faoData, dim = 2)])
      faoData <- mbind(faoData, sua[, ,  cakes])
      faoData[, , "165|Molasses"] <- sua[, , "165|Molasses"]
      faoData <- complete_magpie(faoData)

    }

    faoData[is.na(faoData)] <- 0

    ## check if there is data without an element name
    faoData <- faoData[, , "", invert = TRUE]


    if (any(getNames(faoData) == "remaining.production")) {
      remainProd <- mean(dimSums(faoData[, , "remaining.production"], dim = 1) /
                           dimSums(dimSums(faoData[, , "production"], dim = 3), dim = 1))
      if (remainProd > 0.02) {
        vcat(1, "Aggregation created a 'remaining' category. Production is ",
             round(remainProd, digits = 3) * 100, "% of total")
      }
    }
    if (any(getNames(faoData) == "remaining.area_harvested")) {
      remainArea <- mean(dimSums(faoData[, , "remaining.area_harvested"], dim = 1) /
                           dimSums(dimSums(faoData[, , "area_harvested"], dim = 3), dim = 1))
      if (remainArea > 0.02) {
        vcat(1, "Aggregation created a 'remaining' category. The area harvested is ",
             round(remainArea, digits = 3) * 100, "% of total")
      }
    }

    # remove population item
    faoData <- faoData[, , "total_population_both_sexes", invert = TRUE]

  }

  return(list(x = faoData,
              weight = NULL,
              description = "FAO Commodity Balance and Food Supply data",
              unit = "Unit in Mt/yr, for area Mha, calories in 10^12 kcal/yr",
              note = "food_supply_kcal, protein_supply and fat_supply were calculated from per capita per day values"))
}
