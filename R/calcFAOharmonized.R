#' @title calcFAOharmonized
#' @description Calculate harmonized FAO Commodity Balance and Food Supply data based on CB, only harvested areas
#'              are taken from ProdSTAT. This functions adds the CBCrop, CBLive, FSCrop and FSLive data together.
#'
#' @return FAO harmonized data, weight as NULL, and a description as as a list of MAgPIE objects
#' @author Ulrich Kreidenweis, David Chen, Kristine Karstens
#' @param source "pre2010" or "post2010" "pre2010" returns the FAO sheet that runs 
#' until 2013 (actually 2013, but gets effectively chopped to 2010 by massbalance). "post2010" combines
#' the new FAO FB, SUA, and CB sheets that were re-done for 2010 onwards.
#' @param return whether to return FB (Food balance sheet) or SUA (SUpply Utilization Accounts) 
#' @examples
#' \dontrun{
#' a <- calcOutput("FAOharmonized")
#' }
#' @importFrom utils read.csv

calcFAOharmonized <- function(source = "pre2010", return = "FB") {
  
  if (source == "join2010") {
   # take new values from 2010 onwards  
   pre <- calcOutput("FAOharmonized", source = "pre2010", aggregate = FALSE)
   post <- calcOutput("FAOharmonized", source = "post2010", return = "FB", aggregate = FALSE)  

   mapping <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")
   pre <- toolAggregate(pre, rel = mapping, from = "pre2010_FoodBalanceItem", to = "post2010_FoodBalanceItem",
                             partrel = TRUE, dim = 3.1)
   pre <- pre[, c(2010:2013), invert = TRUE]


   sua <- calcOutput("FAOharmonized", source = "post2010", return = "SUA", aggregate = FALSE)
    #get the brans post 2010 from SUA 
    brans <- c("59|Bran of maize", 
                "17|Bran of wheat",
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

  suab <- sua[, , brans]
  suab <- dimSums(suab[, , getNames(post, dim = 2)], dim = 3.1)
  suab <- add_dimension(suab,  dim = 3.1, add = "ItemCodeItem",  nm = "2600|Brans")

  post <- mbind(post, suab)
  
  out <- mbind(pre, post)
  return(out)


  }

  if (source == "pre2010") {
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
      vcat(1, 'Aggregation created entries without name (""), but containing information. This should not be the case.')
    }
  }

  if (any(getNames(faoData) == "remaining.production")) {
    remainProd <- mean(dimSums(faoData[, , "remaining.production"], dim = 1) /
                         dimSums(dimSums(faoData[, , "production"], dim = 3), dim = 1))
    if (remainProd > 0.02) vcat(1, "Aggregation created a 'remaining' category. Production is",
                                round(remainProd, digits = 3) * 100, "% of total \n")
  }
  if (any(getNames(faoData) == "remaining.area_harvested")) {
    remainArea <- mean(dimSums(faoData[, , "remaining.area_harvested"], dim = 1) /
                         dimSums(dimSums(faoData[, , "area_harvested"], dim = 3), dim = 1))
    if (remainArea > 0.02) vcat(1, "Aggregation created a 'remaining' category. The area harvested is",
                                round(remainArea, digits = 3) * 100, "% of total \n")
  }

  # conversion from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.
  faoData <- faoData / 10^6
  
  } else if (source == "post2010") {
  
  if (return == "FB") {
   # input data: Food Balance, Supply Utilization Account (more disaggregated), Commodity Balance (non-food)
   FB <- readSource("FAO_online", subtype = "FB2010")
   CB <- readSource("FAO_online", subtype = "CB2010")

   mapping <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")
  
 #rename and create columns so identical between FB and SUA
  FB <- add_columns(FB, addnm = "Processed_(t)" , dim = 3.2 )
  FB[ , ,"Processed_(t)" ] <- FB[, , "Processing_(t)"]
  FB <- FB[, , "Processing_(t)", invert = TRUE]
   out <- complete_magpie(mbind(FB, CB), fill = 0)


  } else if (return == "SUA") {
 
  SUA <- readSource("FAO_online", subtype = "SUA2010")

  SUA <- add_columns(SUA, addnm =  "Food_supply_(kcal)_(Kcal)", dim = 3.2 )
  SUA[ , , "Food_supply_(kcal)_(Kcal)"] <- SUA[, , "Calories_Year_(Kcal)"]

  SUA <- add_columns(SUA, addnm =  "Protein_supply_quantity_(t)_(t)", dim = 3.2 )
  SUA[ , , "Protein_supply_quantity_(t)_(t)"] <- SUA[, , "Proteins_Year_(t)"]

  SUA <- add_columns(SUA, addnm =  "Fat_supply_quantity_(t)_(t)" , dim = 3.2 )
  SUA[ , ,  "Fat_supply_quantity_(t)_(t)"] <- SUA[, ,  "Fats_Year_(t)" ]

  SUA <- add_columns(SUA, addnm =  "Losses_(t)" , dim = 3.2 )
  SUA[ , , "Losses_(t)"] <- SUA[, , "Loss_(t)" ]
  
  SUA <- add_columns(SUA, addnm = "Food_(t)" , dim = 3.2 )
  SUA[ , , "Food_(t)"] <- SUA[, , "Food_supply_quantity_(tonnes)_(t)" ]

  SUA <- SUA[ , , c("Food_supply_quantity_(tonnes)_(t)", "Fats_Year_(t)",
                    "Loss_(t)", "Proteins_Year_(t)", "Calories_Year_(Kcal)" ),
                  invert = TRUE]

 #create domestic supply quantity
  SUAd <- dimSums(SUA[, , c("Food_(t)" , "Feed_(t)", "Other_uses_(non_food)_(t)",
                          "Processed_(t)", "Seed_(t)", "Tourist_consumption_(t)", "Losses_(t)")], dim = 3.2)

  SUAd <- add_dimension(SUAd, dim = 3.2, add = "ElementShort", nm = "Domestic_supply_quantity_(t)")
  out <- mbind(SUAd, SUA)
  }


  ## in addition harvested area from Crops Primary

  prod <- readSource("FAO_online", "CropLive2010", convert = TRUE)
    
  ## aggregate Prod to CB units
  aggregation <- toolGetMapping("FAOitems_online_2010update.csv", type = "sectoral", where = "mrfaocore")

  # remove  aggregate categories
  remove <- setdiff(getNames(prod, dim = 1), aggregation$post2010_ProductionItem)
  prod <- prod[, , remove, invert = TRUE]
  
  if (return == "FB") {
    toCol <- "post2010_FoodBalanceItem"
  } else if (return == "SUA") {
    toCol <- "post2010_SupplyUtilizationItem"
  }
  areaHarvested <- toolAggregate(prod, rel = aggregation, from = "post2010_ProductionItem", to = toCol,
                                 dim = 3.1, partrel = TRUE)[, , "area_harvested"]
                    
  commonyears <- intersect(getYears(areaHarvested), getYears(out))

  faoData <- mbind(out[, commonyears, ], areaHarvested[, commonyears, ])

  rm(areaHarvested)

 #change names to old convention, units from tonnes to Mt, hectares to Mha and 10^6kcal to 10^12kcal.

 getNames(faoData, dim = 2) <- tolower(gsub("_\\(.*", "", getNames(faoData, dim  = 2)))
 getNames(faoData, dim = 2) <- tolower(gsub("_quantity", "", getNames(faoData, dim  = 2)))
 
 faoData <- faoData / 1e6
 faoData <- complete_magpie(faoData)

  kcal <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                       years = getItems(faoData, dim = 2),
                       names = paste0(getItems(faoData, dim = 3.1), ".food_supply_kcal"))
  kcal[ , ,  "food_supply_kcal"] <- faoData[, ,  "food_supply"]
  faoData <- mbind(faoData, kcal)
  #remove
  faoData <- faoData[, , "food_supply", invert = TRUE]

 other <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                       years = getItems(faoData, dim = 2),
                       names = paste0(getItems(faoData, dim = 3.1), ".other_util"))
  other[ , ,  "other_util"] <- faoData[, ,  "other_uses"]
  faoData <- mbind(faoData, other)
  #remove 
  faoData <- faoData[, , "other_uses", invert = TRUE]


 waste <- new.magpie(cells_and_regions = getItems(faoData, dim = 1),
                       years = getItems(faoData, dim = 2),
                       names = paste0(getItems(faoData, dim = 3.1), ".waste"))
  waste[ , ,  "waste"] <- faoData[, ,  "losses"]
  faoData <- mbind(faoData, waste)
  #remove 
  faoData <- faoData[, , "losses", invert = TRUE]


  # add tourist consumption to food - but note that this creates a mismatch in food_supply_kcal!!

  faoData[, , "food"] <-  faoData[, , "food"] + faoData[, , "tourist_consumption"]
  faoData <- faoData[, , "tourist_consumption", invert = TRUE]
 
  ### add Fodder data if FB level ###
  
  if (return == "FB") {
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
  }

 faoData[is.na(faoData)] <- 0

  ## check if there is data without an element name
 faoData <- faoData[, , "", invert = TRUE] 


  if (any(getNames(faoData) == "remaining.production")) {
    remainProd <- mean(dimSums(faoData[, , "remaining.production"], dim = 1) /
                         dimSums(dimSums(faoData[, , "production"], dim = 3), dim = 1))
    if (remainProd > 0.02) vcat(1, "Aggregation created a 'remaining' category. Production is",
                                round(remainProd, digits = 3) * 100, "% of total \n")
  }
  if (any(getNames(faoData) == "remaining.area_harvested")) {
    remainArea <- mean(dimSums(faoData[, , "remaining.area_harvested"], dim = 1) /
                         dimSums(dimSums(faoData[, , "area_harvested"], dim = 3), dim = 1))
    if (remainArea > 0.02) vcat(1, "Aggregation created a 'remaining' category. The area harvested is",
                                round(remainArea, digits = 3) * 100, "% of total \n")
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
