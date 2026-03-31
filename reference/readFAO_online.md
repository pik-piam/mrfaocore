# Read FAO_online

Read in FAO data that has been downloaded from the FAOSTAT website.
Files with exception of fodder.csv are aquired according to downloadFAO.

## Usage

``` r
readFAO_online(subtype)
```

## Arguments

- subtype:

  Type of FAO data that should be read. Available types are:

  - \`CBCrop\`: Commodity Balance Crop
    (CommodityBalances_Crops_E_All_Data.zip)

  - \`CBLive\`: Commoditiy Balance Livestock
    (CommodityBalances_LivestockFish_E_All_Data.zip)

  - \`Crop\`: Production Crops ("Production_Crops_E_All_Data.zip")

  - \`CropProc\`: Production Crops Processed
    ("Production_CropsProcessed_E_All_Data.zip")

  - \`FB2010\`: New Food Balance Sheet, 2010 onwards
    ("FoodBalanceSheets_E_All_Data.zip")

  - \`SUA2010\`: New Supply Utilization Accounts, 2010 onwards
    ("SUA_Crops_Livestock_E_All_Data\_(Normalized).zip")

  - \`CB2010\`: New Commodity Balance Sheets, 2010 onwards
    ("CommodityBalances\_(non-food)\_(2010-)\_E_All_Data\_(Normalized).zip")
    \#nolint

  - \`Fertilizer\`: Fertilizer ("Resources_Fertilizers_E_All_Data.zip")

  - \`FertilizerProducts\`: Fertilizer by product
    ("Inputs_FertilizersProduct_E_All_Data\_(Normalized).zip")

  - \`FertilizerNutrients\`: Fertilizer by nutrient
    ("Inputs_FertilizersNutrient_E_All_Data\_(Normalized).zip")

  - \`Fodder\`: Fodder (data that has been manually downloaded from the
    FAOSTAT website as seperate .xls files via a search for "forage" and
    "fodder" withing Production-Crops. These datasets have been added
    together to a "Fodder.csv" file)

  - \`FoodSecurity\`: Food Security Data
    ("Food_Security_Data_E_All_Data.zip")

  - \`FSCrop\`: Food Supply Crops ("FoodSupply_Crops_E_All_Data.zip")

  - \`FSLive\`: Food Supply Livestock
    ("FoodSupply_LivestockFish_E_All_Data.zip")

  - \`Land\`: Land ("Resources_Land_E_All_Data.zip")

  - \`LiveHead\`: Production Live Animals
    ("Production_Livestock_E_All_Data.zip")

  - \`LivePrim\`: Production Livestock Primary
    ("Production_LivestockPrimary_E_All_Data.zip")

  - \`LiveProc\`: Production Livestock Processed
    ("Production_LivestockProcessed_E_All_Data.zip")

  - \`Pop\`: Population ("Population_E_All_Data.zip")

  - \`ForestProdTrade\`: Forestry Production and Trade
    ("Forestry_E_All_Data\_(Normalized).zip")

  - \`PricesProducerAnnual\`: Producer Prices - Annual
    ("Prices_E_All_Data.zip")

  - \`PricesProducerAnnualLCU\`: Producer Prices - Annual in LCU
    ("Prices_E_All_Data.zip")

  - \`ValueOfProd\`: Value of Agricultural Production
    ("Value_of_Production_E_All_Data.zip")

  - \`ValueShares\`: Value shares by industry and primary factors

  - \`Trade\`: Trade quantities and values

## Value

FAO data as MAgPIE object

## Details

Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)

## See also

\[readSource()\]

## Author

Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein,
Edna Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FAO_online", "Crop")
} # }
```
