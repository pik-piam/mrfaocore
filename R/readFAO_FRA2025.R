#' Read FRA2025
#'
#' Read-in an FRA (forest resource assessment) dataset from 2025. 
#' Three subtypes are added compared to FRA2020: expansion, afforestation, net_change.
#'
#' @param subtype data subtype. Available subtypes: forest_area, deforestation, growing_stock, biomass_stock,
#' carbon_stock, management, disturbance, forest_fire, expansion, afforestation, net_change
#' @return Magpie object of the FRA 2025 data
#' @author Simin Yu
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FRA2025", "growing_stock")
#' }
#'
#' @importFrom magclass as.magpie getItems
#' @export


readFRA2025 <- function(subtype) { 
  # Read data files and normalize column names
  yearsData <- read.csv("FRA_Years_2025_11_13.csv", header = TRUE, dec = ".", na.strings = c("", " ", "NA", "."), stringsAsFactors = FALSE)
  annualData <- read.csv("Annual_2025_11_13.csv", header = TRUE, dec = ".", na.strings = c("", " ", "NA", "."), stringsAsFactors = FALSE)
  intervalsData <- read.csv("Intervals_2025_11_13.csv", header = TRUE, dec = ".", na.strings = c("", " ", "NA", "."), stringsAsFactors = FALSE)

  colnames(yearsData) <- gsub("^X", "", colnames(yearsData))
  colnames(annualData) <- gsub("^X", "", colnames(annualData))
  colnames(intervalsData) <- gsub("^X", "", colnames(intervalsData))

  subtypeList <- c("forest_area", "deforestation", "growing_stock", "biomass_stock","carbon_stock", "management", 
                   "disturbance", "forest_fire", "expansion", "afforestation", "net_change")
  if (!(subtype %in% subtypeList)) {
    stop("Invalid or unsupported subtype ", subtype, ". Accepted subtypes are ",
         paste(subtypeList, collapse = ", "), ". Choose one of the accepted subtype.")
  }

  # Clean missing/ partial data 
  clean_data <- function(mp) {
    varCount <- length(getNames(mp))
    yrCount <- length(getYears(mp))
    missingData <- 0
    partialData <- 0
    for (j in getItems(mp, dim = 1.1)) {
      for (i in seq_len(varCount)) {
        naCount <- as.numeric(apply(mp[j, , i], 1, function(x) sum(is.na(x))))
        if (naCount == yrCount) {
          missingData <- missingData + 1
          mp[j, , i] <- 0
        } else if (naCount > 0 && naCount < yrCount) {
          partialData <- partialData + 1
          mp[j, , i][is.na(mp[j, , i])] <- mean(as.numeric(as.vector(mp[j, , i])), na.rm = TRUE)
        }
      }
    }
    if (missingData > 0) {
        message(missingData, " missing data points. Such data will be set to 0.")
    }
    if (partialData > 0) {
        message(partialData, " partial data points. Such data will be set to mean value of reported data.")
    }
    colnames(mp) <- gsub("^X", "y", colnames(mp))

    return(mp)
  }

  # Select columns by identifier prefixes and strip prefixes
  select_and_strip <- function(df, patterns) {
    vars <- grep(paste(patterns, collapse = "|"), colnames(df), value = TRUE)
    id_cols <- c("iso3", "year")
    sel <- df[, colnames(df) %in% c(id_cols, vars), drop = FALSE]
    for (p in patterns) {
      colnames(sel) <- gsub(p, "", colnames(sel))
    }
    return(sel)
  }

  # Convert interval data, mapping periods to their starting years and interpolating the pseudo years. 
  convert_intervals <- function(df, indicator_col, subtype_name) {
    # Keep only needed columns
    df <- df[, c("iso3", "year", indicator_col)]
    df <- df[!is.na(df$iso3) & df$iso3 != "" & !is.na(df$year) & df$year != "", , drop = FALSE]

    # Map periods to starting years
    df$y_start <- vapply(df$year, function(s) {
        if (grepl("-", s)) {
        start <- as.numeric(sub("-.*", "", s))
        return(start)
        } else {
        return(as.numeric(s))
        }
    }, numeric(1))
    countries <- unique(df$iso3)
    start_years <- sort(unique(df$y_start))
    wide_df <- data.frame(iso3 = countries, stringsAsFactors = FALSE)
    for (y in start_years) wide_df[[paste0("y", y)]] <- NA_real_
    # Fill in values from original periods
    for (c in countries) {
        subdf <- df[df$iso3 == c, , drop = FALSE]
        for (i in seq_len(nrow(subdf))) {
        colname <- paste0("y", subdf$y_start[i])
        wide_df[wide_df$iso3 == c, colname] <- as.numeric(subdf[[indicator_col]][i])
        }
    }

    # Add midpoints by copying preceding period's value
    # 1990-2000 → y1995, 2000-2010 → y2005, 2020-2025 → y2025
    if ("y1990" %in% colnames(wide_df)) wide_df$y1995 <- wide_df$y1990
    if ("y2000" %in% colnames(wide_df)) wide_df$y2005 <- wide_df$y2000
    if ("y2020" %in% colnames(wide_df)) wide_df$y2025 <- wide_df$y2020

    # Convert to magpie object
    mp <- as.magpie(wide_df)
    mp <- setNames(mp, subtype_name)
    
    # Sort the years
    years <- getYears(mp)
    sorted_years <- paste0("y", sort(as.numeric(sub("^y", "", years))))
    mp <- mp[, sorted_years, , drop = FALSE]
    return(mp)
  }


  
  out <- NULL
  switch(subtype,
         forest_area = {
           # Unit is 1000 ha
           ids <- c("1a_", "1b_", "1c_", "1d_", "1e_")
           data <- select_and_strip(yearsData, ids)
           out <- as.magpie(data, spatial = "iso3")
           out <- clean_data(out)
         },
         growing_stock = {
           # Unit is m3/ha or million m3
           ids <- c("2a_")
           data <- select_and_strip(yearsData, ids)
           out <- as.magpie(data, spatial = "iso3")
           out <- clean_data(out)
         },
         biomass_stock = {
           # Unit is tDM/ha 
           ids <- c("2c_")
           data <- select_and_strip(yearsData, ids)
           out <- as.magpie(data, spatial = "iso3")
           out <- clean_data(out)
         },
         carbon_stock = {
           # Unit is tC/ha
           ids <- c("2d_")
           data <- select_and_strip(yearsData, ids)
           # Following 2020, "2d_soil_depth_cm" is not needed hence dropped
           drop_idx <- grep("soil_depth", colnames(data))
           if (length(drop_idx) > 0) data <- data[, -drop_idx, drop = FALSE]
           out <- as.magpie(data, spatial = "iso3")
           out <- clean_data(out)
         },
         management = {
           # Unit is 1000 ha
           ids <- c("3a_tot_")
           data <- select_and_strip(yearsData, ids)
           out <- as.magpie(data, spatial = "iso3")
           out <- clean_data(out)
         },
         disturbance = {
           # Unit is 1000 ha
           indicators <- intersect(colnames(annualData), c("5a_insect", "5a_diseases", "5a_weather", "5a_other", "5b_fire_land"))
           out <- NULL
           for (ind in indicators) {
             df_tmp <- annualData[, c("iso3", "year", ind)]
             colnames(df_tmp)[3] <- ind
             # strip prefix
             clean_name <- sub("^5[ab]_", "", ind)
             mp_tmp <- as.magpie(df_tmp)
             mp_tmp <- setNames(mp_tmp, clean_name)
             if (is.null(out)) out <- mp_tmp else out <- mbind(out, mp_tmp)
           }
           out <- clean_data(out)
         },
         forest_fire = {
           # Unit is 1000 ha
           if (!("5b_fire_forest" %in% colnames(annualData))) stop("No 5b_fire_forest found in annual file.")
           df_tmp <- annualData[, c("iso3", "year", "5b_fire_forest")]
           mp_tmp <- as.magpie(df_tmp)
           mp_tmp <- setNames(mp_tmp, "fire_forest")
           out <- clean_data(mp_tmp)
         },
         deforestation = {
           # Unit is 1000 ha/yr
           out <- convert_intervals(intervalsData, "1d_deforestation", "deforestation")
         },
         expansion = {
           # Unit is 1000 ha/yr
           out <- convert_intervals(intervalsData, "1d_expansion", "expansion")
         },
         afforestation = {
           # Unit is 1000 ha/yr
           out <- convert_intervals(intervalsData, "1d_afforestation", "afforestation")
         },
         net_change = {
           # Unit is 1000 ha/yr
           out <- convert_intervals(intervalsData, "1d_nat_exp", "net_change")
         }
  )

  # Remove items matching junk (X0|X1|X2) as in FRA2020
  if (!is.null(out)) {
    items <- getItems(out, dim = 1.1)
    keep_items <- items[!grepl("X0|X1|X2", items)]
    if (length(keep_items) < length(items)) out <- out[, , keep_items, drop = FALSE]
  }

  return(out)
}

