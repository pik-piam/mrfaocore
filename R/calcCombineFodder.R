#' @title calcCombineFodder
#' @description Combine old FAO fodder data (pre-2010 item codes, 1961-2011) with
#'              new Fodder2010 data (CPC codes, 2010-2023). The old data is kept up to
#'              its last year (2011), the new data is used afterwards. New items without
#'              an old equivalent are aggregated into the old item codes (see
#'              FodderItemMapping.csv). Old items without a new equivalent (648 Carrots
#'              for fodder, global production < 0.01 Mt) are set to 0 after the last old
#'              year. Gaps in the newdata (zero production) are filled by carrying the last available value forward.
#'
#' @return Combined fodder data in tonnes (production, feed, domestic_supply) and ha
#'         (area_harvested) as a list with MAgPIE object, weight, unit, and description
#' @author David Chen
#' @seealso [readSource()], [calcOutput()]
#' @examples
#' \dontrun{
#' a <- calcOutput("CombineFodder")
#' }
#' @importFrom magclass add_columns as.magpie collapseNames complete_magpie getItems getSets
#'             getSets<- getYears magpiesort

calcCombineFodder <- function() {

  elems <- c("area_harvested", "production", "feed")

  # ---- Read old and new fodder data (both in tonnes and ha) ----
  old <- readSource("FAO", "Fodder")
  new <- readSource("Fodder2010", convert = TRUE)

  # ---- Remap new items to old item codes ----
  # new-only items (0191993, 0191909, 0191910, 0191911) are aggregated into 639, 643 and 651
  mapping <- toolGetMapping("FodderItemMapping.csv", type = "sectoral", where = "mrfaocore")
  mapping <- mapping[mapping$NewItemCodeItem != "" & !is.na(mapping$NewItemCodeItem), ]
  mapping$OldItemCodeItem <- paste0(mapping$OldItemCode, "|", mapping$OldItemName)
  mapping <- mapping[mapping$NewItemCodeItem %in% getItems(new, dim = 3.1), ]
  new <- toolAggregate(new, rel = mapping, from = "NewItemCodeItem", to = "OldItemCodeItem",
                       dim = 3.1, partrel = TRUE)

  # ---- Add feed (= production for fodder) ----
  # complete_magpie first, as some items have area but no production in the new data (642)
  new <- complete_magpie(new, fill = 0)
  new <- add_columns(new, addnm = "feed", dim = 3.2)
  new[, , "feed"] <- new[, , "production"]

  # ---- Convert to plain arrays (region x year x item x element) over the common dimensions ----
  regions  <- getItems(old, dim = 1)
  items    <- sort(union(getItems(old, dim = 3.1), getItems(new, dim = 3.1)))
  years    <- paste0("y", sort(union(getYears(old, as.integer = TRUE), getYears(new, as.integer = TRUE))))
  lastOld  <- paste0("y", max(getYears(old, as.integer = TRUE)))
  # old data is kept up to its last year, new data is used afterwards
  newYears <- intersect(years[years > lastOld], getYears(new))

  toArray <- function(x) {
    a <- array(0, dim = c(length(regions), length(years), length(items), length(elems)),
               dimnames = list(regions, years, items, elems))
    for (e in elems) {
      xe <- as.array(collapseNames(x[, , e]))
      ii <- intersect(items, dimnames(xe)[[3]])
      yy <- intersect(years, dimnames(xe)[[2]])
      a[, yy, ii, e] <- xe[regions, yy, ii]
    }
    a
  }
  oldArr <- toArray(old)
  newArr <- toArray(new)

  # ---- Combine: old data up to its last year, new data afterwards ----
  # items that only exist in the old data (648) are 0 after the last old year
  combined <- oldArr
  combined[, newYears, , ] <- newArr[, newYears, , ]

  # ---- Fill gaps in the new data by carrying the last available value forward ----
  # zero production is treated as missing, as zeros and missing values cannot be distinguished
  fillable <- rep(items %in% getItems(new, dim = 3.1), each = length(regions))
  for (y in newYears) {
    prev <- years[which(years == y) - 1]
    gap  <- combined[, y, , "production"] == 0 & fillable
    for (e in elems) {
      tmp <- combined[, y, , e]
      tmp[gap] <- combined[, prev, , e][gap]
      combined[, y, , e] <- tmp
    }
  }

  # ---- Back to MAgPIE object, domestic_supply = feed (following calcFAOharmonized convention) ----
  out <- as.magpie(combined, spatial = 1, temporal = 2)
  getSets(out) <- getSets(old)
  out <- add_columns(out, addnm = "domestic_supply", dim = 3.2)
  out[, , "domestic_supply"] <- out[, , "feed"]
  out <- magpiesort(out)

  return(list(x = out,
              weight = NULL,
              unit = "area_harvested: ha, production/feed/domestic_supply: tonnes",
              description = "Combined FAO fodder data: old (1961-2011) and new Fodder2010 (2012-2023)"))
}
