### GLOBAL DALY MAP

## required packages
library(bd)
library(FERG2015)
library(maptools)
library(maps)
library(RColorBrewer)

## subregions
#x <- FERG2015:::countryRegion_2015$WHOSubregion
#table(x)

## world map
dir <- "D:/Dropbox/__PDTF/MapTemplate_generalized_2013/Shapefiles/"
map1 <- readShapeSpatial(paste0(dir, "general_2013.shp"))
map2 <- readShapeSpatial(paste0(dir, "maskline_general_2013.shp"))
map3 <- readShapeSpatial(paste0(dir, "maskpoly_general_2013.shp"))

## define colour coding
## .. cf original figure
col_na <- rgb(210, 210, 210, max = 255)
col_bg <- rgb(224, 232, 255, max = 255)
col_br <- rgb(110, 110, 110, max = 255)
col_lk <- rgb(190, 210, 255, max = 255)

daly_map <-
function(daly, col, file = NULL, save = TRUE) {
  breaks <- pretty(daly)
  cat_lab <- cut(daly, breaks, right = TRUE)
  cat10 <- col
  cat <- cat10[as.numeric(cat_lab)]

  id_ferg <- match(names(daly), FERG2015:::countryRegion_2015$Country)
  iso_ferg <- toupper(FERG2015:::countryRegion_2015$ISO3[id_ferg])
  match_name <- match(map1@data$ISO_3_CODE, iso_ferg)
  col <- cat[match_name]
  
  # .. no data
  iso_no_data <-
    FERG2015:::countryRegion_2015$ISO[
      !(FERG2015:::countryRegion_2015$Country %in% names(daly))]
  id_no_data <- map1@data$ISO_3_CODE %in% toupper(iso_no_data)
  col[id_no_data] <- "white"

  # .. not applicable
  col[is.na(col)] <- col_na

  # make file
  if (save) {
    tiff(file,
         width = 12, height = 5.5, units = "in", res = 300, compress = "lzw")
  }

  par(mar = c(0, 0, 0, 0))

  plot(map1, col = col, bg = col_bg, border = col_br, lwd = .75)
  plot(map3, add = T, lwd = .75,
       col = c(col_na, col_bg, col_na),
       border = c(col_br, col_lk, col_br))
  plot(map2, add = T, lwd = .75,
       col = "white", lty = 1)
  plot(map2, add = T, lwd = .75,
       col = rgb(110, 110, 110, max = 255),
       lty = c(3, 3, 1, 3, 3, 3, 3, 3, 3, 3))

  legend("bottomleft", ncol = 2,
         title = expression(bold("     DALY (per 100 000 people)")),
         box.col = NA, bg = NA, 
         legend = c("0", levels(cat_lab), "Not applicable"),
         fill = c("white", cat10, col_na),
         cex = .9, y.intersp = .9, x.intersp = 0.5, text.width = 20)

  map.scale(x = 110, y = -54, cex = .7,
            ratio = FALSE, relwidth = 0.1, xpd = NA)

  if (save) graphics.off()
}
