#' ---
#' output:
#'   github_document:
#'     html_preview: false
#'     toc: true
#'     toc_depth: 2
#' ---

#' # Settings

## required packages
library(future.apply)

## simulations
set.seed(264)
n <- 1e5

## helpers
lle <-
function(dem, LE) {
  age <- dem$age
  sex <- dem$sex
  col <- 3 - sex
  future_mapply(function(i,j) approx(LE[, 1], LE[, i], j)$y, col, age)
}


#' # Load data
#+ warning=FALSE

## incidence, mortality, by country
source("read-inc-mrt-median.R")
INC_sim <- INC_sim[, !is.na(colnames(INC_sim))]
MRT_sim <- MRT_sim[, !is.na(colnames(MRT_sim))]

## age-sex distributions, by region
source("sim-age.R")
str(reg_inc)
str(reg_mrt)

## match countries to regions
reg_id <-
  match(
    FERG2015:::crpop_2015$WHORegion[
      match(colnames(MRT_sim), FERG2015:::crpop_2015$Country)],
    names(reg_inc))


#' # Run simulations

lle_sim <- matrix(ncol = ncol(INC_sim), nrow = n)
colnames(lle_sim) <- colnames(INC_sim)

for (i in seq(ncol(lle_sim))) {
  country <- colnames(INC_sim)[i]
  LE <- FERG2015:::get_local_LE(country)
  id <- reg_id[which(colnames(INC_sim) == country)]
  LEi <- lle(reg_inc[[id]], LE)
  lle_sim[, i] <-
    future_sapply(seq(n), function(x) mean(sample(LEi, replace = TRUE)))
}

knitr::kable(t(apply(lle_sim, 2, mean_ci)), digits = 1)

save(lle_sim, file = "lle-sim.RData")

##rmarkdown::render("sim-lle.R")