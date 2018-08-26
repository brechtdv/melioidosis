#' ---
#' output:
#'   github_document:
#'     toc: true
#' ---

#' # Settings

## required packages
library(future.apply)

## simulations
set.seed(264)
n <- 1e5

## helper function
rle <-
function(x, LE = c("GBD", "WHO")) {
  LE <- match.arg(LE)
  LT <- switch(LE, GBD = FERG2015:::std_LE_GBD, WHO = FERG2015:::std_LE_WHO)
  approx(LT[, 1], LT[, 2], x)$y
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
    names(reg_mrt))


#' # Simulations

RLE_WHO <- RLE_GBD <-
  matrix(ncol = length(reg_mrt), nrow = n)

sample2 <-
function(x, ...) {
  if (length(x) == 1) {
    x

  } else {
    sample(x,...)
  }
}

plan(multiprocess)

for (i in seq(length(reg_mrt))) {
  RLE_WHO[, i] <-
    future_sapply(seq(n),
           function(x)
             mean(rle(sample2(reg_mrt[[i]]$age, replace = TRUE), "WHO")))
  RLE_GBD[, i] <-
    future_sapply(seq(n),
           function(x)
             mean(rle(sample2(reg_mrt[[i]]$age, replace = TRUE), "GBD")))
}

rbind(sapply(reg_mrt, function(x) mean(x$age)),
      rle(sapply(reg_mrt, function(x) mean(x$age))))
t(apply(RLE_WHO, 2, mean_ci))
t(apply(RLE_GBD, 2, mean_ci))

save(RLE_WHO, RLE_GBD, file = "rle-sim.RData")

##rmarkdown::render("sim-rle.R")