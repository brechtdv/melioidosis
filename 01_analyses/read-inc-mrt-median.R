#' ---
#' title: MELIOIDOSIS / READ INC MRT / MEDIAN
#' output:
#'   github_document:
#'     toc: true
#' ---

## required packages
library(bd)

## helper functions
stats <-
function(x, ...) {
  c(mean = mean(x, ...), quantile(x, probs = c(0.5, 0.025, 0.975), ...))
}

## read data
f <- "Incidence_Mortality_Nature-micro_Seperated_2018.xls"
dta <- readxl(paste0("../02_data/20180815/", f))
str(dta)

## data cleaning
dta <- dta[!is.na(dta[[2]]), ]  # drop empty rows
dta <- dta[-1, ]                # drop first row
colnames(dta) <-
  c("COUNTRY", "INC", "INC.LW", "INC.UP", "MRT", "MRT.LW", "MRT.UP")
str(dta)

## check country names
dta$COUNTRY[duplicated(dta$COUNTRY)]

## match FERG names
id <- match(tolower(dta$COUNTRY), tolower(FERG2015:::crpop_2015$Country))
dta$COUNTRY[is.na(id)]

dta$COUNTRY[dta$COUNTRY == "Bolivia"] <-
  "bolivia (plurinational state of)"
dta$COUNTRY[dta$COUNTRY == "Brunei"] <-
  "brunei darussalam"
dta$COUNTRY[dta$COUNTRY == "Cote d'Ivoire"] <-
  "côte d'ivoire"
dta$COUNTRY[dta$COUNTRY == "Congo Rep"] <-
  "congo"
dta$COUNTRY[dta$COUNTRY == "Congo Dem. Rep."] <-
  "democratic republic of the congo"
dta$COUNTRY[dta$COUNTRY == "Guinea Bissau"] <-
  "guinea-bissau"
dta$COUNTRY[dta$COUNTRY == "Iran"] <-
  "iran (islamic republic of)"
dta$COUNTRY[dta$COUNTRY == "Lao"] <-
  "lao people's democratic republic"
dta$COUNTRY[dta$COUNTRY == "Phillipines"] <-
  "philippines"
dta$COUNTRY[dta$COUNTRY == "Tanzania"] <-
  "united republic of tanzania"
dta$COUNTRY[dta$COUNTRY == "Timor"] <-
  "timor-leste"
dta$COUNTRY[dta$COUNTRY == "Venezuela"] <-
  "venezuela (bolivarian republic of)"
dta$COUNTRY[dta$COUNTRY == "Vietnam"] <-
  "viet nam"
dta$COUNTRY[dta$COUNTRY == "Yemen, Rep"] <-
  "yemen"

id <- match(tolower(dta$COUNTRY), tolower(FERG2015:::crpop_2015$Country))
dta$COUNTRY[is.na(id)]

id <- match(tolower(FERG2015:::crpop_2015$Country), tolower(dta$COUNTRY))
FERG2015:::crpop_2015$Country[is.na(id)]

id <- match(tolower(dta$COUNTRY), tolower(FERG2015:::crpop_2015$Country))
dta$COUNTRY <- FERG2015:::crpop_2015$Country[id]

## make population data frame
pop <-
  data.frame(country = dta$COUNTRY,
             region = FERG2015:::crpop_2015$WHORegion[id],
             subregion = FERG2015:::crpop_2015$SUB[id],
             pop = apply(FERG2015:::Popul_2015[, , id], 3, sum))

## extract inc/mrt
INC <- sapply(dta[, c(3, 2, 4)], as.numeric)
MRT <- sapply(dta[, c(6, 5, 7)], as.numeric)
all(INC[, 1] < INC[, 2])
all(INC[, 2] < INC[, 3])

## add dimnames
colnames(INC) <- colnames(MRT) <- c("lwr", "median", "upr")
rownames(INC) <- rownames(MRT) <- dta$COUNTRY

## show data
knitr::kable(INC)
knitr::kable(MRT)

#+ warning=FALSE
## simulate values
## .. assume perfect correlation to match Table 1 CrI in Direk paper
## .. INC > 165 (68–412) ; MRT > 89 (36–227)
f_gamma <-
function(par, p, q) {
  qfit <- qgamma(p = p, shape = par[1], rate = par[2])
  return(sum((qfit - q)^2))
}

optim_gamma <-
function(q) {
  p <- c(0.025, 0.50, 0.975)
  optim(par = c(1, 1), fn = f_gamma, p = p, q = q)
}

sim <-
function(n, pars) {
  set.seed(264)
  fit <- optim_gamma(pars)
  rgamma(n = n, fit$par[1], fit$par[2])
}

INC_sim <- apply(INC, 1, sim, n = 1e5)
MRT_sim <- apply(MRT, 1, sim, n = 1e5)

stats(rowSums(INC_sim))
stats(rowSums(MRT_sim))

knitr::kable(t(apply(INC_sim, 2, stats)), digits = 0)
knitr::kable(t(apply(MRT_sim, 2, stats)), digits = 0)

## calculate rates per 100k
INC_rt_sim <- t(t(1e5 * INC_sim) / pop$pop)
MRT_rt_sim <- t(t(1e5 * MRT_sim) / pop$pop)

knitr::kable(t(apply(INC_rt_sim, 2, stats, na.rm = TRUE)), digits = 3)
knitr::kable(t(apply(MRT_rt_sim, 2, stats, na.rm = TRUE)), digits = 3)

## R session info
sessionInfo()

##rmarkdown::render("read-inc-mrt-median.R")