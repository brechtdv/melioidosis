### MELIOIDOSIS / RESULTS
### 23/07/2018

## required packages
library(bd)

## helper functions
styler <-
function(x) {
  if (x > 10) {
    formatC(round(x), format = "fg", big.mark = " ")

  } else if (x >= 1) {
    formatC(x, format = "f", digits = 1) 

  } else if (x >= 0.001) {
    formatC(x, format = "f", digits = 3)

  } else if (x == 0) {
    "0.000"

  } else {
    "<0.001"
  }
}

formatr <-
function(x) {
  y <- mean_ci(x)
  y <- sapply(y, styler)

  paste0(y[1], " (", y[2], "-", y[3], ")")
}

## load results
load("mel.RData")

## make summaries / COUNTRY
write_cb(colnames(DALY_WHO))

write_cb(
  cbind(apply(YLL_WHO, 2, formatr),
        apply(YLL_WHO_rt, 2, formatr),
        apply(YLD_all, 2, formatr),
        apply(YLD_rt, 2, formatr),
        apply(DALY_WHO, 2, formatr),
        apply(DALY_WHO_rt, 2, formatr)))

write_cb(
  cbind(apply(YLL_GBD, 2, formatr),
        apply(YLL_GBD_rt, 2, formatr),
        apply(YLD_all, 2, formatr),
        apply(YLD_rt, 2, formatr),
        apply(DALY_GBD, 2, formatr),
        apply(DALY_GBD_rt, 2, formatr)))


## make summaries / REGION
write_cb(colnames(DALY_WHO_reg))

write_cb(
  cbind(c(apply(YLL_WHO_reg, 2, formatr), formatr(YLL_WHO_glo)),
        c(apply(YLL_WHO_reg_rt, 2, formatr), formatr(YLL_WHO_glo_rt)),
        c(apply(YLD_reg, 2, formatr), formatr(YLD_glo)),
        c(apply(YLD_reg_rt, 2, formatr), formatr(YLD_glo_rt)),
        c(apply(DALY_WHO_reg, 2, formatr), formatr(DALY_WHO_glo)),
        c(apply(DALY_WHO_reg_rt, 2, formatr), formatr(DALY_WHO_glo_rt))))

write_cb(
  cbind(c(apply(YLL_GBD_reg, 2, formatr), formatr(YLL_GBD_glo)),
        c(apply(YLL_GBD_reg_rt, 2, formatr), formatr(YLL_GBD_glo_rt)),
        c(apply(YLD_reg, 2, formatr), formatr(YLD_glo)),
        c(apply(YLD_reg_rt, 2, formatr), formatr(YLD_glo_rt)),
        c(apply(DALY_GBD_reg, 2, formatr), formatr(DALY_GBD_glo)),
        c(apply(DALY_GBD_reg_rt, 2, formatr), formatr(DALY_GBD_glo_rt))))
