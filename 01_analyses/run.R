### MELIOIDOSIS DALYS / RUN ANALYSES

rmarkdown::render("read-inc-mrt-median.R")

rmarkdown::render("sim-age.R")
rmarkdown::render("sim-rle.R")
rmarkdown::render("sim-lle.R")

rmarkdown::render("melioidosis-daly.R")

rmarkdown::render("plot-ntd.R")