#' ---
#' title: MELIOIDOSIS / NTD PLOT
#' output:
#'   github_document:
#'     toc: true
#' ---

## required packages
library(bd)
library(ggplot2)
library(ggrepel)

## load data
dta <- readxl("../02_data/20180815/NTDi.xlsx")
dta$Cat <- factor(dta$Cat)
dta$Cat[dta$Disease_State == "Leptospirosis"] <- 1
str(dta)

## make plot
pdf("NTD.pdf", 10, 6)
ggplot(dta, aes(x = Global_Investment, y = DALY)) +
  stat_smooth(method = "lm") +
  geom_point(aes(color = Cat), size = 2.5) +
  theme_bw() +
  scale_x_continuous("Global investment (million dollars)") +
  scale_y_log10("Disability-adjusted life years per 1 million population",
                limits = c(.1, 10)) +
  geom_text_repel(aes(label = Disease_State, color = Cat)) +
  scale_colour_manual(values = c("steelblue", "red3")) +
  theme(legend.position = "none")
graphics.off()

## regression
summary(lm(log(DALY) ~ Global_Investment, dta))
with(dta, cor.test(log(DALY), Global_Investment, method = "pearson"))
with(dta, cor.test(log(DALY), Global_Investment, method = "spearman"))
with(dta, cor.test(DALY, Global_Investment, method = "pearson"))
with(dta, cor.test(DALY, Global_Investment, method = "spearman"))

##rmarkdown::render("plot-ntd.R")