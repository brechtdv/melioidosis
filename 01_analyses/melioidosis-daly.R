#' ---
#' output:
#'   github_document:
#'     toc: true
#' ---

#' # Settings

## required packages
library(bd)
library(FERG2015)
library(ggplot2)
library(ggrepel)
library(mc2d)
library(prevalence)

## simulations
set.seed(264)
n <- 1e5


#' # Load simulations
#+ warning=FALSE

## incidence, mortality, by country
source("read-inc-mrt-median.R")

INC_sim <- INC_sim[, !is.na(colnames(INC_sim))]
MRT_sim <- MRT_sim[, !is.na(colnames(MRT_sim))]

knitr::kable(t(apply(INC_sim, 2, mean_ci)), digits = 0)
knitr::kable(t(mean_ci(rowSums(INC_sim))), digits = 0)

knitr::kable(t(apply(MRT_sim, 2, mean_ci)), digits = 0)
knitr::kable(t(mean_ci(rowSums(MRT_sim))), digits = 0)

## age-sex distributions, by region
source("sim-age.R")
str(reg_inc)
str(reg_mrt)

## residual life expectancy, by region
load("rle-sim.RData")
str(RLE_WHO)
str(RLE_GBD)

## local life expectancy, by country
load("lle-sim.RData")
str(lle_sim)


#' # Years of Life Lost
#+ warning=FALSE, fig.height=5, fig.width=10

## YLL / MRT

reg_id <-
  match(
    FERG2015:::crpop_2015$WHORegion[
      match(colnames(MRT_sim), FERG2015:::crpop_2015$Country)],
    names(reg_mrt))

YLL_WHO <- MRT_sim * RLE_WHO[, reg_id]
mean_ci(rowSums(YLL_WHO))

YLL_GBD <- MRT_sim * RLE_GBD[, reg_id]
mean_ci(rowSums(YLL_GBD))


#' # Years Lived with Disability

dismod <- readxl("../02_data/20180815/MEL_dismod_15-8-18.xlsx", "main")
dismod_tx <- readxl("../02_data/20180815/MEL_dismod_15-8-18.xlsx", "tx")
dismod_pi <- readxl("../02_data/20180815/MEL_dismod_15-8-18.xlsx", "pi")

p_seq <- p_seq_pi <-
YLDcase <- YLDcase_pi_nd <-
  matrix(nrow = n, ncol = nrow(dismod))
colnames(p_seq) <- colnames(p_seq_pi) <-
colnames(YLDcase) <- colnames(YLDcase_pi_nd) <-
  abbreviate(gsub("\\(.*", "", dismod$HS), 10)

for (i in seq(nrow(dismod))) {
  p <- rbeta(n, dismod[i, "X"], dismod[i, "N"] - dismod[i, "X"])
  dw <- FERG2015:::sim_mean(
          n, unlist(dismod[i, c("DW.lwr", "DW", "DW.upr")]), "PROB")
  durs <- rgamma2(n, dismod[i, "DURS"], dismod[i, "DURS.sd"]) / 365
  durh <- rgamma2(n, dismod[i, "DURH"], dismod[i, "DURH.sd"]) / 365

  p_pi <- rbeta(n, dismod_pi[i, "X"], dismod_pi[i, "N"] - dismod_pi[i, "X"])
  dw_pi <- FERG2015:::sim_mean(
             n, unlist(dismod_pi[i, c("DW.lwr", "DW", "DW.upr")]), "PROB")

  p_seq[, i] <- p
  p_seq_pi[, i] <- p_pi

  YLDcase[, i] <- p * dw * (durs + durh)
  YLDcase_pi_nd[, i] <- p * p_pi * dw_pi
}

knitr::kable(t(apply(p_seq, 2, mean_ci)))
knitr::kable(t(apply(p_seq_pi, 2, mean_ci)))

knitr::kable(t(apply(YLDcase, 2, mean_ci)))
knitr::kable(t(mean_ci(rowSums(YLDcase))))

## Pr(survival)

p_surv <- 1 - apply(MRT_sim, 2, sort) / apply(INC_sim, 2, sort)
knitr::kable(t(apply(p_surv, 2, mean_ci)))
median(colMeans(p_surv))
median(colMeans(lle_sim))

## YLD ABtx = YLDcase_tx * Pr(survival)

p_tx <- 1
dw_tx <-
  FERG2015:::sim_mean(
    n, unlist(dismod_tx[1, c("DW.lwr", "DW", "DW.upr")]), "PROB")
dur_tx <-
  rgamma2(n,
          dismod_tx[1, "DUR_Oral_Abx"],
          dismod_tx[1, "DUR_Oral_Abx.sd"]) / 365

YLDcase_tx <- p_surv * (p_tx * dw_tx * dur_tx)


## YLD post-infectious = YLDcase_pi_nd * local LE * Pr(survival)

YLDcase_pi <- array(dim = c(n, ncol(INC_sim), ncol(YLDcase_pi_nd)))

for (i in seq(ncol(YLDcase_pi_nd))) {
  YLDcase_pi[, , i] <- p_surv * lle_sim * YLDcase_pi_nd[, i]
}

YLDcase_pi_sum <- apply(YLDcase_pi, 1:2, sum)
YLDcase_pi_mean <- apply(YLDcase_pi, 2:3, mean)
rowSums(YLDcase_pi_mean)


## multiply INC * YLDcase

YLD <- INC_sim * rowSums(YLDcase)
YLD_tx <- INC_sim * YLDcase_tx
YLD_pi <- INC_sim * YLDcase_pi_sum
YLD_all <- YLD + YLD_tx + YLD_pi

knitr::kable(
  rbind(
    YLD.sym = mean_ci(rowSums(YLD)),
    YLD.tx = mean_ci(rowSums(YLD_tx)),
    YLD.pi = mean_ci(rowSums(YLD_pi)),
    YLD.all = mean_ci(rowSums(YLD_all))))


## .. contributions of the 3 groups
knitr::kable(
  rbind(
    sympt = mean_ci(rowSums(YLD) / rowSums(YLD_all)),
    tx = mean_ci(rowSums(YLD_tx) / rowSums(YLD_all)),
    pi = mean_ci(rowSums(YLD_pi) / rowSums(YLD_all))))


#' # Disability-Adjusted Life Years

DALY_WHO <- YLL_WHO + YLD_all
DALY_GBD <- YLL_GBD + YLD_all

## regional estimates
pop <- pop[!is.na(pop$region), ]
region <- pop$region  # define regional grouping
reg <- levels(region)
nreg <- length(reg)

DALY_WHO_reg <-
  DALY_GBD_reg <-
  YLL_WHO_reg <-
  YLL_GBD_reg <-
  YLD_reg <-
  matrix(0, nrow = n, ncol = nreg)

colnames(DALY_WHO_reg) <-
  colnames(DALY_GBD_reg) <-
  colnames(YLL_WHO_reg) <-
  colnames(YLL_GBD_reg) <-
  colnames(YLD_reg) <-
  reg

for (i in seq(nreg)) {
  DALY_WHO_reg[, i] <-
    rowSums(DALY_WHO[, region == reg[i]], na.rm = TRUE)
  DALY_GBD_reg[, i] <-
    rowSums(DALY_GBD[, region == reg[i]], na.rm = TRUE)
  YLL_WHO_reg[, i] <-
    rowSums(YLL_WHO[, region == reg[i]], na.rm = TRUE)
  YLL_GBD_reg[, i] <-
    rowSums(YLL_GBD[, region == reg[i]], na.rm = TRUE)
  YLD_reg[, i] <-
    rowSums(YLD_all[, region == reg[i]], na.rm = TRUE)
}


## global estimates
DALY_WHO_glo <- rowSums(DALY_WHO, na.rm = TRUE)
DALY_GBD_glo <- rowSums(DALY_GBD, na.rm = TRUE)
YLL_WHO_glo <- rowSums(YLL_WHO, na.rm = TRUE)
YLL_GBD_glo <- rowSums(YLL_GBD, na.rm = TRUE)
YLD_glo <- rowSums(YLD_all, na.rm = TRUE)


## contribution of top 5 countries
top5 <-
  colnames(DALY_WHO) %in%
  c("India", "Bangladesh", "Viet Nam", "Nigeria", "Indonesia")

mean_ci(rowSums(DALY_WHO[, top5]))
mean_ci(rowSums(DALY_WHO[, top5]) / DALY_WHO_glo)

mean_ci(rowSums(DALY_GBD[, top5]))
mean_ci(rowSums(DALY_GBD[, top5]) / DALY_GBD_glo)


## convert to rates
DALY_WHO_rt <- t(t(n * DALY_WHO) / pop$pop)
DALY_GBD_rt <- t(t(n * DALY_GBD) / pop$pop)
YLL_WHO_rt <- t(t(n * YLL_WHO) / pop$pop)
YLL_GBD_rt <- t(t(n * YLL_GBD) / pop$pop)
YLD_rt <- t(t(n * YLD_all) / pop$pop)

pop_reg <- as.numeric(tapply(pop$pop, region, sum, na.rm = TRUE))

DALY_WHO_reg_rt <- t(t(n * DALY_WHO_reg) / pop_reg)
DALY_GBD_reg_rt <- t(t(n * DALY_GBD_reg) / pop_reg)
YLL_WHO_reg_rt <- t(t(n * YLL_WHO_reg) / pop_reg)
YLL_GBD_reg_rt <- t(t(n * YLL_GBD_reg) / pop_reg)
YLD_reg_rt <- t(t(n * YLD_reg) / pop_reg)

DALY_WHO_glo_rt <- n * DALY_WHO_glo / sum(pop$pop, na.rm = TRUE)
DALY_GBD_glo_rt <- n * DALY_GBD_glo / sum(pop$pop, na.rm = TRUE)
YLL_WHO_glo_rt <- n * YLL_WHO_glo / sum(pop$pop, na.rm = TRUE)
YLL_GBD_glo_rt <- n * YLL_GBD_glo / sum(pop$pop, na.rm = TRUE)
YLD_glo_rt <- n * YLD_glo / sum(pop$pop, na.rm = TRUE)

## save results
save(YLD_all, YLL_WHO, YLL_GBD, DALY_WHO, DALY_GBD,
     YLD_reg, YLL_WHO_reg, YLL_GBD_reg, DALY_WHO_reg, DALY_GBD_reg,
     YLD_glo, YLL_WHO_glo, YLL_GBD_glo, DALY_WHO_glo, DALY_GBD_glo,
     YLD_rt, YLL_WHO_rt, YLL_GBD_rt, DALY_WHO_rt, DALY_GBD_rt,
     YLD_reg_rt, YLL_WHO_reg_rt, YLL_GBD_reg_rt,
     DALY_WHO_reg_rt, DALY_GBD_reg_rt,
     YLD_glo_rt, YLL_WHO_glo_rt, YLL_GBD_glo_rt,
     DALY_WHO_glo_rt, DALY_GBD_glo_rt,
     file = "mel.RData")


#' # Comorbidities
como <- readxl("../02_data/20180815/Co-morbidity_Data_15-8-2018.xlsx")
como_sim <-
  with(como,
       mapply(rbeta,
              No_patients,
              Total_patients - No_patients,
              MoreArgs = list(n = n)))
colnames(como_sim) <- como$Co_morbidity
knitr::kable(t(apply(como_sim, 2, mean_ci)))

## proportion YLD
como_yld <-
  rbind(
    mean_ci(como_sim[,1] * YLD_glo),
    mean_ci(como_sim[,2] * YLD_glo),
    mean_ci(como_sim[,3] * YLD_glo),
    mean_ci(como_sim[,4] * YLD_glo))
rownames(como_yld) <- como$Co_morbidity
knitr::kable(como_yld)

## proportion YLD per 100k
como_yld_rt <-
  rbind(
    mean_ci(como_sim[,1] * YLD_glo_rt),
    mean_ci(como_sim[,2] * YLD_glo_rt),
    mean_ci(como_sim[,3] * YLD_glo_rt),
    mean_ci(como_sim[,4] * YLD_glo_rt))
rownames(como_yld_rt) <- como$Co_morbidity
knitr::kable(como_yld_rt)

## proportion DALY
como_daly <-
  rbind(
    mean_ci(como_sim[,1] * DALY_WHO_glo),
    mean_ci(como_sim[,2] * DALY_WHO_glo),
    mean_ci(como_sim[,3] * DALY_WHO_glo),
    mean_ci(como_sim[,4] * DALY_WHO_glo))
rownames(como_daly) <- como$Co_morbidity
knitr::kable(como_daly)

## proportion DALY per 100k
como_daly_rt <-
  rbind(
    mean_ci(como_sim[,1] * DALY_WHO_glo_rt),
    mean_ci(como_sim[,2] * DALY_WHO_glo_rt),
    mean_ci(como_sim[,3] * DALY_WHO_glo_rt),
    mean_ci(como_sim[,4] * DALY_WHO_glo_rt))
rownames(como_daly_rt) <- como$Co_morbidity
knitr::kable(como_daly_rt)


#' # MAIN TEXT RESULTS

## main results
mean_ci(rowSums(DALY_WHO))
mean_ci(rowSums(DALY_GBD))

mean_ci(DALY_WHO_glo_rt)
mean_ci(DALY_GBD_glo_rt)

## proportion YLL/YLD
mean_ci(rowSums(YLL_WHO) / rowSums(DALY_WHO))
mean_ci(rowSums(YLD) / rowSums(DALY_WHO))

mean_ci(rowSums(YLL_GBD) / rowSums(DALY_GBD))
mean_ci(rowSums(YLD) / rowSums(DALY_GBD))


#' # TABLE 2 / DALY BY COUNTRY / REGION / GLOBAL

#' ## WHO life expectancy table

knitr::kable(t(apply(DALY_WHO, 2, mean_ci)), digits = 0)
knitr::kable(t(apply(DALY_WHO_reg, 2, mean_ci)), digits = 0)
knitr::kable(t(mean_ci(DALY_WHO_glo)), digits = 0)

knitr::kable(t(apply(DALY_WHO_rt, 2, mean_ci, na.rm = T)), digits = 3)
knitr::kable(t(apply(DALY_WHO_reg_rt, 2, mean_ci, na.rm = T)), digits = 3)
knitr::kable(t(mean_ci(DALY_WHO_glo_rt, na.rm = T)), digits = 3)

#' ## GBD life expectancy table

knitr::kable(t(apply(DALY_GBD, 2, mean_ci)), digits = 0)
knitr::kable(t(apply(DALY_GBD_reg, 2, mean_ci)), digits = 0)
knitr::kable(t(mean_ci(DALY_GBD_glo)), digits = 0)

knitr::kable(t(apply(DALY_GBD_rt, 2, mean_ci, na.rm = T)), digits = 3)
knitr::kable(t(apply(DALY_GBD_reg_rt, 2, mean_ci, na.rm = T)), digits = 3)
knitr::kable(t(mean_ci(DALY_GBD_glo_rt, na.rm = T)), digits = 3)


#' # FIG 3 / WORLDMAP DALY BY COUNTRY
#+ warning=FALSE, fig.width=12, fig.height=5.5

source("global-daly-map.R")

daly_who <- na.omit(colMeans(DALY_WHO_rt))

col1 <- brewer.pal(5, "Reds")

daly_map(daly_who, col1, "Fig1.tiff", save = F)

pdf("Fig4.pdf", 12, 5.5)
daly_map(daly_who, col1, "Fig1.tiff", save = F)
graphics.off()


#' # FIG 5a / SCATTERPLOT DALY-HAQ
#+ fig.width=8, fig.height=7

haqf <-
  "IHME_GBD_2015_HAQ_INDEX_1990_2015_HAQ_INDEX_AND_VALUES_Y2017M05D18.xls"
haq <- readxl(paste0("../02_data/20180815/", haqf))
haq <- subset(haq, year_id == 2015)
haq <- subset(haq, indicator_name == "Healthcare Access and Quality")

id_haq <- match(names(daly_who), haq$location_name)
names(daly_who)[is.na(id_haq)]

haq$location_name[haq$location_name == "Vietnam"] <-
  "Viet Nam"
haq$location_name[haq$location_name == "Laos"] <-
  "Lao People's Democratic Republic"
haq$location_name[haq$location_name == "Brunei"] <-
  "Brunei Darussalam"
haq$location_name[haq$location_name == "Cote d'Ivoire"] <-
  "Côte d'Ivoire"
haq$location_name[haq$location_name == "Tanzania"] <-
  "United Republic of Tanzania"
haq$location_name[haq$location_name == "The Gambia"] <-
  "Gambia"
haq$location_name[haq$location_name == "Venezuela"] <-
  "Venezuela (Bolivarian Republic of)"
haq$location_name[haq$location_name == "Bolivia"] <-
  "Bolivia (Plurinational State of)"
haq$location_name[haq$location_name == "Iran"] <-
  "Iran (Islamic Republic of)"

id_haq <- match(names(daly_who), haq$location_name)
names(daly_who)[is.na(id_haq)]

df_haq <- data.frame(DALY = daly_who, HAQ = haq$val[id_haq])
df_haq$HAQ <- as.numeric(as.character(df_haq$HAQ))
df_haq$Region <-
  FERG2015:::crpop_2015$WHORegion[
    match(rownames(df_haq), FERG2015:::crpop_2015$Country)]
df_haq$ISO <-
  FERG2015:::crpop_2015$ISO3[
    match(rownames(df_haq), FERG2015:::crpop_2015$Country)]

summary(lm(log(DALY) ~ HAQ, data = df_haq))
with(df_haq, cor.test(log(DALY), HAQ, method = "spearman"))

p1 <-
ggplot(df_haq, aes(x = HAQ, y = DALY)) +
  geom_smooth(method = "lm") + 
  geom_point(aes(fill = Region), size = 3, shape = 21) +
  geom_text_repel(aes(label = ISO), show.legend = FALSE) +
  theme_classic() +
  scale_y_log10("Disability-adjusted life years per 100,000") +
  scale_x_continuous("Healthcare access and quality index")

print(p1)


#' # FIG 5b / SCATTERPLOT DALY-SDI
#+ fig.height=7, fig.width=8

sdi <- readxl("../02_data/20180815/DALY SDI and HAQ Index.xls", skip = 1)
str(sdi)
id_sdi <- match(names(daly_who), sdi$Location)
names(daly_who)[is.na(id_sdi)]

sdi$Location[sdi$Location == "Vietnam"] <-
  "Viet Nam"
sdi$Location[sdi$Location == "Laos"] <-
  "Lao People's Democratic Republic"
sdi$Location[sdi$Location == "Brunei"] <-
  "Brunei Darussalam"
sdi$Location[sdi$Location == "Cote d'Ivoire"] <-
  "Côte d'Ivoire"
sdi$Location[sdi$Location == "Tanzania"] <-
  "United Republic of Tanzania"
sdi$Location[sdi$Location == "The Gambia"] <-
  "Gambia"
sdi$Location[sdi$Location == "Venezuela"] <-
  "Venezuela (Bolivarian Republic of)"
sdi$Location[sdi$Location == "Bolivia"] <-
  "Bolivia (Plurinational State of)"
sdi$Location[sdi$Location == "Iran"] <-
  "Iran (Islamic Republic of)"

id_sdi <- match(names(daly_who), sdi$Location)
names(daly_who)[is.na(id_sdi)]

df_sdi <- data.frame(DALY = daly_who, SDI = sdi$X2015[id_sdi])
df_sdi$Region <-
  FERG2015:::crpop_2015$WHORegion[
    match(rownames(df_sdi), FERG2015:::crpop_2015$Country)]
df_sdi$ISO <-
  FERG2015:::crpop_2015$ISO3[
    match(rownames(df_sdi), FERG2015:::crpop_2015$Country)]

summary(lm(log(DALY) ~ SDI, data = df_sdi))
with(df_sdi, cor.test(log(DALY), SDI, method = "spearman"))

p2 <-
ggplot(df_sdi, aes(x = SDI, y = DALY)) +
  geom_smooth(method = "lm") + 
  geom_point(aes(fill = Region), size = 3, shape = 21) +
  geom_text_repel(aes(label = ISO), show.legend = FALSE) +
  theme_classic() +
  scale_y_log10("Disability-adjusted life years per 100,000") +
  scale_x_continuous("Socio-demographic index")

print(p2)


## combine plots
pdf("Fig5.pdf", 16, 7)
cowplot::plot_grid(
  p1 + theme(legend.position = "none"),
  p2 + theme(legend.position = "none"),
  cowplot::get_legend(p1),
  labels = c("A", "B"),
  nrow = 1,
  rel_widths = c(1, 1, .2))
graphics.off()


#' # R session info
sessionInfo()

## render script
if (FALSE) {
rmarkdown::render(
  "melioidosis-daly.R",
  output_file = paste0("melioidosis-daly-", bd::today(), ".md"))
}
