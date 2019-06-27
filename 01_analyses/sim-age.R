#' ---
#' output:
#'   github_document:
#'     html_preview: false
#'     toc: true
#'     toc_depth: 2
#' ---

#' # Settings

## required packages
library(bd)
library(ggplot2)
library(mc2d)

## helper function
ratio <-
function(x) {
  x[1] / x[2]
}

## simulations
set.seed(264)

#' # Load data
#+ warning=FALSE
df <- readxl("../02_data/20180815/DALY_MEL_15-8-18_EB-cleaned.xls",
             "DALY_16_03_2018_FINAL")
names(df)
table(df$Patients)
table(df$Age)
table(df$min)
table(df$max)

## clean data
df$Age[df$Age == "NA"] <- NA
df$Age <- as.numeric(df$Age)

df$min[df$min == "NA"] <- NA
df$min[df$min == "=1/12"] <- 1/12
df$min <- as.numeric(df$min)

df$max[df$max == "NA"] <- NA
df$max[df$max == ">15"] <- 15
df$max <- as.numeric(df$max)

df$Region[df$Region == "NA"] <- NA

df$Mortality_tot[df$Mortality_tot == "NA"] <- NA
df$Mortality_tot <- as.numeric(df$Mortality_tot)

df$Male[df$Male == "NA"] <- NA
df$Male <- as.numeric(df$Male)

df$Female[df$Female == "NA"] <- NA
df$Female <- as.numeric(df$Female)


#' # Simulations

## do simulations
df_inc <-
  df[, c("Region", "Patients", "Age", "min", "max", "Male", "Female")]
df_mrt <-
  df[, c("Region", "Mortality_tot", "Age", "min", "max", "Male", "Female")]
df_mrt <- subset(df_mrt, Mortality_tot > 0)


do_sim_age <-
function(x) {
  if (all(is.na(x[2:4]))) {
    return(NULL)

  } else if (any(is.na(x[3:4]))) {
    return(rep(x[2], x[1]))

  } else if (is.na(x[2])) {
    return(runif(x[1], x[3], x[4]))

  } else {
    return(rpert(x[1], x[3], x[2], x[4]))
  }
}

do_sim_agesex <-
function(x) {
  y <- do_sim_age(x)

  prob <-
    if (any(is.na(x[5:6])) | sum(x[5:6]) == 0) {
      0.5
    } else {
      x[5] / sum(x[5:6])
  }

  m <- rbinom(length(y), 1, prob) == 1

  data.frame(age = y, sex = m)
}

do_sim <-
function(x){
  as <- apply(x[, -1], 1, do_sim_agesex)
  data.frame(age = unlist(sapply(as, function(x) x$age)),
             sex = unlist(sapply(as, function(x) x$sex)))
}


#' ## INCIDENCE

unique(df_inc$Region)
subset(df_inc, is.na(Region))

regions <- sort(unique(df_inc$Region))
reg_inc <- vector("list", length(regions))
names(reg_inc) <- regions

for (i in seq_along(reg_inc)) {
  reg_inc[[i]] <- do_sim(subset(df_inc, Region == regions[i]))
}


#' ## MORTALITY

unique(df_mrt$Region)
subset(df_mrt, is.na(Region))

regions <- sort(unique(df_mrt$Region))
reg_mrt <- vector("list", length(regions))
names(reg_mrt) <- regions

for (i in seq_along(reg_mrt)) {
  reg_mrt[[i]] <- do_sim(subset(df_mrt, Region == regions[i]))
}


#' # Summarize results

#' ## INCIDENCE

round(
  cbind(n = sapply(reg_inc, nrow),
        t(sapply(reg_inc, function(x) summary(x$age)))))

y <- unlist(sapply(reg_inc, function(x) x$age))
x <- unlist(mapply(rep, regions, sapply(reg_inc, nrow)))
x <- relevel(factor(x), "SEAR")
summary(aov(y ~ x))

#+ fig.height=5, fig.width=6
par(mfrow = c(1, 1))
par(mar = c(4, 10, 4, 1))
plot(TukeyHSD(aov(y ~ x)), las = 1)

#+ fig.width=6, fig.height=5
par(mfrow = c(1, 1))
boxplot(y ~ x)

#+ fig.height=8, fig.width=10
tiff("BoxplotInc.tiff", 10, 8, units = "in", res = 300, compress = "lzw")
par(mfrow = c(2, 3))
for (i in seq_along(reg_inc)) {
  main <- sprintf("%s (n=%s)", names(reg_inc)[i], nrow(reg_inc[[i]]))
  hist(reg_inc[[i]]$age, main = main, xlab = "Age")
}
dev.off()


#' ## MORTALITY

round(
  cbind(n = sapply(reg_mrt, nrow),
        t(sapply(reg_mrt, function(x) summary(x$age)))))

y <- unlist(sapply(reg_mrt, function(x) x$age))
x <- unlist(mapply(rep, regions, sapply(reg_mrt, nrow)))
x <- relevel(factor(x), "SEAR")
summary(aov(y ~ x))

#+ fig.height=5, fig.width=6
par(mfrow = c(1, 1))
par(mar = c(4, 10, 4, 1))
plot(TukeyHSD(aov(y ~ x)), las = 1)

#+ fig.width=6, fig.height=5
par(mfrow = c(1, 1))
boxplot(y ~ x)

#+ fig.height=8, fig.width=10
tiff("BoxplotMrt.tiff", 10, 8, units = "in", res = 300, compress = "lzw")
par(mfrow = c(2, 3))
for (i in seq_along(reg_mrt)) {
  main <- sprintf("%s (n=%s)", names(reg_mrt)[i], nrow(reg_mrt[[i]]))
  hist(reg_mrt[[i]]$age, main = main, xlab = "Age")
}
dev.off()


#' # FIG 4a / BARPLOT GLOBAL INC AGE-SEX
#+ fig.height=4, fig.width=5

df_inc <-
  data.frame(Age = unlist(sapply(reg_inc, function(x) x$age)),
             Sex = unlist(sapply(reg_inc, function(x) x$sex)))

grp <- c(0, 1, seq(5, 85, 5), 100)
df_inc$AgeGrp <- cut(df_inc$Age, grp, include.lowest = TRUE)
df_inc_grp <- as.data.frame(with(df_inc, table(AgeGrp, Sex)))
df_inc_grp[df_inc_grp$Sex == FALSE, "Freq"] <-
  -df_inc_grp[df_inc_grp$Sex == FALSE, "Freq"]

breaks <- pretty(df_inc_grp$Freq)
hjust <- 1-(max(df_inc_grp$Freq)/diff(range(df_inc_grp$Freq)))

AgeGrp <- paste0(head(grp, -1), "-", tail(grp, -1)-1)
AgeGrp[1] <- "<1"
AgeGrp[length(AgeGrp)] <- "85+"

p_inc <-
ggplot(df_inc_grp, aes(y = Freq, x = AgeGrp, group = Sex)) +
  geom_col(aes(fill = Sex)) +
  theme_bw() +
  coord_flip() +
  scale_x_discrete("Age Group", labels = AgeGrp) +
  scale_y_continuous("Cases", breaks = breaks, labels = abs(breaks)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  theme(axis.title.x = element_text(hjust = hjust)) +
  ggtitle("Age and sex distribution of melioidosis cases")


#' # FIG 4b / BARPLOT GLOBAL MRT AGE-SEX
#+ fig.height=4, fig.width=5

df_mrt <-
  data.frame(Age = unlist(sapply(reg_mrt, function(x) x$age)),
             Sex = unlist(sapply(reg_mrt, function(x) x$sex)))

grp <- c(0, 1, seq(5, 85, 5), 100)
df_mrt$AgeGrp <- cut(df_mrt$Age, grp, include.lowest = TRUE)
df_mrt_grp <- as.data.frame(with(df_mrt, table(AgeGrp, Sex)))
df_mrt_grp[df_mrt_grp$Sex == FALSE, "Freq"] <-
  -df_mrt_grp[df_mrt_grp$Sex == FALSE, "Freq"]

breaks <- pretty(df_mrt_grp$Freq)
hjust <- 1-(max(df_mrt_grp$Freq)/diff(range(df_mrt_grp$Freq)))

p_mrt <-
ggplot(df_mrt_grp, aes(y = Freq, x = AgeGrp, group = Sex)) +
  geom_col(aes(fill = Sex)) +
  theme_bw() +
  coord_flip() +
  scale_x_discrete("Age Group", labels = AgeGrp) +
  scale_y_continuous("Deaths", breaks = breaks, labels = abs(breaks)) +
  scale_fill_discrete(labels = c("Female", "Male")) +
  theme(axis.title.x = element_text(hjust = hjust)) +
  ggtitle("Age and sex distribution of melioidosis deaths")

#+ fig.height=5, fig.width=12
#pdf("Fig3.pdf", 12, 5)
cowplot::plot_grid(
  p_inc + theme(legend.position = "none"),
  p_mrt + theme(legend.position = "none"),
  cowplot::get_legend(p_inc),
  labels = c("A", "B"),
  nrow = 1,
  rel_widths = c(1, 1, .25))
#graphics.off()



## tabulate age-sex distribution
## .. note: SexFALSE = female; SexTRUE = male
with(df_inc, table(AgeGrp, Sex))
with(df_inc, table(AgeGrp, Sex) / nrow(df_inc))
t(apply(with(df_inc, table(AgeGrp, Sex)), 1, prop.table))
1/ratio(with(subset(df_inc, Age < 15), tapply(Age, Sex, sum)))
1/ratio(with(subset(df_inc, Age >= 15), tapply(Age, Sex, sum)))

with(df_mrt, table(AgeGrp, Sex))
with(df_mrt, table(AgeGrp, Sex) / nrow(df_mrt))
t(apply(with(df_mrt, table(AgeGrp, Sex)), 1, prop.table))
1/ratio(with(subset(df_mrt, Age < 15), tapply(Age, Sex, sum)))
1/ratio(with(subset(df_mrt, Age >= 15), tapply(Age, Sex, sum)))


##rmarkdown::render("sim-age.R")