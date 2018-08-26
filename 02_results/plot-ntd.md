MELIOIDOSIS / NTD PLOT
================
Bdevleesschauwer
Sun Aug 26 19:42:38 2018

``` r
## required packages
library(bd)
library(ggplot2)
library(ggrepel)

## load data
dta <- readxl("../02_data/20180815/NTDi.xlsx")
dta$Cat <- factor(dta$Cat)
dta$Cat[dta$Disease_State == "Leptospirosis"] <- 1
str(dta)
```

    ## 'data.frame':    17 obs. of  4 variables:
    ##  $ Disease_State    : chr  "African Trypanosomiasis" "Chagas Disease" "Cysticercosis" "Dengue" ...
    ##  $ DALY             : num  0.2 0.24 1.96 2.86 0.69 1.09 4.56 1.06 0.42 2.9 ...
    ##  $ Cat              : Factor w/ 3 levels "1","2","3": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Global_Investment: num  400 221 24 788 4 105 283 516 109 5 ...

``` r
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
```

    ## 
    ## Call:
    ## lm(formula = log(DALY) ~ Global_Investment, data = dta)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7675 -0.4950  0.0213  0.7702  1.5017 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)       0.0317176  0.3262258   0.097    0.924
    ## Global_Investment 0.0003158  0.0011835   0.267    0.793
    ## 
    ## Residual standard error: 1.037 on 15 degrees of freedom
    ## Multiple R-squared:  0.004725,   Adjusted R-squared:  -0.06163 
    ## F-statistic: 0.07122 on 1 and 15 DF,  p-value: 0.7932

``` r
with(dta, cor.test(log(DALY), Global_Investment, method = "pearson"))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  log(DALY) and Global_Investment
    ## t = 0.26686, df = 15, p-value = 0.7932
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4259784  0.5318143
    ## sample estimates:
    ##        cor 
    ## 0.06874062

``` r
with(dta, cor.test(log(DALY), Global_Investment, method = "spearman"))
```

    ## Warning in cor.test.default(log(DALY), Global_Investment, method =
    ## "spearman"): Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  log(DALY) and Global_Investment
    ## S = 845.2, p-value = 0.8916
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##         rho 
    ## -0.03578093

``` r
with(dta, cor.test(DALY, Global_Investment, method = "pearson"))
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  DALY and Global_Investment
    ## t = 0.38818, df = 15, p-value = 0.7033
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.4000944  0.5538262
    ## sample estimates:
    ##        cor 
    ## 0.09972852

``` r
with(dta, cor.test(DALY, Global_Investment, method = "spearman"))
```

    ## Warning in cor.test.default(DALY, Global_Investment, method = "spearman"):
    ## Cannot compute exact p-value with ties

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  DALY and Global_Investment
    ## S = 845.2, p-value = 0.8916
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ##         rho 
    ## -0.03578093

``` r
##rmarkdown::render("plot-ntd.R")
```
