MELIOIDOSIS / READ INC MRT / MEDIAN
================
Bdevleesschauwer
Sun Aug 26 19:33:04 2018

``` r
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
```

    ## 'data.frame':    92 obs. of  7 variables:
    ##  $ Country.name: chr  "East_Asia_and_Pacific" "Indonesia" "Vietnam" "Phillipines" ...
    ##  $ X__1        : chr  "Predicted_Incidence" "20038" "10430" "9116" ...
    ##  $ X__2        : chr  "Lower_Limit_PI" "7859" "4097" "4819" ...
    ##  $ X__3        : chr  "Upper_Limit_PI" "52812" "27480" "18999" ...
    ##  $ X__4        : chr  "Predicted Mortality" "10224" "4703" "4510" ...
    ##  $ X__5        : chr  "Lower_Limit_PM" "3944" "1827" "2369" ...
    ##  $ X__6        : chr  "Upper_Limit_PM" "27524" "12631" "9739" ...

``` r
## data cleaning
dta <- dta[!is.na(dta[[2]]), ]  # drop empty rows
dta <- dta[-1, ]                # drop first row
colnames(dta) <-
  c("COUNTRY", "INC", "INC.LW", "INC.UP", "MRT", "MRT.LW", "MRT.UP")
str(dta)
```

    ## 'data.frame':    83 obs. of  7 variables:
    ##  $ COUNTRY: chr  "Indonesia" "Vietnam" "Phillipines" "Thailand" ...
    ##  $ INC    : chr  "20038" "10430" "9116" "7572" ...
    ##  $ INC.LW : chr  "7859" "4097" "4819" "3396" ...
    ##  $ INC.UP : chr  "52812" "27480" "18999" "17685" ...
    ##  $ MRT    : chr  "10224" "4703" "4510" "2838" ...
    ##  $ MRT.LW : chr  "3944" "1827" "2369" "1259" ...
    ##  $ MRT.UP : chr  "27524" "12631" "9739" "6678" ...

``` r
## check country names
dta$COUNTRY[duplicated(dta$COUNTRY)]
```

    ## character(0)

``` r
## match FERG names
id <- match(tolower(dta$COUNTRY), tolower(FERG2015:::crpop_2015$Country))
dta$COUNTRY[is.na(id)]
```

    ##  [1] "Vietnam"         "Phillipines"     "Lao"            
    ##  [4] "Hong Kong"       "Brunei"          "Timor"          
    ##  [7] "Cote d'Ivoire"   "Tanzania"        "Congo Rep"      
    ## [10] "Congo Dem. Rep." "Guinea Bissau"   "Venezuela"      
    ## [13] "Bolivia"         "Yemen, Rep"      "Iran"

``` r
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
```

    ## [1] "Hong Kong"

``` r
id <- match(tolower(FERG2015:::crpop_2015$Country), tolower(dta$COUNTRY))
FERG2015:::crpop_2015$Country[is.na(id)]
```

    ##   [1] "Afghanistan"                              
    ##   [2] "Albania"                                  
    ##   [3] "Algeria"                                  
    ##   [4] "Andorra"                                  
    ##   [5] "Antigua and Barbuda"                      
    ##   [6] "Armenia"                                  
    ##   [7] "Austria"                                  
    ##   [8] "Azerbaijan"                               
    ##   [9] "Bahamas"                                  
    ##  [10] "Bahrain"                                  
    ##  [11] "Barbados"                                 
    ##  [12] "Belarus"                                  
    ##  [13] "Belgium"                                  
    ##  [14] "Belize"                                   
    ##  [15] "Bosnia and Herzegovina"                   
    ##  [16] "Botswana"                                 
    ##  [17] "Bulgaria"                                 
    ##  [18] "Burundi"                                  
    ##  [19] "Canada"                                   
    ##  [20] "Cape Verde"                               
    ##  [21] "Chile"                                    
    ##  [22] "Comoros"                                  
    ##  [23] "Cook Islands"                             
    ##  [24] "Croatia"                                  
    ##  [25] "Cyprus"                                   
    ##  [26] "Czech Republic"                           
    ##  [27] "Democratic People's Republic of Korea"    
    ##  [28] "Denmark"                                  
    ##  [29] "Djibouti"                                 
    ##  [30] "Dominica"                                 
    ##  [31] "Dominican Republic"                       
    ##  [32] "Ecuador"                                  
    ##  [33] "Egypt"                                    
    ##  [34] "Estonia"                                  
    ##  [35] "Finland"                                  
    ##  [36] "France"                                   
    ##  [37] "Georgia"                                  
    ##  [38] "Germany"                                  
    ##  [39] "Greece"                                   
    ##  [40] "Grenada"                                  
    ##  [41] "Hungary"                                  
    ##  [42] "Iceland"                                  
    ##  [43] "Ireland"                                  
    ##  [44] "Israel"                                   
    ##  [45] "Italy"                                    
    ##  [46] "Jamaica"                                  
    ##  [47] "Japan"                                    
    ##  [48] "Jordan"                                   
    ##  [49] "Kazakhstan"                               
    ##  [50] "Kiribati"                                 
    ##  [51] "Kuwait"                                   
    ##  [52] "Kyrgyzstan"                               
    ##  [53] "Latvia"                                   
    ##  [54] "Lebanon"                                  
    ##  [55] "Lesotho"                                  
    ##  [56] "Libyan Arab Jamahiriya"                   
    ##  [57] "Lithuania"                                
    ##  [58] "Luxembourg"                               
    ##  [59] "Maldives"                                 
    ##  [60] "Malta"                                    
    ##  [61] "Marshall Islands"                         
    ##  [62] "Micronesia (Federated States of)"         
    ##  [63] "Monaco"                                   
    ##  [64] "Mongolia"                                 
    ##  [65] "Montenegro"                               
    ##  [66] "Morocco"                                  
    ##  [67] "Namibia"                                  
    ##  [68] "Nauru"                                    
    ##  [69] "Netherlands"                              
    ##  [70] "New Zealand"                              
    ##  [71] "Niue"                                     
    ##  [72] "Norway"                                   
    ##  [73] "Palau"                                    
    ##  [74] "Poland"                                   
    ##  [75] "Portugal"                                 
    ##  [76] "Qatar"                                    
    ##  [77] "Republic of Korea"                        
    ##  [78] "Republic of Moldova"                      
    ##  [79] "Romania"                                  
    ##  [80] "Russian Federation"                       
    ##  [81] "Rwanda"                                   
    ##  [82] "Saint Kitts and Nevis"                    
    ##  [83] "Saint Lucia"                              
    ##  [84] "Saint Vincent and the Grenadines"         
    ##  [85] "Samoa"                                    
    ##  [86] "San Marino"                               
    ##  [87] "Sao Tome and Principe"                    
    ##  [88] "Serbia"                                   
    ##  [89] "Seychelles"                               
    ##  [90] "Slovakia"                                 
    ##  [91] "Slovenia"                                 
    ##  [92] "Solomon Islands"                          
    ##  [93] "Spain"                                    
    ##  [94] "Swaziland"                                
    ##  [95] "Sweden"                                   
    ##  [96] "Switzerland"                              
    ##  [97] "Syrian Arab Republic"                     
    ##  [98] "Tajikistan"                               
    ##  [99] "The Former Yugoslav Republic of Macedonia"
    ## [100] "Tonga"                                    
    ## [101] "Trinidad and Tobago"                      
    ## [102] "Tunisia"                                  
    ## [103] "Turkey"                                   
    ## [104] "Turkmenistan"                             
    ## [105] "Tuvalu"                                   
    ## [106] "Ukraine"                                  
    ## [107] "United Arab Emirates"                     
    ## [108] "United Kingdom"                           
    ## [109] "United States of America"                 
    ## [110] "Uruguay"                                  
    ## [111] "Uzbekistan"                               
    ## [112] "Vanuatu"

``` r
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
```

    ## [1] TRUE

``` r
all(INC[, 2] < INC[, 3])
```

    ## [1] TRUE

``` r
## add dimnames
colnames(INC) <- colnames(MRT) <- c("lwr", "median", "upr")
rownames(INC) <- rownames(MRT) <- dta$COUNTRY

## show data
knitr::kable(INC)
```

|                                    |   lwr | median |    upr |
| ---------------------------------- | ----: | -----: | -----: |
| Indonesia                          |  7859 |  20038 |  52812 |
| Viet Nam                           |  4097 |  10430 |  27480 |
| Philippines                        |  4819 |   9116 |  18999 |
| Thailand                           |  3396 |   7572 |  17685 |
| China                              |  3099 |   7174 |  15752 |
| Myanmar                            |  2513 |   6247 |  15400 |
| Cambodia                           |   850 |   2083 |   5451 |
| Malaysia                           |   718 |   1752 |   4581 |
| Lao People’s Democratic Republic   |   172 |    420 |   1072 |
| Singapore                          |    66 |    276 |    925 |
| Australia                          |    56 |    149 |    416 |
| Papua New Guinea                   |    43 |    129 |    337 |
| NA                                 |    23 |     67 |    288 |
| Brunei Darussalam                  |    12 |     29 |     71 |
| Timor-Leste                        |     3 |     10 |     35 |
| Fiji                               |     1 |      4 |     14 |
| India                              | 22335 |  52506 | 124652 |
| Bangladesh                         |  7814 |  16931 |  37794 |
| Sri Lanka                          |   705 |   1881 |   4488 |
| Nepal                              |   317 |    914 |   2354 |
| Pakistan                           |    95 |    442 |   1718 |
| Bhutan                             |     5 |     13 |     42 |
| Nigeria                            |  4839 |  13481 |  38348 |
| Guinea                             |   472 |   1372 |   3810 |
| Côte d’Ivoire                      |   414 |   1144 |   3368 |
| Benin                              |   348 |    919 |   2580 |
| Madagascar                         |   326 |    880 |   2464 |
| Burkina Faso                       |   196 |    627 |   2102 |
| Sierra Leone                       |   212 |    600 |   1715 |
| Mali                               |   190 |    580 |   1912 |
| Cameroon                           |   169 |    540 |   1699 |
| Liberia                            |   148 |    445 |   1288 |
| Chad                               |   114 |    401 |   1432 |
| Ghana                              |   111 |    389 |   1446 |
| Niger                              |    78 |    368 |   1371 |
| United Republic of Tanzania        |    74 |    307 |    991 |
| Congo                              |    98 |    262 |    716 |
| Ethiopia                           |    61 |    261 |    885 |
| Mozambique                         |    65 |    238 |    775 |
| Democratic Republic of the Congo   |    53 |    222 |    772 |
| Malawi                             |    69 |    221 |    636 |
| Togo                               |    43 |    157 |    500 |
| Central African Republic           |    45 |    142 |    422 |
| Zambia                             |    30 |    112 |    374 |
| Guinea-Bissau                      |    28 |    100 |    345 |
| Kenya                              |    27 |    100 |    327 |
| Somalia                            |    13 |     71 |    254 |
| Sudan                              |     8 |     62 |    247 |
| Senegal                            |    13 |     60 |    247 |
| Gabon                              |    16 |     45 |    127 |
| South Sudan                        |     8 |     39 |    131 |
| Uganda                             |     5 |     30 |    131 |
| Angola                             |     6 |     29 |    116 |
| South Africa                       |     6 |     28 |    103 |
| Mauritania                         |     6 |     28 |    110 |
| Eritrea                            |     6 |     27 |    101 |
| Gambia                             |     1 |      8 |     33 |
| Zimbabwe                           |     2 |      7 |     28 |
| Equatorial Guinea                  |     2 |      6 |     17 |
| Mauritius                          |     1 |      5 |     18 |
| Brazil                             |   273 |    872 |   2905 |
| Mexico                             |   158 |    550 |   1712 |
| Colombia                           |    43 |    157 |    496 |
| El Salvador                        |    37 |    114 |    295 |
| Venezuela (Bolivarian Republic of) |    31 |    103 |    311 |
| Honduras                           |    22 |     86 |    264 |
| Panama                             |    23 |     67 |    179 |
| Guatemala                          |    20 |     66 |    197 |
| Nicaragua                          |    18 |     65 |    196 |
| Peru                               |    10 |     39 |    128 |
| Haiti                              |     5 |     24 |     86 |
| Cuba                               |     4 |     20 |     89 |
| Argentina                          |     3 |     18 |     75 |
| Costa Rica                         |     5 |     16 |     49 |
| Suriname                           |     4 |     14 |     39 |
| Paraguay                           |     2 |     13 |     59 |
| Bolivia (Plurinational State of)   |     3 |     13 |     49 |
| Guyana                             |     3 |     12 |     36 |
| Yemen                              |    29 |     99 |    302 |
| Saudi Arabia                       |    13 |     52 |    197 |
| Iraq                               |     4 |     21 |    127 |
| Iran (Islamic Republic of)         |     3 |     15 |     73 |
| Oman                               |     2 |      6 |     19 |

``` r
knitr::kable(MRT)
```

|                                    |   lwr | median |   upr |
| ---------------------------------- | ----: | -----: | ----: |
| Indonesia                          |  3944 |  10224 | 27524 |
| Viet Nam                           |  1827 |   4703 | 12631 |
| Philippines                        |  2369 |   4510 |  9739 |
| Thailand                           |  1259 |   2838 |  6678 |
| China                              |  1148 |   2614 |  5828 |
| Myanmar                            |  1449 |   3687 |  9299 |
| Cambodia                           |   464 |   1149 |  3042 |
| Malaysia                           |   209 |    519 |  1352 |
| Lao People’s Democratic Republic   |   104 |    260 |   650 |
| Singapore                          |    11 |     47 |   174 |
| Australia                          |    13 |     35 |    98 |
| Papua New Guinea                   |    26 |     79 |   214 |
| NA                                 |     4 |     12 |    50 |
| Brunei Darussalam                  |     4 |      9 |    21 |
| Timor-Leste                        |     2 |      6 |    22 |
| Fiji                               |     0 |      2 |     6 |
| India                              | 13404 |  31425 | 75601 |
| Bangladesh                         |  4325 |   9454 | 21621 |
| Sri Lanka                          |   230 |    619 |  1501 |
| Nepal                              |   174 |    502 |  1353 |
| Pakistan                           |    58 |    260 |  1059 |
| Bhutan                             |     3 |      8 |    24 |
| Nigeria                            |  2959 |   8324 | 23933 |
| Guinea                             |   288 |    842 |  2425 |
| Côte d’Ivoire                      |   252 |    704 |  2073 |
| Benin                              |   213 |    565 |  1623 |
| Madagascar                         |   196 |    532 |  1528 |
| Burkina Faso                       |   122 |    382 |  1321 |
| Sierra Leone                       |   128 |    371 |  1097 |
| Mali                               |   118 |    356 |  1205 |
| Cameroon                           |   102 |    327 |  1077 |
| Liberia                            |    90 |    276 |   814 |
| Chad                               |    71 |    245 |   903 |
| Ghana                              |    67 |    237 |   891 |
| Niger                              |    48 |    220 |   858 |
| United Republic of Tanzania        |    44 |    183 |   611 |
| Congo                              |    61 |    162 |   457 |
| Ethiopia                           |    38 |    155 |   569 |
| Mozambique                         |    40 |    142 |   492 |
| Democratic Republic of the Congo   |    34 |    133 |   485 |
| Malawi                             |    43 |    133 |   406 |
| Togo                               |    27 |     96 |   313 |
| Central African Republic           |    28 |     86 |   265 |
| Zambia                             |    19 |     67 |   240 |
| Guinea-Bissau                      |    17 |     61 |   220 |
| Kenya                              |    16 |     60 |   205 |
| Somalia                            |     8 |     43 |   161 |
| Sudan                              |     5 |     36 |   153 |
| Senegal                            |     8 |     36 |   160 |
| Gabon                              |    10 |     28 |    77 |
| South Sudan                        |     5 |     22 |    81 |
| Uganda                             |     3 |     17 |    84 |
| Angola                             |     4 |     17 |    74 |
| South Africa                       |     4 |     15 |    62 |
| Mauritania                         |     4 |     17 |    69 |
| Eritrea                            |     3 |     15 |    62 |
| Gambia                             |     1 |      4 |    22 |
| Zimbabwe                           |     1 |      4 |    17 |
| Equatorial Guinea                  |     1 |      4 |    11 |
| Mauritius                          |     0 |      2 |     7 |
| Brazil                             |   105 |    339 |  1127 |
| Mexico                             |    62 |    216 |   671 |
| Colombia                           |    17 |     64 |   206 |
| El Salvador                        |    15 |     45 |   119 |
| Venezuela (Bolivarian Republic of) |    12 |     40 |   121 |
| Honduras                           |    10 |     38 |   127 |
| Panama                             |     9 |     28 |    75 |
| Guatemala                          |    10 |     33 |   100 |
| Nicaragua                          |     8 |     30 |    91 |
| Peru                               |     4 |     16 |    55 |
| Haiti                              |     3 |     14 |    56 |
| Cuba                               |     1 |      5 |    23 |
| Argentina                          |     1 |      7 |    29 |
| Costa Rica                         |     2 |      5 |    16 |
| Suriname                           |     2 |      6 |    17 |
| Paraguay                           |     1 |      5 |    27 |
| Bolivia (Plurinational State of)   |     2 |      7 |    28 |
| Guyana                             |     2 |      6 |    18 |
| Yemen                              |    17 |     59 |   182 |
| Saudi Arabia                       |     4 |     16 |    61 |
| Iraq                               |     2 |     11 |    67 |
| Iran (Islamic Republic of)         |     1 |      6 |    30 |
| Oman                               |     1 |      2 |     6 |

``` r
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
```

    ##     mean      50%     2.5%    97.5% 
    ## 190421.0 187156.1 131442.9 267289.6

``` r
stats(rowSums(MRT_sim))
```

    ##      mean       50%      2.5%     97.5% 
    ## 103379.17 101373.65  70111.67 147795.45

``` r
knitr::kable(t(apply(INC_sim, 2, stats)), digits = 0)
```

|                                    |  mean |   50% |  2.5% |  97.5% |
| ---------------------------------- | ----: | ----: | ----: | -----: |
| Indonesia                          | 23520 | 21484 |  6124 |  52505 |
| Viet Nam                           | 12243 | 11184 |  3191 |  27320 |
| Philippines                        | 10226 |  9772 |  4234 |  18829 |
| Thailand                           |  8636 |  8083 |  2858 |  17565 |
| China                              |  7924 |  7465 |  2795 |  15672 |
| Myanmar                            |  7161 |  6620 |  2085 |  15316 |
| Cambodia                           |  2451 |  2246 |   656 |   5416 |
| Malaysia                           |  2062 |  1891 |   554 |   4551 |
| Lao People’s Democratic Republic   |   489 |   451 |   136 |   1065 |
| Singapore                          |   338 |   287 |    45 |    922 |
| Australia                          |   178 |   161 |    41 |    415 |
| Papua New Guinea                   |   148 |   134 |    37 |    338 |
| NA                                 |    99 |    81 |    10 |    285 |
| Brunei Darussalam                  |    33 |    31 |    10 |     71 |
| Timor-Leste                        |    13 |    11 |     2 |     35 |
| Fiji                               |     5 |     4 |     1 |     14 |
| India                              | 59769 | 55675 | 18862 | 123900 |
| Bangladesh                         | 19039 | 17952 |  6781 |  37500 |
| Sri Lanka                          |  2106 |  1951 |   625 |   4472 |
| Nepal                              |  1046 |   954 |   269 |   2347 |
| Pakistan                           |   571 |   459 |    49 |   1720 |
| Bhutan                             |    16 |    14 |     3 |     42 |
| Nigeria                            | 16140 | 14454 |  3542 |  38229 |
| Guinea                             |  1619 |  1454 |   365 |   3807 |
| Côte d’Ivoire                      |  1390 |  1236 |   286 |   3355 |
| Benin                              |  1102 |   992 |   253 |   2569 |
| Madagascar                         |  1051 |   946 |   241 |   2455 |
| Burkina Faso                       |   785 |   672 |   115 |   2092 |
| Sierra Leone                       |   718 |   642 |   155 |   1708 |
| Mali                               |   726 |   625 |   113 |   1903 |
| Cameroon                           |   661 |   576 |   112 |   1695 |
| Liberia                            |   532 |   473 |   109 |   1285 |
| Chad                               |   510 |   426 |    60 |   1425 |
| Ghana                              |   502 |   415 |    53 |   1441 |
| Niger                              |   467 |   382 |    46 |   1372 |
| United Republic of Tanzania        |   371 |   318 |    54 |    989 |
| Congo                              |   311 |   281 |    75 |    715 |
| Ethiopia                           |   321 |   271 |    41 |    882 |
| Mozambique                         |   292 |   250 |    44 |    773 |
| Democratic Republic of the Congo   |   276 |   231 |    33 |    769 |
| Malawi                             |   262 |   232 |    53 |    635 |
| Togo                               |   191 |   165 |    30 |    499 |
| Central African Republic           |   170 |   151 |    33 |    422 |
| Zambia                             |   138 |   118 |    19 |    372 |
| Guinea-Bissau                      |   125 |   106 |    16 |    343 |
| Kenya                              |   123 |   105 |    18 |    326 |
| Somalia                            |    88 |    73 |     9 |    254 |
| Sudan                              |    80 |    63 |     6 |    248 |
| Senegal                            |    79 |    62 |     6 |    248 |
| Gabon                              |    54 |    48 |    12 |    127 |
| South Sudan                        |    47 |    40 |     6 |    131 |
| Uganda                             |    40 |    31 |     2 |    131 |
| Angola                             |    38 |    30 |     3 |    116 |
| South Africa                       |    35 |    29 |     4 |    103 |
| Mauritania                         |    36 |    29 |     3 |    110 |
| Eritrea                            |    34 |    28 |     3 |    101 |
| Gambia                             |    10 |     8 |     1 |     33 |
| Zimbabwe                           |     9 |     7 |     1 |     28 |
| Equatorial Guinea                  |     7 |     6 |     2 |     17 |
| Mauritius                          |     6 |     5 |     1 |     18 |
| Brazil                             |  1090 |   935 |   162 |   2892 |
| Mexico                             |   666 |   579 |   112 |   1710 |
| Colombia                           |   190 |   165 |    30 |    495 |
| El Salvador                        |   130 |   118 |    32 |    296 |
| Venezuela (Bolivarian Republic of) |   124 |   109 |    23 |    311 |
| Honduras                           |   103 |    89 |    17 |    264 |
| Panama                             |    78 |    70 |    19 |    179 |
| Guatemala                          |    79 |    70 |    15 |    197 |
| Nicaragua                          |    77 |    68 |    14 |    196 |
| Peru                               |    48 |    41 |     7 |    128 |
| Haiti                              |    30 |    25 |     3 |     86 |
| Cuba                               |    27 |    21 |     2 |     89 |
| Argentina                          |    24 |    18 |     2 |     75 |
| Costa Rica                         |    19 |    17 |     3 |     49 |
| Suriname                           |    16 |    14 |     3 |     39 |
| Paraguay                           |    18 |    13 |     1 |     59 |
| Bolivia (Plurinational State of)   |    17 |    14 |     2 |     49 |
| Guyana                             |    14 |    12 |     2 |     36 |
| Yemen                              |   119 |   104 |    21 |    302 |
| Saudi Arabia                       |    67 |    55 |     7 |    197 |
| Iraq                               |    33 |    22 |     1 |    128 |
| Iran (Islamic Republic of)         |    21 |    15 |     1 |     73 |
| Oman                               |     7 |     6 |     1 |     19 |

``` r
knitr::kable(t(apply(MRT_sim, 2, stats)), digits = 0)
```

|                                    |  mean |   50% |  2.5% | 97.5% |
| ---------------------------------- | ----: | ----: | ----: | ----: |
| Indonesia                          | 12095 | 10987 |  3024 | 27480 |
| Viet Nam                           |  5562 |  5055 |  1400 | 12610 |
| Philippines                        |  5126 |  4879 |  2025 |  9644 |
| Thailand                           |  3241 |  3029 |  1056 |  6633 |
| China                              |  2913 |  2741 |  1015 |  5793 |
| Myanmar                            |  4252 |  3914 |  1184 |  9254 |
| Cambodia                           |  1356 |  1240 |   355 |  3023 |
| Malaysia                           |   609 |   558 |   163 |  1344 |
| Lao People’s Democratic Republic   |   299 |   276 |    85 |   646 |
| Singapore                          |    60 |    49 |     6 |   174 |
| Australia                          |    42 |    38 |    10 |    98 |
| Papua New Guinea                   |    92 |    83 |    21 |   214 |
| NA                                 |    16 |    13 |     1 |    50 |
| Brunei Darussalam                  |    10 |    10 |     3 |    21 |
| Timor-Leste                        |     8 |     7 |     1 |    22 |
| Fiji                               |     2 |     2 |     0 |     6 |
| India                              | 35971 | 33457 | 11155 | 75098 |
| Bangladesh                         | 10720 | 10072 |  3684 | 21457 |
| Sri Lanka                          |   697 |   644 |   201 |  1497 |
| Nepal                              |   586 |   530 |   140 |  1353 |
| Pakistan                           |   343 |   271 |    26 |  1061 |
| Bhutan                             |    10 |     9 |     2 |    24 |
| Nigeria                            |  9999 |  8931 |  2139 | 23820 |
| Guinea                             |  1007 |   898 |   211 |  2416 |
| Côte d’Ivoire                      |   854 |   760 |   175 |  2066 |
| Benin                              |   683 |   612 |   150 |  1617 |
| Madagascar                         |   642 |   574 |   140 |  1522 |
| Burkina Faso                       |   485 |   412 |    66 |  1315 |
| Sierra Leone                       |   449 |   398 |    90 |  1094 |
| Mali                               |   450 |   385 |    66 |  1199 |
| Cameroon                           |   407 |   350 |    62 |  1072 |
| Liberia                            |   331 |   293 |    65 |   813 |
| Chad                               |   316 |   262 |    35 |   900 |
| Ghana                              |   307 |   252 |    32 |   889 |
| Niger                              |   285 |   229 |    24 |   858 |
| United Republic of Tanzania        |   224 |   190 |    30 |   609 |
| Congo                              |   195 |   175 |    44 |   455 |
| Ethiopia                           |   197 |   162 |    21 |   568 |
| Mozambique                         |   178 |   151 |    23 |   490 |
| Democratic Republic of the Congo   |   169 |   140 |    18 |   484 |
| Malawi                             |   162 |   142 |    30 |   406 |
| Togo                               |   118 |   101 |    18 |   312 |
| Central African Republic           |   105 |    92 |    19 |   265 |
| Zambia                             |    85 |    71 |    10 |   239 |
| Guinea-Bissau                      |    78 |    65 |     9 |   220 |
| Kenya                              |    75 |    63 |    10 |   204 |
| Somalia                            |    54 |    44 |     5 |   161 |
| Sudan                              |    48 |    37 |     3 |   154 |
| Senegal                            |    49 |    37 |     3 |   160 |
| Gabon                              |    33 |    30 |     8 |    77 |
| South Sudan                        |    28 |    23 |     3 |    81 |
| Uganda                             |    24 |    17 |     1 |    84 |
| Angola                             |    23 |    18 |     1 |    74 |
| South Africa                       |    20 |    16 |     2 |    62 |
| Mauritania                         |    22 |    18 |     2 |    69 |
| Eritrea                            |    20 |    16 |     1 |    62 |
| Gambia                             |     6 |     4 |     0 |    22 |
| Zimbabwe                           |     5 |     4 |     0 |    17 |
| Equatorial Guinea                  |     5 |     4 |     1 |    11 |
| Mauritius                          |     2 |     2 |     0 |     7 |
| Brazil                             |   423 |   363 |    63 |  1122 |
| Mexico                             |   261 |   227 |    44 |   671 |
| Colombia                           |    78 |    67 |    12 |   205 |
| El Salvador                        |    52 |    47 |    13 |   119 |
| Venezuela (Bolivarian Republic of) |    48 |    42 |     9 |   121 |
| Honduras                           |    47 |    40 |     6 |   126 |
| Panama                             |    32 |    29 |     8 |    75 |
| Guatemala                          |    40 |    35 |     7 |   100 |
| Nicaragua                          |    36 |    31 |     6 |    91 |
| Peru                               |    20 |    17 |     3 |    55 |
| Haiti                              |    18 |    15 |     1 |    56 |
| Cuba                               |     7 |     5 |     0 |    23 |
| Argentina                          |     9 |     7 |     1 |    29 |
| Costa Rica                         |     6 |     6 |     1 |    16 |
| Suriname                           |     7 |     6 |     2 |    17 |
| Paraguay                           |     7 |     5 |     0 |    27 |
| Bolivia (Plurinational State of)   |     9 |     7 |     1 |    28 |
| Guyana                             |     7 |     6 |     1 |    18 |
| Yemen                              |    71 |    62 |    12 |   182 |
| Saudi Arabia                       |    21 |    17 |     2 |    61 |
| Iraq                               |    17 |    11 |     0 |    67 |
| Iran (Islamic Republic of)         |     9 |     6 |     0 |    30 |
| Oman                               |     3 |     2 |     1 |     6 |

``` r
## calculate rates per 100k
INC_rt_sim <- t(t(1e5 * INC_sim) / pop$pop)
MRT_rt_sim <- t(t(1e5 * MRT_sim) / pop$pop)

knitr::kable(t(apply(INC_rt_sim, 2, stats, na.rm = TRUE)), digits = 3)
```

|                                    |   mean |    50% |  2.5% |  97.5% |
| ---------------------------------- | -----: | -----: | ----: | -----: |
| Indonesia                          |  9.111 |  8.322 | 2.372 | 20.338 |
| Viet Nam                           | 13.084 | 11.953 | 3.410 | 29.197 |
| Philippines                        | 10.053 |  9.607 | 4.163 | 18.511 |
| Thailand                           | 12.578 | 11.774 | 4.162 | 25.583 |
| China                              |  0.567 |  0.534 | 0.200 |  1.122 |
| Myanmar                            | 13.665 | 12.632 | 3.979 | 29.227 |
| Cambodia                           | 15.797 | 14.476 | 4.229 | 34.900 |
| Malaysia                           |  6.713 |  6.154 | 1.804 | 14.813 |
| Lao People’s Democratic Republic   |  7.345 |  6.762 | 2.045 | 15.989 |
| Singapore                          |  6.115 |  5.179 | 0.816 | 16.658 |
| Australia                          |  0.749 |  0.674 | 0.174 |  1.743 |
| Papua New Guinea                   |  1.869 |  1.695 | 0.462 |  4.262 |
| NA                                 |    NaN |     NA |    NA |     NA |
| Brunei Darussalam                  |  7.973 |  7.388 | 2.376 | 16.895 |
| Timor-Leste                        |  1.022 |  0.862 | 0.132 |  2.806 |
| Fiji                               |  0.560 |  0.470 | 0.067 |  1.563 |
| India                              |  4.566 |  4.253 | 1.441 |  9.465 |
| Bangladesh                         | 11.811 | 11.136 | 4.206 | 23.263 |
| Sri Lanka                          | 10.169 |  9.419 | 3.015 | 21.589 |
| Nepal                              |  3.650 |  3.329 | 0.939 |  8.189 |
| Pakistan                           |  0.301 |  0.242 | 0.026 |  0.908 |
| Bhutan                             |  2.090 |  1.826 | 0.364 |  5.309 |
| Nigeria                            |  8.908 |  7.978 | 1.955 | 21.100 |
| Guinea                             | 13.393 | 12.027 | 3.016 | 31.486 |
| Côte d’Ivoire                      |  6.014 |  5.351 | 1.236 | 14.518 |
| Benin                              | 10.418 |  9.380 | 2.397 | 24.295 |
| Madagascar                         |  4.337 |  3.903 | 0.993 | 10.128 |
| Burkina Faso                       |  4.337 |  3.712 | 0.635 | 11.551 |
| Sierra Leone                       |  9.927 |  8.874 | 2.141 | 23.603 |
| Mali                               |  4.154 |  3.579 | 0.646 | 10.894 |
| Cameroon                           |  2.896 |  2.521 | 0.489 |  7.425 |
| Liberia                            | 11.817 | 10.514 | 2.428 | 28.550 |
| Chad                               |  3.637 |  3.044 | 0.431 | 10.171 |
| Ghana                              |  1.819 |  1.503 | 0.193 |  5.224 |
| Niger                              |  2.349 |  1.918 | 0.229 |  6.895 |
| United Republic of Tanzania        |  0.689 |  0.590 | 0.101 |  1.835 |
| Congo                              |  6.217 |  5.624 | 1.493 | 14.306 |
| Ethiopia                           |  0.322 |  0.271 | 0.041 |  0.883 |
| Mozambique                         |  1.041 |  0.893 | 0.155 |  2.758 |
| Democratic Republic of the Congo   |  0.362 |  0.304 | 0.044 |  1.009 |
| Malawi                             |  1.489 |  1.323 | 0.302 |  3.615 |
| Togo                               |  2.573 |  2.221 | 0.404 |  6.728 |
| Central African Republic           |  3.750 |  3.311 | 0.720 |  9.275 |
| Zambia                             |  0.859 |  0.732 | 0.120 |  2.312 |
| Guinea-Bissau                      |  7.085 |  5.987 | 0.925 | 19.392 |
| Kenya                              |  0.260 |  0.222 | 0.038 |  0.690 |
| Somalia                            |  0.633 |  0.521 | 0.066 |  1.826 |
| Sudan                              |  0.206 |  0.162 | 0.015 |  0.641 |
| Senegal                            |  0.530 |  0.417 | 0.039 |  1.653 |
| Gabon                              |  2.781 |  2.493 | 0.616 |  6.567 |
| South Sudan                        |  0.399 |  0.336 | 0.051 |  1.098 |
| Uganda                             |  0.100 |  0.077 | 0.006 |  0.327 |
| Angola                             |  0.136 |  0.108 | 0.011 |  0.417 |
| South Africa                       |  0.064 |  0.052 | 0.006 |  0.186 |
| Mauritania                         |  0.868 |  0.695 | 0.073 |  2.631 |
| Eritrea                            |  0.711 |  0.580 | 0.069 |  2.085 |
| Gambia                             |  0.527 |  0.410 | 0.035 |  1.676 |
| Zimbabwe                           |  0.059 |  0.047 | 0.005 |  0.177 |
| Equatorial Guinea                  |  0.605 |  0.540 | 0.129 |  1.441 |
| Mauritius                          |  0.495 |  0.408 | 0.052 |  1.428 |
| Brazil                             |  0.529 |  0.454 | 0.079 |  1.404 |
| Mexico                             |  0.529 |  0.460 | 0.089 |  1.359 |
| Colombia                           |  0.395 |  0.341 | 0.063 |  1.027 |
| El Salvador                        |  2.057 |  1.867 | 0.511 |  4.682 |
| Venezuela (Bolivarian Republic of) |  0.397 |  0.349 | 0.072 |  1.000 |
| Honduras                           |  1.144 |  0.994 | 0.191 |  2.947 |
| Panama                             |  1.963 |  1.776 | 0.472 |  4.512 |
| Guatemala                          |  0.486 |  0.428 | 0.091 |  1.212 |
| Nicaragua                          |  1.274 |  1.115 | 0.225 |  3.224 |
| Peru                               |  0.152 |  0.130 | 0.022 |  0.407 |
| Haiti                              |  0.280 |  0.231 | 0.030 |  0.802 |
| Cuba                               |  0.237 |  0.180 | 0.013 |  0.778 |
| Argentina                          |  0.055 |  0.043 | 0.004 |  0.173 |
| Costa Rica                         |  0.404 |  0.354 | 0.072 |  1.019 |
| Suriname                           |  2.932 |  2.612 | 0.611 |  7.043 |
| Paraguay                           |  0.267 |  0.200 | 0.013 |  0.889 |
| Bolivia (Plurinational State of)   |  0.155 |  0.127 | 0.015 |  0.457 |
| Guyana                             |  1.842 |  1.608 | 0.318 |  4.692 |
| Yemen                              |  0.443 |  0.387 | 0.078 |  1.121 |
| Saudi Arabia                       |  0.213 |  0.173 | 0.021 |  0.624 |
| Iraq                               |  0.091 |  0.060 | 0.002 |  0.354 |
| Iran (Islamic Republic of)         |  0.027 |  0.019 | 0.001 |  0.092 |
| Oman                               |  0.177 |  0.154 | 0.030 |  0.451 |

``` r
knitr::kable(t(apply(MRT_rt_sim, 2, stats, na.rm = TRUE)), digits = 3)
```

|                                    |  mean |   50% |  2.5% |  97.5% |
| ---------------------------------- | ----: | ----: | ----: | -----: |
| Indonesia                          | 4.685 | 4.256 | 1.171 | 10.645 |
| Viet Nam                           | 5.944 | 5.402 | 1.496 | 13.476 |
| Philippines                        | 5.040 | 4.797 | 1.991 |  9.481 |
| Thailand                           | 4.720 | 4.411 | 1.539 |  9.661 |
| China                              | 0.209 | 0.196 | 0.073 |  0.415 |
| Myanmar                            | 8.114 | 7.469 | 2.259 | 17.660 |
| Cambodia                           | 8.741 | 7.990 | 2.288 | 19.480 |
| Malaysia                           | 1.981 | 1.816 | 0.531 |  4.373 |
| Lao People’s Democratic Republic   | 4.494 | 4.146 | 1.279 |  9.700 |
| Singapore                          | 1.081 | 0.887 | 0.109 |  3.141 |
| Australia                          | 0.176 | 0.158 | 0.040 |  0.410 |
| Papua New Guinea                   | 1.160 | 1.045 | 0.268 |  2.700 |
| NA                                 |   NaN |    NA |    NA |     NA |
| Brunei Darussalam                  | 2.454 | 2.296 | 0.809 |  4.997 |
| Timor-Leste                        | 0.629 | 0.525 | 0.074 |  1.765 |
| Fiji                               | 0.246 | 0.208 | 0.032 |  0.676 |
| India                              | 2.748 | 2.556 | 0.852 |  5.737 |
| Bangladesh                         | 6.650 | 6.248 | 2.285 | 13.311 |
| Sri Lanka                          | 3.365 | 3.107 | 0.970 |  7.226 |
| Nepal                              | 2.045 | 1.849 | 0.488 |  4.722 |
| Pakistan                           | 0.181 | 0.143 | 0.014 |  0.560 |
| Bhutan                             | 1.249 | 1.108 | 0.252 |  3.036 |
| Nigeria                            | 5.519 | 4.930 | 1.181 | 13.147 |
| Guinea                             | 8.330 | 7.428 | 1.745 | 19.979 |
| Côte d’Ivoire                      | 3.696 | 3.287 | 0.758 |  8.940 |
| Benin                              | 6.462 | 5.789 | 1.421 | 15.287 |
| Madagascar                         | 2.648 | 2.369 | 0.577 |  6.281 |
| Burkina Faso                       | 2.678 | 2.273 | 0.364 |  7.263 |
| Sierra Leone                       | 6.200 | 5.502 | 1.242 | 15.110 |
| Mali                               | 2.577 | 2.206 | 0.377 |  6.862 |
| Cameroon                           | 1.782 | 1.533 | 0.271 |  4.693 |
| Liberia                            | 7.363 | 6.520 | 1.450 | 18.066 |
| Chad                               | 2.253 | 1.868 | 0.247 |  6.422 |
| Ghana                              | 1.113 | 0.915 | 0.114 |  3.222 |
| Niger                              | 1.431 | 1.149 | 0.123 |  4.314 |
| United Republic of Tanzania        | 0.416 | 0.353 | 0.056 |  1.131 |
| Congo                              | 3.894 | 3.501 | 0.886 |  9.115 |
| Ethiopia                           | 0.197 | 0.163 | 0.021 |  0.568 |
| Mozambique                         | 0.637 | 0.538 | 0.082 |  1.748 |
| Democratic Republic of the Congo   | 0.222 | 0.184 | 0.024 |  0.635 |
| Malawi                             | 0.920 | 0.808 | 0.168 |  2.310 |
| Togo                               | 1.590 | 1.365 | 0.239 |  4.202 |
| Central African Republic           | 2.310 | 2.025 | 0.416 |  5.831 |
| Zambia                             | 0.529 | 0.443 | 0.062 |  1.486 |
| Guinea-Bissau                      | 4.389 | 3.659 | 0.503 | 12.399 |
| Kenya                              | 0.158 | 0.134 | 0.021 |  0.432 |
| Somalia                            | 0.391 | 0.317 | 0.037 |  1.161 |
| Sudan                              | 0.123 | 0.095 | 0.008 |  0.397 |
| Senegal                            | 0.328 | 0.250 | 0.019 |  1.071 |
| Gabon                              | 1.713 | 1.544 | 0.399 |  3.984 |
| South Sudan                        | 0.235 | 0.193 | 0.024 |  0.681 |
| Uganda                             | 0.060 | 0.043 | 0.002 |  0.209 |
| Angola                             | 0.083 | 0.064 | 0.005 |  0.267 |
| South Africa                       | 0.036 | 0.029 | 0.003 |  0.112 |
| Mauritania                         | 0.537 | 0.426 | 0.042 |  1.652 |
| Eritrea                            | 0.409 | 0.321 | 0.029 |  1.283 |
| Gambia                             | 0.302 | 0.209 | 0.007 |  1.111 |
| Zimbabwe                           | 0.034 | 0.027 | 0.002 |  0.108 |
| Equatorial Guinea                  | 0.388 | 0.345 | 0.079 |  0.938 |
| Mauritius                          | 0.187 | 0.151 | 0.017 |  0.559 |
| Brazil                             | 0.205 | 0.176 | 0.031 |  0.545 |
| Mexico                             | 0.208 | 0.181 | 0.035 |  0.533 |
| Colombia                           | 0.162 | 0.139 | 0.025 |  0.425 |
| El Salvador                        | 0.823 | 0.745 | 0.199 |  1.887 |
| Venezuela (Bolivarian Republic of) | 0.154 | 0.136 | 0.028 |  0.389 |
| Honduras                           | 0.523 | 0.445 | 0.072 |  1.411 |
| Panama                             | 0.814 | 0.734 | 0.190 |  1.892 |
| Guatemala                          | 0.245 | 0.215 | 0.044 |  0.616 |
| Nicaragua                          | 0.587 | 0.513 | 0.101 |  1.497 |
| Peru                               | 0.063 | 0.053 | 0.008 |  0.174 |
| Haiti                              | 0.171 | 0.136 | 0.014 |  0.523 |
| Cuba                               | 0.060 | 0.045 | 0.003 |  0.201 |
| Argentina                          | 0.021 | 0.016 | 0.001 |  0.067 |
| Costa Rica                         | 0.132 | 0.116 | 0.024 |  0.332 |
| Suriname                           | 1.286 | 1.148 | 0.275 |  3.062 |
| Paraguay                           | 0.111 | 0.077 | 0.003 |  0.406 |
| Bolivia (Plurinational State of)   | 0.087 | 0.070 | 0.007 |  0.261 |
| Guyana                             | 0.946 | 0.835 | 0.182 |  2.339 |
| Yemen                              | 0.264 | 0.230 | 0.045 |  0.675 |
| Saudi Arabia                       | 0.066 | 0.053 | 0.006 |  0.193 |
| Iraq                               | 0.048 | 0.031 | 0.001 |  0.187 |
| Iran (Islamic Republic of)         | 0.011 | 0.008 | 0.000 |  0.038 |
| Oman                               | 0.061 | 0.055 | 0.014 |  0.142 |

``` r
## R session info
sessionInfo()
```

    ## R version 3.5.1 (2018-07-02)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 7 x64 (build 7601) Service Pack 1
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Dutch_Belgium.1252  LC_CTYPE=Dutch_Belgium.1252   
    ## [3] LC_MONETARY=Dutch_Belgium.1252 LC_NUMERIC=C                  
    ## [5] LC_TIME=Dutch_Belgium.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] RColorBrewer_1.1-2 maps_3.3.0         maptools_0.9-3    
    ##  [4] sp_1.3-1           prevalence_0.4.0   rjags_4-6         
    ##  [7] coda_0.19-1        ggrepel_0.8.0      FERG2015_0.3.0    
    ## [10] future.apply_1.0.0 future_1.9.0       mc2d_0.1-18       
    ## [13] mvtnorm_1.0-8      ggplot2_3.0.0      bd_0.0.12         
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] spam_2.2-0           tidyselect_0.2.4     listenv_0.7.0       
    ##  [4] purrr_0.2.5          rJava_0.9-10         lattice_0.20-35     
    ##  [7] XLConnect_0.2-15     colorspace_1.3-2     htmltools_0.3.6     
    ## [10] yaml_2.2.0           rlang_0.2.1          pillar_1.3.0        
    ## [13] foreign_0.8-70       glue_1.3.0           withr_2.1.2         
    ## [16] readxl_1.1.0         bindrcpp_0.2.2       plyr_1.8.4          
    ## [19] bindr_0.1.1          stringr_1.3.1        fields_9.6          
    ## [22] dotCall64_1.0-0      munsell_0.5.0        gtable_0.2.0        
    ## [25] cellranger_1.1.0     codetools_0.2-15     evaluate_0.11       
    ## [28] labeling_0.3         knitr_1.20           parallel_3.5.1      
    ## [31] rworldmap_1.3-6      highr_0.7            Rcpp_0.12.18        
    ## [34] xtable_1.8-2         XLConnectJars_0.2-15 scales_0.5.0        
    ## [37] backports_1.1.2      digest_0.6.15        stringi_1.1.7       
    ## [40] dplyr_0.7.6          cowplot_0.9.3        grid_3.5.1          
    ## [43] rprojroot_1.3-2      tools_3.5.1          magrittr_1.5        
    ## [46] lazyeval_0.2.1       tibble_1.4.2         crayon_1.3.4        
    ## [49] pkgconfig_2.0.1      assertthat_0.2.0     rmarkdown_1.10      
    ## [52] globals_0.12.1       R6_2.2.2             compiler_3.5.1

``` r
##rmarkdown::render("read-inc-mrt-median.R")
```
