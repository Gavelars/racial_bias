---
title: "Racial Bias Analysis"
output: 
  html_document:
    keep_md: yes
---

LOAD PACKAGES

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.2.1     ✔ readr     2.2.0
## ✔ forcats   1.0.1     ✔ stringr   1.6.0
## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
## ✔ purrr     1.2.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(broom)
library(snakecase)
```

LOAD DATA - REMOVE INVALID TRIALS - TRIALS WITH UNAFFILIATED PEDESTRIANS

``` r
data_file <- read_csv("master_file.csv") %>% 
  mutate(
    ethnicity = as.factor(ethnicity),
    gender = as.factor(gender),
    location = as.factor(location),
    first_car_yield = as.factor(first_car_yield),
    did_car_proceed_before_across = as.factor(did_car_proceed_before_across)
  )
```

```
## Rows: 363 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (9): ethnicity, gender, location, date, time_of_day, first_car_yield, di...
## dbl (4): order, trial_number, num_cars_pass_before_yield, time_to_cross_street
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data <- data_file %>% drop_na(valid_trial)

data_19th_rm <- data %>% filter(location != "19th")
```

GENERAL DESCRIPTIVE STATS

``` r
data %>% 
  summarise(
    n = n(),
    mean_time = mean(time_to_cross_street, na.rm = TRUE),
    sd_time = sd(time_to_cross_street, na.rm = TRUE), 
    min_time = min(time_to_cross_street, na.rm = TRUE),
    max_time = max(time_to_cross_street, na.rm = TRUE),
    mean_cars = mean(num_cars_pass_before_yield, na.rm = TRUE), 
    sd_cars = sd(num_cars_pass_before_yield, na.rm = TRUE)
  )
```

```
## # A tibble: 1 × 7
##       n mean_time sd_time min_time max_time mean_cars sd_cars
##   <int>     <dbl>   <dbl>    <dbl>    <dbl>     <dbl>   <dbl>
## 1   240      5.62    2.72     2.07     22.7     0.617   0.995
```

FREQUENCY TABLES

``` r
data %>% count(ethnicity)
```

```
## # A tibble: 2 × 2
##   ethnicity     n
##   <fct>     <int>
## 1 asian       120
## 2 white       120
```

``` r
data %>% count(gender)
```

```
## # A tibble: 2 × 2
##   gender     n
##   <fct>  <int>
## 1 man      120
## 2 woman    120
```

``` r
data %>% count(location)
```

```
## # A tibble: 4 × 2
##   location        n
##   <fct>       <int>
## 1 19th           60
## 2 2nd            60
## 3 bessborough    60
## 4 victoria       60
```

``` r
data %>% count(first_car_yield)
```

```
## # A tibble: 2 × 2
##   first_car_yield     n
##   <fct>           <int>
## 1 no                 92
## 2 yes               148
```

``` r
data %>% count(did_car_proceed_before_across)
```

```
## # A tibble: 3 × 2
##   did_car_proceed_before_across     n
##   <fct>                         <int>
## 1 no                               20
## 2 yes                             199
## 3 <NA>                             21
```

FIRST CAR YIELD DESCRIPTIVE STATS

``` r
data %>% 
  group_by(gender) %>% 
  count(first_car_yield) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 4 × 4
## # Groups:   gender [2]
##   gender first_car_yield     n percentage
##   <fct>  <fct>           <int>      <dbl>
## 1 man    no                 50       41.7
## 2 man    yes                70       58.3
## 3 woman  no                 42       35  
## 4 woman  yes                78       65
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(first_car_yield) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 4 × 4
## # Groups:   ethnicity [2]
##   ethnicity first_car_yield     n percentage
##   <fct>     <fct>           <int>      <dbl>
## 1 asian     no                 44       36.7
## 2 asian     yes                76       63.3
## 3 white     no                 48       40  
## 4 white     yes                72       60
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(first_car_yield) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 8 × 5
## # Groups:   ethnicity, gender [4]
##   ethnicity gender first_car_yield     n percentage
##   <fct>     <fct>  <fct>           <int>      <dbl>
## 1 asian     man    no                 16       26.7
## 2 asian     man    yes                44       73.3
## 3 asian     woman  no                 28       46.7
## 4 asian     woman  yes                32       53.3
## 5 white     man    no                 34       56.7
## 6 white     man    yes                26       43.3
## 7 white     woman  no                 14       23.3
## 8 white     woman  yes                46       76.7
```

``` r
  .groups = "drop"
```

FIRST CAR YIELD - LOGISTIC REGRESSION
GENDER - LOCATION FIXED EFFECTS

``` r
m1 <- glm(first_car_yield ~ gender + factor(location),
          data = data,
          family = binomial())

tidy(m1)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                   -0.793     0.312     -2.54 0.0110      
## 2 genderwoman                    0.339     0.292      1.16 0.246       
## 3 factor(location)2nd            0.758     0.376      2.02 0.0438      
## 4 factor(location)bessborough    1.48      0.392      3.76 0.000167    
## 5 factor(location)victoria       2.66      0.486      5.47 0.0000000461
```

``` r
summary(m1)
```

```
## 
## Call:
## glm(formula = first_car_yield ~ gender + factor(location), family = binomial(), 
##     data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.7928     0.3119  -2.542 0.011032 *  
## genderwoman                   0.3390     0.2921   1.160 0.245889    
## factor(location)2nd           0.7578     0.3759   2.016 0.043765 *  
## factor(location)bessborough   1.4764     0.3923   3.764 0.000167 ***
## factor(location)victoria      2.6587     0.4864   5.466 4.61e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 319.52  on 239  degrees of freedom
## Residual deviance: 275.78  on 235  degrees of freedom
## AIC: 285.78
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(coef(m1))
```

```
##                 (Intercept)                 genderwoman 
##                   0.4525612                   1.4034791 
##         factor(location)2nd factor(location)bessborough 
##                   2.1336757                   4.3771795 
##    factor(location)victoria 
##                  14.2781016
```

ETHNICITY - LOCATION FIXED EFFECTS

``` r
m2 <- glm(first_car_yield ~ ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m2)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                   -0.536     0.306    -1.75  0.0798      
## 2 ethnicitywhite                -0.169     0.291    -0.581 0.561       
## 3 factor(location)2nd            0.754     0.375     2.01  0.0443      
## 4 factor(location)bessborough    1.47      0.391     3.76  0.000173    
## 5 factor(location)victoria       2.65      0.485     5.46  0.0000000486
```

``` r
summary(m2)
```

```
## 
## Call:
## glm(formula = first_car_yield ~ ethnicity + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.5356     0.3058  -1.752 0.079823 .  
## ethnicitywhite               -0.1690     0.2910  -0.581 0.561275    
## factor(location)2nd           0.7539     0.3748   2.011 0.044287 *  
## factor(location)bessborough   1.4688     0.3911   3.756 0.000173 ***
## factor(location)victoria      2.6472     0.4852   5.456 4.86e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 319.52  on 239  degrees of freedom
## Residual deviance: 276.80  on 235  degrees of freedom
## AIC: 286.8
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(coef(m2))
```

```
##                 (Intercept)              ethnicitywhite 
##                   0.5853231                   0.8444752 
##         factor(location)2nd factor(location)bessborough 
##                   2.1252318                   4.3441853 
##    factor(location)victoria 
##                  14.1148880
```

GENDER + ETHNICITY - LOCATION FIXED EFFECTS

``` r
m3 <- glm(first_car_yield ~ gender + ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m3)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                   -0.709     0.343    -2.07  0.0385      
## 2 genderwoman                    0.339     0.292     1.16  0.246       
## 3 ethnicitywhite                -0.170     0.292    -0.583 0.560       
## 4 factor(location)2nd            0.759     0.376     2.02  0.0436      
## 5 factor(location)bessborough    1.48      0.393     3.77  0.000166    
## 6 factor(location)victoria       2.66      0.487     5.47  0.0000000453
```

``` r
summary(m3)
```

```
## 
## Call:
## glm(formula = first_car_yield ~ gender + ethnicity + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.7091     0.3427  -2.069 0.038506 *  
## genderwoman                   0.3395     0.2923   1.161 0.245533    
## ethnicitywhite               -0.1701     0.2919  -0.583 0.560081    
## factor(location)2nd           0.7592     0.3762   2.018 0.043592 *  
## factor(location)bessborough   1.4789     0.3927   3.766 0.000166 ***
## factor(location)victoria      2.6626     0.4869   5.469 4.53e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 319.52  on 239  degrees of freedom
## Residual deviance: 275.44  on 234  degrees of freedom
## AIC: 287.44
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(coef(m3))
```

```
##                 (Intercept)                 genderwoman 
##                   0.4920680                   1.4042115 
##              ethnicitywhite         factor(location)2nd 
##                   0.8435989                   2.1365047 
## factor(location)bessborough    factor(location)victoria 
##                   4.3882738                  14.3335038
```

GENDER*ETHNICITY - LOCATION FIXED EFFECTS

``` r
m4 <- glm(first_car_yield ~ gender*ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m4)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                  -0.0906     0.385    -0.235 0.814       
## 2 genderwoman                  -1.08       0.436    -2.48  0.0131      
## 3 ethnicitywhite               -1.59       0.444    -3.59  0.000330    
## 4 factor(location)2nd           0.855      0.401     2.13  0.0329      
## 5 factor(location)bessborough   1.66       0.422     3.94  0.0000812   
## 6 factor(location)victoria      2.94       0.518     5.68  0.0000000138
## 7 genderwoman:ethnicitywhite    2.88       0.640     4.51  0.00000653
```

``` r
summary(m4)
```

```
## 
## Call:
## glm(formula = first_car_yield ~ gender * ethnicity + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -0.0906     0.3848  -0.235  0.81385    
## genderwoman                  -1.0809     0.4357  -2.481  0.01311 *  
## ethnicitywhite               -1.5943     0.4440  -3.591  0.00033 ***
## factor(location)2nd           0.8550     0.4009   2.133  0.03295 *  
## factor(location)bessborough   1.6643     0.4223   3.941 8.12e-05 ***
## factor(location)victoria      2.9411     0.5182   5.676 1.38e-08 ***
## genderwoman:ethnicitywhite    2.8843     0.6397   4.509 6.53e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 319.52  on 239  degrees of freedom
## Residual deviance: 253.13  on 233  degrees of freedom
## AIC: 267.13
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(coef(m4))
```

```
##                 (Intercept)                 genderwoman 
##                   0.9133814                   0.3393063 
##              ethnicitywhite         factor(location)2nd 
##                   0.2030472                   2.3513765 
## factor(location)bessborough    factor(location)victoria 
##                   5.2821285                  18.9371865 
##  genderwoman:ethnicitywhite 
##                  17.8910219
```

MEAN NUMBER OF CARS PASS BEFORE YIELD - DESCRIPTIVE STATS

``` r
data %>% 
  group_by(gender) %>% 
  summarise(
    n = n(),
    mean = mean(num_cars_pass_before_yield),
    sd = sd(num_cars_pass_before_yield),
    .groups = "drop"
  )
```

```
## # A tibble: 2 × 4
##   gender     n  mean    sd
##   <fct>  <int> <dbl> <dbl>
## 1 man      120 0.583 0.805
## 2 woman    120 0.65  1.16
```

``` r
data %>% 
  group_by(ethnicity) %>% 
  summarise(
    n = n(),
    mean = mean(num_cars_pass_before_yield),
    sd = sd(num_cars_pass_before_yield),
    .groups = "drop"
  )
```

```
## # A tibble: 2 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian       120 0.533 0.829
## 2 white       120 0.7   1.13
```

``` r
data %>% 
  group_by(ethnicity, gender) %>% 
  summarise(
    n = n(),
    mean = mean(num_cars_pass_before_yield),
    sd = sd(num_cars_pass_before_yield),
    .groups = "drop"
  )
```

```
## # A tibble: 4 × 5
##   ethnicity gender     n  mean    sd
##   <fct>     <fct>  <int> <dbl> <dbl>
## 1 asian     man       60 0.383 0.715
## 2 asian     woman     60 0.683 0.911
## 3 white     man       60 0.783 0.846
## 4 white     woman     60 0.617 1.37
```

MEAN NUMBER OF CARS PASS BEFORE YIELD - LINEAR REGRESSION
GENDER - LOCATION FIXED EFFECTS

``` r
m5 <- lm(num_cars_pass_before_yield ~ gender + factor(location),
         data = data)

tidy(m5)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   1.25       0.131     9.54  1.96e-18
## 2 genderwoman                   0.0667     0.117     0.569 5.70e- 1
## 3 factor(location)2nd          -0.667      0.166    -4.02  7.80e- 5
## 4 factor(location)bessborough  -0.850      0.166    -5.13  6.15e- 7
## 5 factor(location)victoria     -1.15       0.166    -6.94  3.86e-11
```

``` r
summary(m5)
```

```
## 
## Call:
## lm(formula = num_cars_pass_before_yield ~ gender + factor(location), 
##     data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3167 -0.4667 -0.1667  0.4167  4.6833 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  1.25000    0.13106   9.537  < 2e-16 ***
## genderwoman                  0.06667    0.11723   0.569     0.57    
## factor(location)2nd         -0.66667    0.16578  -4.021 7.80e-05 ***
## factor(location)bessborough -0.85000    0.16578  -5.127 6.15e-07 ***
## factor(location)victoria    -1.15000    0.16578  -6.937 3.86e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.908 on 235 degrees of freedom
## Multiple R-squared:  0.1815,	Adjusted R-squared:  0.1676 
## F-statistic: 13.03 on 4 and 235 DF,  p-value: 1.345e-09
```

ETHNICITY - LOCATION FIXED EFFECTS

``` r
m6 <- lm(num_cars_pass_before_yield ~ ethnicity + factor(location),
         data = data)

tidy(m6)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    1.2       0.131      9.19 2.16e-17
## 2 ethnicitywhite                 0.167     0.117      1.43 1.55e- 1
## 3 factor(location)2nd           -0.667     0.165     -4.04 7.36e- 5
## 4 factor(location)bessborough   -0.850     0.165     -5.15 5.62e- 7
## 5 factor(location)victoria      -1.15      0.165     -6.96 3.33e-11
```

``` r
summary(m6)
```

```
## 
## Call:
## lm(formula = num_cars_pass_before_yield ~ ethnicity + factor(location), 
##     data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3667 -0.5167 -0.2167  0.4667  4.6333 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.2000     0.1306   9.189  < 2e-16 ***
## ethnicitywhite                0.1667     0.1168   1.427    0.155    
## factor(location)2nd          -0.6667     0.1652  -4.036 7.36e-05 ***
## factor(location)bessborough  -0.8500     0.1652  -5.146 5.62e-07 ***
## factor(location)victoria     -1.1500     0.1652  -6.962 3.33e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9048 on 235 degrees of freedom
## Multiple R-squared:  0.1874,	Adjusted R-squared:  0.1736 
## F-statistic: 13.55 on 4 and 235 DF,  p-value: 5.914e-10
```

GENDER + ETHNICITY - LOCATION FIXED EFFECTS

``` r
m7 <- lm(num_cars_pass_before_yield ~ gender + ethnicity + factor(location),
         data = data)

tidy(m7)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   1.17       0.143     8.14  2.29e-14
## 2 genderwoman                   0.0667     0.117     0.570 5.69e- 1
## 3 ethnicitywhite                0.167      0.117     1.42  1.56e- 1
## 4 factor(location)2nd          -0.667      0.165    -4.03  7.54e- 5
## 5 factor(location)bessborough  -0.850      0.165    -5.14  5.84e- 7
## 6 factor(location)victoria     -1.15       0.165    -6.95  3.56e-11
```

``` r
summary(m7)
```

```
## 
## Call:
## lm(formula = num_cars_pass_before_yield ~ gender + ethnicity + 
##     factor(location), data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4000 -0.5000 -0.2333  0.4333  4.6000 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  1.16667    0.14326   8.144 2.29e-14 ***
## genderwoman                  0.06667    0.11697   0.570    0.569    
## ethnicitywhite               0.16667    0.11697   1.425    0.156    
## factor(location)2nd         -0.66667    0.16542  -4.030 7.54e-05 ***
## factor(location)bessborough -0.85000    0.16542  -5.138 5.84e-07 ***
## factor(location)victoria    -1.15000    0.16542  -6.952 3.56e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9061 on 234 degrees of freedom
## Multiple R-squared:  0.1885,	Adjusted R-squared:  0.1712 
## F-statistic: 10.87 on 5 and 234 DF,  p-value: 2.026e-09
```

GENDER*ETHNICITY - LOCATION FIXED EFFECTS

``` r
m8 <- lm(num_cars_pass_before_yield ~ gender*ethnicity + factor(location),
         data = data)

tidy(m8)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    1.05      0.154      6.83 7.34e-11
## 2 genderwoman                    0.300     0.164      1.83 6.92e- 2
## 3 ethnicitywhite                 0.400     0.164      2.43 1.57e- 2
## 4 factor(location)2nd           -0.667     0.164     -4.06 6.81e- 5
## 5 factor(location)bessborough   -0.850     0.164     -5.17 5.00e- 7
## 6 factor(location)victoria      -1.15      0.164     -7.00 2.76e-11
## 7 genderwoman:ethnicitywhite    -0.467     0.232     -2.01 4.58e- 2
```

``` r
summary(m8)
```

```
## 
## Call:
## lm(formula = num_cars_pass_before_yield ~ gender * ethnicity + 
##     factor(location), data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4500 -0.4500 -0.2000  0.3167  4.7167 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.0500     0.1537   6.829 7.34e-11 ***
## genderwoman                   0.3000     0.1644   1.825   0.0692 .  
## ethnicitywhite                0.4000     0.1644   2.434   0.0157 *  
## factor(location)2nd          -0.6667     0.1644  -4.056 6.81e-05 ***
## factor(location)bessborough  -0.8500     0.1644  -5.172 5.00e-07 ***
## factor(location)victoria     -1.1500     0.1644  -6.997 2.76e-11 ***
## genderwoman:ethnicitywhite   -0.4667     0.2324  -2.008   0.0458 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9002 on 233 degrees of freedom
## Multiple R-squared:  0.2023,	Adjusted R-squared:  0.1818 
## F-statistic: 9.851 on 6 and 233 DF,  p-value: 1.112e-09
```

TIME TO ENTER INTERSECTION - DESCRIPTIVE STATS

``` r
data %>% 
  group_by(gender) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street),
    .groups = "drop"
  )
```

```
## # A tibble: 2 × 4
##   gender     n  mean    sd
##   <fct>  <int> <dbl> <dbl>
## 1 man      120  5.29  1.93
## 2 woman    120  5.96  3.30
```

``` r
data %>% 
  group_by(ethnicity) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street),
    .groups = "drop"
  )
```

```
## # A tibble: 2 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian       120  4.75  1.20
## 2 white       120  6.49  3.45
```

``` r
data %>% 
  group_by(gender, ethnicity) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street),
    .groups = "drop"
  )
```

```
## # A tibble: 4 × 5
##   gender ethnicity     n  mean    sd
##   <fct>  <fct>     <int> <dbl> <dbl>
## 1 man    asian        60  4.27 0.924
## 2 man    white        60  6.30 2.13 
## 3 woman  asian        60  5.24 1.26 
## 4 woman  white        60  6.68 4.40
```

TIME TO ENTER INTERSECTION - LINEAR REGRESSION
GENDER - LOCATION FIXED EFFECTS 

``` r
m9 <- lm(time_to_cross_street ~ gender + factor(location),
         data = data)

tidy(m9)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    7.36      0.348     21.2  2.41e-56
## 2 genderwoman                    0.669     0.311      2.15 3.27e- 2
## 3 factor(location)2nd           -2.61      0.440     -5.92 1.12e- 8
## 4 factor(location)bessborough   -2.39      0.440     -5.42 1.46e- 7
## 5 factor(location)victoria      -3.31      0.440     -7.52 1.18e-12
```

``` r
summary(m9)
```

```
## 
## Call:
## lm(formula = time_to_cross_street ~ gender + factor(location), 
##     data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3228 -1.0260 -0.2792  0.7078 14.6472 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   7.3639     0.3480  21.160  < 2e-16 ***
## genderwoman                   0.6689     0.3113   2.149   0.0327 *  
## factor(location)2nd          -2.6067     0.4402  -5.922 1.12e-08 ***
## factor(location)bessborough  -2.3868     0.4402  -5.422 1.46e-07 ***
## factor(location)victoria     -3.3083     0.4402  -7.515 1.18e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.411 on 235 degrees of freedom
## Multiple R-squared:  0.2262,	Adjusted R-squared:  0.213 
## F-statistic: 17.17 on 4 and 235 DF,  p-value: 2.277e-12
```

ETHNICITY - LOCATION FIXED EFFECTS

``` r
m10 <- lm(time_to_cross_street ~ ethnicity + factor(location),
         data = data)

tidy(m10)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                     6.83     0.328     20.8  2.56e-55
## 2 ethnicitywhite                  1.74     0.293      5.93 1.07e- 8
## 3 factor(location)2nd            -2.61     0.415     -6.29 1.55e- 9
## 4 factor(location)bessborough    -2.39     0.415     -5.76 2.65e- 8
## 5 factor(location)victoria       -3.31     0.415     -7.98 6.42e-14
```

``` r
summary(m10)
```

```
## 
## Call:
## lm(formula = time_to_cross_street ~ ethnicity + factor(location), 
##     data = data)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.528 -1.375 -0.165  0.838 14.112 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.8290     0.3277  20.837  < 2e-16 ***
## ethnicitywhite                1.7386     0.2931   5.931 1.07e-08 ***
## factor(location)2nd          -2.6067     0.4146  -6.288 1.55e-09 ***
## factor(location)bessborough  -2.3868     0.4146  -5.758 2.65e-08 ***
## factor(location)victoria     -3.3083     0.4146  -7.980 6.42e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.271 on 235 degrees of freedom
## Multiple R-squared:  0.3137,	Adjusted R-squared:  0.302 
## F-statistic: 26.85 on 4 and 235 DF,  p-value: < 2.2e-16
```

GENDER + ETHNICITY - LOCATION FIXED EFFECTS

``` r
m11 <- lm(time_to_cross_street ~ gender + ethnicity + factor(location),
         data = data)

tidy(m11)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    6.49      0.356     18.3  6.91e-47
## 2 genderwoman                    0.669     0.290      2.30 2.22e- 2
## 3 ethnicitywhite                 1.74      0.290      5.98 8.07e- 9
## 4 factor(location)2nd           -2.61      0.411     -6.35 1.14e- 9
## 5 factor(location)bessborough   -2.39      0.411     -5.81 2.03e- 8
## 6 factor(location)victoria      -3.31      0.411     -8.05 4.09e-14
```

``` r
summary(m11)
```

```
## 
## Call:
## lm(formula = time_to_cross_street ~ gender + ethnicity + factor(location), 
##     data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1932 -1.3005 -0.0771  0.9372 13.7779 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.4946     0.3558  18.255  < 2e-16 ***
## genderwoman                   0.6689     0.2905   2.303   0.0222 *  
## ethnicitywhite                1.7386     0.2905   5.985 8.07e-09 ***
## factor(location)2nd          -2.6067     0.4108  -6.345 1.14e-09 ***
## factor(location)bessborough  -2.3868     0.4108  -5.810 2.03e-08 ***
## factor(location)victoria     -3.3083     0.4108  -8.053 4.09e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.25 on 234 degrees of freedom
## Multiple R-squared:  0.3289,	Adjusted R-squared:  0.3145 
## F-statistic: 22.93 on 5 and 234 DF,  p-value: < 2.2e-16
```

GENDER*ETHNICITY - LOCATION FIXED EFFECTS

``` r
m12 <- lm(time_to_cross_street ~ gender*ethnicity + factor(location),
         data = data)

tidy(m12)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    6.35      0.384     16.5  4.24e-41
## 2 genderwoman                    0.963     0.411      2.34 1.99e- 2
## 3 ethnicitywhite                 2.03      0.411      4.95 1.43e- 6
## 4 factor(location)2nd           -2.61      0.411     -6.35 1.14e- 9
## 5 factor(location)bessborough   -2.39      0.411     -5.81 2.04e- 8
## 6 factor(location)victoria      -3.31      0.411     -8.05 4.14e-14
## 7 genderwoman:ethnicitywhite    -0.589     0.581     -1.01 3.12e- 1
```

``` r
summary(m12)
```

```
## 
## Call:
## lm(formula = time_to_cross_street ~ gender * ethnicity + factor(location), 
##     data = data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.3403 -1.2607 -0.0653  0.9408 13.9250 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.3475     0.3843  16.519  < 2e-16 ***
## genderwoman                   0.9632     0.4108   2.345   0.0199 *  
## ethnicitywhite                2.0328     0.4108   4.949 1.43e-06 ***
## factor(location)2nd          -2.6067     0.4108  -6.345 1.14e-09 ***
## factor(location)bessborough  -2.3868     0.4108  -5.810 2.04e-08 ***
## factor(location)victoria     -3.3083     0.4108  -8.054 4.14e-14 ***
## genderwoman:ethnicitywhite   -0.5885     0.5810  -1.013   0.3121    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.25 on 233 degrees of freedom
## Multiple R-squared:  0.3318,	Adjusted R-squared:  0.3146 
## F-statistic: 19.29 on 6 and 233 DF,  p-value: < 2.2e-16
```

CAR PROCEED THROUGH INTERSECTION - DESCRIPTIVE STATS

``` r
data %>% 
  group_by(gender) %>% 
  count(did_car_proceed_before_across) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 6 × 4
## # Groups:   gender [2]
##   gender did_car_proceed_before_across     n percentage
##   <fct>  <fct>                         <int>      <dbl>
## 1 man    no                                6        5  
## 2 man    yes                             102       85  
## 3 man    <NA>                             12       10  
## 4 woman  no                               14       11.7
## 5 woman  yes                              97       80.8
## 6 woman  <NA>                              9        7.5
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(did_car_proceed_before_across) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 6 × 4
## # Groups:   ethnicity [2]
##   ethnicity did_car_proceed_before_across     n percentage
##   <fct>     <fct>                         <int>      <dbl>
## 1 asian     no                                7       5.83
## 2 asian     yes                             102      85   
## 3 asian     <NA>                             11       9.17
## 4 white     no                               13      10.8 
## 5 white     yes                              97      80.8 
## 6 white     <NA>                             10       8.33
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(did_car_proceed_before_across) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 12 × 5
## # Groups:   ethnicity, gender [4]
##    ethnicity gender did_car_proceed_before_across     n percentage
##    <fct>     <fct>  <fct>                         <int>      <dbl>
##  1 asian     man    no                                3       5   
##  2 asian     man    yes                              53      88.3 
##  3 asian     man    <NA>                              4       6.67
##  4 asian     woman  no                                4       6.67
##  5 asian     woman  yes                              49      81.7 
##  6 asian     woman  <NA>                              7      11.7 
##  7 white     man    no                                3       5   
##  8 white     man    yes                              49      81.7 
##  9 white     man    <NA>                              8      13.3 
## 10 white     woman  no                               10      16.7 
## 11 white     woman  yes                              48      80   
## 12 white     woman  <NA>                              2       3.33
```

``` r
  .groups = "drop"
```

CAR PROCEED THROUGH INTERSECTION - LOGISTIC REGRESSION 
GENDER - LOCATION FIXED EFFECTS

``` r
m13 <- glm(did_car_proceed_before_across ~ gender + factor(location),
           data = data,
           family = binomial()
           )

tidy(m13)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic    p.value
##   <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
## 1 (Intercept)                   2.70       0.594    4.55   0.00000537
## 2 genderwoman                  -0.904      0.512   -1.77   0.0773    
## 3 factor(location)2nd          -0.284      0.629   -0.452  0.651     
## 4 factor(location)bessborough   1.18       0.865    1.36   0.174     
## 5 factor(location)victoria      0.0281     0.645    0.0436 0.965
```

``` r
summary(m13)
```

```
## 
## Call:
## glm(formula = did_car_proceed_before_across ~ gender + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  2.70164    0.59379   4.550 5.37e-06 ***
## genderwoman                 -0.90412    0.51178  -1.767   0.0773 .  
## factor(location)2nd         -0.28416    0.62877  -0.452   0.6513    
## factor(location)bessborough  1.17526    0.86492   1.359   0.1742    
## factor(location)victoria     0.02811    0.64458   0.044   0.9652    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 133.85  on 218  degrees of freedom
## Residual deviance: 126.49  on 214  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 136.49
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m13))
```

```
##                 (Intercept)                 genderwoman 
##                  14.9041495                   0.4048974 
##         factor(location)2nd factor(location)bessborough 
##                   0.7526476                   3.2389832 
##    factor(location)victoria 
##                   1.0285116
```

ETHNICITY - LOCATION FIXED EFFECTS

``` r
m14 <- glm(did_car_proceed_before_across ~ ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m14)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic    p.value
##   <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
## 1 (Intercept)                   2.53       0.568    4.46   0.00000813
## 2 ethnicitywhite               -0.675      0.493   -1.37   0.171     
## 3 factor(location)2nd          -0.266      0.626   -0.424  0.671     
## 4 factor(location)bessborough   1.19       0.863    1.38   0.169     
## 5 factor(location)victoria      0.0459     0.642    0.0715 0.943
```

``` r
summary(m14)
```

```
## 
## Call:
## glm(formula = did_car_proceed_before_across ~ ethnicity + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  2.53414    0.56798   4.462 8.13e-06 ***
## ethnicitywhite              -0.67526    0.49345  -1.368    0.171    
## factor(location)2nd         -0.26581    0.62624  -0.424    0.671    
## factor(location)bessborough  1.18797    0.86333   1.376    0.169    
## factor(location)victoria     0.04592    0.64229   0.071    0.943    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 133.85  on 218  degrees of freedom
## Residual deviance: 127.91  on 214  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 137.91
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m14))
```

```
##                 (Intercept)              ethnicitywhite 
##                  12.6055692                   0.5090231 
##         factor(location)2nd factor(location)bessborough 
##                   0.7665862                   3.2804249 
##    factor(location)victoria 
##                   1.0469872
```

GENDER + ETHNICITY - LOCATION FIXED EFFECTS

``` r
m15 <- glm(did_car_proceed_before_across ~ gender + ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m15)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic    p.value
##   <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
## 1 (Intercept)                   3.05       0.670    4.56   0.00000512
## 2 genderwoman                  -0.883      0.514   -1.72   0.0858    
## 3 ethnicitywhite               -0.647      0.497   -1.30   0.193     
## 4 factor(location)2nd          -0.276      0.632   -0.436  0.663     
## 5 factor(location)bessborough   1.18       0.867    1.36   0.173     
## 6 factor(location)victoria      0.0244     0.648    0.0377 0.970
```

``` r
summary(m15)
```

```
## 
## Call:
## glm(formula = did_car_proceed_before_across ~ gender + ethnicity + 
##     factor(location), family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  3.05468    0.66993   4.560 5.12e-06 ***
## genderwoman                 -0.88282    0.51389  -1.718   0.0858 .  
## ethnicitywhite              -0.64697    0.49731  -1.301   0.1933    
## factor(location)2nd         -0.27569    0.63236  -0.436   0.6629    
## factor(location)bessborough  1.18260    0.86744   1.363   0.1728    
## factor(location)victoria     0.02444    0.64771   0.038   0.9699    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 133.85  on 218  degrees of freedom
## Residual deviance: 124.73  on 213  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 136.73
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m15))
```

```
##                 (Intercept)                 genderwoman 
##                  21.2143403                   0.4136137 
##              ethnicitywhite         factor(location)2nd 
##                   0.5236280                   0.7590474 
## factor(location)bessborough    factor(location)victoria 
##                   3.2628473                   1.0247388
```

GENDER*ETHNICITY - LOCATION FIXED EFFECTS

``` r
m16 <- glm(did_car_proceed_before_across ~ gender*ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m16)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   2.74       0.730    3.76   0.000172
## 2 genderwoman                  -0.376      0.794   -0.473  0.636   
## 3 ethnicitywhite               -0.0874     0.845   -0.103  0.918   
## 4 factor(location)2nd          -0.268      0.634   -0.423  0.673   
## 5 factor(location)bessborough   1.19       0.869    1.37   0.172   
## 6 factor(location)victoria      0.0171     0.650    0.0263 0.979   
## 7 genderwoman:ethnicitywhite   -0.853      1.05    -0.809  0.419
```

``` r
summary(m16)
```

```
## 
## Call:
## glm(formula = did_car_proceed_before_across ~ gender * ethnicity + 
##     factor(location), family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  2.74112    0.72958   3.757 0.000172 ***
## genderwoman                 -0.37556    0.79366  -0.473 0.636071    
## ethnicitywhite              -0.08738    0.84451  -0.103 0.917587    
## factor(location)2nd         -0.26819    0.63450  -0.423 0.672527    
## factor(location)bessborough  1.18645    0.86916   1.365 0.172237    
## factor(location)victoria     0.01709    0.64986   0.026 0.979019    
## genderwoman:ethnicitywhite  -0.85274    1.05470  -0.809 0.418795    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 133.85  on 218  degrees of freedom
## Residual deviance: 124.07  on 212  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 138.07
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m16))
```

```
##                 (Intercept)                 genderwoman 
##                  15.5043836                   0.6869039 
##              ethnicitywhite         factor(location)2nd 
##                   0.9163251                   0.7647626 
## factor(location)bessborough    factor(location)victoria 
##                   3.2754325                   1.0172375 
##  genderwoman:ethnicitywhite 
##                   0.4262466
```

CARS STOP CLOSE OR FAR BINNING

``` r
data$car_stop_close_or_far_bin <- ifelse(data$car_stop_close_or_far == "far", 1,
                                  ifelse(data$car_stop_close_or_far == "close", 0, NA))
```

CARS STOP CLOSE OR FAR - DESCRIPTIVE STATS

``` r
data %>% 
  group_by(gender) %>% 
  count(car_stop_close_or_far) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 6 × 4
## # Groups:   gender [2]
##   gender car_stop_close_or_far     n percentage
##   <fct>  <chr>                 <int>      <dbl>
## 1 man    close                     7       5.83
## 2 man    far                     101      84.2 
## 3 man    <NA>                     12      10   
## 4 woman  close                     8       6.67
## 5 woman  far                     103      85.8 
## 6 woman  <NA>                      9       7.5
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(car_stop_close_or_far) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 6 × 4
## # Groups:   ethnicity [2]
##   ethnicity car_stop_close_or_far     n percentage
##   <fct>     <chr>                 <int>      <dbl>
## 1 asian     close                    11       9.17
## 2 asian     far                      98      81.7 
## 3 asian     <NA>                     11       9.17
## 4 white     close                     4       3.33
## 5 white     far                     106      88.3 
## 6 white     <NA>                     10       8.33
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(car_stop_close_or_far) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 12 × 5
## # Groups:   ethnicity, gender [4]
##    ethnicity gender car_stop_close_or_far     n percentage
##    <fct>     <fct>  <chr>                 <int>      <dbl>
##  1 asian     man    close                     5       8.33
##  2 asian     man    far                      51      85   
##  3 asian     man    <NA>                      4       6.67
##  4 asian     woman  close                     6      10   
##  5 asian     woman  far                      47      78.3 
##  6 asian     woman  <NA>                      7      11.7 
##  7 white     man    close                     2       3.33
##  8 white     man    far                      50      83.3 
##  9 white     man    <NA>                      8      13.3 
## 10 white     woman  close                     2       3.33
## 11 white     woman  far                      56      93.3 
## 12 white     woman  <NA>                      2       3.33
```

``` r
  .groups = "drop"
```

CARS STOP CLOSE OR FAR - LOGISTIC REGRESSION
GENDER - LOCATION FIXED EFFECTS

``` r
m17 <- glm(car_stop_close_or_far_bin ~ gender + factor(location),
           data = data,
           family = binomial()
)

tidy(m17)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   1.66       0.486     3.41  0.000654
## 2 genderwoman                  -0.0917     0.549    -0.167 0.867   
## 3 factor(location)2nd           1.20       0.710     1.70  0.0899  
## 4 factor(location)bessborough   2.43       1.08      2.25  0.0244  
## 5 factor(location)victoria      1.33       0.708     1.88  0.0596
```

``` r
summary(m17)
```

```
## 
## Call:
## glm(formula = car_stop_close_or_far_bin ~ gender + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  1.65792    0.48645   3.408 0.000654 ***
## genderwoman                 -0.09174    0.54891  -0.167 0.867264    
## factor(location)2nd          1.20316    0.70953   1.696 0.089939 .  
## factor(location)bessborough  2.43202    1.08044   2.251 0.024388 *  
## factor(location)victoria     1.33334    0.70783   1.884 0.059607 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 109.38  on 218  degrees of freedom
## Residual deviance: 100.21  on 214  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 110.21
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m17))
```

```
##                 (Intercept)                 genderwoman 
##                   5.2483813                   0.9123412 
##         factor(location)2nd factor(location)bessborough 
##                   3.3306221                  11.3818228 
##    factor(location)victoria 
##                   3.7936800
```

ETHNICITY - LOCATION FIXED EFFECTS

``` r
m18 <- glm(car_stop_close_or_far_bin ~ ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m18)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                     1.15     0.440      2.61 0.00909
## 2 ethnicitywhite                  1.14     0.613      1.86 0.0633 
## 3 factor(location)2nd             1.23     0.718      1.71 0.0878 
## 4 factor(location)bessborough     2.48     1.09       2.28 0.0226 
## 5 factor(location)victoria        1.37     0.716      1.91 0.0560
```

``` r
summary(m18)
```

```
## 
## Call:
## glm(formula = car_stop_close_or_far_bin ~ ethnicity + factor(location), 
##     family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)   
## (Intercept)                   1.1470     0.4397   2.609  0.00909 **
## ethnicitywhite                1.1390     0.6132   1.857  0.06326 . 
## factor(location)2nd           1.2256     0.7180   1.707  0.08782 . 
## factor(location)bessborough   2.4761     1.0860   2.280  0.02261 * 
## factor(location)victoria      1.3685     0.7160   1.911  0.05597 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 109.379  on 218  degrees of freedom
## Residual deviance:  96.367  on 214  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 106.37
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m18))
```

```
##                 (Intercept)              ethnicitywhite 
##                    3.148652                    3.123645 
##         factor(location)2nd factor(location)bessborough 
##                    3.406303                   11.894337 
##    factor(location)victoria 
##                    3.929574
```

GENDER + ETHNICITY - LOCATION FIXED EFFECTS 

``` r
m19 <- glm(car_stop_close_or_far_bin ~ gender + ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m19)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                    1.22      0.526     2.32   0.0205
## 2 genderwoman                   -0.142     0.557    -0.255  0.799 
## 3 ethnicitywhite                 1.15      0.614     1.87   0.0621
## 4 factor(location)2nd            1.22      0.718     1.70   0.0886
## 5 factor(location)bessborough    2.47      1.09      2.28   0.0227
## 6 factor(location)victoria       1.37      0.716     1.91   0.0562
```

``` r
summary(m19)
```

```
## 
## Call:
## glm(formula = car_stop_close_or_far_bin ~ gender + ethnicity + 
##     factor(location), family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)                   1.2191     0.5263   2.317   0.0205 *
## genderwoman                  -0.1417     0.5568  -0.255   0.7991  
## ethnicitywhite                1.1457     0.6140   1.866   0.0621 .
## factor(location)2nd           1.2228     0.7181   1.703   0.0886 .
## factor(location)bessborough   2.4743     1.0862   2.278   0.0227 *
## factor(location)victoria      1.3677     0.7163   1.910   0.0562 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 109.379  on 218  degrees of freedom
## Residual deviance:  96.302  on 213  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 108.3
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m19))
```

```
##                 (Intercept)                 genderwoman 
##                   3.3842418                   0.8678586 
##              ethnicitywhite         factor(location)2nd 
##                   3.1445132                   3.3968473 
## factor(location)bessborough    factor(location)victoria 
##                  11.8732114                   3.9262930
```

GENDER*ETHNICITY - LOCATION FIXED EFFECTS 

``` r
m20 <- glm(car_stop_close_or_far_bin ~ gender*ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m20)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                    1.28      0.564     2.27   0.0232
## 2 genderwoman                   -0.260     0.661    -0.393  0.694 
## 3 ethnicitywhite                 0.928     0.877     1.06   0.290 
## 4 factor(location)2nd            1.22      0.719     1.70   0.0896
## 5 factor(location)bessborough    2.48      1.09      2.28   0.0226
## 6 factor(location)victoria       1.37      0.717     1.91   0.0555
## 7 genderwoman:ethnicitywhite     0.410     1.23      0.334  0.738
```

``` r
summary(m20)
```

```
## 
## Call:
## glm(formula = car_stop_close_or_far_bin ~ gender * ethnicity + 
##     factor(location), family = binomial(), data = data)
## 
## Coefficients:
##                             Estimate Std. Error z value Pr(>|z|)  
## (Intercept)                   1.2806     0.5642   2.270   0.0232 *
## genderwoman                  -0.2600     0.6609  -0.393   0.6940  
## ethnicitywhite                0.9281     0.8774   1.058   0.2902  
## factor(location)2nd           1.2199     0.7185   1.698   0.0896 .
## factor(location)bessborough   2.4766     1.0864   2.280   0.0226 *
## factor(location)victoria      1.3725     0.7167   1.915   0.0555 .
## genderwoman:ethnicitywhite    0.4098     1.2265   0.334   0.7383  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 109.379  on 218  degrees of freedom
## Residual deviance:  96.191  on 212  degrees of freedom
##   (21 observations deleted due to missingness)
## AIC: 110.19
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(coef(m20))
```

```
##                 (Intercept)                 genderwoman 
##                   3.5989542                   0.7710282 
##              ethnicitywhite         factor(location)2nd 
##                   2.5296422                   3.3867670 
## factor(location)bessborough    factor(location)victoria 
##                  11.9007476                   3.9450151 
##  genderwoman:ethnicitywhite 
##                   1.5064690
```

HISTOGRAMS
ETHNICITY

``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
         fill = ethnicity)) +
  geom_histogram(binwidth = 0.5) +
  labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  )
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

GENDER

``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
         fill = gender)) +
  geom_histogram(binwidth = 0.5) +
  labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  )
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

VISUALIZATIONS TIME BY RACE

``` r
data %>% 
  ggplot(aes(x = ethnicity,
         y = time_to_cross_street,
         fill = ethnicity)) +
  geom_boxplot() +
  labs(
    x = "Ethnicicty",
    y = "Time to Cross (s)"
  ) +
  scale_x_discrete(labels = c(
    "white" = "White",
    "asian" = "South Asian",
    "black" = "Black"
   )) +
  theme_minimal(
  )
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

TIME BY GENDER

``` r
data %>% 
  ggplot(aes(x = gender,
         y = time_to_cross_street,
         fill = gender)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    x = "Gender",
    y = "Time to Cross (s)"
  ) + 
  scale_x_discrete(labels = c(
    "man" = "Man",
    "woman" = "Woman"
   )) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

TIME TO CROSS THE STREET - VIOLIN PLOT WITH JITTER OVERLAY - DIVIDED BY GENDER

``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
             y = gender)) +
  geom_violin() +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    x = "Time to Cross (s)",
    y = "Gender"
  ) + 
    scale_y_discrete(labels = c(
    "man" = "Man",
    "woman" = "Woman"
   )) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

TIME TO CROSS THE STREET - VIOLIN PLOT WITH JITTER OVERLAY - DIVIDED BY ETHNICITY

``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
             y = ethnicity)) +
  geom_violin() +
  geom_jitter(show.legend = FALSE) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, linewidth = 1.2) +
  labs(
    x = "Time to Cross (s)",
    y = "Gender"
  ) + 
    scale_y_discrete(labels = c(
    "asian" = "South Asian",
    "white" = "White",
    "black" = "Black"
   )) +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

TIME TO CROSS - QQPLOT - BY ETHNICITY 

``` r
data %>% 
  ggplot(aes(sample = time_to_cross_street)) +
  geom_qq() +
  stat_qq_line() +
  facet_wrap(~ethnicity)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

TIME TO CROSS - QQPLOT - BY GENDER 

``` r
data %>% 
  ggplot(aes(sample = time_to_cross_street)) +
  geom_qq() +
  stat_qq_line() +
  facet_wrap(~gender)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

19TH REMOVED - TIME TO CROSS BY GENDER

``` r
data_19th_rm %>% 
  ggplot(aes(x = gender,
         y = time_to_cross_street,
         fill = gender)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    x = "Gender",
    y = "Time to Cross (s)"
  ) + 
  scale_x_discrete(labels = c(
    "man" = "Man",
    "woman" = "Woman"
  )) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-39-1.png)<!-- -->

TIME BY RACE AND GENDER - GROUPED BY LOCATION

``` r
data%>% 
  ggplot(aes(x = ethnicity,
             y = time_to_cross_street,
               fill = gender)) +
  geom_boxplot() +
  facet_wrap(~location) +
  labs(
    x = "Ethnicity",
    y = "Time to Cross (s)",
    fill = "Gender"
  ) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

CARS PASSED BY RACE AND GENDER

``` r
data %>% 
  ggplot(aes(x = ethnicity,
             y = num_cars_pass_before_yield,
             fill = gender)) +
  geom_col() +
  labs (
    x = "Ethnicity",
    y = "Number of Cars Passed",
    fill = "Gender"
  ) +
   scale_x_discrete(labels = c(
    "asian" = "South Asian",
    "white" = "White",
    "black" = "Black"
   )) + 
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

PROPORTION OF FIRST CAR YIELD RACE VISUAL

``` r
data %>% 
  ggplot(aes(x = ethnicity,
             fill = first_car_yield)
         ) +
  geom_bar(position = "fill") +
  labs (
    x = "Ethnicity",
    y = "Proportion",
    fill = "Did the First Car Yield"
  ) +
   scale_x_discrete(labels = c(
    "asian" = "South Asian",
    "white" = "White",
    "black" = "Black"
   )) + 
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

PROPORTION OF FIRST CAR YIELD GENDER VISUAL

``` r
data %>% 
  ggplot(aes(x = gender,
             fill = first_car_yield)
         ) +
  geom_bar(position = "fill") +
  labs (
    x = "Gender",
    y = "Proportion",
    fill = "Did the First Car Yield"
  ) +
  scale_x_discrete(labels = c(
    "man" = "Man",
    "woman" = "Woman"
  )) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

ODDS FIRST CAR YIELD

``` r
or1 <- tidy(m4, conf.int = TRUE, exponentiate = TRUE)

ggplot(or1[-1,], aes(x = estimate,
                    y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             colour = "red") +
  scale_x_log10() +
  scale_y_discrete(labels = c(
    "genderman" = "Man (vs Woman)",
    "genderwoman" = "Woman (vs Man)",
    "ethnicityasian" = "South Asian",
    "ethnicityblack" = "Black",
    "ethnicitywhite" = "White",
    "genderwoman:ethnicitywhite" = "White & Woman",
    "factor(location)victoria" = "Victoria",
    "factor(location)bessborough" = "Bessborough",
    "factor(location)2nd" = "2nd",
    "factor(location)19th" = "19th")) +
  labs(
    x = "Odds Ratio (95% CI)",
    y = "",
    title = "Logistic Regression Results"
  ) +
  theme_minimal()
```

```
## Warning: `geom_errorbarh()` was deprecated in ggplot2 4.0.0.
## ℹ Please use the `orientation` argument of `geom_errorbar()` instead.
## This warning is displayed once per session.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## `height` was translated to `width`.
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

ODDS CAR PROCEEDED

``` r
or2 <- tidy(m16, conf.int = TRUE, exponentiate = TRUE)

ggplot(or2[-1,], aes(x = estimate,
                    y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             colour = "red") +
  scale_x_log10() +
  scale_y_discrete(labels = c(
    "genderman" = "Male (vs Female)",
    "genderwoman" = "Female (vs Male)",
    "ethnicityasian" = "South Asian",
    "ethnicityblack" = "Black",
    "ethnicitywhite" = "White",
    "factor(location)victoria" = "Victoria",
    "factor(location)bessborough" = "Bessborough",
    "factor(location)2nd" = "2nd",
    "factor(location)19th" = "19th",
    "genderwoman:ethnicitywhite" = "White & Woman")) +
  labs(
    x = "Odds Ratio (95% CI)",
    y = "",
    title = "Logistic Regression Results"
  ) +
  theme_minimal()
```

```
## `height` was translated to `width`.
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

ODDS CAR CLOSE//FAR

``` r
or3 <- tidy(m20, conf.int = TRUE, exponentiate = TRUE)

ggplot(or3[-1,], aes(x = estimate,
                    y = reorder(term, estimate))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high),
                 height = 0.2) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             colour = "red") +
  scale_x_log10() +
  scale_y_discrete(labels = c(
    "genderman" = "Male (vs Female)",
    "genderwoman" = "Female (vs Male)",
    "ethnicityasian" = "South Asian",
    "ethnicityblack" = "Black",
    "ethnicitywhite" = "White",
    "factor(location)victoria" = "Victoria",
    "factor(location)bessborough" = "Bessborough",
    "factor(location)2nd" = "2nd",
    "factor(location)19th" = "19th",
    "genderwoman:ethnicitywhite" = "White & Woman")) +
  labs(
    x = "Odds Ratio (95% CI)",
    y = "",
    title = "Logistic Regression Results"
  ) +
  theme_minimal()
```

```
## `height` was translated to `width`.
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-46-1.png)<!-- -->
