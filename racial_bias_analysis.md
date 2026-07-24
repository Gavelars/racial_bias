---
title: "Racial Bias Analysis"
output: 
  html_document:
    keep_md: yes
---

# 1. Load Packages

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
library(gtsummary)
```

```
## Warning: package 'gtsummary' was built under R version 4.6.1
```

``` r
library(broom.helpers)
```

```
## Warning: package 'broom.helpers' was built under R version 4.6.1
```

```
## 
## Attaching package: 'broom.helpers'
## 
## The following objects are masked from 'package:gtsummary':
## 
##     all_categorical, all_continuous, all_contrasts, all_dichotomous,
##     all_interaction, all_intercepts
```

``` r
library(gt)
```

```
## Warning: package 'gt' was built under R version 4.6.1
```

# 2. Load data - Remove invalid trials - Trials with unaffiliated pedestrians

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
## Rows: 384 Columns: 15
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (10): ethnicity, gender, location, date, time_of_day, close_side_first_c...
## dbl  (5): order, trial_number, close_side_num_cars_pass_before_yield, num_ca...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
data <- data_file %>% drop_na(valid_trial)

data_19th_rm <- data %>% filter(location != "19th")
```

# 3. General descriptive statistics

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
## 1   341      5.72    2.67     2.07     22.7     0.619    1.09
```

# 4. Frequency tables 

``` r
data %>% count(ethnicity)
```

```
## # A tibble: 3 × 2
##   ethnicity     n
##   <fct>     <int>
## 1 asian       120
## 2 black       101
## 3 white       120
```

``` r
data %>% count(gender)
```

```
## # A tibble: 2 × 2
##   gender     n
##   <fct>  <int>
## 1 man      180
## 2 woman    161
```

``` r
data %>% count(location)
```

```
## # A tibble: 4 × 2
##   location        n
##   <fct>       <int>
## 1 19th           82
## 2 2nd            90
## 3 bessborough    90
## 4 victoria       79
```

``` r
data %>% count(first_car_yield)
```

```
## # A tibble: 2 × 2
##   first_car_yield     n
##   <fct>           <int>
## 1 no                129
## 2 yes               212
```

``` r
data %>% count(did_car_proceed_before_across)
```

```
## # A tibble: 3 × 2
##   did_car_proceed_before_across     n
##   <fct>                         <int>
## 1 no                               31
## 2 yes                             284
## 3 <NA>                             26
```
# 5. First car yield
# 5a. First car yield - Descriptive stats

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
## 1 man    no                 81       45  
## 2 man    yes                99       55  
## 3 woman  no                 48       29.8
## 4 woman  yes               113       70.2
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(first_car_yield) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 6 × 4
## # Groups:   ethnicity [3]
##   ethnicity first_car_yield     n percentage
##   <fct>     <fct>           <int>      <dbl>
## 1 asian     no                 44       36.7
## 2 asian     yes                76       63.3
## 3 black     no                 37       36.6
## 4 black     yes                64       63.4
## 5 white     no                 48       40  
## 6 white     yes                72       60
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(first_car_yield) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 12 × 5
## # Groups:   ethnicity, gender [6]
##    ethnicity gender first_car_yield     n percentage
##    <fct>     <fct>  <fct>           <int>      <dbl>
##  1 asian     man    no                 16       26.7
##  2 asian     man    yes                44       73.3
##  3 asian     woman  no                 28       46.7
##  4 asian     woman  yes                32       53.3
##  5 black     man    no                 31       51.7
##  6 black     man    yes                29       48.3
##  7 black     woman  no                  6       14.6
##  8 black     woman  yes                35       85.4
##  9 white     man    no                 34       56.7
## 10 white     man    yes                26       43.3
## 11 white     woman  no                 14       23.3
## 12 white     woman  yes                46       76.7
```

``` r
  .groups = "drop"
```

# 5b. First car yield - Logistic regression 
# 5b1. Gender - Location fixed effects

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
## 1 (Intercept)                   -0.760     0.259     -2.94 0.00331     
## 2 genderwoman                    0.777     0.246      3.16 0.00158     
## 3 factor(location)2nd            0.510     0.315      1.62 0.105       
## 4 factor(location)bessborough    1.25      0.328      3.81 0.000138    
## 5 factor(location)victoria       2.30      0.402      5.71 0.0000000111
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
## (Intercept)                  -0.7596     0.2586  -2.938 0.003306 ** 
## genderwoman                   0.7766     0.2457   3.160 0.001576 ** 
## factor(location)2nd           0.5099     0.3145   1.621 0.104934    
## factor(location)bessborough   1.2490     0.3277   3.811 0.000138 ***
## factor(location)victoria      2.2992     0.4025   5.713 1.11e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 452.32  on 340  degrees of freedom
## Residual deviance: 398.36  on 336  degrees of freedom
## AIC: 408.36
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(cbind(OR = coef(m1), confint(m1)))
```

```
## Waiting for profiling to be done...
```

```
##                                    OR     2.5 %     97.5 %
## (Intercept)                 0.4678572 0.2783944  0.7698593
## genderwoman                 2.1739772 1.3491075  3.5405830
## factor(location)2nd         1.6651785 0.9015432  3.1011339
## factor(location)bessborough 3.4866971 1.8506339  6.7068823
## factor(location)victoria    9.9657915 4.6709178 22.8370623
```

``` r
ci1 <- exp(confint(m1))
```

```
## Waiting for profiling to be done...
```

# 5b2. Ethnicity - Location fixed effects

``` r
m2 <- glm(first_car_yield ~ ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m2)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                  -0.348      0.275    -1.26  0.207       
## 2 ethnicityblack                0.0403     0.298     0.135 0.892       
## 3 ethnicitywhite               -0.162      0.285    -0.569 0.570       
## 4 factor(location)2nd           0.522      0.309     1.69  0.0917      
## 5 factor(location)bessborough   1.24       0.322     3.84  0.000125    
## 6 factor(location)victoria      2.22       0.396     5.62  0.0000000194
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
## (Intercept)                 -0.34767    0.27531  -1.263 0.206648    
## ethnicityblack               0.04034    0.29849   0.135 0.892487    
## ethnicitywhite              -0.16193    0.28473  -0.569 0.569558    
## factor(location)2nd          0.52195    0.30947   1.687 0.091684 .  
## factor(location)bessborough  1.23700    0.32248   3.836 0.000125 ***
## factor(location)victoria     2.22385    0.39591   5.617 1.94e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 452.32  on 340  degrees of freedom
## Residual deviance: 408.08  on 335  degrees of freedom
## AIC: 420.08
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(cbind(OR = coef(m2), confint(m2)))
```

```
## Waiting for profiling to be done...
```

```
##                                    OR     2.5 %    97.5 %
## (Intercept)                 0.7063338 0.4089071  1.208026
## ethnicityblack              1.0411686 0.5799115  1.873216
## ethnicitywhite              0.8505027 0.4855888  1.485811
## factor(location)2nd         1.6853085 0.9216268  3.107799
## factor(location)bessborough 3.4452751 1.8469960  6.556634
## factor(location)victoria    9.2428049 4.3864464 20.910250
```

``` r
ci2 <- exp(confint(m2))
```

```
## Waiting for profiling to be done...
```

# 5b3. Gender + Ethnicity - Location fixed effects

``` r
m3 <- glm(first_car_yield ~ gender + ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m3)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                   -0.734     0.306    -2.40  0.0166      
## 2 genderwoman                    0.791     0.247     3.20  0.00135     
## 3 ethnicityblack                 0.106     0.304     0.348 0.728       
## 4 ethnicitywhite                -0.167     0.290    -0.578 0.563       
## 5 factor(location)2nd            0.498     0.315     1.58  0.114       
## 6 factor(location)bessborough    1.24      0.328     3.78  0.000159    
## 7 factor(location)victoria       2.31      0.403     5.73  0.0000000103
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
## (Intercept)                  -0.7340     0.3064  -2.395 0.016601 *  
## genderwoman                   0.7912     0.2469   3.205 0.001352 ** 
## ethnicityblack                0.1057     0.3040   0.348 0.728130    
## ethnicitywhite               -0.1675     0.2896  -0.578 0.563056    
## factor(location)2nd           0.4982     0.3153   1.580 0.114149    
## factor(location)bessborough   1.2402     0.3284   3.777 0.000159 ***
## factor(location)victoria      2.3061     0.4027   5.726 1.03e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 452.32  on 340  degrees of freedom
## Residual deviance: 397.51  on 334  degrees of freedom
## AIC: 411.51
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(cbind(OR = coef(m3), confint(m3)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR     2.5 %     97.5 %
## (Intercept)                  0.4799702 0.2603018  0.8687516
## genderwoman                  2.2061368 1.3661517  3.6019123
## ethnicityblack               1.1114666 0.6128268  2.0231773
## ethnicitywhite               0.8458107 0.4782789  1.4916400
## factor(location)2nd          1.6457223 0.8894204  3.0695594
## factor(location)bessborough  3.4561610 1.8319703  6.6561253
## factor(location)victoria    10.0355514 4.7010862 23.0086577
```

``` r
ci3 <- exp(confint(m3))
```

```
## Waiting for profiling to be done...
```

# 5b4. Gender*Ethnicity - Location fixed effects

``` r
m4 <- glm(first_car_yield ~ gender*ethnicity + factor(location),
          data = data,
          family = binomial())

tidy(m4)
```

```
## # A tibble: 9 × 5
##   term                        estimate std.error statistic       p.value
##   <chr>                          <dbl>     <dbl>     <dbl>         <dbl>
## 1 (Intercept)                    0.142     0.362     0.393 0.694        
## 2 genderwoman                   -1.03      0.424    -2.43  0.0150       
## 3 ethnicityblack                -1.27      0.426    -2.99  0.00276      
## 4 ethnicitywhite                -1.52      0.430    -3.54  0.000400     
## 5 factor(location)2nd            0.484     0.335     1.44  0.149        
## 6 factor(location)bessborough    1.32      0.349     3.77  0.000161     
## 7 factor(location)victoria       2.52      0.421     5.98  0.00000000229
## 8 genderwoman:ethnicityblack     3.19      0.690     4.62  0.00000387   
## 9 genderwoman:ethnicitywhite     2.75      0.616     4.47  0.00000777
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
## (Intercept)                   0.1423     0.3618   0.393 0.694071    
## genderwoman                  -1.0309     0.4237  -2.433 0.014978 *  
## ethnicityblack               -1.2744     0.4257  -2.994 0.002757 ** 
## ethnicitywhite               -1.5214     0.4298  -3.540 0.000400 ***
## factor(location)2nd           0.4843     0.3354   1.444 0.148814    
## factor(location)bessborough   1.3171     0.3491   3.773 0.000161 ***
## factor(location)victoria      2.5182     0.4214   5.976 2.29e-09 ***
## genderwoman:ethnicityblack    3.1864     0.6899   4.618 3.87e-06 ***
## genderwoman:ethnicitywhite    2.7527     0.6156   4.471 7.77e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 452.32  on 340  degrees of freedom
## Residual deviance: 366.36  on 332  degrees of freedom
## AIC: 384.36
## 
## Number of Fisher Scoring iterations: 4
```

``` r
exp(cbind(OR = coef(m4), confint(m4)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR      2.5 %     97.5 %
## (Intercept)                  1.1529385 0.57001397  2.3769449
## genderwoman                  0.3566921 0.15274135  0.8090178
## ethnicityblack               0.2795900 0.11895652  0.6348571
## ethnicitywhite               0.2184131 0.09195026  0.4985055
## factor(location)2nd          1.6229618 0.84314164  3.1504457
## factor(location)bessborough  3.7326346 1.90344484  7.5047533
## factor(location)victoria    12.4068466 5.60935918 29.5120903
## genderwoman:ethnicityblack  24.2001696 6.51880112 98.9155951
## genderwoman:ethnicitywhite  15.6842895 4.79404768 53.8552413
```

``` r
ci4 <- exp(confint(m4))
```

```
## Waiting for profiling to be done...
```

# 5c1. Combined models - Odd Ratio with 95% CI - gtsummary

``` r
tbl1 <- tbl_regression(
  m1,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl2 <- tbl_regression(
  m2,
  exponentiate = TRUE,
  label = list(
    ethnicity ~ "Racialization",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl3 <- tbl_regression(
  m3,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl4 <- tbl_regression(
  m4,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          label == "woman * white" ~ "Women * White",
          label == "woman * black" ~ "Women * Black",
          TRUE ~ label)
    ))
```

# 5c2. Table 1 - First car yield combined models table

``` r
table1 <- tbl_merge(
  tbls = list(tbl1, tbl2, tbl3, tbl4),
  tab_spanner = c(
    "Gender",
    "Racialization",
    "Gender and Racialization",
    "Gender and Racialization Interaction"
  )
) %>% 
  modify_table_body(
    ~ .x %>% 
      mutate(
        row_order = case_when(
          variable == "gender" ~ 1,
          variable == "ethnicity" ~ 2,
          variable == "gender:ethnicity" ~ 3,
          variable == "location" ~ 4,
          TRUE ~ 99
        )
      ) %>% 
      arrange(row_order, row_type != "label") %>% 
      select(-row_order)
  )
```

```
## The number rows in the tables to be merged do not match, which may result in
## rows appearing out of order.
## ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
##   `quiet=TRUE` to silence message.
```

``` r
as_gt(table1) %>% 
  gtsave(
    filename = "table1.png",
    vwidth = 2200,
    zoom = 2
  )
```

```
## file:///C:/Users/KADEGA~1/AppData/Local/Temp/RtmpKYF5jM/file290462fb2fd8.html screenshot completed
```

# 6. Mean number of cars before yield
# 6a. Mean number of cars before yield - Descriptive stats

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
## 1 man      180 0.7    1.13
## 2 woman    161 0.528  1.04
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
## # A tibble: 3 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian       120 0.533 0.829
## 2 black       101 0.624 1.30 
## 3 white       120 0.7   1.13
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
## # A tibble: 6 × 5
##   ethnicity gender     n  mean    sd
##   <fct>     <fct>  <int> <dbl> <dbl>
## 1 asian     man       60 0.383 0.715
## 2 asian     woman     60 0.683 0.911
## 3 black     man       60 0.933 1.58 
## 4 black     woman     41 0.171 0.442
## 5 white     man       60 0.783 0.846
## 6 white     woman     60 0.617 1.37
```

# 6b. Mean number of cars before yield - Linear regression 
# 6b1. Gender - Location fixed effects

``` r
m5 <- lm(num_cars_pass_before_yield ~ gender + factor(location),
         data = data)

tidy(m5)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    1.41      0.122     11.6  3.14e-26
## 2 genderwoman                   -0.169     0.109     -1.55 1.22e- 1
## 3 factor(location)2nd           -0.710     0.154     -4.62 5.59e- 6
## 4 factor(location)bessborough   -0.932     0.154     -6.06 3.65e- 9
## 5 factor(location)victoria      -1.18      0.159     -7.44 8.56e-13
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
## -1.4057 -0.4736 -0.2248  0.4736  7.5943 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.4057     0.1217  11.555  < 2e-16 ***
## genderwoman                  -0.1694     0.1094  -1.549    0.122    
## factor(location)2nd          -0.7099     0.1538  -4.616 5.59e-06 ***
## factor(location)bessborough  -0.9321     0.1538  -6.060 3.65e-09 ***
## factor(location)victoria     -1.1809     0.1587  -7.439 8.56e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.007 on 336 degrees of freedom
## Multiple R-squared:  0.1619,	Adjusted R-squared:  0.1519 
## F-statistic: 16.23 on 4 and 336 DF,  p-value: 3.657e-12
```

# 6b2. Ethnicity - Location fixed effects

``` r
m6 <- lm(num_cars_pass_before_yield ~ ethnicity + factor(location),
         data = data)

tidy(m6)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   1.24       0.133     9.34  1.42e-18
## 2 ethnicityblack                0.0957     0.137     0.700 4.85e- 1
## 3 ethnicitywhite                0.167      0.130     1.28  2.02e- 1
## 4 factor(location)2nd          -0.719      0.154    -4.66  4.59e- 6
## 5 factor(location)bessborough  -0.941      0.154    -6.10  2.94e- 9
## 6 factor(location)victoria     -1.18       0.159    -7.39  1.15e-12
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
## -1.4093 -0.4681 -0.2323  0.4763  7.6617 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  1.24263    0.13309   9.337  < 2e-16 ***
## ethnicityblack               0.09567    0.13673   0.700    0.485    
## ethnicitywhite               0.16667    0.13032   1.279    0.202    
## factor(location)2nd         -0.71896    0.15431  -4.659 4.59e-06 ***
## factor(location)bessborough -0.94118    0.15431  -6.099 2.94e-09 ***
## factor(location)victoria    -1.17703    0.15918  -7.394 1.15e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.009 on 335 degrees of freedom
## Multiple R-squared:  0.1601,	Adjusted R-squared:  0.1475 
## F-statistic: 12.77 on 5 and 335 DF,  p-value: 2.269e-11
```

# 6b3. Gender + Ethnicity - Location fixed effects

``` r
m7 <- lm(num_cars_pass_before_yield ~ gender + ethnicity + factor(location),
         data = data)

tidy(m7)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   1.32       0.143     9.27  2.41e-18
## 2 genderwoman                  -0.170      0.110    -1.54  1.23e- 1
## 3 ethnicityblack                0.0786     0.137     0.574 5.66e- 1
## 4 ethnicitywhite                0.167      0.130     1.28  2.01e- 1
## 5 factor(location)2nd          -0.710      0.154    -4.60  5.89e- 6
## 6 factor(location)bessborough  -0.932      0.154    -6.05  3.97e- 9
## 7 factor(location)victoria     -1.18       0.159    -7.43  8.94e-13
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
## -1.4905 -0.4706 -0.2214  0.3889  7.5976 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  1.32385    0.14285   9.268  < 2e-16 ***
## genderwoman                 -0.16985    0.10995  -1.545    0.123    
## ethnicityblack               0.07856    0.13690   0.574    0.566    
## ethnicitywhite               0.16667    0.13006   1.282    0.201    
## factor(location)2nd         -0.70956    0.15411  -4.604 5.89e-06 ***
## factor(location)bessborough -0.93178    0.15411  -6.046 3.97e-09 ***
## factor(location)victoria    -1.18104    0.15887  -7.434 8.94e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.007 on 334 degrees of freedom
## Multiple R-squared:  0.166,	Adjusted R-squared:  0.151 
## F-statistic: 11.08 on 6 and 334 DF,  p-value: 2.831e-11
```

# 6b4. Gender*Ethnicity - Location fixed effects

``` r
m8 <- lm(num_cars_pass_before_yield ~ gender*ethnicity + factor(location),
         data = data)

tidy(m8)
```

```
## # A tibble: 9 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    1.08      0.158      6.79 5.17e-11
## 2 genderwoman                    0.300     0.180      1.66 9.72e- 2
## 3 ethnicityblack                 0.550     0.180      3.05 2.47e- 3
## 4 ethnicitywhite                 0.400     0.180      2.22 2.72e- 2
## 5 factor(location)2nd           -0.677     0.151     -4.47 1.07e- 5
## 6 factor(location)bessborough   -0.899     0.151     -5.94 7.28e- 9
## 7 factor(location)victoria      -1.20      0.156     -7.67 1.94e-13
## 8 genderwoman:ethnicityblack    -1.06      0.271     -3.92 1.06e- 4
## 9 genderwoman:ethnicitywhite    -0.467     0.255     -1.83 6.82e- 2
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
## -1.6260 -0.4760 -0.1810  0.3007  7.3740 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   1.0760     0.1585   6.791 5.17e-11 ***
## genderwoman                   0.3000     0.1803   1.663 0.097157 .  
## ethnicityblack                0.5500     0.1803   3.050 0.002475 ** 
## ethnicitywhite                0.4000     0.1803   2.218 0.027232 *  
## factor(location)2nd          -0.6768     0.1514  -4.470 1.07e-05 ***
## factor(location)bessborough  -0.8990     0.1514  -5.938 7.28e-09 ***
## factor(location)victoria     -1.1950     0.1558  -7.669 1.94e-13 ***
## genderwoman:ethnicityblack   -1.0622     0.2707  -3.925 0.000106 ***
## genderwoman:ethnicitywhite   -0.4667     0.2550  -1.830 0.068184 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9878 on 332 degrees of freedom
## Multiple R-squared:  0.203,	Adjusted R-squared:  0.1838 
## F-statistic: 10.57 on 8 and 332 DF,  p-value: 3.119e-13
```

# 6c1. Combined models - gtsummary

``` r
tbl5 <- tbl_regression(
  m5,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location"
    )
  )%>%
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl6 <- tbl_regression(
  m6,
  intercept = TRUE,
  label = list(
    ethnicity ~ "Racialization",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>%
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl7 <- tbl_regression(
  m7,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl8 <- tbl_regression(
  m8,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          label == "woman * white" ~ "Women * White",
          label == "woman * black" ~ "Women * Black",
          TRUE ~ label)
    ))
```

# 6c2. Table 2 - Mean number of cars before yield combined models table

``` r
table2 <- tbl_merge(
  tbls = list(tbl5, tbl6, tbl7, tbl8),
  tab_spanner = c(
    "Gender",
    "Racialization",
    "Gender and Racialization",
    "Gender and Racialization Interaction"
  )
) %>%
  modify_table_body(
    ~ .x %>% 
      mutate(
        row_order = case_when(
          variable == "(Intercept)" ~ 1,
          variable == "gender" ~ 2,
          variable == "ethnicity" ~ 3,
          variable == "gender:ethnicity" ~ 4,
          variable == "location" ~ 5,
          TRUE ~ 99
        )
      ) %>% 
      arrange(row_order, row_type != "label") %>% 
      select(-row_order)
  ) %>% 
  remove_abbreviation("CI = Confidence Interval")
```

```
## The number rows in the tables to be merged do not match, which may result in
## rows appearing out of order.
## ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
##   `quiet=TRUE` to silence message.
```

``` r
as_gt(table2) %>% 
  gtsave(
    filename = "table2.png",
    vwidth = 2200,
    zoom = 2
  )
```

```
## file:///C:/Users/KADEGA~1/AppData/Local/Temp/RtmpKYF5jM/file290468db1223.html screenshot completed
```

# 7. Mean time to enter intersection
# 7a. Mean time to enter intersection - Descriptive stats

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
## 1 man      180  5.59  2.36
## 2 woman    161  5.87  2.98
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
## # A tibble: 3 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian       120  4.75  1.20
## 2 black       101  5.95  2.57
## 3 white       120  6.49  3.45
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
## # A tibble: 6 × 5
##   gender ethnicity     n  mean    sd
##   <fct>  <fct>     <int> <dbl> <dbl>
## 1 man    asian        60  4.27 0.924
## 2 man    black        60  6.19 2.99 
## 3 man    white        60  6.30 2.13 
## 4 woman  asian        60  5.24 1.26 
## 5 woman  black        41  5.61 1.77 
## 6 woman  white        60  6.68 4.40
```

# 7b. Mean time to enter intersection - Linear regression
# 7b1. Gender - Location fixed effects

``` r
m9 <- lm(time_to_cross_street ~ gender + factor(location),
         data = data)

tidy(m9)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    7.68      0.286     26.8  1.41e-85
## 2 genderwoman                    0.289     0.257      1.12 2.63e- 1
## 3 factor(location)2nd           -2.66      0.362     -7.35 1.50e-12
## 4 factor(location)bessborough   -2.21      0.362     -6.12 2.62e- 9
## 5 factor(location)victoria      -3.48      0.373     -9.34 1.41e-18
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
## -4.2559 -1.1330 -0.1959  0.8387 14.7141 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   7.6772     0.2861  26.838  < 2e-16 ***
## genderwoman                   0.2887     0.2573   1.122    0.263    
## factor(location)2nd          -2.6589     0.3616  -7.352 1.50e-12 ***
## factor(location)bessborough  -2.2129     0.3616  -6.119 2.62e-09 ***
## factor(location)victoria     -3.4846     0.3733  -9.336  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.367 on 336 degrees of freedom
## Multiple R-squared:  0.2257,	Adjusted R-squared:  0.2165 
## F-statistic: 24.49 on 4 and 336 DF,  p-value: < 2.2e-16
```

# 7b2. Ethnicity - Location fixed effects

``` r
m10 <- lm(time_to_cross_street ~ ethnicity + factor(location),
         data = data)

tidy(m10)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                     6.85     0.297     23.0  4.39e-71
## 2 ethnicityblack                  1.21     0.305      3.98 8.62e- 5
## 3 ethnicitywhite                  1.74     0.291      5.98 5.81e- 9
## 4 factor(location)2nd            -2.67     0.344     -7.74 1.15e-13
## 5 factor(location)bessborough    -2.22     0.344     -6.45 3.93e-10
## 6 factor(location)victoria       -3.48     0.355     -9.80 4.26e-20
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
##     Min      1Q  Median      3Q     Max 
## -4.5445 -1.2579 -0.2019  0.8655 14.0955 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.8459     0.2970  23.047  < 2e-16 ***
## ethnicityblack                1.2131     0.3052   3.975 8.62e-05 ***
## ethnicitywhite                1.7386     0.2909   5.977 5.81e-09 ***
## factor(location)2nd          -2.6671     0.3444  -7.744 1.15e-13 ***
## factor(location)bessborough  -2.2211     0.3444  -6.449 3.93e-10 ***
## factor(location)victoria     -3.4811     0.3553  -9.798  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.253 on 335 degrees of freedom
## Multiple R-squared:  0.3008,	Adjusted R-squared:  0.2904 
## F-statistic: 28.83 on 5 and 335 DF,  p-value: < 2.2e-16
```

# 7b3. Gender + Ethnicity - Location fixed effects

``` r
m11 <- lm(time_to_cross_street ~ gender + ethnicity + factor(location),
         data = data)

tidy(m11)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    6.69      0.319     21.0  6.72e-63
## 2 genderwoman                    0.320     0.246      1.30 1.93e- 1
## 3 ethnicityblack                 1.25      0.306      4.07 5.83e- 5
## 4 ethnicitywhite                 1.74      0.291      5.98 5.63e- 9
## 5 factor(location)2nd           -2.68      0.344     -7.80 8.10e-14
## 6 factor(location)bessborough   -2.24      0.344     -6.50 2.88e-10
## 7 factor(location)victoria      -3.47      0.355     -9.79 4.76e-20
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
## -4.3914 -1.2767 -0.1148  0.8686 13.9284 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.6928     0.3191  20.971  < 2e-16 ***
## genderwoman                   0.3202     0.2456   1.304    0.193    
## ethnicityblack                1.2453     0.3058   4.072 5.83e-05 ***
## ethnicitywhite                1.7386     0.2906   5.984 5.63e-09 ***
## factor(location)2nd          -2.6849     0.3443  -7.798 8.10e-14 ***
## factor(location)bessborough  -2.2389     0.3443  -6.502 2.88e-10 ***
## factor(location)victoria     -3.4735     0.3549  -9.786  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.251 on 334 degrees of freedom
## Multiple R-squared:  0.3044,	Adjusted R-squared:  0.2919 
## F-statistic: 24.35 on 6 and 334 DF,  p-value: < 2.2e-16
```

# 7b4. Gender*Ethnicity - Location fixed effects

``` r
m12 <- lm(time_to_cross_street ~ gender*ethnicity + factor(location),
         data = data)

tidy(m12)
```

```
## # A tibble: 9 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                    6.35      0.359     17.7  7.53e-50
## 2 genderwoman                    0.963     0.408      2.36 1.89e- 2
## 3 ethnicityblack                 1.92      0.408      4.69 3.99e- 6
## 4 ethnicitywhite                 2.03      0.408      4.98 1.04e- 6
## 5 factor(location)2nd           -2.64      0.343     -7.69 1.69e-13
## 6 factor(location)bessborough   -2.19      0.343     -6.39 5.64e-10
## 7 factor(location)victoria      -3.49      0.353     -9.90 2.01e-20
## 8 genderwoman:ethnicityblack    -1.52      0.613     -2.47 1.39e- 2
## 9 genderwoman:ethnicitywhite    -0.589     0.578     -1.02 3.09e- 1
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
## -4.3451 -1.2842 -0.0958  0.8711 13.9202 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   6.3523     0.3588  17.702  < 2e-16 ***
## genderwoman                   0.9632     0.4084   2.358   0.0189 *  
## ethnicityblack                1.9155     0.4084   4.690 3.99e-06 ***
## ethnicitywhite                2.0328     0.4084   4.978 1.04e-06 ***
## factor(location)2nd          -2.6365     0.3429  -7.690 1.69e-13 ***
## factor(location)bessborough  -2.1905     0.3429  -6.389 5.64e-10 ***
## factor(location)victoria     -3.4941     0.3529  -9.902  < 2e-16 ***
## genderwoman:ethnicityblack   -1.5163     0.6129  -2.474   0.0139 *  
## genderwoman:ethnicitywhite   -0.5885     0.5775  -1.019   0.3090    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.237 on 332 degrees of freedom
## Multiple R-squared:  0.317,	Adjusted R-squared:  0.3005 
## F-statistic: 19.26 on 8 and 332 DF,  p-value: < 2.2e-16
```

# 7c1. Combined models - gtsummary

``` r
tbl9 <- tbl_regression(
  m9,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location"
    )
  )%>%
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl10 <- tbl_regression(
  m10,
  intercept = TRUE,
  label = list(
    ethnicity ~ "Racialization",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>%
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl11 <- tbl_regression(
  m11,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl12 <- tbl_regression(
  m12,
  intercept = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_column_unhide(columns = std.error) %>% 
  modify_column_hide(columns = conf.low) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "(Intercept)" ~ "Constant",
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          label == "woman * white" ~ "Women * White",
          label == "woman * black" ~ "Women * Black",
          TRUE ~ label)
    ))
```

# 7c2. Table 3 - Mean time before entering intersection combined models table

``` r
table3 <- tbl_merge(
  tbls = list(tbl9, tbl10, tbl11, tbl12),
  tab_spanner = c(
    "Gender",
    "Racialization",
    "Gender and Racialization",
    "Gender and Racialization Interaction"
  )
) %>%
  modify_table_body(
    ~ .x %>% 
      mutate(
        row_order = case_when(
          variable == "(Intercept)" ~ 1,
          variable == "gender" ~ 2,
          variable == "ethnicity" ~ 3,
          variable == "gender:ethnicity" ~ 4,
          variable == "location" ~ 5,
          TRUE ~ 99
        )
      ) %>% 
      arrange(row_order, row_type != "label") %>% 
      select(-row_order)
  ) %>% 
  remove_abbreviation("CI = Confidence Interval")
```

```
## The number rows in the tables to be merged do not match, which may result in
## rows appearing out of order.
## ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
##   `quiet=TRUE` to silence message.
```

``` r
as_gt(table3) %>% 
  gtsave(
    filename = "table3.png",
    vwidth = 2200,
    zoom = 2
  )
```

```
## file:///C:/Users/KADEGA~1/AppData/Local/Temp/RtmpKYF5jM/file290441f84e70.html screenshot completed
```

# 8. Car proceed through intersection
# 8a. Car proceed through intersection - Descriptive stats

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
## 1 man    no                               10       5.56
## 2 man    yes                             153      85   
## 3 man    <NA>                             17       9.44
## 4 woman  no                               21      13.0 
## 5 woman  yes                             131      81.4 
## 6 woman  <NA>                              9       5.59
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(did_car_proceed_before_across) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 9 × 4
## # Groups:   ethnicity [3]
##   ethnicity did_car_proceed_before_across     n percentage
##   <fct>     <fct>                         <int>      <dbl>
## 1 asian     no                                7       5.83
## 2 asian     yes                             102      85   
## 3 asian     <NA>                             11       9.17
## 4 black     no                               11      10.9 
## 5 black     yes                              85      84.2 
## 6 black     <NA>                              5       4.95
## 7 white     no                               13      10.8 
## 8 white     yes                              97      80.8 
## 9 white     <NA>                             10       8.33
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(did_car_proceed_before_across) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 17 × 5
## # Groups:   ethnicity, gender [6]
##    ethnicity gender did_car_proceed_before_across     n percentage
##    <fct>     <fct>  <fct>                         <int>      <dbl>
##  1 asian     man    no                                3       5   
##  2 asian     man    yes                              53      88.3 
##  3 asian     man    <NA>                              4       6.67
##  4 asian     woman  no                                4       6.67
##  5 asian     woman  yes                              49      81.7 
##  6 asian     woman  <NA>                              7      11.7 
##  7 black     man    no                                4       6.67
##  8 black     man    yes                              51      85   
##  9 black     man    <NA>                              5       8.33
## 10 black     woman  no                                7      17.1 
## 11 black     woman  yes                              34      82.9 
## 12 white     man    no                                3       5   
## 13 white     man    yes                              49      81.7 
## 14 white     man    <NA>                              8      13.3 
## 15 white     woman  no                               10      16.7 
## 16 white     woman  yes                              48      80   
## 17 white     woman  <NA>                              2       3.33
```

``` r
  .groups = "drop"
```

# 8b. Car proceed through intersection - Logistic regression  
# 8b1. Gender - Location fixed effects

``` r
m13 <- glm(did_car_proceed_before_across ~ gender + factor(location),
           data = data,
           family = binomial()
           )

tidy(m13)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic      p.value
##   <chr>                          <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)                    2.50      0.461     5.42  0.0000000601
## 2 genderwoman                   -0.891     0.404    -2.20  0.0275      
## 3 factor(location)2nd           -0.106     0.502    -0.211 0.833       
## 4 factor(location)bessborough    0.638     0.571     1.12  0.264       
## 5 factor(location)victoria       0.467     0.573     0.815 0.415
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
## (Intercept)                   2.4988     0.4612   5.418 6.01e-08 ***
## genderwoman                  -0.8914     0.4043  -2.204   0.0275 *  
## factor(location)2nd          -0.1058     0.5021  -0.211   0.8331    
## factor(location)bessborough   0.6384     0.5711   1.118   0.2637    
## factor(location)victoria      0.4669     0.5725   0.815   0.4148    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 202.60  on 314  degrees of freedom
## Residual deviance: 194.61  on 310  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 204.61
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(cbind(OR = coef(m13), confint(m13)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR     2.5 %     97.5 %
## (Intercept)                 12.1682787 5.2804842 32.6651526
## genderwoman                  0.4100955 0.1785199  0.8856211
## factor(location)2nd          0.8995713 0.3256884  2.3934653
## factor(location)bessborough  1.8933709 0.6204880  6.0782630
## factor(location)victoria     1.5949748 0.5208722  5.1295710
```

``` r
ci13 <- exp(confint(m13))
```

```
## Waiting for profiling to be done...
```

# 8b2. Ethnicity - Location fixed effects

``` r
m14 <- glm(did_car_proceed_before_across ~ ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m14)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic    p.value
##   <chr>                          <dbl>     <dbl>     <dbl>      <dbl>
## 1 (Intercept)                    2.45      0.506     4.84  0.00000132
## 2 ethnicityblack                -0.613     0.509    -1.21  0.228     
## 3 ethnicitywhite                -0.671     0.492    -1.36  0.172     
## 4 factor(location)2nd           -0.114     0.500    -0.227 0.820     
## 5 factor(location)bessborough    0.621     0.569     1.09  0.275     
## 6 factor(location)victoria       0.494     0.570     0.866 0.386
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
## (Intercept)                   2.4478     0.5060   4.837 1.32e-06 ***
## ethnicityblack               -0.6135     0.5087  -1.206    0.228    
## ethnicitywhite               -0.6709     0.4916  -1.365    0.172    
## factor(location)2nd          -0.1136     0.4999  -0.227    0.820    
## factor(location)bessborough   0.6205     0.5688   1.091    0.275    
## factor(location)victoria      0.4937     0.5700   0.866    0.386    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 202.60  on 314  degrees of freedom
## Residual deviance: 197.52  on 309  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 209.52
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(cbind(OR = coef(m14), confint(m14)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR     2.5 %    97.5 %
## (Intercept)                 11.5623175 4.6402115 34.348155
## ethnicityblack               0.5414598 0.1906557  1.445683
## ethnicitywhite               0.5112450 0.1847135  1.307458
## factor(location)2nd          0.8926418 0.3244709  2.364178
## factor(location)bessborough  1.8599125 0.6121265  5.944908
## factor(location)victoria     1.6383236 0.5378181  5.246557
```

``` r
ci14 <- exp(confint(m14))
```

```
## Waiting for profiling to be done...
```

# 8b3. Gender + Ethnicity - Location fixed effects

``` r
m15 <- glm(did_car_proceed_before_across ~ gender + ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m15)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic     p.value
##   <chr>                          <dbl>     <dbl>     <dbl>       <dbl>
## 1 (Intercept)                   2.96       0.575     5.15  0.000000265
## 2 genderwoman                  -0.904      0.408    -2.22  0.0268     
## 3 ethnicityblack               -0.681      0.516    -1.32  0.187      
## 4 ethnicitywhite               -0.641      0.495    -1.29  0.196      
## 5 factor(location)2nd          -0.0691     0.506    -0.137 0.891      
## 6 factor(location)bessborough   0.668      0.575     1.16  0.245      
## 7 factor(location)victoria      0.446      0.575     0.775 0.438
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
## (Intercept)                  2.96060    0.57525   5.147 2.65e-07 ***
## genderwoman                 -0.90398    0.40810  -2.215   0.0268 *  
## ethnicityblack              -0.68064    0.51567  -1.320   0.1869    
## ethnicitywhite              -0.64072    0.49522  -1.294   0.1957    
## factor(location)2nd         -0.06914    0.50640  -0.137   0.8914    
## factor(location)bessborough  0.66779    0.57462   1.162   0.2452    
## factor(location)victoria     0.44606    0.57526   0.775   0.4381    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 202.60  on 314  degrees of freedom
## Residual deviance: 192.29  on 308  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 206.29
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(cbind(OR = coef(m15), confint(m15)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR     2.5 %     97.5 %
## (Intercept)                 19.3094739 6.7915194 65.7087377
## genderwoman                  0.4049539 0.1750102  0.8808777
## ethnicityblack               0.5062938 0.1758888  1.3694433
## ethnicitywhite               0.5269146 0.1892133  1.3578933
## factor(location)2nd          0.9332002 0.3354212  2.5060863
## factor(location)bessborough  1.9499275 0.6350280  6.3024670
## factor(location)victoria     1.5621438 0.5073118  5.0479472
```

``` r
ci15 <- exp(confint(m15))
```

```
## Waiting for profiling to be done...
```

# 8b4. Gender*Ethnicity - Location fixed effects

``` r
m16 <- glm(did_car_proceed_before_across ~ gender*ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m16)
```

```
## # A tibble: 9 × 5
##   term                        estimate std.error statistic  p.value
##   <chr>                          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)                   2.63       0.678     3.88  0.000106
## 2 genderwoman                  -0.374      0.791    -0.473 0.636   
## 3 ethnicityblack               -0.321      0.791    -0.406 0.684   
## 4 ethnicitywhite               -0.0906     0.842    -0.108 0.914   
## 5 factor(location)2nd          -0.0608     0.508    -0.120 0.905   
## 6 factor(location)bessborough   0.673      0.577     1.17  0.243   
## 7 factor(location)victoria      0.440      0.577     0.764 0.445   
## 8 genderwoman:ethnicityblack   -0.583      1.04     -0.561 0.575   
## 9 genderwoman:ethnicitywhite   -0.838      1.05     -0.797 0.425
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
## (Intercept)                  2.63004    0.67835   3.877 0.000106 ***
## genderwoman                 -0.37447    0.79123  -0.473 0.636011    
## ethnicityblack              -0.32132    0.79055  -0.406 0.684409    
## ethnicitywhite              -0.09060    0.84222  -0.108 0.914337    
## factor(location)2nd         -0.06075    0.50837  -0.120 0.904875    
## factor(location)bessborough  0.67276    0.57667   1.167 0.243357    
## factor(location)victoria     0.44041    0.57677   0.764 0.445115    
## genderwoman:ethnicityblack  -0.58330    1.04021  -0.561 0.574964    
## genderwoman:ethnicitywhite  -0.83792    1.05077  -0.797 0.425199    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 202.60  on 314  degrees of freedom
## Residual deviance: 191.64  on 306  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 209.64
## 
## Number of Fisher Scoring iterations: 5
```

``` r
exp(cbind(OR = coef(m16), confint(m16)))
```

```
## Waiting for profiling to be done...
```

```
##                                     OR      2.5 %    97.5 %
## (Intercept)                 13.8743563 4.25071521 65.048106
## genderwoman                  0.6876512 0.12953304  3.281835
## ethnicityblack               0.7251890 0.13676814  3.457081
## ethnicitywhite               0.9133857 0.16180428  5.154822
## factor(location)2nd          0.9410560 0.33707294  2.537925
## factor(location)bessborough  1.9596426 0.63578898  6.359222
## factor(location)victoria     1.5533471 0.50292241  5.033233
## genderwoman:ethnicityblack   0.5580517 0.07016422  4.496951
## genderwoman:ethnicitywhite   0.4326092 0.05101030  3.468824
```

``` r
ci16 <- exp(confint(m16))
```

```
## Waiting for profiling to be done...
```

# 8c1. Combined models - Odd Ratio with 95% CI - gtsummary

``` r
tbl13 <- tbl_regression(
  m13,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl14 <- tbl_regression(
  m14,
  exponentiate = TRUE,
  label = list(
    ethnicity ~ "Racialization",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl15 <- tbl_regression(
  m15,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl16 <- tbl_regression(
  m16,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          label == "woman * white" ~ "Women * White",
          label == "woman * black" ~ "Women * Black",
          TRUE ~ label)
    ))
```

# 8c2. Table 4 - Car Proceed combined models table

``` r
table4 <- tbl_merge(
  tbls = list(tbl13, tbl14, tbl15, tbl16),
  tab_spanner = c(
    "Gender",
    "Racialization",
    "Gender and Racialization",
    "Gender and Racialization Interaction"
  )
) %>% 
  modify_table_body(
    ~ .x %>% 
      mutate(
        row_order = case_when(
          variable == "gender" ~ 1,
          variable == "ethnicity" ~ 2,
          variable == "gender:ethnicity" ~ 3,
          variable == "location" ~ 4,
          TRUE ~ 99
        )
      ) %>% 
      arrange(row_order, row_type != "label") %>% 
      select(-row_order)
  )
```

```
## The number rows in the tables to be merged do not match, which may result in
## rows appearing out of order.
## ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
##   `quiet=TRUE` to silence message.
```

``` r
as_gt(table4) %>% 
  gtsave(
    filename = "table4.png",
    vwidth = 2200,
    zoom = 2
  )
```

```
## file:///C:/Users/KADEGA~1/AppData/Local/Temp/RtmpKYF5jM/file290413d73c16.html screenshot completed
```

# 9. Cars stop close or far 
# 9a. Cars stop close or far binning

``` r
data$car_stop_close_or_far_bin <- ifelse(data$car_stop_close_or_far == "far", 1,
                                  ifelse(data$car_stop_close_or_far == "close", 0, NA))
```

# 9b. Cars stop close or far - Descritpive stats

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
## 1 man    close                    11       6.11
## 2 man    far                     152      84.4 
## 3 man    <NA>                     17       9.44
## 4 woman  close                    10       6.21
## 5 woman  far                     142      88.2 
## 6 woman  <NA>                      9       5.59
```

``` r
  .groups = "drop"

data %>% 
  group_by(ethnicity) %>% 
  count(car_stop_close_or_far) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 9 × 4
## # Groups:   ethnicity [3]
##   ethnicity car_stop_close_or_far     n percentage
##   <fct>     <chr>                 <int>      <dbl>
## 1 asian     close                    11       9.17
## 2 asian     far                      98      81.7 
## 3 asian     <NA>                     11       9.17
## 4 black     close                     6       5.94
## 5 black     far                      90      89.1 
## 6 black     <NA>                      5       4.95
## 7 white     close                     4       3.33
## 8 white     far                     106      88.3 
## 9 white     <NA>                     10       8.33
```

``` r
  .groups = "drop"
  
data %>% 
  group_by(ethnicity, gender) %>% 
  count(car_stop_close_or_far) %>% 
  mutate(percentage = n / sum(n) * 100)
```

```
## # A tibble: 17 × 5
## # Groups:   ethnicity, gender [6]
##    ethnicity gender car_stop_close_or_far     n percentage
##    <fct>     <fct>  <chr>                 <int>      <dbl>
##  1 asian     man    close                     5       8.33
##  2 asian     man    far                      51      85   
##  3 asian     man    <NA>                      4       6.67
##  4 asian     woman  close                     6      10   
##  5 asian     woman  far                      47      78.3 
##  6 asian     woman  <NA>                      7      11.7 
##  7 black     man    close                     4       6.67
##  8 black     man    far                      51      85   
##  9 black     man    <NA>                      5       8.33
## 10 black     woman  close                     2       4.88
## 11 black     woman  far                      39      95.1 
## 12 white     man    close                     2       3.33
## 13 white     man    far                      50      83.3 
## 14 white     man    <NA>                      8      13.3 
## 15 white     woman  close                     2       3.33
## 16 white     woman  far                      56      93.3 
## 17 white     woman  <NA>                      2       3.33
```

``` r
  .groups = "drop"
```

# 9c. Cars stop close or far - Logistic regression 
# 9c1. Gender - Location fixed effects

``` r
m17 <- glm(car_stop_close_or_far_bin ~ gender + factor(location),
           data = data,
           family = binomial()
)

tidy(m17)
```

```
## # A tibble: 5 × 5
##   term                        estimate std.error statistic   p.value
##   <chr>                          <dbl>     <dbl>     <dbl>     <dbl>
## 1 (Intercept)                  1.62        0.397    4.09   0.0000427
## 2 genderwoman                  0.00762     0.463    0.0164 0.987    
## 3 factor(location)2nd          1.34        0.610    2.20   0.0277   
## 4 factor(location)bessborough  2.12        0.788    2.69   0.00708  
## 5 factor(location)victoria     1.30        0.610    2.14   0.0326
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
## (Intercept)                 1.623823   0.396772   4.093 4.27e-05 ***
## genderwoman                 0.007618   0.463106   0.016  0.98688    
## factor(location)2nd         1.342696   0.609784   2.202  0.02767 *  
## factor(location)bessborough 2.121835   0.787828   2.693  0.00708 ** 
## factor(location)victoria    1.304099   0.610399   2.136  0.03264 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 154.31  on 314  degrees of freedom
## Residual deviance: 142.50  on 310  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 152.5
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(cbind(OR = coef(m17), confint(m17)))
```

```
## Waiting for profiling to be done...
```

```
##                                   OR     2.5 %    97.5 %
## (Intercept)                 5.072445 2.4401681 11.723659
## genderwoman                 1.007647 0.4040071  2.539482
## factor(location)2nd         3.829355 1.2384122 14.389117
## factor(location)bessborough 8.346442 2.1383406 55.271111
## factor(location)victoria    3.684367 1.1899353 13.857748
```

``` r
ci17 <- exp(confint(m17))
```

```
## Waiting for profiling to be done...
```

# 9c2. Ethnicity - Location fixed effects

``` r
m18 <- glm(car_stop_close_or_far_bin ~ ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m18)
```

```
## # A tibble: 6 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                    1.16      0.411     2.82  0.00482
## 2 ethnicityblack                 0.488     0.544     0.897 0.370  
## 3 ethnicitywhite                 1.13      0.612     1.85  0.0638 
## 4 factor(location)2nd            1.36      0.615     2.21  0.0275 
## 5 factor(location)bessborough    2.15      0.792     2.71  0.00667
## 6 factor(location)victoria       1.33      0.616     2.16  0.0308
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
## (Intercept)                   1.1591     0.4112   2.819  0.00482 **
## ethnicityblack                0.4877     0.5436   0.897  0.36970   
## ethnicitywhite                1.1348     0.6122   1.854  0.06381 . 
## factor(location)2nd           1.3562     0.6150   2.205  0.02745 * 
## factor(location)bessborough   2.1478     0.7917   2.713  0.00667 **
## factor(location)victoria      1.3298     0.6159   2.159  0.03083 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 154.31  on 314  degrees of freedom
## Residual deviance: 138.65  on 309  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 150.65
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(cbind(OR = coef(m18), confint(m18)))
```

```
## Waiting for profiling to be done...
```

```
##                                   OR     2.5 %    97.5 %
## (Intercept)                 3.187210 1.4747774  7.515973
## ethnicityblack              1.628500 0.5754414  5.029079
## ethnicitywhite              3.110610 1.0014862 11.743958
## factor(location)2nd         3.881225 1.2413530 14.708867
## factor(location)bessborough 8.566412 2.1749291 57.018910
## factor(location)victoria    3.780436 1.2077290 14.352290
```

``` r
ci18 <- exp(confint(m18))
```

```
## Waiting for profiling to be done...
```

# 9c3. Gender + Ethnicity - Location fixed effects

``` r
m19 <- glm(car_stop_close_or_far_bin ~ gender + ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m19)
```

```
## # A tibble: 7 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                   1.17       0.472    2.48   0.0133 
## 2 genderwoman                  -0.0205     0.470   -0.0437 0.965  
## 3 ethnicityblack                0.486      0.546    0.890  0.373  
## 4 ethnicitywhite                1.14       0.613    1.85   0.0638 
## 5 factor(location)2nd           1.36       0.615    2.21   0.0274 
## 6 factor(location)bessborough   2.15       0.792    2.71   0.00666
## 7 factor(location)victoria      1.33       0.616    2.16   0.0310
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
## (Intercept)                  1.16924    0.47208   2.477  0.01326 * 
## genderwoman                 -0.02053    0.46987  -0.044  0.96516   
## ethnicityblack               0.48562    0.54567   0.890  0.37349   
## ethnicitywhite               1.13571    0.61261   1.854  0.06375 . 
## factor(location)2nd          1.35686    0.61526   2.205  0.02743 * 
## factor(location)bessborough  2.14856    0.79188   2.713  0.00666 **
## factor(location)victoria     1.32896    0.61621   2.157  0.03103 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 154.31  on 314  degrees of freedom
## Residual deviance: 138.64  on 308  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 152.64
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(cbind(OR = coef(m19), confint(m19)))
```

```
## Waiting for profiling to be done...
```

```
##                                    OR     2.5 %    97.5 %
## (Intercept)                 3.2195549 1.3255860  8.585594
## genderwoman                 0.9796834 0.3874847  2.500274
## ethnicityblack              1.6251832 0.5716772  5.035890
## ethnicitywhite              3.1133767 1.0016638 11.761556
## factor(location)2nd         3.8839807 1.2416741 14.725362
## factor(location)bessborough 8.5725120 2.1755969 57.073884
## factor(location)victoria    3.7771088 1.2056910 14.347073
```

``` r
ci19 <- exp(confint(m19))
```

```
## Waiting for profiling to be done...
```

# 9c4. Gender*Ethnicity - Location fixed effects

``` r
m20 <- glm(car_stop_close_or_far_bin ~ gender*ethnicity + factor(location),
           data = data,
           family = binomial()
           )

tidy(m20)
```

```
## # A tibble: 9 × 5
##   term                        estimate std.error statistic p.value
##   <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)                    1.29      0.542     2.39  0.0171 
## 2 genderwoman                   -0.257     0.659    -0.389 0.697  
## 3 ethnicityblack                 0.249     0.718     0.346 0.729  
## 4 ethnicitywhite                 0.928     0.876     1.06  0.290  
## 5 factor(location)2nd            1.34      0.617     2.17  0.0299 
## 6 factor(location)bessborough    2.14      0.793     2.69  0.00707
## 7 factor(location)victoria       1.34      0.617     2.18  0.0295 
## 8 genderwoman:ethnicityblack     0.552     1.13      0.489 0.625  
## 9 genderwoman:ethnicitywhite     0.402     1.22      0.328 0.743
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
## (Intercept)                   1.2925     0.5418   2.385  0.01706 * 
## genderwoman                  -0.2566     0.6591  -0.389  0.69709   
## ethnicityblack                0.2487     0.7180   0.346  0.72905   
## ethnicitywhite                0.9279     0.8761   1.059  0.28952   
## factor(location)2nd           1.3387     0.6166   2.171  0.02992 * 
## factor(location)bessborough   2.1354     0.7928   2.694  0.00707 **
## factor(location)victoria      1.3434     0.6171   2.177  0.02947 * 
## genderwoman:ethnicityblack    0.5524     1.1290   0.489  0.62464   
## genderwoman:ethnicitywhite    0.4015     1.2247   0.328  0.74301   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 154.31  on 314  degrees of freedom
## Residual deviance: 138.37  on 306  degrees of freedom
##   (26 observations deleted due to missingness)
## AIC: 156.37
## 
## Number of Fisher Scoring iterations: 6
```

``` r
exp(cbind(OR = coef(m20), confint(m20)))
```

```
## Waiting for profiling to be done...
```

```
##                                    OR     2.5 %    97.5 %
## (Intercept)                 3.6417658 1.3490280 11.759466
## genderwoman                 0.7737042 0.2026445  2.843111
## ethnicityblack              1.2823693 0.3106504  5.621667
## ethnicitywhite              2.5293129 0.5016007 18.659036
## factor(location)2nd         3.8139830 1.2157170 14.489624
## factor(location)bessborough 8.4604390 2.1424071 56.392199
## factor(location)victoria    3.8320125 1.2210926 14.575620
## genderwoman:ethnicityblack  1.7374556 0.1991747 18.875381
## genderwoman:ethnicitywhite  1.4941375 0.1235074 18.248242
```

``` r
ci20 <- exp(confint(m20))
```

```
## Waiting for profiling to be done...
```

# 9c1. Combined models - Odd Ratio with 95% CI - gtsummary

``` r
tbl17 <- tbl_regression(
  m17,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl18 <- tbl_regression(
  m18,
  exponentiate = TRUE,
  label = list(
    ethnicity ~ "Racialization",
    "factor(location)" ~ "Intersection Location"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl19 <- tbl_regression(
  m19,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          TRUE ~ label)
    ))

tbl20 <- tbl_regression(
  m20,
  exponentiate = TRUE,
  label = list(
    gender ~ "Gender",
    "factor(location)" ~ "Intersection Location",
    ethnicity ~ "Racialization"
  )) %>% 
  modify_table_body(
    ~.x %>% 
      mutate(
        label = case_when(
          label == "man" ~ "Men",
          label == "woman" ~ "Women",
          label == "asian" ~ "South Asian",
          label == "white" ~ "White",
          label == "black" ~ "Black",
          label == "19th" ~ "19th Street",
          label == "2nd"  ~ "2nd Avenue",
          label == "bessborough" ~ "Bessborough",
          label == "victoria" ~ "Victoria Avenue",
          label == "woman * white" ~ "Women * White",
          label == "woman * black" ~ "Women * Black",
          TRUE ~ label)
    ))
```

# 9c2. Table 5 - Car yield close//far combined models table

``` r
table5 <- tbl_merge(
  tbls = list(tbl17, tbl18, tbl19, tbl20),
  tab_spanner = c(
    "Gender",
    "Racialization",
    "Gender and Racialization",
    "Gender and Racialization Interaction"
  )
) %>% 
  modify_table_body(
    ~ .x %>% 
      mutate(
        row_order = case_when(
          variable == "gender" ~ 1,
          variable == "ethnicity" ~ 2,
          variable == "gender:ethnicity" ~ 3,
          variable == "location" ~ 4,
          TRUE ~ 99
        )
      ) %>% 
      arrange(row_order, row_type != "label") %>% 
      select(-row_order)
  )
```

```
## The number rows in the tables to be merged do not match, which may result in
## rows appearing out of order.
## ℹ See `tbl_merge()` (`?gtsummary::tbl_merge()`) help file for details. Use
##   `quiet=TRUE` to silence message.
```

``` r
as_gt(table5) %>% 
  gtsave(
    filename = "table5.png",
    vwidth = 2200,
    zoom = 2
  )
```

```
## file:///C:/Users/KADEGA~1/AppData/Local/Temp/RtmpKYF5jM/file290450cd3ea0.html screenshot completed
```

# 10. Histograms
# 10a. Ethnicity


``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
         fill = ethnicity)) +
  geom_histogram((aes(y = after_stat(density))),binwidth = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(data$time_to_cross_street), sd = sd(data$time_to_cross_street)), colour = "red", linewidth = 1) +
    labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  )
```

```
## Warning: Multiple drawing groups in `geom_function()`
## ℹ Did you use the correct group, colour, or fill aesthetics?
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-41-1.png)<!-- -->


``` r
params_eth <- data %>%
  group_by(ethnicity) %>%
  summarise(
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street)
  )

curve_data_eth <- params_eth %>%
  rowwise() %>%
  do({
    tibble(
      ethnicity = .$ethnicity,
      x = seq(min(data$time_to_cross_street),
              max(data$time_to_cross_street),
              length.out = 200),
      y = dnorm(x, .$mean, .$sd)
    )
  })

ggplot(data, aes(time_to_cross_street, fill = ethnicity)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5) +
  geom_line(data = curve_data_eth,
            aes(x = x, y = y),
            colour = "red",
            linewidth = 1,
            inherit.aes = FALSE) +
  labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  ) +
  facet_wrap(~ethnicity)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-42-1.png)<!-- -->

# 10b. Gender

``` r
data %>% 
  ggplot(aes(x = time_to_cross_street,
         fill = gender)) +
  geom_histogram((aes(y = after_stat(density))),binwidth = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(data$time_to_cross_street), sd = sd(data$time_to_cross_street)), colour = "red", linewidth = 1) +
  labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  )
```

```
## Warning: Multiple drawing groups in `geom_function()`
## ℹ Did you use the correct group, colour, or fill aesthetics?
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-43-1.png)<!-- -->



``` r
params_gen <- data %>%
  group_by(gender) %>%
  summarise(
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street)
  )

curve_data_gen <- params_gen %>%
  rowwise() %>%
  do({
    tibble(
      gender = .$gender,
      x = seq(min(data$time_to_cross_street),
              max(data$time_to_cross_street),
              length.out = 200),
      y = dnorm(x, .$mean, .$sd)
    )
  })

ggplot(data, aes(time_to_cross_street, fill = gender)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5) +
  geom_line(data = curve_data_gen,
            aes(x = x, y = y),
            colour = "red",
            linewidth = 1,
            inherit.aes = FALSE) +
  labs(
    x = "Time to Cross (s)",
    y = "Count"
  ) +
  theme_minimal(
  ) +
  facet_wrap(~gender)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

# 11. Visualizations
# 11a. Time by ethnicity

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

# 11b. Time by gender

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-46-1.png)<!-- -->

# 11c. Time to enter the street - Violin Plot with Jitter overlay - Facet by gender

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-47-1.png)<!-- -->

# 11d. Time to enter the street - Violin Plot with Jitter overlay - Facet by ethnicity

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

# 11e. Time to enter - QQplot - By ethnicity  

``` r
data %>% 
  ggplot(aes(sample = time_to_cross_street)) +
  geom_qq() +
  stat_qq_line() +
  facet_wrap(~ethnicity)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

# 11f. Time to enter - QQplot - By gender 

``` r
data %>% 
  ggplot(aes(sample = time_to_cross_street)) +
  geom_qq() +
  stat_qq_line() +
  facet_wrap(~gender)
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

# 11g. 19th removed - Time to enter - By gender

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

# 11h. Time by ethnicity and gender - grouped by location 

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

# 11i. Cars passed by ethnicity and gender

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-53-1.png)<!-- -->

# 11j. Proportion of first car yields - By ethnicity 

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

# 11k. Proportion of first car yield - By gender 

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-55-1.png)<!-- -->

# 11l. Odds first car yield

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

# 11m. Odds car proceeded

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-57-1.png)<!-- -->

# 11n. Odds car stopped close//far 

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

![](racial_bias_analysis_files/figure-html/unnamed-chunk-58-1.png)<!-- -->
