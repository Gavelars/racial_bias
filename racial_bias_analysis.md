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
## Rows: 255 Columns: 12
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (9): ethnicity, gender, location, date, time_of_day, first_car_yield, di...
## dbl (3): trial_number, num_cars_pass_before_yield, time_to_cross_street
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
## 1   210      5.57    2.85     2.07     22.7     0.567    1.02
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
## 2 white        90
```

``` r
data %>% count(gender)
```

```
## # A tibble: 2 × 2
##   gender     n
##   <fct>  <int>
## 1 man       90
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
## 2 2nd            45
## 3 bessborough    45
## 4 victoria       60
```

``` r
data %>% count(first_car_yield)
```

```
## # A tibble: 2 × 2
##   first_car_yield     n
##   <fct>           <int>
## 1 no                 70
## 2 yes               140
```

``` r
data %>% count(did_car_proceed_before_across)
```

```
## # A tibble: 3 × 2
##   did_car_proceed_before_across     n
##   <fct>                         <int>
## 1 no                               18
## 2 yes                             175
## 3 <NA>                             17
```

TIME TO CROSS BY RACE DESCRIPTIVE STATS


``` r
data %>% 
  group_by(ethnicity) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street)
  )
```

```
## # A tibble: 2 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian       120  4.75  1.20
## 2 white        90  6.65  3.88
```

19th TIMES REMOVED

``` r
data_19th_rm %>% 
  group_by(ethnicity) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street)
  )
```

```
## # A tibble: 2 × 4
##   ethnicity     n  mean    sd
##   <fct>     <int> <dbl> <dbl>
## 1 asian        90  4.61  1.06
## 2 white        60  4.86  1.22
```


ONE-WAY ANOVA RACE


``` r
race_model <- aov(
  time_to_cross_street~ethnicity,
  data = data
)

tidy(race_model)
```

```
## # A tibble: 2 × 6
##   term         df sumsq meansq statistic     p.value
##   <chr>     <dbl> <dbl>  <dbl>     <dbl>       <dbl>
## 1 ethnicity     1  184. 184.        25.4  0.00000101
## 2 Residuals   208 1510.   7.26      NA   NA
```

``` r
TukeyHSD(race_model)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = time_to_cross_street ~ ethnicity, data = data)
## 
## $ethnicity
##                 diff      lwr     upr p adj
## white-asian 1.893639 1.152938 2.63434 1e-06
```

19th REMOVED - ONE-WAY ANOVA RACE


``` r
race_model <- aov(
  time_to_cross_street~ethnicity,
  data = data_19th_rm
)

tidy(race_model)
```

```
## # A tibble: 2 × 6
##   term         df  sumsq meansq statistic p.value
##   <chr>     <dbl>  <dbl>  <dbl>     <dbl>   <dbl>
## 1 ethnicity     1   2.27   2.27      1.78   0.184
## 2 Residuals   148 188.     1.27     NA     NA
```

``` r
TukeyHSD(race_model)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = time_to_cross_street ~ ethnicity, data = data_19th_rm)
## 
## $ethnicity
##                  diff        lwr       upr     p adj
## white-asian 0.2510556 -0.1206324 0.6227435 0.1840026
```

ONE-WAY ANOVA LOCATION


``` r
location_model <- aov(
  time_to_cross_street~location,
  data = data
)

tidy(location_model)
```

```
## # A tibble: 2 × 6
##   term         df sumsq meansq statistic   p.value
##   <chr>     <dbl> <dbl>  <dbl>     <dbl>     <dbl>
## 1 location      3  402. 134.        21.3  4.51e-12
## 2 Residuals   206 1293.   6.28      NA   NA
```

``` r
TukeyHSD(location_model)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = time_to_cross_street ~ location, data = data)
## 
## $location
##                            diff        lwr        upr     p adj
## 2nd-19th             -3.0878889 -4.3674969 -1.8082809 0.0000000
## bessborough-19th     -2.4558889 -3.7354969 -1.1762809 0.0000083
## victoria-19th        -3.3083333 -4.4930201 -2.1236466 0.0000000
## bessborough-2nd       0.6320000 -0.7359585  1.9999585 0.6296602
## victoria-2nd         -0.2204444 -1.5000524  1.0591635 0.9702907
## victoria-bessborough -0.8524444 -2.1320524  0.4271635 0.3131102
```

TIME TO CROSS BY GENDER DESCRIPTIVE STATS


``` r
data %>% 
  group_by(gender) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street)
  )
```

```
## # A tibble: 2 × 4
##   gender     n  mean    sd
##   <fct>  <int> <dbl> <dbl>
## 1 man       90  5.04  2.00
## 2 woman    120  5.96  3.30
```


T-TEST


``` r
t.test(
  time_to_cross_street ~ gender,
  data = data
)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  time_to_cross_street by gender
## t = -2.4893, df = 199.95, p-value = 0.01362
## alternative hypothesis: true difference in means between group man and group woman is not equal to 0
## 95 percent confidence interval:
##  -1.6400207 -0.1902016
## sample estimates:
##   mean in group man mean in group woman 
##            5.042222            5.957333
```

TIME TO CROSS BY RACE X GENDER


``` r
data %>% 
  group_by(ethnicity, gender) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street),
    .groups = "drop"
  )
```

```
## # A tibble: 4 × 5
##   ethnicity gender     n  mean    sd
##   <fct>     <fct>  <int> <dbl> <dbl>
## 1 asian     man       60  4.27 0.924
## 2 asian     woman     60  5.24 1.26 
## 3 white     man       30  6.58 2.62 
## 4 white     woman     60  6.68 4.40
```


19th TIMES REMOVED - TIME TO CROSS BY RACE X GENDER


``` r
data_19th_rm %>% 
  group_by(ethnicity, gender) %>% 
  summarise(
    n = n(),
    mean = mean(time_to_cross_street),
    sd = sd(time_to_cross_street),
    .groups = "drop"
  )
```

```
## # A tibble: 4 × 5
##   ethnicity gender     n  mean    sd
##   <fct>     <fct>  <int> <dbl> <dbl>
## 1 asian     man       45  4.10 0.820
## 2 asian     woman     45  5.12 1.04 
## 3 white     man       15  5.08 0.784
## 4 white     woman     45  4.79 1.33
```


TWO-WAY ANOVA


``` r
time_model <- aov(
  time_to_cross_street ~ ethnicity*gender,
  data = data
)

tidy(time_model)
```

```
## # A tibble: 4 × 6
##   term                df   sumsq meansq statistic      p.value
##   <chr>            <dbl>   <dbl>  <dbl>     <dbl>        <dbl>
## 1 ethnicity            1  184.   184.       25.6   0.000000914
## 2 gender               1   19.0   19.0       2.64  0.106      
## 3 ethnicity:gender     1    9.01   9.01      1.25  0.264      
## 4 Residuals          206 1482.     7.19     NA    NA
```

NUMBER OF CARS PASSED BY GENDER AND/OR RACE DESCRIPTIVE STATS


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
## 3 white     man       30 0.6   0.894
## 4 white     woman     60 0.617 1.37
```

GENDER COMPARISON


``` r
t.test(
  num_cars_pass_before_yield ~ gender,
  data = data
)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  num_cars_pass_before_yield by gender
## t = -1.4518, df = 205.91, p-value = 0.1481
## alternative hypothesis: true difference in means between group man and group woman is not equal to 0
## 95 percent confidence interval:
##  -0.45849447  0.06960558
## sample estimates:
##   mean in group man mean in group woman 
##           0.4555556           0.6500000
```

RACE COMPARISON


``` r
cars_race <- aov(
  num_cars_pass_before_yield ~ ethnicity,
  data = data
)

tidy(cars_race)
```

```
## # A tibble: 2 × 6
##   term         df   sumsq meansq statistic p.value
##   <chr>     <dbl>   <dbl>  <dbl>     <dbl>   <dbl>
## 1 ethnicity     1   0.311  0.311     0.301   0.584
## 2 Residuals   208 215.     1.03     NA      NA
```

LOCATION COMPARISON


``` r
cars_location <- aov(
  num_cars_pass_before_yield ~ location, 
  data = data
)

tidy(cars_location)
```

```
## # A tibble: 2 × 6
##   term         df sumsq meansq statistic   p.value
##   <chr>     <dbl> <dbl>  <dbl>     <dbl>     <dbl>
## 1 location      3  45.7 15.2        18.5  1.19e-10
## 2 Residuals   206 170.   0.825      NA   NA
```

``` r
TukeyHSD(cars_location)
```

```
##   Tukey multiple comparisons of means
##     95% family-wise confidence level
## 
## Fit: aov(formula = num_cars_pass_before_yield ~ location, data = data)
## 
## $location
##                            diff        lwr        upr     p adj
## 2nd-19th             -0.8388889 -1.3027212 -0.3750566 0.0000301
## bessborough-19th     -0.9722222 -1.4360545 -0.5083899 0.0000009
## victoria-19th        -1.1500000 -1.5794253 -0.7205747 0.0000000
## bessborough-2nd      -0.1333333 -0.6291909  0.3625242 0.8983497
## victoria-2nd         -0.3111111 -0.7749434  0.1527212 0.3070898
## victoria-bessborough -0.1777778 -0.6416101  0.2860545 0.7537290
```

RACE AND GENDER COMPARISON


``` r
cars_model <- aov(
  num_cars_pass_before_yield ~ ethnicity * gender,
  data = data
)

tidy(cars_model)
```

```
## # A tibble: 4 × 6
##   term                df   sumsq meansq statistic p.value
##   <chr>            <dbl>   <dbl>  <dbl>     <dbl>   <dbl>
## 1 ethnicity            1   0.311  0.311     0.302   0.584
## 2 gender               1   1.74   1.74      1.69    0.195
## 3 ethnicity:gender     1   0.963  0.963     0.934   0.335
## 4 Residuals          206 213.     1.03     NA      NA
```

FIRST CAR YIELD BY RACE AND/OR GENDER RACE COMPARISON


``` r
data %>% 
  count(ethnicity, first_car_yield)
```

```
## # A tibble: 4 × 3
##   ethnicity first_car_yield     n
##   <fct>     <fct>           <int>
## 1 asian     no                 44
## 2 asian     yes                76
## 3 white     no                 26
## 4 white     yes                64
```

``` r
chisq.test(
  table(data$ethnicity,
        data$first_car_yield)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$ethnicity, data$first_car_yield)
## X-squared = 1.0719, df = 1, p-value = 0.3005
```

LOCATION COMPARISON


``` r
data %>% 
  count(location, first_car_yield)
```

```
## # A tibble: 8 × 3
##   location    first_car_yield     n
##   <fct>       <fct>           <int>
## 1 19th        no                 39
## 2 19th        yes                21
## 3 2nd         no                 16
## 4 2nd         yes                29
## 5 bessborough no                  8
## 6 bessborough yes                37
## 7 victoria    no                  7
## 8 victoria    yes                53
```

``` r
chisq.test(
  table(data$location,
        data$first_car_yield)
)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table(data$location, data$first_car_yield)
## X-squared = 44.75, df = 3, p-value = 1.046e-09
```

GENDER COMPARISON


``` r
data %>% 
  count(gender, first_car_yield)
```

```
## # A tibble: 4 × 3
##   gender first_car_yield     n
##   <fct>  <fct>           <int>
## 1 man    no                 28
## 2 man    yes                62
## 3 woman  no                 42
## 4 woman  yes                78
```

``` r
chisq.test(
  table(data$gender,
        data$first_car_yield)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$gender, data$first_car_yield)
## X-squared = 0.19687, df = 1, p-value = 0.6573
```

LOGISTIC REGRESSION


``` r
firstcar_model <- glm(
  first_car_yield ~ ethnicity + gender,
  data = data,
  family = binomial()
)

tidy(firstcar_model)
```

```
## # A tibble: 3 × 5
##   term           estimate std.error statistic p.value
##   <chr>             <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)       0.670     0.246     2.72  0.00646
## 2 ethnicitywhite    0.396     0.305     1.30  0.195  
## 3 genderwoman      -0.243     0.303    -0.801 0.423
```

DID CAR PROCEED BY RACE AND/OR GENDER RACE COMPARISON


``` r
data %>% 
  count(ethnicity, did_car_proceed_before_across)
```

```
## # A tibble: 6 × 3
##   ethnicity did_car_proceed_before_across     n
##   <fct>     <fct>                         <int>
## 1 asian     no                                7
## 2 asian     yes                             102
## 3 asian     <NA>                             11
## 4 white     no                               11
## 5 white     yes                              73
## 6 white     <NA>                              6
```

``` r
chisq.test(
  table(data$ethnicity,
        data$did_car_proceed_before_across)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$ethnicity, data$did_car_proceed_before_across)
## X-squared = 1.7714, df = 1, p-value = 0.1832
```

LOCATION COMPARISON


``` r
data %>% 
  count(location, did_car_proceed_before_across)
```

```
## # A tibble: 11 × 3
##    location    did_car_proceed_before_across     n
##    <fct>       <fct>                         <int>
##  1 19th        no                                5
##  2 19th        yes                              43
##  3 19th        <NA>                             12
##  4 2nd         no                                5
##  5 2nd         yes                              36
##  6 2nd         <NA>                              4
##  7 bessborough no                                2
##  8 bessborough yes                              42
##  9 bessborough <NA>                              1
## 10 victoria    no                                6
## 11 victoria    yes                              54
```

``` r
chisq.test(
  table(data$location,
        data$did_car_proceed_before_across)
)
```

```
## Warning in chisq.test(table(data$location,
## data$did_car_proceed_before_across)): Chi-squared approximation may be
## incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table(data$location, data$did_car_proceed_before_across)
## X-squared = 1.6879, df = 3, p-value = 0.6396
```

GENDER COMPARISON


``` r
data %>% 
  count(gender, did_car_proceed_before_across)
```

```
## # A tibble: 6 × 3
##   gender did_car_proceed_before_across     n
##   <fct>  <fct>                         <int>
## 1 man    no                                4
## 2 man    yes                              78
## 3 man    <NA>                              8
## 4 woman  no                               14
## 5 woman  yes                              97
## 6 woman  <NA>                              9
```

``` r
chisq.test(
  table(data$gender,
        data$did_car_proceed_before_across)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$gender, data$did_car_proceed_before_across)
## X-squared = 2.4843, df = 1, p-value = 0.115
```

LOGISTIC REGRESSION


``` r
proceed_model <- glm(
  did_car_proceed_before_across ~ ethnicity + gender,
  data = data,
  family = binomial()
)

tidy(proceed_model)
```

```
## # A tibble: 3 × 5
##   term           estimate std.error statistic      p.value
##   <chr>             <dbl>     <dbl>     <dbl>        <dbl>
## 1 (Intercept)       3.21      0.564      5.69 0.0000000125
## 2 ethnicitywhite   -0.628     0.517     -1.21 0.225       
## 3 genderwoman      -0.910     0.597     -1.52 0.127
```

DID CAR YIELD CLOSE OR FAR BY RACE AND/OR GENDER GENDER COMPARISON


``` r
data %>% 
  count(gender, car_stop_close_or_far)
```

```
## # A tibble: 6 × 3
##   gender car_stop_close_or_far     n
##   <fct>  <chr>                 <int>
## 1 man    close                     7
## 2 man    far                      75
## 3 man    <NA>                      8
## 4 woman  close                     8
## 5 woman  far                     103
## 6 woman  <NA>                      9
```

``` r
chisq.test(
  table(data$gender,
        data$car_stop_close_or_far)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$gender, data$car_stop_close_or_far)
## X-squared = 0.004767, df = 1, p-value = 0.945
```

RACE COMPARISON


``` r
data %>% 
  count(ethnicity, car_stop_close_or_far)
```

```
## # A tibble: 6 × 3
##   ethnicity car_stop_close_or_far     n
##   <fct>     <chr>                 <int>
## 1 asian     close                    11
## 2 asian     far                      98
## 3 asian     <NA>                     11
## 4 white     close                     4
## 5 white     far                      80
## 6 white     <NA>                      6
```

``` r
chisq.test(
  table(data$ethnicity,
        data$car_stop_close_or_far)
)
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  table(data$ethnicity, data$car_stop_close_or_far)
## X-squared = 1.2101, df = 1, p-value = 0.2713
```

LOCATION COMPARISON


``` r
data %>% 
  count(location, car_stop_close_or_far)
```

```
## # A tibble: 11 × 3
##    location    car_stop_close_or_far     n
##    <fct>       <chr>                 <int>
##  1 19th        close                     8
##  2 19th        far                      40
##  3 19th        <NA>                     12
##  4 2nd         close                     3
##  5 2nd         far                      38
##  6 2nd         <NA>                      4
##  7 bessborough close                     1
##  8 bessborough far                      43
##  9 bessborough <NA>                      1
## 10 victoria    close                     3
## 11 victoria    far                      57
```

``` r
chisq.test(
  table(data$location,
        data$car_stop_close_or_far)
)
```

```
## Warning in chisq.test(table(data$location, data$car_stop_close_or_far)):
## Chi-squared approximation may be incorrect
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  table(data$location, data$car_stop_close_or_far)
## X-squared = 7.8093, df = 3, p-value = 0.05012
```

LOGISTIC REGRESSION


``` r
data$car_stop_close_or_far_bin <- ifelse(data$car_stop_close_or_far == "far", 1,
                                  ifelse(data$car_stop_close_or_far == "close", 0, NA))

yield_model <- glm(
  car_stop_close_or_far_bin ~ ethnicity + gender,
  data = data,
  family = binomial()
)

tidy(yield_model)
```

```
## # A tibble: 3 × 5
##   term           estimate std.error statistic     p.value
##   <chr>             <dbl>     <dbl>     <dbl>       <dbl>
## 1 (Intercept)      2.17       0.413    5.25   0.000000152
## 2 ethnicitywhite   0.802      0.613    1.31   0.191      
## 3 genderwoman      0.0336     0.551    0.0610 0.951
```

VISUALIZATIONS TIME BY RACE


``` r
data %>% 
  ggplot(aes(x = ethnicity,
         y = time_to_cross_street,
         fill = ethnicity)) +
  geom_boxplot() +
  labs(
    x = "Race",
    y = "Time to Cross (s)"
  ) +
  theme_minimal(
  )
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

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
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-33-1.png)<!-- -->
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
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

TIME BY RACE AND GENDER - GROUPED BY LOCATION


``` r
data%>% 
  ggplot(aes(x = ethnicity,
             y = time_to_cross_street,
               fill = gender)) +
  geom_boxplot() +
  facet_wrap(~location) +
  labs(
    x = "Race",
    y = "Time to Cross (s)",
    fill = "Gender"
  ) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

CARS PASSED BY RACE AND GENDER


``` r
data %>% 
  ggplot(aes(x = ethnicity,
             y = num_cars_pass_before_yield,
             fill = gender)) +
  geom_col() +
  labs (
    x = "Race",
    y = "Number of Cars Passed",
    fill = "Gender"
  ) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

PROPORTION OF FIRST CAR YIELD RACE VISUAL


``` r
data %>% 
  ggplot(aes(x = ethnicity,
             fill = first_car_yield)
         ) +
  geom_bar(position = "fill") +
  labs (
    x = "Race",
    y = "Proportion",
    fill = "Did the First Car Yield"
  ) +
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

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
  theme_minimal()
```

![](racial_bias_analysis_files/figure-html/unnamed-chunk-38-1.png)<!-- -->
