Homework 6
================
Jeanette Shekelle
11/27/2018

### Introduction:

The purpose of this document is to practice fitting linear models for P8105 Homework 6.

### Problem 1

#### Reading in the data:

``` r
homicide_data = read_csv(file = "./data/homicide_data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

Cleaning the data:

``` r
homicide_data_clean = 
  homicide_data %>% 
  janitor::clean_names() %>% 
  mutate(city_state = paste(city, state, sep = ', '),
         solved = as.factor(ifelse(disposition == 'Closed by arrest', 'solved', 'unsolved'))) %>% 
  filter(!(city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL"))) %>% 
  mutate(victim_race = as.factor(ifelse(victim_race == 'White', 'white', 'non-white' )),
         victim_age = as.numeric(victim_age),
         victim_sex = as.factor(victim_sex),
         victim_race = fct_relevel(victim_race, "white"))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

Fitting logistic regression using glm:

``` r
fit_logistic = 
  homicide_data_clean%>% 
  filter(city_state == "Baltimore, MD") %>%
  glm(solved ~ victim_age + victim_race + victim_sex, data = ., family = binomial()) %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate),
         low_OR = exp(estimate * 1.96 - std.error),
         high_OR = exp(estimate * 1.96 + std.error)) %>%
  select(OR, low_OR, high_OR, p.value) %>% 
  knitr::kable(digits = 3)
```
