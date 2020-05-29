Size data analysis
================

``` r
sp_raw <- read.csv(here::here("analysis", "species_data", "species_list_working.csv"), stringsAsFactors = F, strip.white = T, na.strings = "")
```

### Data cleanup

Trivial:

  - remove common names columns (special characters get messed up in
    Excel)
  - rename `species_id` column (got messed up in excel)

Some substance:

  - For species that have been split, fill in values

<!-- end list -->

``` r
colnames(sp_raw)[1] <- "species_id"

sp_clean <- sp_raw %>%
  select(-english_common_name, -french_common_name, -spanish_common_name, -sporder, -family) 

name_change <- filter(sp_clean, not_in_dunning == 1)

sp_clean <- filter(sp_clean, is.na(not_in_dunning)) %>%
  mutate(added_flag = NA)

for(i in 1:nrow(name_change)) {
    
  if(!is.na(name_change$close_subspecies[i])) {
  matched_rows <- filter(sp_clean,
                         genus == name_change$close_genus[i],
                         species == name_change$close_species[i],
                         subspecies == name_change$close_subspecies[i])
  } else {
    matched_rows <- filter(sp_clean,
                         genus == name_change$close_genus[i],
                         species == name_change$close_species[i])
  }
  
  sp_to_add <- matched_rows %>%
    mutate(species_id = name_change$species_id[i],
           id = name_change$id[i],
           added_flag = 1)
  
  sp_clean <- bind_rows(sp_clean, sp_to_add)
  
}
```

### SD fit

``` r
sp_for_sd <- filter(sp_raw,
                    !is.na(sd)) %>%
  mutate(mass = as.numeric(mass),
         sd = as.numeric(sd)) %>%
  mutate(var = sd^2) %>%
  mutate(log_m = log(mass),
         log_var = log(var))


sd_fit <- lm(sp_for_sd, formula = log_var ~ log_m)

summary(sd_fit)
```

    ## 
    ## Call:
    ## lm(formula = log_var ~ log_m, data = sp_for_sd)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.1567 -0.4005 -0.0103  0.4502  8.2246 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -5.27312    0.12738  -41.40   <2e-16 ***
    ## log_m        1.99461    0.03302   60.41   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.065 on 566 degrees of freedom
    ## Multiple R-squared:  0.8657, Adjusted R-squared:  0.8655 
    ## F-statistic:  3649 on 1 and 566 DF,  p-value: < 2.2e-16

``` r
sp_for_sd <- mutate(sp_for_sd,
                    log_var_est = -5.273 + (log_m * 1.995)) %>%
  mutate(var_est = exp(log_var_est))


ggplot(sp_for_sd, aes(x = log_m, y = log_var)) +
  geom_point(alpha = .4) +
  theme_bw() +
  geom_line(aes(x = log_m, y = log_var_est))
```

![](sd_relationship_files/figure-gfm/fit%20sd-1.png)<!-- -->

``` r
ggplot(sp_for_sd, aes(x = mass, y = var)) +
  geom_point(alpha = .4) +
  theme_bw() +
  geom_line(aes(x = mass, y = var_est))
```

![](sd_relationship_files/figure-gfm/fit%20sd-2.png)<!-- -->
