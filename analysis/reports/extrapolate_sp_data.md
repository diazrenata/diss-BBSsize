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
  select(-english_common_name, -french_common_name, -spanish_common_name, -sporder, -family) %>%
  mutate(mass = as.numeric(mass))

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

Could do:

1.  For each species id, get a mean and then extrapolate the sd from
    that. This raises the ?, what to do when some records for a species
    have an sd and some do not?
2.  Or, for every mean without an sd, extrapolate an sd. Then get
    average mean and sd for each species.

For now I am going to implement number 2.

``` r
sd_pars <- get_sd_parameters(raw_size_data = sp_raw)


for(i in 1:nrow(sp_clean)) {
  
  if(is.na(sp_clean$sd[i])) {
    sp_clean$estimated_sd[i] =  TRUE
    sp_clean$sd[i] = estimate_sd(sp_clean$mass[i], pars = sd_pars)
  } else {
    sp_clean$estimated_sd[i] = FALSE
  }
  
}
```

``` r
sp_means <- sp_clean %>%
  group_by(species_id, id, genus, species) %>%
  summarize(mean_mass = mean(mass),
            mean_sd = mean(sd),
            contains_estimates = any(estimated_sd)) %>%
  ungroup()
```

    ## `summarise()` has grouped output by 'species_id', 'id', 'genus'. You can override using the `.groups` argument.

``` r
ggplot(sp_means, aes(x = log(mean_mass), y = log(mean_sd), color = contains_estimates)) +
  geom_point() +
  theme_bw()
```

![](extrapolate_sp_data_files/figure-gfm/take%20mean%20by%20species%20ID-1.png)<!-- -->

``` r
mean(sp_means$contains_estimates)
```

    ## [1] 0.4465558
