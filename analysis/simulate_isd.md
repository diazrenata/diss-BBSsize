Size data analysis
================

``` r
loadd(bbs_rtrg_1_4)
loadd(sp_mean_size_dat)
```

``` r
long_dat <- bbs_rtrg_1_4$abundance %>%
  mutate(year = bbs_rtrg_1_4$covariates$year) %>%
  tidyr::pivot_longer(-year, names_to = "id", values_to = "nind")
```

``` r
long_dat <- long_dat %>%
  left_join(sp_mean_size_dat) %>%
  select(year, nind, id, mean_mass, mean_sd)
```

    ## Joining, by = "id"

``` r
set.seed(1977)

ind_size_dat <- list()

for(i in 1:nrow(long_dat)) {
  if(long_dat$nind[i] > 0 ) {
  ind_size_dat[[i]] <- data.frame(
    year = long_dat$year[i],
    id = long_dat$id[i],
    ind_size = rnorm(n = long_dat$nind[i], mean = long_dat$mean_mass[i], sd = long_dat$mean_sd[i]),
    stringsAsFactors = F
  )
  }
}

ind_size_dat <- bind_rows(ind_size_dat)
```

``` r
ggplot(ind_size_dat, aes(x = ind_size)) +
  geom_density() +
  theme_bw()+
  ggtitle("All years")
```

![](simulate_isd_files/figure-gfm/plot%20for%20fun-1.png)<!-- -->

``` r
ggplot(ind_size_dat, aes(x = log(ind_size))) +
  geom_density() +
  theme_bw()+
  ggtitle("All years")
```

![](simulate_isd_files/figure-gfm/plot%20for%20fun-2.png)<!-- -->

``` r
ggplot(filter(ind_size_dat, year == 1994), aes(x = log(ind_size))) +
  geom_density() +
  theme_bw() +
  ggtitle("One year")
```

![](simulate_isd_files/figure-gfm/plot%20for%20fun-3.png)<!-- -->

``` r
set.seed(1977)

ind_size_dat2 <- simulate_size_dat(bbs_rtrg_1_4, sp_mean_size_dat)
```

    ## Joining, by = "id"

``` r
ggplot(ind_size_dat2, aes(x = ind_size)) +
  geom_density() +
  theme_bw()+
  ggtitle("All years")
```

![](simulate_isd_files/figure-gfm/try%20fxn-1.png)<!-- -->

``` r
ggplot(ind_size_dat2, aes(x = log(ind_size))) +
  geom_density() +
  theme_bw()+
  ggtitle("All years")
```

![](simulate_isd_files/figure-gfm/try%20fxn-2.png)<!-- -->

``` r
ggplot(filter(ind_size_dat2, year == 1994), aes(x = log(ind_size))) +
  geom_density() +
  theme_bw() +
  ggtitle("One year")
```

![](simulate_isd_files/figure-gfm/try%20fxn-3.png)<!-- -->
