raw_size_dat = (read.csv(here::here("analysis", "species_data", "species_list_working.csv"),  stringsAsFactors = F, strip.white = T, na.strings = ""))



clean_size_dat = (clean_sp_size_data(raw_size_dat))


fitted_pars = (get_sd_parameters(raw_size_dat))


sd_size_dat = (add_estimated_sds(clean_size_data = clean_size_dat,
                                       sd_pars = fitted_pars))

sp_mean_size_dat = (get_sp_mean_size(sd_size_dat))

write.csv(sp_mean_size_dat, "dat_to_include/species_mean_sd_sizes.csv", row.names = F)
write.csv(raw_size_dat, "dat_to_include/species_list_with_masses.csv", row.names = F)
