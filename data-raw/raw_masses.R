## code to prepare `raw_masses` dataset goes here

raw_masses <- read.csv(here::here("dat_to_include", "species_list_with_masses.csv"))

usethis::use_data(raw_masses, overwrite = TRUE)
