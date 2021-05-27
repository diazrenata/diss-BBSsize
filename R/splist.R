make_full_species_list <- function() {

  datasets <- MATSS::build_bbs_datasets_plan()

  rtrgs <- datasets$target

  rtrgs_split <- strsplit(rtrgs, split = "_")

  toload <- data.frame(dataset_name = rtrgs,
                       route = NA,
                       region = NA)

  for(i in 1:nrow(toload)) {
    toload$route[i] <- rtrgs_split[[i]][[3]]
    toload$region[i] <- rtrgs_split[[i]][[4]]
  }


  splists <- list()

  for(i in 1:nrow(toload)) {
    this_dat <- MATSS::get_bbs_route_region_data(route = toload$route[i], region = toload$region[i])
    splists[[i]] <- this_dat$metadata$species_table
  }

  splist <- dplyr::bind_rows(splists) %>%
    dplyr::distinct()

  write.csv(splist, "dat_to_include/species_list_from_MATSS.csv", row.names = F)

}
