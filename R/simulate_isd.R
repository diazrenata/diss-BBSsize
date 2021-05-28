#' Simulate ISD
#'
#' @param a_dataset matss dataset
#' @param mean_size_data table of id, sd, mean
#'
#' @return long dataframe of timestep, species, and individual size drawn from rnorm
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate left_join select bind_rows
#' @importFrom here here
simulate_size_dat <- function(a_dataset, mean_size_data = NULL) {

  # Make long
  long_dat <- a_dataset$abundance %>%
    dplyr::mutate(year = a_dataset$covariates$year) %>%
    tidyr::pivot_longer(-year, names_to = "id", values_to = "nind")

  # Add means and sds
  long_dat <- long_dat %>%
    dplyr::left_join(mean_size_data) %>%
    dplyr::select(year, nind, id, mean_mass, mean_sd)


  ind_size_dat <- list()

  for(i in 1:nrow(long_dat)) {
    if(long_dat$nind[i] > 0 ) {
      ind_size_dat[[i]] <- data.frame(
        year = long_dat$year[i],
        id = long_dat$id[i],
        ind_size = rnorm(n = long_dat$nind[i], mean = long_dat$mean_mass[i], sd = long_dat$mean_sd[i]),
        stringsAsFactors = F)

      while(any(ind_size_dat[[i]]$ind_size < 0)) {

        negative_sizes <- which(ind_size_dat[[i]]$ind_size < 0)

        ind_size_dat[[i]]$ind_size[negative_sizes] <-  rnorm(n = length(negative_sizes), mean = long_dat$mean_mass[i], sd = long_dat$mean_sd[i])

      }

    }
  }

  ind_size_dat <- dplyr::bind_rows(ind_size_dat)
}

