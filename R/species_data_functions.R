#' List species
#'
#' @param a_dataset matss dataset
#'
#' @return list of species names
#' @export
#'
list_species <- function(a_dataset) {

    species_table <- a_dataset$metadata$species_table

    return(as.data.frame(species_table))

}


fit_mean_sd <- function(species_size_data) {

}
