#' Estimate b (energy)
#'
#' @param body_mass mass
#' @return estimated b (metabolic rate) using pars from Fristoe 2015
#' @export

estimate_b <- function(body_mass) {
  return(10.5 * (body_mass ^ .713))
}


#' Make IBD (individual energy)
#'
#' @param an_isd an isd with column ind_size
#'
#' @return an_isd with added column for energy
#' @export
#'
#' @importFrom dplyr mutate
make_ibd <- function(an_isd) {

  an_isd <- an_isd %>%
    dplyr::mutate(ind_b = estimate_b(ind_size))

  return(an_isd)
}
