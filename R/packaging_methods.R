#' Summarize over individuals
#'
#' To create a MATSS compatible dataset
#'
#' @param an_ibd cols for ind_size, ind_b
#' @param currency "mass" or "energy"
#'
#' @return species totals
#' @export
#'
#' @importFrom dplyr group_by summarize ungroup
#' @importFrom tidyr pivot_wider
summarize_over_individuals <- function(an_ibd, currency) {

  if(currency == "mass") {
    community_ts <- an_ibd %>%
      dplyr::group_by(year, id) %>%
      dplyr::summarize(mass = sum(ind_size)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = year, names_from = id, values_from = mass, values_fill = 0)
  } else if (currency == "energy") {
    community_ts <- an_ibd %>%
      dplyr::group_by(year, id) %>%
      dplyr::summarize(energy = sum(ind_b)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(id_cols = year, names_from = id, values_from = energy, values_fill = 0)
  }

  return(community_ts)

}


#' Swap currencies
#'
#' @param a_dataset a MATSS dataset
#' @param an_ibd an ibd
#' @param currency which currency to put in
#'
#' @return MATSS dataset with different currency
#' @export
#'
#' @importFrom dplyr tibble
#'
swap_currencies <- function(a_dataset, an_ibd, currency) {

  new_ts <- summarize_over_individuals(an_ibd = an_ibd, currency = currency)

  if(nrow(new_ts) == nrow(a_dataset$abundance)) {

    new_dataset <- a_dataset
    new_dataset$abundance <-tibble::tibble(dplyr::select(new_ts, - year))
    new_dataset$metadata$currency <- currency

    return(new_dataset)
      } else {
    return(NA)
  }

}
