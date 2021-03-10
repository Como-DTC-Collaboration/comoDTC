#' Create a contact matrix
#'
#' Function that loads and processes a contact matrix for use in the CoMo model ode solver
#'
#' @param country String representing the country for which the contact matrix will
#' be loaded. Naming conventions for countries must be in line with those used
#' in input excel file.
#' @param contact_type A loaded matrix representing the contact between different age groups
#' in a given social setting e.g. contact_work gives contact matrix for the work place.
#' @param n_5yr_agegroups_popstruc Number of 5 year age groups starting with 0-4yr in the population.
#' @return A square contact matrix for specified country with dimension equal to \code{n_5yr_agegroups_popstruc}
#' @examples
#' get_contact_matrix("United Kingdom of Great Britain", contact_home, 21)
#'
get_contact_matrix <- function(country, contact_type, n_5yr_agegroups_popstruc){

  c <- contact_type[[country]] %>% as.matrix()
  contact <- matrix(0, nrow = n_5yr_agegroups_popstruc, ncol = n_5yr_agegroups_popstruc)

  for (i in 1:n_5yr_agegroups_popstruc){
    for(j in 1:n_5yr_agegroups_popstruc){
      contact[i,j] <- ifelse(i<=nrow(c) & j<=nrow(c), c[i,j],
                             ifelse(i<=nrow(c) & j>nrow(c), contact[i,nrow(c)], contact[nrow(c),j]))
    }
  }

  return(contact)
}

