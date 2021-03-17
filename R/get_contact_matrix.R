#' Create a contact matrix
#'
#' Function that loads and processes a contact matrix for use in the CoMo model
#' ode solver. This function carries out a dimension change on the input
#' contact matrix (from 16x16 to 21x21) to allow for the larger number of
#' population categories provided by the population data
#' (0-100+ years in 5 year intervals) compared to the contact data (0-80 in 5
#' year intervals). This is done by repeating entries in the contact data
#' corresponding to the oldest group (75-80 years) for the last population data
#' age ranges (i.e. all 5 year categories for 80+ years).
#'
#' @param country String representing the country for which the contact matrix
#' will be created. Naming conventions for countries must be in line with those
#' used in input excel file.
#' @param contact_type A named list of 16x16 matrices. Names are countries for
#' which contact matrices are available. Each contact matrix represents the
#' contact between different age groups in a given social setting e.g.
#' contact_work gives contact matrix for the work place.
#' @param n_5yr_agegroups_popstruc Number of 5 year age groups in the population
#'  structure data. These ranges start with 0-4yrs and continue with 5 year gaps
#'  up until the last category of 100+ years old. Default value set to 21.
#'
#' @return A square contact matrix for the specified country with dimension
#' equal to \code{n_5yr_agegroups_popstruc}.
#'
get_contact_matrix <- function(country, contact_type
                               , n_5yr_agegroups_popstruc = 21) {
  #choose contact matrix for given country
  c <- contact_type[[country]] %>% as.matrix()

  #create a square matrix of 0s with dimension n_5yr_agegroups_popstruc
  contact <- matrix(0, nrow = n_5yr_agegroups_popstruc,
                    ncol = n_5yr_agegroups_popstruc)

  #name dimensions of the matrix with age ranges defined in population data
  colnames(contact) <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                         "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                         "60-64", "65-69", "70-74", "75-79", "80-84", "85-89",
                         "90-94", "95-99", "100+")

  rownames(contact) <- colnames(contact)

  #loop over dimensions of contact, assign the elements of contact
  #corresponding to the first 16 age groups to be the corresponding elements of
  #the matrix c. assign the elements in the last 5 age groups of contact
  #to be the elements of c corresponding to the oldest age
  for (i in 1:n_5yr_agegroups_popstruc) {
    for (j in 1:n_5yr_agegroups_popstruc) {
      contact[i, j] <- ifelse(i <= nrow(c) & j <= nrow(c), c[i, j],
                             ifelse(i <= nrow(c) & j > nrow(c),
                                    contact[i, nrow(c)], contact[nrow(c), j]))
    }
  }
  #return new, larger contact matrix
  return(contact)
}
