# function that loads and processes a contact matrix
# for use in the como model ode solver
get_contact_matrix <- function(country, contact_type, n_agegroups_popstruc){
  #inputs:
  # coutry: string representing the country for which the contact matrix will
  # be loaded. Naming conventions for countries must be in line with those used
  # in input excel file

  #contact_type: a loaded matrix representing the contact between different age groups
  # in a given social setting e.g. contact_work gives contact matrix for the work
  #place

  #
  c <- contact_type[[country]] %>% as.matrix()
  contact <- matrix(0, nrow = n_agegroups_popstruc, ncol = n_agegroups_popstruc)

  for (i in 1:n_agegroups_popstruc){
    for(j in 1:n_agegroups_popstruc){
      contact[i,j] <- ifelse(i<=nrow(c) & j<=nrow(c), c[i,j],
                             ifelse(i<=nrow(c) & j>nrow(c), contact[i,nrow(c)], contact[nrow(c),j]))
    }
  }

  return(contact)
}

