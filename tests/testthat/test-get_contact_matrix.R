#testing for the get_contact_matrix.R function
test_that("get_contact_matrix produces expected 21x21 square matrix output", {

  #run the function with Great Britain and contact_home
  home_matrix <- get_contact_matrix("United Kingdom of Great Britain",
                                    contact_home)

  #expect the length of both dimensions of the output to be equal to 21.
  expect_length(home_matrix[1, ], 21)
  expect_length(home_matrix[, 1], 21)
})

test_that("get_contact_matrix copies contact entries for 75-80 years category
          succesfully for age groups 80-100+", {

  #run the function with Great Britain and contact_home
  home_matrix <- get_contact_matrix("United Kingdom of Great Britain",
                                    contact_home)


  #test whether the entries of output matrix corresponding to age categories 80+
  # equal the contacts of the last age category

  #loop over entries 16:21 of output matrix, test whether these are equal to
  #entry 16.
  for (i in 16:21) {
    expect_equal(home_matrix[i, ], home_matrix[16, ])
    expect_equal(home_matrix[, i], home_matrix[, 16])
  }

})
