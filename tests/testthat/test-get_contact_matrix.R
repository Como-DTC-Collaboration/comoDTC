#testing for the get_contact_matrix.R function
test_that("get_contact_matrix produces expected 21x21 square matrix output", {

  #load("./data/contact_home.rda")
  #run the function with Great Britain and contact_home
  home_matrix <- get_contact_matrix("United Kingdom of Great Britain",
                                    contact_home, 21)

  #expect the length of both dimensions of the output to be equal to 21.
  expect_length(home_matrix[1, ], 21)
  expect_length(home_matrix[, 1], 21)
})
