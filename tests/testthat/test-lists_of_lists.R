test_that("extracting sublist properties works", {
  list_of_lists <- list(
    list(NAME="Bob"),
    list(NAME="Billy"),
    list(NAME="Fred", ROLE="COBBLER")
  )

  list_of_lists_missing_name <- list(
    list(AHHH="Bob"),
    list(NAME="Billy"),
    list(NAME="Fred", ROLE="COBBLER")
  )

  list_of_vectors <- list(
    c(NAME="BOB"),
    c(NAME="Billy"),
    c(NAME="FRED")
  )

  empty_list <- list()

  expect_equivalent(list_of_lists_retrieve_second_level_property(list_of_lists, name_of_property = "NAME"), c("Bob", "Billy", "Fred"))
  expect_error(list_of_lists_retrieve_second_level_property(list_of_lists, name_of_property = "PROPERTYDOESNTEXIST"))
  expect_error(list_of_lists_retrieve_second_level_property(list_of_lists_missing_name, name_of_property = "NAME"))
  expect_error(list_of_lists_retrieve_second_level_property(list_of_vectors, name_of_property = "NAME"))
  expect_error(list_of_lists_retrieve_second_level_property(empty_list, name_of_property = "NAME"))
})
