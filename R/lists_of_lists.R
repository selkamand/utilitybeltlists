
#' Retrieve element from sublist of nested list
#'
#' @details
#' Will error if list is zero_length
#'
#' @param list_of_lists A list containing at least one sublist lists (list)
#' @param name_of_property The name of the property to retrieve. For example if your sublists have some element named 'petal_length', you could use name_of_property == petal_lengt h (string)
#' @param function_for_sublist_assertion Some function that takes one argument (teach sublist) and returns TRUE/FALSE. Function is fed into an assertion. By default, no assertion is applied (function). This property is useful if you want to check all sublists are of a certain class.
#' @family nested_lists
#' @return vector of the same length as the input list (vector)
#' @export
#'
list_of_lists_retrieve_second_level_property <- function(list_of_lists, name_of_property, function_for_sublist_assertion=function(x) { return(TRUE) }){
  #Assertions
  assert_that(is.function(function_for_sublist_assertion))
  assert_that(assertthat::is.string(name_of_property))
  assert_that(is_list_of_lists(list_of_lists))
  assert_that(length(list_of_lists) > 0)

  #Main
  main_result <- purrr::map(.x = list_of_lists, .f = function(second_level_objects) {
    assert_that(function_for_sublist_assertion(second_level_objects))
    assert_that(magrittr::is_in(name_of_property, names(second_level_objects)))
    return(second_level_objects[[name_of_property]])
  } ) %>%
    unlist
  #Output Assertions
  assert_that(length(main_result) == length(list_of_lists))
  assert_that(!is.list(main_result))
  return(main_result)
}


#' Check if an object is a list of lists
#'
#' @param object any R object (anything)
#'
#' @return TRUE if object is a list of lists, FALSE if it is not. Any non-list elements will cause this function to return FALSE
#' @export
#'
#' @family nested_lists
is_list_of_lists <- function(object){

  if(!is.list(object)) { return(FALSE) }

  purrr::map_lgl(object, .f = is.list) %>%
    all() %>%
    return()
}
