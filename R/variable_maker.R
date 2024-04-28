#' Make a new variable
#'
#' Takes two existing variables and makes a new one, useful for when one metric
#' doesn't tell you much on its own, but two together might. Make sure to re-store
#' this in the place of your original data, as it simply adds a variable to all
#' of the data.
#'
#'
#'
#' @param data_list A team list
#' @param var1 the first variable you'd like to combine
#' @param var2 the second variable you'd like to combine
#' @param operator "sum", "sub", "div" "mult"
#' @param new_var_name (TODO: Explain this)
#'
#' @returns An updated list with the new variable in it
#' @examples
#' variable_maker(usu_2023, "GP", "GS", "sum", "gps")
#' # This returns a list similar to usu_t2023 but with an added variable in
#' # every list for "gps" which is a sum of the "GP" and "GS" functions. this
#' # works very similarly for any 2 variables. It also adds the team average to
#' # the "avg" player so that standardization in plots is easier.
#'
#'
#' @export
variable_maker <- function(data_list, var1, var2, operator = "sum",
                           new_var_name) {
  operation <- switch(operator,
                      "sum" = function(x, y) x + y,
                      "div" = function(x, y) x / y,
                      "sub" = function(x, y) x - y,
                      "mult" = function(x, y) x * y)
  new_values <- numeric(length(data_list))
  for (i in seq_along(data_list)) {
    sub_list <- data_list[[i]]
    new_var <- operation(sub_list[[var1]], sub_list[[var2]])
    sub_list[[new_var_name]] <- new_var
    new_values[i] <- new_var
    data_list[[i]] <- sub_list
  }
  avg_val <- mean(new_values, na.rm = TRUE)
  data_list$avg[[new_var_name]] <- avg_val

  return(data_list)
}
