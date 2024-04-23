variable_maker <- function(data_list, var1, var2, operator = "sum", new_var_name) {
  operation <- switch(operator,
                      "sum" = function(x, y) x + y,
                      "div" = function(x, y) x / y,
                      "sub" = function(x, y) x - y,
                      "mult" = function(x, y) x * y)

  if (!(var1 %in% names(data_list) && var2 %in% names(data_list))) {
    stop("Variables not found in the list")
  }

  new_var <- operation(data_list[[var1]], data_list[[var2]])

  data_list[[new_var_name]] <- new_var

  avg_val <- mean(new_var, na.rm = TRUE)

  data_list[[new_var_name]] <- data_list[[new_var_name]] / avg_val

  return(data_list)
}

