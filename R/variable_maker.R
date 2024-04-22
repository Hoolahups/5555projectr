variable_maker <- function(dframe, var1, operator = "sum", var2, new_var_name) {
  # Dynamically create the expression based on the operator
  operation <- switch(operator,
                      "sum" = ~ .data[[var1]] + .data[[var2]],
                      "div" = ~ .data[[var1]] / .data[[var2]],
                      "sub" = ~ .data[[var1]] - .data[[var2]],
                      "mult" = ~ .data[[var1]] * .data[[var2]])

  # Apply the operation to create a new variable
  dframe <- dframe %>%
    mutate(!!new_var_name := !!operation)

  # Calculate the value for the 'avg' row
  avg_val <- dframe %>%
    filter(row.names(.) == "avg") %>%
    pull(!!new_var_name)

  # Normalize all the new variable values by the 'avg' row's new variable value
  dframe <- dframe %>%
    mutate(!!new_var_name := .data[[new_var_name]] / avg_val)

  return(dframe)
}
