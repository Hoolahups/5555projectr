variable_maker <- function(dframe, var1, operator = "sum", var2, new_var_name) {
  operation <- switch(operator,
                      "sum" = ~ .data[[var1]] + .data[[var2]],
                      "div" = ~ .data[[var1]] / .data[[var2]],
                      "sub" = ~ .data[[var1]] - .data[[var2]],
                      "mult" = ~ .data[[var1]] * .data[[var2]])

  dframe <- dframe %>%
    mutate(!!new_var_name := !!operation)

  avg_val <- dframe %>%
    filter(row.names(.) == "avg") %>%
    pull(!!new_var_name)

  dframe <- dframe %>%
    mutate(!!new_var_name := .data[[new_var_name]] / avg_val)

  return(dframe)
}
