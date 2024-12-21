#' Title For Discretisation
#'
#' @param df
#' @param var
#' @param method
#' @param bins
#'
#' @returns
#' @export
#'
#' @examples
discretisation<- function(df, var, method = c("equal_width", "equal_frequency"), bins = 4) {
  # Ensure the method is one of the allowed values
  method <- match.arg(method)

  # Select the specified variable
  variable <- df[[var]]

  # Perform discretization based on the method
  if (method == "equal_width") {
    # Equal-width binning
    breaks <- seq(min(variable, na.rm = TRUE), max(variable, na.rm = TRUE), length.out = bins + 1)
    discretized_var <- cut(variable, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  } else if (method == "equal_frequency") {
    # Equal-frequency binning
    discretized_var <- cut(variable, breaks = quantile(variable, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
  }

  # Add the discretized variable to the data frame
  df[[paste(var, "discretized", sep = "_")]] <- discretized_var

  return(df)
}
