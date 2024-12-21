#' Title For generating Z-Scores
#'
#' @param df
#' @param variable
#'
#' @returns
#' @export
#'
#' @examples
zscore <- function(df, variable) {
  # Calculate the mean and standard deviation of the specified variable
  mean_A <- mean(df[[variable]], na.rm = TRUE)
  std_A <- sd(df[[variable]], na.rm = TRUE)

  # Apply the Z-score normalization formula
  df[[paste0(variable, "_zscore")]] <- (df[[variable]] - mean_A) / std_A

  return(df)
}
