#' Title Normalisation of Variable between 0 and 1
#'
#' @param df
#' @param variable
#' @param new_min_A
#' @param new_max_A
#'
#' @returns
#' @export
#'
#' @examples
normalise <- function(df, variable, new_min_A, new_max_A) {
  # Get the minimum and maximum values of the specified variable
  min_A <- min(df[[variable]], na.rm = TRUE)
  max_A <- max(df[[variable]], na.rm = TRUE)

  # Apply the normalization formula
  df[[paste0(variable, "_normalized")]] <- (df[[variable]] - min_A) / (max_A - min_A) * (new_max_A - new_min_A) + new_min_A

  return(df)
}
