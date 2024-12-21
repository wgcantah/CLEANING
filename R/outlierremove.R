#' Title Removing Outliers from Data Frames
#'
#' @param df
#'
#' @returns Returns Cleaned Data Frame
#' @export
#'
#' @examples

outlierremove <- function(df) {
  # Identify numerical columns
  num_cols <- sapply(df, is.numeric)

  # Calculate quantiles and IQR for numerical columns
  Q1 <- apply(df[, num_cols], 2, quantile, probs = 0.25, na.rm = TRUE)
  Q3 <- apply(df[, num_cols], 2, quantile, probs = 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define outlier criteria
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Remove rows with outliers
  df_clean <- df
  for (col in names(df)[num_cols]) {
    df_clean <- df_clean[!(df_clean[[col]] < lower_bound[col] | df_clean[[col]] > upper_bound[col]), ]
  }

  return(df_clean)
}

