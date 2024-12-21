#' Title Outlier Detector
#'
#' @param df
#'
#' @returns Prints out all outliers in the Data Set
#' @export
#'
#' @examples
find_outliers <- function(df) {
  # Identify numeric columns
  num_cols <- sapply(df, is.numeric)

  # Initialize an empty list to store outlier values
  outliers_list <- list()

  # Iterate over numeric columns to find outliers
  for (col in names(df)[num_cols]) {
    Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    # Identify outliers
    outliers <- df[df[[col]] < lower_bound | df[[col]] > upper_bound, col, drop = FALSE]

    # Store outliers in the list
    if (nrow(outliers) > 0) {
      outliers_list[[col]] <- outliers
    }
  }

  # Print outliers
  if (length(outliers_list) > 0) {
    for (col in names(outliers_list)) {
      cat("Outliers in column", col, ":\n")
      print(outliers_list[[col]])
      cat("\n")
    }
  } else {
    cat("No outliers found.\n")
  }
}

