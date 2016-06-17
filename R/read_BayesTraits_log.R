#' Read BayesTraits log
#'
#' Read in an arbitrary BayesTraits log file and return with
#' sanitized headers
#'
#' @param x Log file to read
#'
#' @return `data.frame` with the contents of the log file.
#'
#' @export
#'
read_BayesTraits_log <- function(x) {
  # Read whole file in
  log_file_lines <- readLines(x)

  # Find the header row
  header_row <- logical(length = length(log_file_lines))
  for (i in 1:length(log_file_lines)) {
    if (grepl("^Iteration\\t", log_file_lines[[i]])) {
      header_row[i] <- TRUE
    }
  }
  header_row <- which.max(header_row)

  log_file <- read_tsv(x, skip = header_row - 1) %>%
    as.data.frame()

  # Clean up column names
  names(log_file) <- gsub(" ", "_", names(log_file), fixed = TRUE)
  names(log_file) <- gsub("_-", "", names(log_file), fixed = TRUE)
  names(log_file) <- gsub(",", "_", names(log_file), fixed = TRUE)
  names(log_file) <- gsub("(", "_", names(log_file), fixed = TRUE)
  names(log_file) <- gsub(")", "", names(log_file), fixed = TRUE)
  names(log_file) <- gsub("-", "_", names(log_file), fixed = TRUE)
  names(log_file) <- gsub("^", "", names(log_file), fixed = TRUE)

  # Drop the empty last column
  log_file <- log_file[, 1:(ncol(log_file) - 1)]

  return(log_file)
}
