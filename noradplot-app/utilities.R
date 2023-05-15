validateData <- function(data) {
  if (grepl("\\s+", data)) {
    # Data contains whitespace or tab characters
    return(TRUE)
  }
  # Add more format checks if needed
  # e.g., check for comma-separated data
  
  FALSE
}