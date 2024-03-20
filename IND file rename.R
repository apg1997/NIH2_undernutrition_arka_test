# Set the directory path
directory <- "C:/Users/ArkaprabhaGun/Documents/NIH2_undernutrition_arka_test/countries/IND/data/Undernutrition/"

# Get list of all .txt files in the directory
files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)

# Loop through each file
for(file in files) {
  # Read the content of the file
  content <- tryCatch(
    {
      readLines(file)
    },
    error = function(e) {
      message("Error reading file: ", file)
      return(NULL)
    }
  )
  
  if (is.null(content)) {
    next  # Skip to the next file if there's an error reading this one
  }
  
  # Replace entries containing "IND" with "IND"
  content <- gsub("ind", "IND", content)
  
  # Overwrite the original file with the modified content
  writeLines(content, con = file)
  
  # Optional: Print confirmation message for each file processed
  cat("Processed:", file, "\n")
}
