# Coordinate conversion from Degrees Decimal.Minutes to DD using R
# Laura Feyrer 2023
#now included in functions_WSDBprep.R project

# #libraries
# library(readr)
# library(here)
# 
# input_file <-"CoriolisMultiBeamValidatedEntry2023.csv"
# species = "speciesCodes.csv" 

#read in csv of data with Degrees Decimal Minute Lat/Long in the first 4 separate variables 

convert_to_decimal_degrees <- function(coord) {
  # Remove degree symbols and other unwanted characters
  coord <- iconv(coord, from = "latin1", to = "ASCII", sub = "")
  
  # Handle coordinates with 'd' for degrees and cardinal directions
  coord <- gsub("d", " ", coord)
  coord <- gsub("[NW]", "", coord)
  
  # Insert a space between degrees and minutes if needed
  if (grepl("^\\d{4,}", coord)) {
    coord <- paste0(substr(coord, 1, 2), " ", substr(coord, 3, nchar(coord)))
  }
  # Trim leading and trailing spaces
  coord <- trimws(coord)
  
  #remove any characters except numbers
  cleaned_coord <- gsub("[^-0-9. ]", "", coord)
 
  # Correct instances of double decimal points
  if (grepl("\\.{2,}", cleaned_coord)) {
    cleaned_coord <- gsub("\\.{2,}", ".", cleaned_coord)
  }
  
  # Split by space to get degrees, minutes, and seconds
  components <- unlist(strsplit(cleaned_coord, " "))
  
  # Check the number of components to determine the format
  n <- length(components)
  
  if (n == 1) {
    # Already in decimal format
    return(round(as.numeric(components[1]), 5))
  } else if (n == 2) {
    # Degrees and decimal minutes
    degrees <- as.numeric(components[1])
    minutes <- as.numeric(components[2])
    return(round(degrees + (minutes / 60), 5))
  } else if (n == 3) {
    # Degrees, minutes, and seconds
    degrees <- as.numeric(components[1])
    minutes <- as.numeric(components[2])
    seconds <- as.numeric(components[3])
    return(round(degrees + (minutes / 60) + (seconds / 3600), 5))
  } else {
    # Unknown format
    return(NA)
  }
}

# Function to troubleshoot and find the problematic latitude
troubleshoot_latitudes <- function(latitudes) {
  problems <- NULL
  
  for (i in seq_along(latitudes)) {
    result <- convert_to_decimal_degrees(latitudes[i])
    if (is.na(result)) {
      problems <- c(problems, latitudes[i])
    }
  }
  
  return(problems)
}


