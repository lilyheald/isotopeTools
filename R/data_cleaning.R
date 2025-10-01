#' Clean isotope data
#'
#' Reads a CSV file, renames isotope columns, adds Group classification,
#' and returns a cleaned data.frame.
#'
#' @param file Path to raw isotope CSV
#' @return A cleaned data.frame
#' @export
clean_isotope_data <- function(file) {
  data_file <- utils::read.csv(file)
  
  # Rename columns
  names(data_file)[names(data_file) == "δ13C"] <- "carbon"
  names(data_file)[names(data_file) == "δ18O"] <- "oxygen"
  
  # Add group
  data_file <- dplyr::mutate(
    data_file,
    Group = dplyr::case_when(
      stringr::str_starts(Tooth, "BOS") ~ "cattle",
      stringr::str_starts(Tooth, "OC") ~ "ovicaprine",
      TRUE ~ NA_character_
    )
  )
  
  return(data_file)
}
