#' Clean isotope data
#'
#' Reads raw CSV, renames isotope columns, assigns groups, and writes cleaned CSV.
#'
#' @param input_file Path to raw data file
#' @param output_file Path to save cleaned data
#' @return Cleaned dataframe
#' @export
clean_isotope_data <- function(input_file, output_file = "data/cleaned_data.csv") {
  data_file <- read.csv(input_file)

  names(data_file)[names(data_file) == "δ13C"] <- "carbon"
  names(data_file)[names(data_file) == "δ18O"] <- "oxygen"

  data_file <- data_file %>%
    dplyr::mutate(Group = dplyr::case_when(
      stringr::str_starts(Tooth, "BOS") ~ "cattle",
      stringr::str_starts(Tooth, "OC") ~ "ovicaprine",
      TRUE ~ NA_character_
    ))

  utils::write.csv(data_file, output_file, row.names = FALSE)
  return(data_file)
}