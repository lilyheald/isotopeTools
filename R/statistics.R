#' Generate summary statistics and save as PNG
#'
#' @param data A data.frame with columns Tooth, carbon, and oxygen
#' @param dec Number of decimal places
#' @param output_dir Directory to save output
#' @return A summary table (invisibly) and saves a PNG file
#' @export
statistics_results <- function(data, dec, output_dir) {
  
  # Calculate summary statistics
  summary_table <- dplyr::summarise(
    dplyr::group_by(data, Tooth),
    n = dplyr::n(),
    `Carbon Mean` = round(mean(carbon), dec),
    `Carbon Min.` = round(min(carbon), dec),
    `Carbon Max.` = round(max(carbon), dec),
    `Carbon Min. - Max.` = round((`Carbon Min.` - `Carbon Max.`), dec),
    `Carbon σ` = round(stats::sd(carbon), dec),
    `Oxygen Mean` = round(mean(oxygen), dec),
    `Oxygen Min.` = round(min(oxygen), dec),
    `Oxygen Max.` = round(max(oxygen), dec),
    `Oxygen Min. - Max.` = round((`Oxygen Min.` - `Oxygen Max.`), dec),
    `Oxygen σ` = round(stats::sd(oxygen), dec)
  )
  
  # Build HTML table using fully qualified calls
  table_html <- kableExtra::kable(summary_table)
  table_html <- kableExtra::column_spec(table_html, 1, extra_css = "white-space: nowrap;")
  table_html <- kableExtra::column_spec(table_html, 1:ncol(summary_table), border_left = TRUE, border_right = TRUE)
  table_html <- kableExtra::kable_classic(table_html, full_width = TRUE, html_font = "Cambria")
  
  # Temporary HTML file
  temp_html_file <- tempfile(fileext = ".html")
  kableExtra::save_kable(table_html, temp_html_file)
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Save as PNG using webshot2
  file_name <- file.path(output_dir, "summary_statistics.png")
  webshot2::webshot(temp_html_file, file = file_name, vwidth = 800, vheight = 600)
  
  message("Saved: ", file_name)
  
  # Return summary table invisibly
  invisible(summary_table)
}
