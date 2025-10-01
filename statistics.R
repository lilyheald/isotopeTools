#' Generate summary statistics and save as PNG
#'
#' @param data A data.frame with columns Tooth, carbon, and oxygen
#' @param dec Number of decimal places
#' @param output_dir Directory to save output
#' @return A summary table (invisibly) and saves a PNG file
#' @export
statistics_results <- function(data, dec, output_dir) {
  summary_table <- data %>%
    dplyr::group_by(Tooth) %>%
    dplyr::summarize(
      n = dplyr::n(),
      `Carbon Mean` = round(mean(carbon), dec),
      `Carbon Min.` = round(min(carbon), dec),
      `Carbon Max.` = round(max(carbon), dec),
      `Carbon Min. - Max.` = round((`Carbon Min.` - `Carbon Max.`), dec),
      `Carbon σ` = round(sd(carbon), dec),
      `Oxygen Mean` = round(mean(oxygen), dec),
      `Oxygen Min.` = round(min(oxygen), dec),
      `Oxygen Max.` = round(max(oxygen), dec),
      `Oxygen Min. - Max.` = round((`Oxygen Min.` - `Oxygen Max.`), dec),
      `Oxygen σ` = round(sd(oxygen), dec)
    )

  table_html <- summary_table %>%
    kableExtra::kable() %>%
    kableExtra::column_spec(1, extra_css = "white-space: nowrap;") %>%
    kableExtra::column_spec(1:ncol(summary_table), border_left = TRUE, border_right = TRUE) %>%
    kableExtra::kable_classic(full_width = TRUE, html_font = "Cambria")

  temp_html_file <- tempfile(fileext = ".html")
  kableExtra::save_kable(table_html, temp_html_file)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  file_name <- file.path(output_dir, "summary_statistics.png")

  webshot2::webshot(temp_html_file, file = file_name, vwidth = 800, vheight = 600)

  message("Saved: ", file_name)
  invisible(summary_table)
}