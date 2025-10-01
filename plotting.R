#' Dual-axis isotope plots for each tooth
#' @export
dual_axes <- function(data, carbon_color, oxygen_color, bos_primary, bos_secondary,
                      oc_primary, oc_secondary, line_width, point_size, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  unique_teeth <- unique(data$Tooth)

  for (tooth_id in unique_teeth) {
    subset_data <- dplyr::filter(data, Tooth == tooth_id)
    if (nrow(subset_data) == 0) next
    group_name <- unique(subset_data$Group)

    if (group_name == "cattle") {
      carbon_axis <- bos_primary
      oxygen_axis <- bos_secondary
    } else if (group_name == "ovicaprine") {
      carbon_axis <- oc_primary
      oxygen_axis <- oc_secondary
    } else next

    b <- diff(carbon_axis) / diff(oxygen_axis)
    a <- carbon_axis[1] - b * oxygen_axis[1]

    p <- ggplot2::ggplot(subset_data, ggplot2::aes(x = ERJ, y = carbon)) +
      ggplot2::geom_line(color = carbon_color, linewidth = line_width) +
      ggplot2::geom_line(ggplot2::aes(y = a + oxygen * b), color = oxygen_color, linewidth = line_width) +
      ggplot2::geom_point(color = carbon_color, size = point_size) +
      ggplot2::geom_point(ggplot2::aes(y = a + oxygen * b), color = oxygen_color, size = point_size) +
      ggplot2::scale_y_continuous(
        bquote('δ'^13~'C (vPDB‰)'),
        sec.axis = ggplot2::sec_axis(~ (. - a) / b, name = bquote('δ'^18~'O (vPDB‰)'))
      ) +
      ggplot2::xlab("Distance from ERJ (mm)") +
      ggplot2::theme_bw() +
      ggplot2::scale_x_reverse(limits = c(45, 0)) +
      ggplot2::coord_cartesian(ylim = carbon_axis)

    file_name <- file.path(output_dir, paste0(group_name, "_", gsub("#", "_", tooth_id), ".png"))
    ggplot2::ggsave(file_name, plot = p, width = 6, height = 4, dpi = 300)
    message("Saved: ", file_name)
  }
}

#' Save carbon plots for all species and elements
#' @export
save_all_indiv_carbon_plots <- function(data, species_colors, output_dir) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  unique_species <- unique(data$Species)
  unique_elements <- unique(data$Element)

  for (species in unique_species) {
    for (element in unique_elements) {
      subset_data <- dplyr::filter(data, Species == species & Element == element)
      if (nrow(subset_data) == 0) next

      p <- ggplot2::ggplot(subset_data, ggplot2::aes(x = ERJ, y = carbon, group = Tooth, color = Species)) +
        ggplot2::scale_x_reverse() +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = -10, linetype = "dashed", color = "black") +
        ggplot2::scale_color_manual(values = species_colors) +
        ggplot2::labs(title = paste0(species, " ", element),
             x = "Distance from Enamel Root Junction (mm)",
             y = bquote('δ'^13~'C (vPDB‰)')) +
        ggplot2::theme_bw() +
        ggplot2::annotate("text",
               x = max(subset_data$ERJ, na.rm = TRUE) * 0.9, y = -9.7,
               label = "100% C3 consumer", size = 4, color = "black")

      file_name <- file.path(output_dir, paste0(species, "_", element, ".png"))
      ggplot2::ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)
      message("Saved: ", file_name)
    }
  }
}