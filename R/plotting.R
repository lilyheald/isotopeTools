#' Dual-axis carbon and oxygen plot per tooth
#'
#' Creates and saves a dual-axis plot for each tooth showing δ13C and δ18O.
#'
#' @param data Cleaned isotope data.frame
#' @param carbon_color Color for carbon line/points
#' @param oxygen_color Color for oxygen line/points
#' @param bos_primary Numeric vector of min/max carbon for cattle
#' @param bos_secondary Numeric vector of min/max oxygen for cattle
#' @param oc_primary Numeric vector of min/max carbon for ovicaprine
#' @param oc_secondary Numeric vector of min/max oxygen for ovicaprine
#' @param line_width Line width
#' @param point_size Point size
#' @param output_dir Directory to save plots
#' @export
dual_axes <- function(data, carbon_color, oxygen_color,
                      bos_primary, bos_secondary,
                      oc_primary, oc_secondary,
                      line_width, point_size, output_dir) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  unique_teeth <- unique(data$Tooth)
  
  for (tooth_id in unique_teeth) {
    subset_data <- data[data$Tooth == tooth_id, ]
    if (nrow(subset_data) == 0) next
    
    group_name <- unique(subset_data$Group)
    if (group_name == "cattle") {
      carbon_axis <- bos_primary
      oxygen_axis <- bos_secondary
    } else if (group_name == "ovicaprine") {
      carbon_axis <- oc_primary
      oxygen_axis <- oc_secondary
    } else next
    
    # Scaling for secondary axis
    b <- diff(carbon_axis) / diff(oxygen_axis)
    a <- carbon_axis[1] - b * oxygen_axis[1]
    
    # Create plot
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
      ggplot2::theme(
        axis.line.y.right = ggplot2::element_line(color = oxygen_color),
        axis.ticks.y.right = ggplot2::element_line(color = oxygen_color),
        axis.text.y.right = ggplot2::element_text(color = oxygen_color),
        axis.title.y.right = ggplot2::element_text(color = oxygen_color),
        axis.line.y.left = ggplot2::element_line(color = carbon_color),
        axis.ticks.y.left = ggplot2::element_line(color = carbon_color),
        axis.text.y.left = ggplot2::element_text(color = carbon_color),
        axis.title.y.left = ggplot2::element_text(color = carbon_color)
      ) +
      ggplot2::ggtitle(paste("Tooth ID:", tooth_id)) +
      ggplot2::scale_x_reverse(limits = c(45, 0)) +
      ggplot2::coord_cartesian(ylim = carbon_axis)
    
    # Save plot
    file_name <- file.path(output_dir, paste0(group_name, "_", gsub("#", "_", tooth_id), ".png"))
    ggplot2::ggsave(filename = file_name, plot = p, width = 6, height = 4, dpi = 300)
    message("Saved: ", file_name)
  }
}

#' Generate carbon plots per species/element
#'
#' Creates and saves δ13C plots grouped by species and element.
#'
#' @param data Cleaned isotope data.frame
#' @param species_colors Named vector of species colors
#' @param output_dir Directory to save plots
#' @export
save_all_indiv_carbon_plots <- function(data, species_colors, output_dir) {
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  species_list <- unique(data$Species)
  element_list <- unique(data$Element)
  
  for (species in species_list) {
    for (element in element_list) {
      subset_data <- data[data$Species == species & data$Element == element, ]
      if (nrow(subset_data) == 0) next
      
      p <- ggplot2::ggplot(subset_data, ggplot2::aes(x = ERJ, y = carbon, group = Tooth, color = Species)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = -10, linetype = "dashed") +
        ggplot2::scale_color_manual(values = species_colors) +
        ggplot2::labs(
          title = paste0(species, " ", element),
          x = "Distance from Enamel Root Junction (mm)",
          y = bquote('δ'^13~'C (vPDB‰)')
        ) +
        ggplot2::theme_bw() +
        ggplot2::annotate("text",
                          x = max(subset_data$ERJ, na.rm = TRUE) * 0.9,
                          y = -9.7,
                          label = "100% C3 consumer",
                          size = 4)
      
      file_name <- file.path(output_dir, paste0(species, "_", element, ".png"))
      ggplot2::ggsave(filename = file_name, plot = p, width = 8, height = 6, dpi = 300)
      message("Saved: ", file_name)
    }
  }
}
