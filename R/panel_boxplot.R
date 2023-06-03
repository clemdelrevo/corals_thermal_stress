panel_boxplot <- function(boxplot_threshold_family, boxplot_threshold_statut, boxplot_threshold_habitat, boxplot_threshold_trait) {
  
  targets::tar_load(boxplot_threshold_family)
  targets::tar_load(boxplot_threshold_statut)
  targets::tar_load(boxplot_threshold_habitat)
  targets::tar_load(boxplot_threshold_trait)

  
  first_panel <- cowplot::plot_grid(boxplot_threshold_habitat$boxplot, boxplot_threshold_trait$boxplot , ncol = 1, align = "hv",
                     labels = c("A", "B"))
  
  ggplot2::ggsave("outputs/figure/first_panel_boxplot.png", plot = first_panel, dpi = 500,
                  width = 30, height = 23, units = 'cm')
  
  ggplot2::ggsave("outputs/figure/boxplot_iucn.png", plot = boxplot_threshold_statut$boxplot, dpi = 500,
                  width = 25, height = 15, units = 'cm')
                     
  ggplot2::ggsave("outputs/figure/boxplot_family.png", plot = boxplot_threshold_family$boxplot, dpi = 500,
                  width = 25, height = 15, units = 'cm')
  
  
}
 