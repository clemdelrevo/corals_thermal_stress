get_years_threshold <- function(kalmus_impacts_family) {
  
  #targets::tar_load(kalmus_impacts_family)
  
  optimistic  <- kalmus_impacts_family[kalmus_impacts_family$model == "ssp126_5yrs_DHW8", ]
  pessimistic <- kalmus_impacts_family[kalmus_impacts_family$model == "ssp585_10yrs_DHW4", ]
  
  year_threshold <- function(data, title) {
    
    ggplot2::ggplot(data = data, ggplot2::aes(x = year, y = pourc, group= family, color = family))+
      ggplot2::geom_line()+
      ggplot2::geom_hline(yintercept = 50)+
      ggplot2::geom_hline(yintercept = 90, col = "red")+
      ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = 2023, ymin = -Inf, ymax = Inf), 
                         alpha = 0.009, linewidth = 0.01, color = "black")+
      ggplot2::xlab("")+
      ggplot2::ylab("")+
      ggplot2::scale_x_continuous(breaks = seq(2000, 2100, 10))+
      ggplot2::ggtitle(title)+
      ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold", size = 11))+
      ggplot2::scale_color_manual(values = c("#FFFF00", "#00FF00", "#FF3300", "#CC33FF", "#00CCFF", "#FF00CC",
                                             "#993300", "#0000FF", "#000033", "#FF99FF", "#009966", "#FF9933",
                                             "#CC0033", "#666666", "#00CC99", "#003300", "#6633CC", "#FF0099",
                                             "#0066FF", "#660066", "#66CC00", "#CCCC00", "#3300CC", "#FF6699",
                                             "#3399CC", "#FF0000", "#FF9966", "#666600", "#CCFFCC"))
  }
  
  optimistic_plot  <- year_threshold(optimistic, "ssp126_5yrs_DHW8")
  pessimistic_plot <- year_threshold(pessimistic, "ssp585_10yrs_DHW4.8")
    
  cowplot::plot_grid(optimistic_plot, pessimistic_plot, ncol = 1, align = "h", axis = "t")+
    cowplot::draw_label("pourcentage moyen d'exposition des espèces/ famille", 
                        x = 0.01, y = 0.5, angle = 90, size = 11)
  
}
