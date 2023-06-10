get_sp_histo <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_global <- final_impacts_global[final_impacts_global$family != "Tubiporidae" & final_impacts_global$family != "Scleractinia incertae sedis", ]

  median <- apply(final_impacts_global[, names(final_impacts_global) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3")], 2, median)
  
  sp_hist <- function(x, bins, xlim, title, vline, ...) {
    
    ggplot2::ggplot(final_impacts_global, ggplot2::aes(x = .data[[x]]))+
      ggplot2::geom_histogram(bins = bins, breaks = ..., color = "black", fill = "lightgrey")+
      ggplot2::theme_classic()+
      ggplot2::xlim(xlim)+
      ggplot2::ylab("nombre d'espèces")+
      ggplot2::xlab("% d'exposition du range au stress thermique")+
      ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13, hjust = 0.5),
                     axis.text.x = ggplot2::element_text(size = 12),
                     axis.text.y = ggplot2::element_text(size = 12))+
      ggplot2::geom_vline(xintercept = vline, linetype = "dashed")
    
  }  
  
  present_stress <- sp_hist(x = "present_stress", 50, c(0, 101), "1986-2019", median[[1]])
  stress_1.5     <- sp_hist(x = "stress_1.5", 50, c(0, 101), "1.5°C", median[[2]])
  stress_2       <- sp_hist(x = "stress_2", 50, c(0, 101), "2°C", median[[3]])
  stress_3       <- sp_hist(x = "stress_3", 1, c(0, 100), "3°C", median[[4]], breaks = c(98,100))
  
  impact_sp <- cowplot::plot_grid(present_stress, stress_1.5, stress_2, stress_3, ncol = 2, align = "hv",
                                  labels = c("A", "B", "C", "D"))
  
  dir.create("outputs/figure", showWarnings = FALSE)
  dir.create("outputs/figure/global_figure", showWarnings = FALSE)
  
  ggplot2::ggsave("outputs/figure/global_figure/global_impact_histo.png", plot = impact_sp, dpi = 500)
  
  return(impact_sp)
  
}

get_rank <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_global <- final_impacts_global[final_impacts_global$family != "Tubiporidae" & final_impacts_global$family != "Scleractinia incertae sedis", ]
  
  final_impacts_global$rank_present <- rank(final_impacts_global$present_stress)
  final_impacts_global$rank_1.5 <- rank(final_impacts_global$stress_1.5)

  kendall <- cor.test(final_impacts_global$rank_present, final_impacts_global$rank_1.5, method = "kendall")
    
  plot_rank <- ggplot2::ggplot(final_impacts_global, ggplot2::aes(x = rank_present, y = rank_1.5))+
    ggplot2::geom_point(size = 0.8, position = ggplot2::position_jitter())+
    ggplot2::theme_classic()+
    ggplot2::xlab("rang de l'espèce (1986-2019)")+
    ggplot2::ylab("rang de l'espèce (+1.5°C)")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(size = 12))+
    ggplot2::labs(subtitle = paste0("Test de corrélation des rangs de Kendall, p-value = ", sprintf("%.4e", as.numeric(kendall$p.value)), ", tau = ", round(kendall$estimate, 4)))
  
  ggplot2::ggsave("outputs/figure/global_figure/plot_rank.png", plot = plot_rank, dpi = 500, 
                  width = 10, height = 7)
  
}

get_residual <- function(biodiv_stat) {
  
  #targets::tar_load(biodiv_stat)
  
  rownames(biodiv_stat$biodiv_stat) <- biodiv_stat$biodiv_stat$ecoregion
  
  residuals <- ggplot2::ggplot(biodiv_stat$biodiv_stat, ggplot2::aes(y = residual, x = area_region, color = realm))+
    ggplot2::geom_point(size = 4)+
    ggplot2::theme_classic()+
    ggplot2::geom_text(label = rownames(biodiv_stat$biodiv_stat), nudge_y = 15, nudge_x = 20, check_overlap = TRUE)+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")+
    ggplot2::labs(color = "Royaume")+
    ggplot2::ylab("Résidus")+
    ggplot2::xlab(bquote("Aire de distribution ("*km^2*")"))+
    ggplot2::scale_color_brewer(palette = "Set1")+
    ggplot2::geom_segment(ggplot2::aes(x = area_region, y = residual, xend = area_region, yend = 0), color = "darkgrey")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.x = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(size = 12))
  
  ggplot2::ggsave("outputs/figure/global_figure/residuals.png", plot = residuals, dpi = 500, 
                  width = 10, height = 7)
  
}


get_stress_range <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  median_range  <- median(final_impacts_exposed$range)
  median_impact <- apply(final_impacts_exposed[, c("present_stress", "stress_1.5", "stress_2", "stress_3")], 2, median)
  
  cor_test_present <- cor.test(final_impacts_exposed$range, final_impacts_exposed$present_stress, method = "spearman")
  cor_test_1.5     <- cor.test(final_impacts_exposed$range, final_impacts_exposed$stress_1.5, method = "spearman")

  plot_range <- function(y, median_impact, title, label) {
    
    ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = range, y = .data[[y]]))+
      ggplot2::geom_point()+
      ggplot2::theme_classic()+
      ggplot2::geom_hline(yintercept = median_impact, color = "yellow")+
      ggplot2::geom_vline(xintercept = median_range, color = "yellow")+
      ggplot2::xlab(bquote("aire de distribution ("*km^2*")"))+
      ggplot2::ylab("")+
      ggplot2::ylim(0, 100)+
      ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
                     axis.text.x = ggplot2::element_text(size = 13,),
                     axis.text.y = ggplot2::element_text(size = 13,),
                     axis.title.x = ggplot2::element_text(size = 15, face = "bold"))+
      ggplot2::geom_smooth(method = "lm", ggplot2::aes(group = 1), color = "red")+
      ggplot2::annotate("text", label = label, x = 150000, y = 100)
      
  }
  
  present_stress <- plot_range(y = "present_stress", median_impact = median_impact[1], "Present stress", paste0("p-value = ", round(cor_test_present$p.value, 3), ", rho = ", round(cor_test_present$estimate, 2)))
  stress_1.5     <- plot_range(y = "stress_1.5", median_impact = median_impact[2], "1.5°C", paste0("p-value = ", round(cor_test_1.5$p.value, 3), ", rho = ", round(cor_test_1.5$estimate, 2)))

  cowplot::plot_grid(present_stress, stress_1.5, ncol = 1)+
    cowplot::draw_label("% d'exposition du range au stress thermique", 
                        x = 0.01, y = 0.5, angle = 90, size = 15, fontface = "bold")

}
