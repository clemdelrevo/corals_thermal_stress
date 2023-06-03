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
  
  present_stress <- sp_hist(x = "present_stress", 50, c(0, 101), "1989-2019", median[[1]])
  stress_1.5     <- sp_hist(x = "stress_1.5", 50, c(0, 101), "1.5°C", median[[2]])
  stress_2       <- sp_hist(x = "stress_2", 50, c(0, 101), "2°C", median[[3]])
  stress_3       <- sp_hist(x = "stress_3", 1, c(0, 100), "3°C", median[[4]], breaks = c(98,100))
  
  impact_sp <- cowplot::plot_grid(present_stress, stress_1.5, stress_2, stress_3, ncol = 2, align = "hv",
                                  labels = c("A", "B", "C", "D"))
  
  dir.create("outputs/figure", showWarnings = FALSE)
  
  ggplot2::ggsave("outputs/figure/global_impact_histo.png", plot = impact_sp, dpi = 500)
  
  return(impact_sp)
  
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


boxplot_habitat <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  final_impacts_global <- na.omit(final_impacts_global)
  
  only_reef <- final_impacts_global[final_impacts_global$in_reef == 1 & final_impacts_global$out_reef == 0, ]
  only_reef$habitat <- "in_reef"
  not_only_reef <- final_impacts_global[final_impacts_global$in_reef == 1 & final_impacts_global$out_reef == 1, ]
  not_only_reef$habitat <- 'in_out_reef'
  
  impacts <- data.frame(rbind(only_reef, not_only_reef))
  
  reef_habitat <- tapply(impacts$specie, impacts$habitat, length)
  
  barplot(reef_habitat)
  
  
  boxplot(impacts$stress_1.5 ~ impacts$habitat)
  
}

barplot_zee <- function(final_impacts_zee) {
  
  #targets::tar_load(final_impacts_zee)
  
  final_impacts_zee <- final_impacts_zee[final_impacts_zee$area == "area_exposed", ]
  #final_impacts_zee <- final_impacts_zee[!grepl("Uninhabited or sparsely inhabited with reefs", final_impacts_zee$note), ]
  
  
  
  impact_zee <- data.frame(do.call(rbind, lapply(levels(as.factor(final_impacts_zee$region)), function(zee){
    
    #zee = "AMERICAN SAMOA"
    message(zee)
    zee <- final_impacts_zee[final_impacts_zee$region == zee, ]
    
    nb_sp     <- nrow(zee)
    sp_1.5_50 <- nrow(zee[zee$stress_1.5 > 50, ])
    sp_1.5_90 <- nrow(zee[zee$stress_1.5 > 90, ])
    impact1.5_50 <- (sp_1.5_50 * 100) / nb_sp
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    
    sp_2_50 <- nrow(zee[zee$stress_2 > 50, ])
    sp_2_90 <- nrow(zee[zee$stress_2 > 90, ])
    impact2_50 <- (sp_2_50 * 100) / nb_sp
    impact2_90 <- (sp_2_90 * 100) / nb_sp
    
    dependance    <- unique(zee$dependance)
    adaptability  <- unique(zee$adaptability)
    vulnerability <- unique(zee$vulnerability)
    
    impact_zee <- data.frame(region = unique(zee$region), impact1.5_50, impact1.5_90, impact2_50, impact2_90, dependance, adaptability, vulnerability)
    
    return(impact_zee)
    
  })))
  
  mean_dependance <- tapply(impact_zee$impact1.5_90, impact_zee$dependance, mean)
  st_dependance   <- tapply(impact_zee$impact1.5_90, impact_zee$dependance, plotrix::std.error)
  dependance      <- data.frame(moy = mean_dependance, st = st_dependance, dependance = names(mean_dependance))
  dependance$dependance <- dependance$dependance |>
    forcats::fct_relevel("Low", "Medium", "High", "Very High")
  summary(aov(impact_zee$impact1.5_50 ~ impact_zee$dependance))
  pairwise.t.test(impact_zee$impact1.5_50, impact_zee$dependance)
  
  mean_adaptability <- tapply(impact_zee$impact1.5_90, impact_zee$adaptability, mean)
  st_adaptability   <- tapply(impact_zee$impact1.5_90, impact_zee$adaptability, plotrix::std.error)
  adaptability      <- data.frame(moy = mean_adaptability, st = st_adaptability, adaptability = names(mean_adaptability))
  adaptability$adaptability <- adaptability$adaptability |>
    forcats::fct_relevel("Very low", "Low", "Medium", "High")
  
  mean_vulnerability <- tapply(impact_zee$impact1.5_90, impact_zee$vulnerability, mean)
  st_vulnerability   <- tapply(impact_zee$impact1.5_90, impact_zee$vulnerability, plotrix::std.error)
  vulnerability      <- data.frame(moy = mean_vulnerability, st = st_vulnerability, vulnerability = names(mean_vulnerability))
  vulnerability$vulnerability <- vulnerability$vulnerability |>
    forcats::fct_relevel("Low", "Medium", "High", "Very High")
  
  zee <- function(data, x, factor) {
    
    ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = moy, fill = .data[[factor]]))+
      ggplot2::geom_col()+
      ggplot2::scale_fill_ordinal(direction = -1)+
      ggplot2::geom_errorbar(ggplot2::aes(ymin = moy - st, ymax = moy + st), width = 0.2)+
      ggplot2::ylab("% moyen d'espèces dont l'exposition \nde l'aire de distribution > 90%")
    
  }

  dependance_plot    <- zee(dependance, x = "dependance", factor = "dependance")
  adaptability_plot  <- zee(adaptability, x = "adaptability", factor = "adaptability")
  vulnerability_plot <- zee(vulnerability, x = "vulnerability", factor = "vulnerability")
  
  
  cowplot::plot_grid(dependance_plot, adaptability_plot, vulnerability_plot, align = "hv")
  
}




