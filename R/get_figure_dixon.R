
get_stress_range <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  median_range  <- median(final_impacts_exposed$range)
  median_impact <- apply(final_impacts_exposed[, c("present_stress", "stress_1.5", "stress_2", "stress_3")], 2, median)
  
  plot_range <- function(y, median_impact, title) {
    
    ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = range, y = .data[[y]], color = family))+
    ggplot2::geom_point()+
    ggplot2::geom_hline(yintercept = median_impact)+
    ggplot2::geom_vline(xintercept = median_range)+
    ggplot2::xlab(bquote("aire de distribution ("*km^2*")"))+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::ylim(0, 100)+
    ggplot2::ggtitle(title)+
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))+
    ggplot2::geom_smooth(method = "loess", ggplot2::aes(group = 1), color = "black")+
    ggplot2::scale_color_manual(values = c("#FFFF00", "#00FF00", "#FF3300", "#CC33FF", "#00CCFF", "#FF00CC",
                                           "#993300", "#0000FF", "#000033", "#FF99FF", "#009966", "#FF9933",
                                           "#CC0033", "#666666", "#00CC99", "#003300", "#6633CC", "#FF0099",
                                           "#0066FF", "#660066", "#66CC00", "#CCCC00", "#3300CC", "#FF6699",
                                           "#3399CC", "#FF0000", "#FF9966", "#666600", "#CCFFCC"))
    
  }
  
  present_stress <- plot_range(y = "present_stress", median_impact = median_impact[1], "Present stress")
  stress_1.5     <- plot_range(y = "stress_1.5", median_impact = median_impact[2], "1.5°C")
  stress_2       <- plot_range(y = "stress_2", median_impact = median_impact[3], "2°C")
  stress_3       <- plot_range(y = "stress_3", median_impact = median_impact[4], "3°C")
  
  cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
}


boxplot_habitat <- function(final_impacts_global) {
  
  targets::tar_load(final_impacts_global)
  
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
  final_impacts_zee <- final_impacts_zee[!grepl("Uninhabited or sparsely inhabited with reefs", final_impacts_zee$note), ]
  
  
  
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
  
  mean_dependance <- tapply(impact_zee$impact1.5_50, impact_zee$dependance, mean)
  st_dependance <- tapply(impact_zee$impact1.5_50, impact_zee$dependance, plotrix::std.error)
  dependance <- data.frame(moy = mean_dependance, st = st_dependance, dependance = names(mean_dependance))
  dependance$dependance <- dependance$dependance |>
    forcats::fct_relevel("Low", "Medium", "High", "Very High")
  summary(aov(impact_zee$impact1.5_50 ~ impact_zee$dependance))
  pairwise.t.test(impact_zee$impact1.5_50, impact_zee$dependance)
  
  mean_adaptability <- tapply(impact_zee$impact1.5_90, impact_zee$adaptability, mean)
  st_adaptability <- tapply(impact_zee$impact1.5_50, impact_zee$adaptability, plotrix::std.error)
  
  mean_vulnerability <- tapply(impact_zee$impact1.5_90, impact_zee$vulnerability, mean)
  st_vulnerability <- tapply(impact_zee$impact1.5_50, impact_zee$vulnerability, plotrix::std.error)
  
  ggplot2::ggplot(dependance, ggplot2::aes(x = as.factor(dependance), y = moy, fill = as.factor(dependance)))+
    ggplot2::geom_col()+
    ggplot2::scale_fill_ordinal(direction = -1)+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = moy - st, ymax = moy + st), width = 0.2)
  
}


