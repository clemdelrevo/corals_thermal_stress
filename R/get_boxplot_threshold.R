get_boxplot_impact_family <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  #targets::tar_load(final_impacts_region)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  #final_impacts_exposed <- na.omit(final_impacts_exposed)  
  
  final_impacts_all <- final_impacts_exposed
  final_impacts_all$family <- "All"
  
  final_impacts <- rbind(final_impacts_all, final_impacts_exposed)

  final_impacts$family <- final_impacts$family |> forcats::fct_relevel(
      c("Scleractinia incertae sedis", "Rhizangiidae", "Psammocoridae"              
      ,"Poritidae", "Pocilloporidae", "Plesiastreidae"             
      ,"Plerogyridae", "Oulastreidae", "Oculinidae"                 
      ,"Montastraeidae", "Milleporidae", "Merulinidae"                
      ,"Meandrinidae", "Lobophylliidae", "Leptastreidae"              
      ,"Helioporidae", "Fungiidae", "Faviidae"                   
      ,"Euphylliidae", "Diploastraeidae", "Dendrophylliidae"           
      ,"Coscinaraeidae", "Cladocoridae", "Caryophylliidae"            
      ,"Astrocoeniidae", "Astrangiidae"                       
      ,"Agariciidae", "Acroporidae", "All"                 
    ))
  

boxplot_family <- function(x, y) {
  
  ggplot2::ggplot(final_impacts, ggplot2::aes(x = .data[[x]], y = .data[[y]]))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
  
  }
    
  present_stress <- boxplot_family(x = "family", y = "present_stress")
  stress_1.5     <- boxplot_family(x = "family", y ="stress_1.5")
  stress_2       <- boxplot_family(x = "family", y = "stress_2")
  stress_3       <- boxplot_family(x = "family", y = "stress_3")
  

  boxplot_threshold_family <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
  
   return(boxplot_threshold_family)
  
}


get_boxplot_impacts_region <- function(final_impacts_region, final_impacts_global){
  
  #targets::tar_load(final_impacts_region)
  #targets::tar_load(final_impacts_global)
  
  final_impacts_region <- final_impacts_region[final_impacts_region$area == "area_exposed", ]
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
 
  final_impacts_all <- final_impacts_global
  final_impacts_all$region   <- "Global"
  final_impacts_all$in_reef  <- NULL
  final_impacts_all$out_reef <- NULL
  final_impacts_all$statut   <- NULL
  
  final_impacts_region <- rbind(final_impacts_all, final_impacts_region)
  
  final_impacts_region$region <- final_impacts_region$region |>
    forcats::fct_relevel("Global", "Australia")
  
  boxplot_region <- function(x, y) {
    
    ggplot2::ggplot(final_impacts_region, ggplot2::aes(x = forcats::fct_rev(.data[[x]]), y = .data[[y]]))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
    
  }
  
  present_stress <- boxplot_region(x = "region", y = "present_stress")
  stress_1.5     <- boxplot_region(x = "region", y = "stress_1.5")
  stress_2       <- boxplot_region(x = "region", y = "stress_2")
  stress_3       <- boxplot_region(x = "region", y = "stress_3")
  
  boxplot_threshold_region <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
  return(boxplot_threshold_region)
  
}


get_boxplot_impact_statut <- function(final_impacts_global){
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  final_impacts_exposed$statut <- final_impacts_exposed$statut |> 
    forcats::fct_relevel("CR", "EN", "VU", "NT", "LC", "DD")
  
  boxplot_statut <- function(x, y) {
  
    ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = .data[[x]], y = .data[[y]]))+
      ggplot2::geom_boxplot()+
      ggplot2::ylim(0, 100)
    
  }
  
  present_stress <- boxplot_statut(x = "statut", y = "present_stress")
  stress_1.5     <- boxplot_statut(x = "statut", y = "stress_1.5")
  stress_2       <- boxplot_statut(x = "statut", y = "stress_2")
  stress_3       <- boxplot_statut(x = "statut", y = "stress_3")
  
  boxplot_threshold_statut <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
  return(boxplot_threshold_statut)
  
}

get_stress_range <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  median_range  <- median(final_impacts_exposed$range)
  median_impact <- apply(final_impacts_exposed[, c("present_stress", "stress_1.5", "stress_2", "stress_3")], 2, median)
  
  
  
  plot_range <- function(y, median_impact) {
    
    ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = range, y = .data[[y]], color = family))+
    ggplot2::geom_point()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::geom_hline(yintercept = median_impact)+
    ggplot2::geom_vline(xintercept = median_range)+
    ggplot2::xlab(bquote("aire de distribution ("*km^2*")"))+
    ggplot2::ylab("pourcentage d'exposition au stress thermique")+
    ggplot2::ylim(0, 100)
    
  }
  
  present_stress <- plot_range(y = "present_stress", median_impact = median_impact[1])
  stress_1.5     <- plot_range(y = "stress_1.5", median_impact = median_impact[2])
  stress_2       <- plot_range(y = "stress_2", median_impact = median_impact[3])
  stress_3       <- plot_range(y = "stress_3", median_impact = median_impact[4])
  
  plot_stress_range <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
  return(plot_stress_range)
  
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
  
  targets::tar_load(final_impacts_zee)
  
  final_impacts_zee <- final_impacts_zee[final_impacts_zee$area == "area_exposed", ]
  
  final_impacts_zee$note <- trimws(final_impacts_zee$note)
  
  final_impacts_zee <- final_impacts_zee[!grepl("Uninhabited or sparsely inhabited with reefs", final_impacts_zee$note), ]
  
  impact_zee <- lapply(levels(as.factor(final_impacts_zee$region)), function(zee){
    
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
    
  })
  
  impact_zee <- do.call(rbind, impact_zee)
  
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


