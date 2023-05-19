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
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  #final_impacts_exposed <- na.omit(final_impacts_exposed)  
  
  final_impacts_all <- final_impacts_exposed
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

impact_ecoregion_map <- function(){
  
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(ecoregions)
  
  
  
  exposed_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  
  impact_ecoregion <- lapply(levels(as.factor(exposed_ecoregion$region)), function(ecoregion){
    
    #ecoregion = "Central Somali Coast"
    message(ecoregion)
    ecoregion <- exposed_ecoregion[exposed_ecoregion$region == ecoregion, ]
    
    nb_sp     <- nrow(ecoregion)
    sp_impact_50 <- nrow(ecoregion[ecoregion$stress_2 > 50, ])
    sp_impact_90 <- nrow(ecoregion[ecoregion$stress_2 > 90, ])
    impact_50 <- (sp_impact_50 * 100) / nb_sp
    impact_90 <- (sp_impact_90 * 100) / nb_sp
    
    impact_ecoregion <- data.frame(region = unique(ecoregion$region), impact_50, impact_90)
    
    return(impact_ecoregion)
    
  })
  
  impact_ecoregion <- data.frame(do.call(rbind, impact_ecoregion))
  good_ecoregions  <- levels(as.factor(impact_ecoregion$region))
  good_ecoregions  <- ecoregions[ecoregions$ECOREGION %in% good_ecoregions, ]
  good_ecoregions  <- good_ecoregions[order(good_ecoregions$ECOREGION), ]
  good_ecoregions  <- sf::st_geometry(good_ecoregions)
  impact_ecoregion <- sf::st_as_sf(cbind(impact_ecoregion, good_ecoregions))
  
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ...) {
  
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_ecoregion, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world)+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::ggtitle(...)+
      ggplot2::theme(legend.title = ggplot2::element_blank(),
                     title = ggplot2::element_text(hjust = 0.5))
  
  }
  
  map_50 <- map_impact(fill = "impact_50", "stress thermique sur plus de 50% de l'occurence")
  map_90 <- map_impact(fill = "impact_90", "stress thermique sur plus de 90% de l'occurence")
  
  map_impact <- cowplot::ggdraw(xlim = c(0, 20), ylim = c(0, 40))+
    cowplot::draw_plot(map_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map_90, x = 0, y = 0, width = 20, height = 20)

}

impacts_province_map <- function() {
  
  #targets::tar_load(final_impacts_province)
  
  exposed_province <- final_impacts_province[final_impacts_province$area == "area_exposed", ]
  
  impact_province <- lapply(levels(as.factor(exposed_province$region)), function(province){
    
    #province = "Central Somali Coast"
    message(province)
    province <- exposed_province[exposed_province$region == province, ]
    
    nb_sp     <- nrow(province)
    sp_impact_50 <- nrow(province[province$stress_2 > 50, ])
    sp_impact_90 <- nrow(province[province$stress_2 > 90, ])
    impact_50 <- (sp_impact_50 * 100) / nb_sp
    impact_90 <- (sp_impact_90 * 100) / nb_sp
    
    impact_province <- data.frame(region = unique(province$region), impact_50, impact_90)
    
    return(impact_province)
    
  })
  
  
  impact_province <- data.frame(do.call(rbind, impact_province))
  good_ecoregions  <- levels(as.factor(impact_ecoregion$region))
  good_ecoregions  <- ecoregions[ecoregions$ECOREGION %in% good_ecoregions, ]
  good_ecoregions  <- good_ecoregions[order(good_ecoregions$ECOREGION), ]
  good_ecoregions  <- sf::st_geometry(good_ecoregions)
  impact_ecoregion <- sf::st_as_sf(cbind(impact_ecoregion, good_ecoregions))

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
  
  boxplot(impacts$stress_1.5 ~ impacts$habitat)
  
}
