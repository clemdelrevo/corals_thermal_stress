impact_ecoregion_map <- function(final_impacts_ecoregion, ecoregions){
  
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(ecoregions)
  
  exposed_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  
  impact_ecoregion <- lapply(levels(as.factor(exposed_ecoregion$region)), function(ecoregion){
    
    #ecoregion = "Central Somali Coast"
    message(ecoregion)
    ecoregion <- exposed_ecoregion[exposed_ecoregion$region == ecoregion, ]
    
    nb_sp     <- nrow(ecoregion)
    sp_1.5_90 <- nrow(ecoregion[ecoregion$stress_1.5 > 90, ])

    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    mean_1.5  <-  mean(ecoregion$stress_1.5)

    impact_ecoregion <- data.frame(region = unique(ecoregion$region), impact1.5_90, mean_1.5)
    
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
  
  map_impact <- function(fill, ..., legend_size, labs) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_ecoregion, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world, fill = "lightgrey", color = "lightgrey")+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::theme_classic()+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), 
                     legend.text = ggplot2::element_text(size = legend_size),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1))+
      ggplot2::labs(fill = labs)
    
  }
  
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8, labs = "% d'espèces")
  map_mean_impact <- map_impact(fill = "mean_1.5", "% moyen d'exposition", legend_size = 8, labs = "% d'exposition")
  
  ecoregion_map <- cowplot::plot_grid(map_mean_impact, map1.5_90, ncol = 1, align = "hv")

  return(list(impact_ecoregion, ecoregion_map))
  
}

impacts_province_map <- function(final_impacts_province, ecoregions) {
  
  #targets::tar_load(final_impacts_province)
  #targets::tar_load(ecoregions)
  
  exposed_province <- final_impacts_province[final_impacts_province$area == "area_exposed", ]
  
  impact_province <- lapply(levels(as.factor(exposed_province$region)), function(province){
    
    #province = "Central Polynesia"
    message(province)
    province <- exposed_province[exposed_province$region == province, ]
    
    nb_sp     <- nrow(province)
    sp_1.5_90 <- nrow(province[province$stress_1.5 > 90, ])
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    mean_1.5  <- mean(province$stress_1.5)
    
    impact_province <- data.frame(region = unique(province$region), impact1.5_90, mean_1.5)
    
    return(impact_province)
    
  })
  
  impact_province <- data.frame(do.call(rbind, impact_province))
  
  geom_province <- setNames(lapply(levels(as.factor(impact_province$region)), function(province){
    
    #province = "Andaman"
    
    province <- ecoregions[ecoregions$PROVINCE == province, ]
    geom     <- sf::st_union(province)
    geom     <- sf::st_make_valid(geom)

    return(geom)
    
  }), as.factor(impact_province$region))
  
  
  geom_province   <- sf::st_as_sfc(do.call(rbind, geom_province))
  impact_province <- sf::st_as_sf(cbind(impact_province, geom_province))
  impact_province <- sf::st_make_valid(impact_province)
  impact_province <- sf::st_set_crs(impact_province, 4326)
  impact_province <- sf::st_make_valid(impact_province)
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ..., legend_size, labs) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_province, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world, fill = "lightgrey", color = "lightgrey")+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::theme_classic()+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), 
                     legend.text = ggplot2::element_text(size = legend_size),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1))+
      ggplot2::labs(fill = labs)
    
  }
  
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8, labs = "% d'espèces")
  map_mean_impact <- map_impact(fill = "mean_1.5", "% moyen d'exposition", legend_size = 8, labs = "% d'exposition")
  
  province_map <- cowplot::plot_grid(map_mean_impact, map1.5_90, ncol = 1, align = "hv")
  
  return(list(impact_province, province_map))
  
}


richness_map <- function(final_impacts_ecoregion, ecoregions) {
  
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(ecoregions)
  
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  nb_sp <- tapply(final_impacts_ecoregion$specie, final_impacts_ecoregion$region, length)
  sp_ecoregion <- data.frame(nb_sp, ecoregion = names(nb_sp))
  rownames(sp_ecoregion) <- 1:nrow(sp_ecoregion)
  
  ecoregions <- ecoregions[ecoregions$ECOREGION %in% sp_ecoregion$ecoregion, ]
  ecoregions <- ecoregions[order(ecoregions$ECOREGION),]
    
  sp_ecoregion <- sf::st_as_sf(cbind(sp_ecoregion, sf::st_geometry(ecoregions)))
  sp_ecoregion <- sf::st_make_valid(sp_ecoregion)

  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  richness <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = sp_ecoregion, ggplot2::aes(fill = nb_sp))+
    ggplot2::geom_sf(data = world, fill = "lightgrey", color = "lightgrey")+
    ggplot2::theme_classic()+
    ggplot2::scale_fill_viridis_b()+
    ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
    ggplot2::labs(fill = "RS")+
    ggplot2::theme(panel.background = ggplot2::element_rect(color = "black", linewidth = 0.5))
  
  ggplot2::ggsave("outputs/figure/richness_map.png", plot = richness, dpi = 500, width = 30, height = 20, units = "cm")
  
  richness
  
}

impact_realm_map <- function(final_impacts_realm, ecoregions) {
  
  #targets::tar_load(final_impacts_realm)
  #targets::tar_load(ecoregions)
  
  exposed_realm <- final_impacts_realm[final_impacts_realm$area == "area_exposed", ]
  
  impact_realm <- lapply(levels(as.factor(exposed_realm$region)), function(realm){
    
    #realm = "Central Indo-Pacific"
    message(realm)
    realm <- exposed_realm[exposed_realm$region == realm, ]
    
    nb_sp     <- nrow(realm)
    sp_1.5_90 <- nrow(realm[realm$stress_1.5 > 90, ])
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    mean_1.5  <- mean(realm$stress_1.5)
    
    impact_realm <- data.frame(region = unique(realm$region), impact1.5_90, mean_1.5)
    
    return(impact_realm)
    
  })
  
  
  impact_realm <- data.frame(do.call(rbind, impact_realm))
  
  geom_realm <- setNames(lapply(levels(as.factor(impact_realm$region)), function(realm){
    
    #realm = "Eastern Indo-Pacific"
    
    realm <- ecoregions[ecoregions$REALM == realm, ]
    geom  <- sf::st_union(realm)
    geom  <- sf::st_make_valid(geom)
    
    return(geom)
    
  }), as.factor(impact_realm$region))
  
  geom_realm   <- sf::st_as_sfc(do.call(rbind, geom_realm))
  impact_realm <- sf::st_as_sf(cbind(impact_realm, geom_realm))
  impact_realm <- sf::st_set_crs(impact_realm, 4326)
  impact_realm <- impact_realm[impact_realm$region != "Temperate Northern Atlantic" & impact_realm$region != "Temperate Northern Pacific" & impact_realm$region != "Temperate Australasia", ]
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ..., legend_size, labs) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_realm, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world, fill = "lightgrey", color = "lightgrey")+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::theme_classic()+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), 
                     legend.text = ggplot2::element_text(size = legend_size),
                     panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 1))+
      ggplot2::labs(fill = labs)
    
  }
  
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8, labs = "% d'espèces")
  map_mean_impact <- map_impact(fill = "mean_1.5", "% moyen d'exposition", legend_size = 8, labs = "% d'exposition")
  
  realm_map <- cowplot::plot_grid(map_mean_impact, map1.5_90, ncol = 1, align = "hv")
  
  return(list(impact_realm, realm_map))
  
}