impact_ecoregion_map <- function(final_impacts_ecoregion, ecoregions){
  
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(ecoregions)
  
  exposed_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  
  impact_ecoregion <- lapply(levels(as.factor(exposed_ecoregion$region)), function(ecoregion){
    
    #ecoregion = "Central Somali Coast"
    message(ecoregion)
    ecoregion <- exposed_ecoregion[exposed_ecoregion$region == ecoregion, ]
    
    nb_sp     <- nrow(ecoregion)
    sp_1.5_50 <- nrow(ecoregion[ecoregion$stress_1.5 > 50, ])
    sp_1.5_90 <- nrow(ecoregion[ecoregion$stress_1.5 > 90, ])
    impact1.5_50 <- (sp_1.5_50 * 100) / nb_sp
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    
    sp_2_50 <- nrow(ecoregion[ecoregion$stress_2 > 50, ])
    sp_2_90 <- nrow(ecoregion[ecoregion$stress_2 > 90, ])
    impact2_50 <- (sp_2_50 * 100) / nb_sp
    impact2_90 <- (sp_2_90 * 100) / nb_sp
    
    impact_ecoregion <- data.frame(region = unique(ecoregion$region),impact1.5_50, impact1.5_90, impact2_50, impact2_90)
    
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
  
  map_impact <- function(fill, ..., legend_size) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_ecoregion, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world)+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = legend_size))+
      ggplot2::labs(fill = "% d'espèces")
    
  }
  
  map1.5_50 <- map_impact(fill = "impact1.5_50", "> 50% de l'occurence", legend_size = 8)
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8)
  map2_50   <- map_impact(fill = "impact2_50", "> 50% de l'occurence", legend_size = 6)
  map2_90   <- map_impact(fill = "impact2_90", "> 90% de l'occurence", legend_size = 8)
  
  map_impact_ecoregion <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 41))+
    cowplot::draw_plot(map1.5_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map1.5_90, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(map2_50, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map2_90, x = 20, y = 0, width = 20, height = 20)+
    cowplot::draw_plot_label("1.5°C", x = 8, y = 41)+
    cowplot::draw_plot_label("2°C", x = 28, y = 41)
  
  return(map_impact_ecoregion)
  
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
    sp_1.5_50 <- nrow(province[province$stress_1.5 > 50, ])
    sp_1.5_90 <- nrow(province[province$stress_1.5 > 90, ])
    impact1.5_50 <- (sp_1.5_50 * 100) / nb_sp
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    
    sp_2_50 <- nrow(province[province$stress_2 > 50, ])
    sp_2_90 <- nrow(province[province$stress_2 > 90, ])
    impact2_50 <- (sp_2_50 * 100) / nb_sp
    impact2_90 <- (sp_2_90 * 100) / nb_sp
    
    impact_province <- data.frame(region = unique(province$region), impact1.5_50, impact1.5_90, impact2_50, impact2_90)
    
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
  impact_province <- sf::st_wrap_dateline(impact_province)
  impact_province <- sf::st_make_valid(impact_province)
  impact_province <- sf::st_set_crs(impact_province, 4326)
  impact_province <- sf::st_make_valid(impact_province)
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ..., legend_size) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_province, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world)+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = legend_size))+
      ggplot2::labs(fill = "% d'espèces")
    
  }
  
  map1.5_50 <- map_impact(fill = "impact1.5_50", "> 50% de l'occurence", legend_size = 8)
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8)
  map2_50   <- map_impact(fill = "impact2_50", "> 50% de l'occurence", legend_size = 6)
  map2_90   <- map_impact(fill = "impact2_90", "> 90% de l'occurence", legend_size = 8)
  
  map_impact_province <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 41))+
    cowplot::draw_plot(map1.5_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map1.5_90, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(map2_50, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map2_90, x = 20, y = 0, width = 20, height = 20)+
    cowplot::draw_plot_label("1.5°C", x = 8, y = 41)+
    cowplot::draw_plot_label("2°C", x = 28, y = 41)
  
  return(map_impact_province)
  
}

impact_realm_map <- function(){
  
  #targets::tar_load(final_impacts_realm)
  #targets::tar_load(ecoregions)
  
  exposed_realm <- final_impacts_realm[final_impacts_realm$area == "area_exposed", ]
  
  impact_realm <- lapply(levels(as.factor(exposed_realm$region)), function(realm){
    
    #realm = "Central Indo-Pacific"
    message(realm)
    realm <- exposed_realm[exposed_realm$region == realm, ]
    
    nb_sp     <- nrow(realm)
    sp_1.5_50 <- nrow(realm[realm$stress_1.5 > 50, ])
    sp_1.5_90 <- nrow(realm[realm$stress_1.5 > 90, ])
    impact1.5_50 <- (sp_1.5_50 * 100) / nb_sp
    impact1.5_90 <- (sp_1.5_90 * 100) / nb_sp
    
    sp_2_50 <- nrow(realm[realm$stress_2 > 50, ])
    sp_2_90 <- nrow(realm[realm$stress_2 > 90, ])
    impact2_50 <- (sp_2_50 * 100) / nb_sp
    impact2_90 <- (sp_2_90 * 100) / nb_sp
    
    impact_realm <- data.frame(region = unique(realm$region), impact1.5_50, impact1.5_90, impact2_50, impact2_90)
    
    return(impact_realm)
    
  })
  
  
  impact_realm <- data.frame(do.call(rbind, impact_realm))
  
  geom_realm <- setNames(lapply(levels(as.factor(impact_realm$region)), function(realm){
    
    #realm = "Eastern Indo-Pacific"
    
    realm <- ecoregions[ecoregions$REALM == realm, ]
    realm <- sf::st_transform(realm, 32663)
    realm <- sf::st_make_valid(realm)
    geom  <- sf::st_union(realm)
    geom  <- sf::st_make_valid(geom)
    union_polygon <- sf::st_cast(geom, "POLYGON")
    
    geom  <- sf::st_transform(geom, 4326)
    geom  <- sf::st_make_valid(geom)
    geom  <- sf::st_wrap_dateline(geom)
    
    return(geom)
    
  }), as.factor(impact_realm$region))
  
  
  geom_realm   <- sf::st_as_sfc(do.call(rbind, geom_realm))
  impact_realm <- sf::st_as_sf(cbind(impact_realm, geom_realm))
  impact_realm <- sf::st_make_valid(impact_realm)
  impact_realm <- sf::st_wrap_dateline(impact_realm)
  impact_realm <- sf::st_make_valid(impact_realm)
  impact_realm <- sf::st_set_crs(impact_realm, 4326)
  impact_realm <- sf::st_make_valid(impact_realm)
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ..., legend_size) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_realm, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world)+
      ggplot2::scale_fill_gradientn(colours = c("#33CCFF", "#FFFF00", "#FF0000"))+
      ggplot2::ggtitle(...)+
      ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), 
                     legend.position = "bottom",
                     legend.text = ggplot2::element_text(size = legend_size))+
      ggplot2::labs(fill = "% d'espèces")
    
  }
  
  map1.5_50 <- map_impact(fill = "impact1.5_50", "> 50% de l'occurence", legend_size = 8)
  map1.5_90 <- map_impact(fill = "impact1.5_90", "> 90% de l'occurence", legend_size = 8)
  map2_50   <- map_impact(fill = "impact2_50", "> 50% de l'occurence", legend_size = 6)
  map2_90   <- map_impact(fill = "impact2_90", "> 90% de l'occurence", legend_size = 8)
  
  map_impact_realm <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 41))+
    cowplot::draw_plot(map1.5_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map1.5_90, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(map2_50, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map2_90, x = 20, y = 0, width = 20, height = 20)+
    cowplot::draw_plot_label("1.5°C", x = 8, y = 41)+
    cowplot::draw_plot_label("2°C", x = 28, y = 41)
  
  
}