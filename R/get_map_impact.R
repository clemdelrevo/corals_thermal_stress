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
      ggplot2::geom_sf(data = world, fill = "#CC9966", color = "#CC9966")+
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
  
  cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 41))+
    cowplot::draw_plot(map1.5_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map1.5_90, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(map2_50, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map2_90, x = 20, y = 0, width = 20, height = 20)+
    cowplot::draw_plot_label("1.5°C", x = 8, y = 41)+
    cowplot::draw_plot_label("2°C", x = 28, y = 41)
  
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
  impact_province <- sf::st_set_crs(impact_province, 4326)
  impact_province <- sf::st_make_valid(impact_province)
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  map_impact <- function(fill, ..., legend_size) {
    
    ggplot2::ggplot()+
      ggplot2::geom_sf(data = impact_province, ggplot2::aes(fill = .data[[fill]]))+
      ggplot2::geom_sf(data = world, fill = "#CC9966", color = "#CC9966")+
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
  
  cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 41))+
    cowplot::draw_plot(map1.5_50, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map1.5_90, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(map2_50, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(map2_90, x = 20, y = 0, width = 20, height = 20)+
    cowplot::draw_plot_label("1.5°C", x = 8, y = 41)+
    cowplot::draw_plot_label("2°C", x = 28, y = 41)
  
}


impacts_inreef_map <- function(final_impacts_ecoregion, ecoregions) {
  
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
    ggplot2::geom_sf(data = world, fill = "#CC9966", color = "#CC9966")+
    ggplot2::scale_fill_viridis_b()+
    ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
    ggplot2::labs(fill = "RS")

  sp_in_reef <- final_impacts_ecoregion[!is.na(final_impacts_ecoregion$in_reef), ]
  sp_in_reef <- sp_in_reef[sp_in_reef$in_reef == 1,]
  sp_in_reef <- sp_in_reef[sp_in_reef$out_reef == 0, ]
  nb_in_reef <- tapply(sp_in_reef$specie, sp_in_reef$region, length)

  prop_sp <- cbind(sp_ecoregion, nb_in_reef)
  prop_sp$prop_in_reef <- (prop_sp$nb_in_reef * 100) / prop_sp$nb_sp
  
  prop_in_reef <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = prop_sp, ggplot2::aes(fill = prop_in_reef))+
    ggplot2::geom_sf(data = world, fill = "#CC9966", color = "#CC9966")+
    ggplot2::scale_fill_viridis_b()+
    ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
    ggplot2::labs(fill = "% d'espèces")
  
  sp_in_reef1.5 <- sp_in_reef[sp_in_reef$stress_1.5 > 50, ]
  in_reef1.5    <- tapply(sp_in_reef1.5$specie, sp_in_reef1.5$region, length)
  
  prop_sp <- prop_sp[prop_sp$ecoregion %in% names(in_reef1.5), ]
  prop_sp <- cbind(prop_sp, in_reef1.5)
  prop_sp$prop_1.5 <- (prop_sp$in_reef1.5 * 100) / prop_sp$nb_in_reef
  
  prop1.5 <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = prop_sp, ggplot2::aes(fill = prop_1.5))+
    ggplot2::geom_sf(data = world, fill = "#CC9966", color = "#CC9966")+
    ggplot2::scale_fill_viridis_b()+
    ggplot2::coord_sf(xlim = c(-180, 180), ylim = c(-45, 45))+
    ggplot2::labs(fill = "% d'espèces")
  
  cowplot::ggdraw(xlim = c(0,20), ylim = c(0,30))+
    cowplot::draw_plot(richness, x = 0, y = 20, width = 20, height = 10)+
    cowplot::draw_plot(prop_in_reef, x = 0.2, y = 10, width = 20, height = 10)+
    cowplot::draw_plot(prop1.5, x = 0.2, y = 0, width = 20, height = 10)
  
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
  
  return(map_impact_realm)
  
}