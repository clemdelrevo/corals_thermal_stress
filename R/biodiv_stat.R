get_biodiv_stat <- function(ecoregion_map, final_impacts_ecoregion, final_taxonomy, ecoregions, analyse_dixon, final_impacts_global) {
  
  #targets::tar_load(ecoregion_map)
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(ecoregions)
  #targets::tar_load(analyse_dixon)
  #targets::tar_load(final_impacts_global)

  ecoregion_stat <- ecoregion_map$stat
  ecoregion_stat <- ecoregion_stat[order(ecoregion_stat$impact1.5_90), ]
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$family != "Tubiporidae" & final_impacts_ecoregion$family != "Scleractinia incertae sedis", ]
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_global <- final_impacts_global[final_impacts_global$family != "Tubiporidae" & final_impacts_global$family != "Scleractinia incertae sedis", ]
  ecoregion_protected  <- sf::st_drop_geometry(ecoregion_stat[ecoregion_stat$mean_1.5 < 90, ])
  
  biodiv_stat <- data.frame(do.call(rbind, lapply(levels(as.factor(ecoregion_protected$region)), function(ecoregion) {
    
    #ecoregion = "Andaman and Nicobar Islands"
    message(ecoregion)
    ecoregion_sub <- final_impacts_ecoregion[final_impacts_ecoregion$region == ecoregion, ]
    realm <- ecoregions$REALM[ecoregions$ECOREGION %in% ecoregion]
    
    total_richness_family  <- tapply(final_taxonomy$final_genus_sp , final_taxonomy$final_family, length)
    family_stat  <- data.frame(family = names(total_richness_family), richness = total_richness_family, row.names = 1:length(total_richness_family))
    specie_ecoregion <- ecoregion_sub[ecoregion_sub$stress_1.5 < 90, ]
    family_ecoregion <- tapply(specie_ecoregion$specie, specie_ecoregion$family, length)
    
    indice <- match(names(family_ecoregion), levels(as.factor(family_stat$family)))
    family_stat$richness_eco <- NA
    family_stat$richness_eco[indice] <- family_ecoregion
    family_stat$family_impact <- round(((family_stat$richness_eco * 100) / family_stat$richness), 2)
    family_impact <- family_stat$family_impact
    names(family_impact) <- family_stat$family 
    
    richness       <- nrow(specie_ecoregion)
    coral_richness <- length(final_taxonomy$final_genus_sp)
    pourc_richness <- round(((richness * 100) / coral_richness), 2)
    
    biodiv_stat <- data.frame(realm = realm, ecoregion = ecoregion, total_richness_protect = richness,
                              pourc_richness = pourc_richness, t(family_impact))
    
  })))
    
  ecoregion_size <- data.frame(do.call(rbind, lapply(levels(as.factor(biodiv_stat$ecoregion)), function(ecoregion){
    
    #ecoregion = "Southeast Madagascar"
    message(ecoregion)
    
    ecoregion_sub <- ecoregions[ecoregions$ECOREGION == ecoregion, ]
    analyse_dixon <- analyse_dixon[analyse_dixon$stress_1.5 <= 0.2, ]
    intersection  <- sf::st_intersects(analyse_dixon, ecoregion_sub)
    area_region   <- nrow(analyse_dixon[sapply(intersection, length) != 0,])
    if(area_region == 0) {return(NULL)}
    
    ecoregion_size <- data.frame(ecoregion = ecoregion, area_region = area_region)
    
    return(ecoregion_size)
    
  })))
  
  biodiv_stat <- merge(biodiv_stat, ecoregion_size, by = "ecoregion")
  biodiv_stat <- biodiv_stat[order(biodiv_stat$area_region),]
  
  sars_data <- biodiv_stat[, names(biodiv_stat) %in% c("area_region", "total_richness_protect")]
  sars_data <- sars_data[, c("area_region", "total_richness_protect")]
  fit       <- sars::sar_power(data = sars_data)
  sars      <- plot(fit)

  biodiv_stat$residual  <- fit$residuals
  biodiv_stat$rank      <- (nrow(biodiv_stat) + 1) - rank(biodiv_stat$residual)
  
  ecoregions  <- ecoregions[order(ecoregions$ECOREGION), ]
  geom        <- sf::st_geometry(ecoregions[ecoregions$ECOREGION %in% biodiv_stat$ecoregion, ])
  biodiv_stat <- biodiv_stat[order(biodiv_stat$ecoregion), ]
  biodiv_stat <- sf::st_sf(cbind(biodiv_stat, geom))
  
  area_must_protected <- sf::st_drop_geometry(biodiv_stat[biodiv_stat$residual > 0, ])
  area_must_protected <- area_must_protected[order(area_must_protected$rank), ]
  
  regions <- unique(area_must_protected$ecoregion)
  cumul_richness   <- numeric(length(regions))
  cumul_area       <- numeric(length(regions))
  protected_specie <- final_impacts_ecoregion[final_impacts_ecoregion$stress_1.5 < 90, ]
  
  for(i in 1:length(regions)) {
    
    #i = 2
    specie <- unique(protected_specie$specie[protected_specie$region %in% regions[1:i]])
    cumul_richness[i] <- length(specie)
    area   <-  biodiv_stat$area_region[biodiv_stat$ecoregion %in% regions[1:i]]
    cumul_area[i] <- sum(area)
    
  }
  
  cumul_richness_area <- data.frame(region = regions, cumul_richness = cumul_richness, cumul_area = cumul_area)
  
  list_specie <- unique(protected_specie$specie[protected_specie$region %in% regions])
  list_specie <- list_specie[order(list_specie)]
  
  key_species <- final_impacts_global[final_impacts_global$specie %in% list_specie, ]
  
  conservation_family <- data.frame(total_richness = tapply(final_impacts_global$specie, final_impacts_global$family, length))
  conservation_family$key_richness <- NA
  conservation_family$family <- rownames(conservation_family)
  key_richness <- tapply(key_species$specie, key_species$family, length)
  indice <- match(names(key_richness), levels(as.factor(conservation_family$family)))
  conservation_family$key_richness[indice] <- key_richness
  conservation_family$key_perc <- (conservation_family$key_richness * 100) / conservation_family$total_richness
  
  conservation_statut <- data.frame(total_richness = tapply(final_impacts_global$specie, final_impacts_global$statut, length))
  conservation_statut$key_richness <- NA
  conservation_statut$statut <- rownames(conservation_statut)
  key_richness <- tapply(key_species$specie, key_species$statut, length)
  indice <- match(names(key_richness), levels(as.factor(conservation_statut$statut)))
  conservation_statut$key_richness[indice] <- key_richness
  conservation_statut$key_perc <- (conservation_statut$key_richness * 100) / conservation_statut$total_richness
  
  export_data <- sf::st_drop_geometry(biodiv_stat[, names(biodiv_stat)%in% c("ecoregion", "total_richness_protect", "pourc_richness", "area_region")])
  export_data <- export_data[order(export_data$total_richness_protect), ]
  
  dir.create("outputs/biodiv_data/", showWarnings = FALSE)
  
  write.csv2(export_data, "outputs/biodiv_data/hotspot.csv")
  
  return(list(biodiv_stat = biodiv_stat, cumul_richness_area = cumul_richness_area, list_specie = list_specie,
              family_stat = conservation_family, statut_stat = conservation_statut, sars = sars))
  
}
