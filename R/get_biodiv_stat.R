get_biodi_stat <- function(ecoregion_map, final_impacts_ecoregion, final_taxonomy) {
  
  #targets::tar_load(ecoregion_map)
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(final_taxonomy)

  ecoregion_stat <- ecoregion_map[[1]]
  ecoregion_stat <- ecoregion_stat[order(ecoregion_stat$impact1.5_90), ]
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  ecoregion_protected <- sf::st_drop_geometry(ecoregion_stat[ecoregion_stat$impact1.5_90 < 20, ])
  
  
  biodiv_stat <- data.frame(do.call(rbind, lapply(levels(as.factor(ecoregion_protected$region)), function(ecoregion) {
    
    #ecoregion = "Andaman and Nicobar Islands"
    message(ecoregion)
    ecoregion_sub <- final_impacts_ecoregion[final_impacts_ecoregion$region == ecoregion, ]
    
    total_richness_family  <- tapply(final_taxonomy$final_genus_sp , final_taxonomy$final_family, length)
    family_stat  <- data.frame(family = names(total_richness_family), richness = total_richness_family, row.names = 1:length(total_richness_family))
    specie_ecoregion <- ecoregion_sub[ecoregion_sub$stress_1.5 < 90, ]
    family_ecoregion <- tapply(specie_ecoregion$specie, specie_ecoregion$family, length)
    
    indice <- match(names(family_ecoregion), levels(as.factor(family_stat$family)))
    family_stat$richness_eco <- NA
    family_stat$richness_eco[indice] <- family_ecoregion
    family_stat$family_impact <- (family_stat$richness_eco * 100) / family_stat$richness
    family_impact <- family_stat$family_impact
    names(family_impact) <- family_stat$family 
    
    richness       <- nrow(specie_ecoregion)
    coral_richness <- length(final_taxonomy$final_genus_sp)
    pourc_richness <- (richness * 100) / coral_richness
    
    biodiv_stat <- data.frame(ecoregion = ecoregion, total_richness_protect = richness,
                              pourc_richness = pourc_richness, t(family_impact))
    
  })))
    
  regions <- unique(ecoregion_protected$region)
  cumul_richness <- numeric(length(regions))
  
  for(i in 1:length(regions)) {
    
    #i = 2
    specie <- unique(final_impacts_ecoregion$specie[final_impacts_ecoregion$region %in% regions[1:i]])
    cumul_richness[i] <- length(specie)
    
  }
  
  cumul_richness <- data.frame(region = regions, cumul_richness = cumul_richness)
  
  return(list(biodiv_stat, cumul_richness))
  
}
