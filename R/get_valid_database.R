get_valid_ctdb <- function(taxonomy, coral_traits_final){

  #targets::tar_load(taxonomy)
  #targets::tar_load(coral_traits_final)

  valid_taxon <- unique(taxonomy$final_genus_sp)
  valid_taxon  <- na.omit(valid_taxon)

  valid_ctdb <- do.call(rbind, parallel::mclapply(1:length(valid_taxon), function(i) {

    #i = 20
    # get valid name taxon and his synonym
    sp_valid <- valid_taxon[[i]]
    syn      <- taxonomy$genus_sp[taxonomy$final_genus_sp %in% sp_valid]
    
    if(nrow(coral_traits_final[coral_traits_final$specie_name %in% syn,]) > 1) {

      # if taxon have synonyms, get maximum depth lower limit and minimum depth upper limit
      multi_sp        <- coral_traits_final[coral_traits_final$specie_name %in% syn,]
      depth_lower     <- max(multi_sp$depth_lower)
      depth_upper     <- min(multi_sp$depth_upper)
      final_genus_sp  <- sp_valid
      data            <- cbind(final_genus_sp, depth_lower, depth_upper)
      
      return(data)
      
    } else {
      
      # if taxon have not, just get the depth range
      single_sp           <- coral_traits_final[coral_traits_final$specie_name %in% syn,]
      names(single_sp)[1] <- "final_genus_sp"
      
      return(single_sp)
      
    }
  
  }))
 
  valid_ctdb <- valid_ctdb[order(valid_ctdb$final_genus_sp),]
  
  return(valid_ctdb)
   
}


get_valid_coral_range <- function(taxonomy, corals_range){
  
  #targets::tar_load(taxonomy)
  #targets::tar_load(corals_range)
  
  valid_taxon  <- unique(taxonomy$final_genus_sp)
  valid_taxon  <- na.omit(valid_taxon)

  valid_corals_range <- do.call(rbind, parallel::mclapply(1:length(valid_taxon), function(i) {
    
    #i = 36
    sp_valid <- valid_taxon[[i]]
    message("coral range: ", i, " (", sp_valid, ")")
    syn      <- taxonomy$genus_sp[taxonomy$final_genus_sp %in% sp_valid]

    if (nrow(corals_range[corals_range$sci_name %in% syn, ]) > 1){
      
      #library(sf)
      
      # if taxon have synonyms, get union of the ranges
      multi_sp   <- corals_range[corals_range$sci_name %in% syn, ]
      union_geom <- sf::st_union(multi_sp)
      union_geom <- sf::st_make_valid(union_geom)
      data <-  data.frame(final_genus_sp = sp_valid)
      data <- sf::st_set_geometry(data, union_geom)

      return(data)
      
    } else {
      
      # if taxon have not, just get the range of the specie
      single_sp <- corals_range[corals_range$sci_name %in% syn, ]
      single_sp <- single_sp |>
        dplyr::select(sci_name, geometry)
      names(single_sp)[1] <- "final_genus_sp"
      
      return(single_sp)
      
    }
    
  })) 
  
  valid_corals_range <- valid_corals_range[order(valid_corals_range$final_genus_sp),]
  
  return(valid_corals_range)
  
}

get_valid_habitats <- function(taxonomy, habitat_sp, correspondance_aca_iucn, Other = 1){
  
  #targets::tar_load(taxonomy)
  #targets::tar_load(habitat_sp)
  #targets::tar_load(correspondance_aca_iucn)
  
  habitat_sp     <- habitat_sp[-658]
  valid_taxon    <- unique(taxonomy$final_genus_sp)
  valid_taxon    <- na.omit(valid_taxon)
  valid_habitats <- list()
  
  for(i in seq_along(valid_taxon)){
    
    #i = 99
    sp_valid <- valid_taxon[[i]]
    syn      <- taxonomy$genus_sp[taxonomy$final_genus_sp == sp_valid]
    syn      <- na.omit(syn)

    if(length(which(names(habitat_sp) %in% syn)) == 0){
      
      next
      
    }else if(length(which(names(habitat_sp) %in% syn)) > 1){
    
      # if taxon have synonyms, get all the habitats
      multi_sp <- names(habitat_sp) %in% syn
      n        <- which(multi_sp)
      fusion   <- c(habitat_sp[n])

      habitat_list <- lapply(fusion, function(sp) {
    
        habitat_filter <- as.character(sp$habitat)
        
        return(habitat_filter)
    
        })
  
      final_habitats <- unique(unlist(habitat_list, use.names = FALSE))
      final_genus_sp <- rep(sp_valid, length(final_habitats))
      data           <- data.frame(cbind(final_genus_sp, final_habitats))
        
      valid_habitats[[i]] <- data
        
    }else{
      
      # if taxon have not, just get habitats
      single_sp <- names(habitat_sp) %in% syn
      n         <- which(single_sp)
      final_habitats <- unlist(habitat_sp[[n]]$habitat, use.names = FALSE)
      final_genus_sp <- rep(sp_valid, length(final_habitats))
      data           <- data.frame(cbind(final_genus_sp, final_habitats))
      
      valid_habitats[[i]] <- data
      
    }
  
  }
    
  valid_habitats <- valid_habitats[!sapply(valid_habitats, is.null)]
  
  # correspondance between aca and iucn
  valid_habitats <- parallel::mclapply(1:length(valid_habitats), function(i){
    
    message("specie ", i)
    #i = 627
    
    final_genus_sp <- unique(valid_habitats[[i]]$final_genus_sp)
    iucn_hab_sp <- valid_habitats[[i]]
    
    if(length(iucn_hab_sp$final_habitats) == 1){
      
      val_aca_hab_sp <- correspondance_aca_iucn[, iucn_hab_sp$final_habitats]
      aca_hab_sp <- rownames(correspondance_aca_iucn)
      data_all <- data.frame(t(val_aca_hab_sp))
      colnames(data_all) <- aca_hab_sp
      data <- cbind(final_genus_sp, data_all)
      
      data$Other <- Other
      
      data
      
    } else {
      
    aca_hab_sp <- correspondance_aca_iucn[, iucn_hab_sp$final_habitats]
    
    aca_hab_sp <- apply(aca_hab_sp, 1, sum)
    
    aca_hab_sp_1 <- names(aca_hab_sp[aca_hab_sp != 0])
    val_aca_hab_sp_1 <- rep(1, length(aca_hab_sp_1))
    data_1 <- data.frame(t(val_aca_hab_sp_1))
    colnames(data_1) <- aca_hab_sp_1
    
    aca_hab_sp_0 = names(aca_hab_sp[aca_hab_sp == 0])
    val_aca_hab_sp_0 <- rep(0, length(aca_hab_sp_0))
    data_0 <- data.frame(t(val_aca_hab_sp_0))
    colnames(data_0) <- aca_hab_sp_0
    
    data <- cbind(final_genus_sp, data_1, data_0)
    data <- data[, c("final_genus_sp", "Small Reef", "Plateau", "Reef Slope", "Sheltered Reef Slope", 
                     "Reef Crest", "Outer Reef Flat", "Inner Reef Flat", "Back Reef Slope", 
                     "Deep Lagoon", "Shallow Lagoon", "Patch Reefs", "Terrestrial Reef Flat")]
    
    data$Other <- Other
    
    data
    
    }
    
  })
  
  valid_habitats <- do.call(rbind, valid_habitats)
  valid_habitats <- valid_habitats[order(valid_habitats$final_genus_sp), ]

  return(valid_habitats)
  
}
  
# ---- CORRESPONDANCE HABITATS STUFF -------------------------------------------

### IUCN habitats

# see webpage  : https://www.iucnredlist.org/resources/habitat-classification-scheme
# see document : https://nc.iucnredlist.org/redlist/content/attachment_files/dec_2012_guidance_habitats_classification_scheme.pdf

# 9.8 Marine Neritic – Coral Reef     - Massive limestone structure built up through the cementing and depositional activities of colonial stony corals, predominantly of the order Scleractinia, and other calcareous invertebrate and algal species.
#  9.8.1 Outer reef channel           - Coral reef habitat on the foreslope (see 9.8.3) within or around the surge channels (spur and groove formations).
#  9.8.2 Back slope                   - The area opposite of the foreslope (see 9.8.3), also referring to the reef flat or inner part of a barrier reef or atoll.
#  9.8.3 Foreslope (outer reef slope) - The outer, seaward margin of a coral reef, also referring to the seaward side of a barrier reef or atoll.
#  9.8.4 Lagoon                       - A shallow (less than a depth of 200 m), sheltered body of water separated from the open sea by coral reefs; also refers to the area between the shore and a fringing reef, between the coast and a barrier reef, or the portion of an atoll surrounded by the reef.
#  9.8.5 Inter-reef soft substrate    - Area between reefs typically consisting of sandy substrate (see 9.4), but sometimes also with clay or silt sediments (see 9.5 and 9.6).
#  9.8.6 Inter-reef rubble substrate  - Area between reefs consisting predominantly of coral or calcareous fragments.

# IUCN -> ALLEN
# Outer reef channel           - Reef Crest, Sheltered Reef Slope, Reef Slope, Plateau, Small Reef
# Back slope                   - Inner Reef Flat, Outer Reef Flat, Back Reef Slope, 
# Foreslope (outer reef slope) - Reef Crest, Sheltered Reef Slope, Reef Slope, Plateau, Small Reef
# Lagoon                       - Shallow Lagoon, Deep Lagoon, Terrestrial Reef Flat, Patch Reefs
# Inter-reef soft substrate    - Shallow Lagoon, Deep Lagoon, Terrestrial Reef Flat, Patch Reefs
# Inter-reef rubble substrate  - Shallow Lagoon, Deep Lagoon, Terrestrial Reef Flat, Patch Reefs


### ALLEN habitats

# see webpage  : https://allencoralatlas.org/methods/#habitatmaps
# see document : https://storage.googleapis.com/coral-atlas-field-data/training-materials/AllenCoralAtlas_GeomorphicClasses_v3.pdf

# .Shallow Lagoon        - Shallow Lagoon is any closed to semi-enclosed, sheltered, flat-bottomed shallow sediment-dominated lagoon area.
# .Deep Lagoon           - Deep Lagoon is any sheltered broad body of water semi-enclosed to enclosed by reef, with a variable depth (but shallower than surrounding ocean) and a soft bottom dominated by reef-derived sediment.
# .Inner Reef Flat       - Inner Reef Flat is a low energy, sediment-dominated, horizontal to gently sloping platform behind the Outer Reef Flat.
# .Outer Reef Flat       - Adjacent to the seaward edge of the reef, Outer Reef Flat is a level (near horizontal), broad and shallow platform that displays strong wave-driven zonation
# .Reef Crest            - Reef Crest is a zone marking the boundary between the reef flat and the reef slope, generally shallow and characterized by highest wave energy absorbance.
# .Terrestrial Reef Flat - Terrestrial Reef Flat is a broad, flat, shallow to semi-exposed area of fringing reef found directly attached to land at one side, and subject to freshwater run-off, nutrients and sediment.
# .Sheltered Reef Slope  - Sheltered Reef Slope is any submerged, sloping area extending into Deep Water but protected from strong directional prevailing wind or current, either by land or by opposing reef structures.
# .Reef Slope            - Reef Slope is a submerged, sloping area extending seaward from the Reef Crest (or Flat) towards the shelf break. Windward facing, or any direction if no dominant prevailing wind or current exists.
# .Plateau               - Plateau is any deeper submerged, hard-bottomed, horizontal to gently sloping seaward facing reef feature.
# .Back Reef Slope       - Back Reef Slope is a complex, interior, - often gently sloping - reef zone occurring behind the Reef Flat. Of variable depth (but deeper than Reef Flat and more sloped), it is sheltered, sediment-dominated and often punctuated by coral outcrops.
# .Patch Reef            - Patch Reef is any small, detached to semi-detached lagoonal coral outcrop arising from sand-bottomed area.
