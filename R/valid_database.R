# Merge the depths of synonyms taxon or get depth of valid taxon ----

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

# Merge the range of synonyms taxon or get the range of valid taxon ----

get_valid_coral_range <- function(taxonomy, corals_range){
  
  #targets::tar_load(taxonomy)
  #targets::tar_load(corals_range)
  
  valid_taxon  <- unique(taxonomy$final_genus_sp)
  valid_taxon  <- na.omit(valid_taxon)
  valid_taxon  <- valid_taxon[order(valid_taxon)]

  valid_corals_range <- do.call(rbind, lapply(1:length(valid_taxon), function(i) {
    
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
  
  return(valid_corals_range)
  
}

# Merge habitat of synonyms taxon or get habitat of valid taxon ----

get_valid_habitats <- function(taxonomy, habitat_sp, correspondance_aca_iucn, Other = 1){
  
  #targets::tar_load(taxonomy)
  #targets::tar_load(habitat_sp)
  #targets::tar_load(correspondance_aca_iucn)
  
  habitat_sp       <- habitat_sp[-658]
  valid_taxon      <- unique(taxonomy$final_genus_sp)
  valid_taxon      <- na.omit(valid_taxon)
  id_statut        <- 1:9
  statut           <- c("EX", "EW", "CR", "EN", "VU", "NT", "LC", "DD", "NE")
  names(id_statut) <- statut
  valid_habitats   <- list()
  
  for(i in seq_along(valid_taxon)){
    
    #i = 12
    message(i)
    sp_valid <- valid_taxon[[i]]
    syn      <- taxonomy$genus_sp[taxonomy$final_genus_sp %in% sp_valid]

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
      
      statut_list <- lapply(fusion, function(sp) {
        
        statut <- as.character(sp$category)
        
        return(statut)
        
      })
  
      final_statut   <- unique(unlist(statut_list, use.names = FALSE))
      final_statut   <- names(which.min(id_statut[final_statut]))
      final_habitats <- unique(unlist(habitat_list, use.names = FALSE))
      final_statut   <- rep(final_statut, length(final_habitats))
      final_genus_sp <- rep(sp_valid, length(final_habitats))
      data           <- data.frame(cbind(final_genus_sp, final_habitats, final_statut))
        
      valid_habitats[[i]] <- data
        
    }else{
      
      # if taxon have not, just get habitats
      single_sp <- names(habitat_sp) %in% syn
      n         <- which(single_sp)
      final_habitats <- unlist(habitat_sp[[n]]$habitat, use.names = FALSE)
      final_statut   <- unlist(habitat_sp[[n]]$category, use.names = FALSE)
      final_genus_sp <- rep(sp_valid, length(final_habitats))
      data           <- data.frame(cbind(final_genus_sp, final_habitats, final_statut))
      
      valid_habitats[[i]] <- data
      
    }
  
  }
    
  valid_habitats <- valid_habitats[!sapply(valid_habitats, is.null)]
  valid_habitats <- do.call(rbind, valid_habitats)
  
  
  in_reef  <- c("Back Slope", "Foreslope (Outer Reef Slope)", "Inter-Reef Rubble Substrate", 
               "Inter-Reef Soft Substrate", "Lagoon", "Outer Reef Channel", "Marine Neritic - Coral Reef")
  out_reef <- c("Artificial/Marine - Marine Anthropogenic Structures", "Marine Intertidal - Mangrove Submerged Roots", 
                "Marine Intertidal - Rocky Shoreline", "Marine Intertidal - Tidepools", "Marine Neritic - Seagrass (Submerged)", 
                "Marine Neritic - Subtidal Loose Rock/pebble/gravel", "Marine Neritic - Subtidal Muddy", 
                "Marine Neritic - Subtidal Rock and Rocky Reefs", "Marine Neritic - Subtidal Sandy", "Marine Neritic - Subtidal Sandy-Mud" )
  
  data.frame(do.call(rbind, lapply(levels(as.factor(valid_habitats$final_genus_sp)), function(sp){
    
    #sp = "Acropora_abrolhosensis"
    message(sp)
    
    specie <- valid_habitats[valid_habitats$final_genus_sp == sp, ]
    
    if(any(specie$final_habitats %in% in_reef)) {
      
      final_valid_habitats <- data.frame(final_genus_sp = unique(specie$final_genus_sp),
                                   final_statut = unique(specie$final_statut),
                                   in_reef = 1)
      
    } else {
      
      final_valid_habitats <- data.frame(final_genus_sp = unique(specie$final_genus_sp),
                                   final_statut = unique(specie$final_statut),
                                   in_reef = 0)
      
    }
    
    
    if(any(specie$final_habitats %in% out_reef)) {
      
      final_valid_habitats$out_reef = 1
      
    } else {
      
      final_valid_habitats$out_reef = 0
      
    }
    
    return(final_valid_habitats)
    
  })))
  
  
  
}
  
 