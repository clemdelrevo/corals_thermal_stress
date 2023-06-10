# Create final taxonomy of valid name species ---

get_final_taxonomy <- function(taxonomy, valid_ctdb, valid_habitats, valid_corals_range){
  
  #targets::tar_load(taxonomy)
  #targets::tar_load(valid_ctdb)
  #targets::tar_load(valid_habitats)
  #targets::tar_load(valid_corals_range)
  
  # create the base of the taxonomy
  taxo_unique <- taxonomy[!duplicated(taxonomy$final_genus_sp),]
  
  # add range 
  intermed_taxo   <- merge(taxo_unique, valid_corals_range, by = "final_genus_sp")
  sp_name_range   <- intermed_taxo$final_genus_sp
  # recover taxon that have no range data
  sp_geom_na      <- taxo_unique[!taxo_unique$final_genus_sp %in% sp_name_range, ]
  sp_geom_na$geometry <- NA
  intermed_taxo <- rbind(intermed_taxo, sp_geom_na)
  
  # add habitat
  intermed_taxo_2 <- merge(intermed_taxo, valid_habitats, by = "final_genus_sp")
  sp_name_habitat <- intermed_taxo_2$final_genus_sp
  # recover taxon that have no habitat data
  sp_habitat_na   <- intermed_taxo[!intermed_taxo$final_genus_sp %in% sp_name_habitat, ]
  sp_habitat_na$in_reef      <- NA
  sp_habitat_na$out_reef     <- NA
  sp_habitat_na$final_statut <- NA
  intermed_taxo_2 <- rbind(intermed_taxo_2, sp_habitat_na)
  
  # add traits
  final_taxonomy  <- merge(intermed_taxo_2, valid_ctdb, by = "final_genus_sp")
  sp_name_ctdb    <- final_taxonomy$final_genus_sp
  # recover taxon that have no depth data
  sp_ctdb_na      <- intermed_taxo_2[!intermed_taxo_2$final_genus_sp %in% sp_name_ctdb, ]
  sp_ctdb_na$depth_lower <- NA
  sp_ctdb_na$depth_upper <- NA
  final_taxonomy <- rbind(final_taxonomy, sp_ctdb_na)
  
  final_taxonomy <- final_taxonomy[order(final_taxonomy$final_genus_sp), ]
  
  final_taxonomy  <- sf::st_as_sf(final_taxonomy)
  
  return(final_taxonomy)
  
}
