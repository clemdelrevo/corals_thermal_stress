# Get presence_absence of corals species in each point of dixon's projection

get_analyse_dixon <- function(thermal_dixon, final_taxonomy, ecoregions) {
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(thermal_dixon)
  #targets::tar_load(ecoregions)

  final_taxonomy_empty <- sf::st_is_empty(final_taxonomy)
  final_taxonomy       <- final_taxonomy[!final_taxonomy_empty, ]

  # get geometry of corals range
  geoms <- sf::st_geometry(final_taxonomy)
  empty <- sf::st_is_empty(geoms)
  geoms <- geoms[!empty]

  get_analyses_sp <- setNames(parallel::mclapply(geoms, function(sp){
    
    analyses_sp <- sf::st_intersects(thermal_dixon, sp)
    
    return(sapply(analyses_sp, length))

  }), final_taxonomy$final_genus_sp)
  
  analyse_dixon <- data.frame(get_analyses_sp)
  
  analyse_dixon <- cbind(thermal_dixon, analyse_dixon)
  
  return(analyse_dixon)
  
}