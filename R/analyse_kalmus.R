get_analyse_kalmus <- function(thermal_kalmus, final_taxonomy) {
  
  
  final_taxonomy_empty <- sf::st_is_empty(final_taxonomy)
  final_taxonomy       <- final_taxonomy[!final_taxonomy_empty, ]
  
  geoms <- sf::st_geometry(final_taxonomy)
  empty <- sf::st_is_empty(geoms)
  geoms <- geoms[!empty]
  
  get_analyses_sp <- setNames(parallel::mclapply(geoms, function(sp){
    
    analyses_sp <- sf::st_intersects(thermal_kalmus, sp)
    
    return(sapply(analyses_sp, length))
    
  }), final_taxonomy$final_genus_sp)
  
  analyse_kalmus <- data.frame(get_analyses_sp)
  
  analyse_kalmus <- cbind(thermal_kalmus, analyse_kalmus)
  
  return(analyse_kalmus)
  
}