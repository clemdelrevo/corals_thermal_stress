get_analyse_dixon <- function(thermal_dixon, final_taxonomy, ecoregions, reef_at_risk) {
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(thermal_dixon)

  final_taxonomy_empty <- sf::st_is_empty(final_taxonomy)
  final_taxonomy       <- final_taxonomy[!final_taxonomy_empty, ]
  
  geoms <- sf::st_geometry(final_taxonomy)
  empty <- sf::st_is_empty(geoms)
  geoms <- geoms[!empty]
  
  get_analyses_sp <- setNames(parallel::mclapply(geoms, function(sp){
    
    analyses_sp <- sf::st_intersects(thermal_dixon, sp)
    
    return(sapply(analyses_sp, length))

  }, mc.cores = 8), final_taxonomy$final_genus_sp)
  
  analyse_dixon <- data.frame(get_analyses_sp)
  
  analyse_dixon <- cbind(thermal_dixon, analyse_dixon)
  
  analyse_dixon <- parallel::mclapply(levels(as.factor(ecoregions$ECOREGION)), function(region){
  
    #region = "Yellow Sea"
    message(region)
    ecoregion    <- ecoregions[ecoregions$ECOREGION == region, ]
    intersection <- sf::st_intersection(analyse_dixon, ecoregion)
    if(nrow(intersection) == 0) {return(NULL)}
  
    return(intersection)
    
  })

  analyse_dixon <- data.frame(do.call(rbind, analyse_dixon))
  
  analyse_dixon <- analyse_dixon[, !names(analyse_dixon) %in% c("ECO_CODE", "PROV_CODE", "RLM_CODE", "ALT_CODE", "ECO_CODE_X", "geometry")]
  
  return(analyse_dixon)
  
}