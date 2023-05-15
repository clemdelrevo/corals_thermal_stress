get_eo_analyses_dixon <- function(thermal_dixon, final_taxonomy) {
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(thermal_dixon)
  
  #thermal_dixon <- thermal_dixon[thermal_dixon$regions %in% c("Persian Gulf", "Red Sea"), ]
  #final_taxonomy <- final_taxonomy[c(2, 83, 127, 711), ]
  
  geoms <- sf::st_geometry(final_taxonomy)
  
  get_analyses_sp <- setNames(parallel::mclapply(geoms, function(sp){
    
    #sp = geoms[[1]]
    
    #i = 1
    # sp       <- final_taxonomy[i, "final_genus_sp"]
    # sp_name  <- sp$final_genus_sp
    # 
    # if(sf::st_is_empty(sp)) {message(paste0("no range for ", sp_name)) ; return(sp)}
    # 
    # message(sp_name)
    
    # geoms <- sf::st_geometry(thermal_dixon)
    # 
    # intersect_line <- pbmcapply::pbmclapply(1:nrow(thermal_dixon), function(l) {
    #   
    #   #l = 1
    #   thermal_line <- thermal_dixon[l, ]
    #   analyses_sp  <- sf::st_intersects(thermal_line, sp, sparse = FALSE)
    #   
    #   if(analyses_sp == FALSE) {analyses_sp = 0 ; return(analyses_sp)}
    #   
    #   if(analyses_sp == TRUE) {analyses_sp = 1 ; return(analyses_sp)}
    #   
    #   
    # })
    # 
    # analyse_sp <- data.frame(do.call(rbind, intersect_line))
    # colnames(analyse_sp) <- sp_name
    
    analyses_sp <- sf::st_intersects(thermal_dixon, sp)
    
    
    return(sapply(analyses_sp, length))

  }), final_taxonomy$final_genus_sp)
  
  analyse_dixon <- data.frame(get_analyses_sp)
  
  analyse_dixon <- cbind(thermal_dixon, analyse_dixon)
  
  return(analyse_dixon)

}