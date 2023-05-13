get_analyses_dixon <- function(thermal_stress, final_taxonomy) {
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(dixon_4km)
  
  get_analyses_sp <- lapply(1:nrow(final_taxonomy), function(i){
    
    #i = 716
    sp      <- final_taxonomy[i, ]
    sp_name <- sp$final_genus_sp
    
    if(sf::st_is_empty(sp)) {message(paste0("no range for ", sp_name)) ; return(sp)}
    
    message(sp_name)
    
    intersect_line <- pbmcapply::pbmclapply(1:nrow(dixon_4km), function(l) {
      
      #l = 1
      thermal_line <- dixon_4km[l, ]
      analyses_sp  <- sf::st_intersects(thermal_line, sp, sparse = FALSE)
      
      if(analyses_sp == FALSE) {analyses_sp = 0 ; return(analyses_sp)}
      
      if(analyses_sp == TRUE) {analyses_sp = 1 ; return(analyses_sp)}
      
      
    }, mc.cores = 8)
    
    analyse_sp <- data.frame(do.call(rbind, intersect_line))
    colnames(analyse_sp) <- sp_name

  })
  
  final_analyse_sp <- cbind(analyses_sp)
  
  final_dixon_4km <- data.frame(cbind(dixon_4km, final_analyse_sp))

}