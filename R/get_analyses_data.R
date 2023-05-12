get_analyses_data <- function(thermal_stress, final_taxonomy) {
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(thermal_stress)
  
  get_analyses_sp <- lapply(1:nrow(final_taxonomy), function(i){
    
    i = 1
    sp      <- final_taxonomy[i, ]
    sp_name <- sp$final_genus_sp
    
    if(sf::st_is_empty(sp)) {message(paste0("no range for ", sp_name)), return(sp)}
    
    analyses_sp <- sf::st_intersects(thermal_stress, final_taxonomy)
    
    
    
  })
  
  
  
  
  
}