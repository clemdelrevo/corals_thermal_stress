get_4km_data <- function(thermal_stress, millenium_grid) {
  
  #targets::tar_load(thermal_stress)
  #targets::tar_load(millenium_grid)
  
  data_4km <- lapply(1:nrow(millenium_grid), function(l) {
    
    #l = 7601
    message(l)
    millenium_line <- millenium_grid[l, ]
    
    intersect_line <- pbmcapply::pbmclapply(1:nrow(thermal_stress), function(i){
  
      #i = 1
      thermal_line <- thermal_stress[i, ]
      data_line    <- sf::st_intersects(millenium_line, thermal_line, sparse = FALSE)
      
    }, mc.cores = 80)
    
    thermal_sub    <- sf::st_drop_geometry(thermal_stress[intersect_line == TRUE,])
    
    mean_threshold <- round(apply(thermal_sub, 2, mean))
    
    millenium_cell <- cbind(millenium_line, t(mean_threshold))
    
  })
  
  data_4km <- do.call(rbind, data_4km)
  
}