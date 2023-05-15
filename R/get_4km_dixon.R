get_4km_dixon <- function(thermal_stress, millenium_grid) {
  
  #targets::tar_load(thermal_dixon)
  #targets::tar_load(millenium_grid)
  
  thermal_stress <- thermal_stress[thermal_stress$regions == "East Pacific", ]
  millenium_grid <- sf::st_crop(millenium_grid, xmin = -109.87, xmax = -77.44, ymin = -1.40, ymax = 23.46)
  
  dixon_4km <- lapply(1:nrow(millenium_grid), function(l) {
    
    #l = 1
    message(l)
    millenium_line <- millenium_grid[l, ]
    
    intersect_line <- pbmcapply::pbmclapply(1:nrow(thermal_dixon), function(i){
  
      #i = 2
      thermal_line <- thermal_dixon[i, ]
      data_line <- sf::st_intersects(millenium_line, thermal_line, sparse = FALSE)
      
      return(data_line)
      
    }, mc.cores = 8)
    
    if(all(intersect_line == FALSE)) {
      
      na <- rep(NA, 6)
      col <- colnames(thermal_stress)[1:6] 
      names(na) <- col
      
      dixon_cell <- cbind(millenium_line, t(na))
      
      return(dixon_cell)
       
    } else {
      
      millenium_threshold <- sf::st_drop_geometry(thermal_stress[intersect_line == TRUE, ])
      
      mean_thermal_threshold <- apply(millenium_threshold[, 2:6], 2, mean)

      dixon_cell <- cbind(millenium_line, regions = unique(millenium_threshold$regions), 
                                     t(mean_thermal_threshold))
      
      return(dixon_cell)
    
    }
    
  })
  
  dixon_4km <- do.call(rbind, dixon_4km)
  
}