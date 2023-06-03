get_global_impact_100km <- function(dixon_100km, millenium_grid_100) {
  
  #targets::tar_load(dixon_100km)
  #targets::tar_load(millenium_grid_100)
  
  dixon_100km$geometry <- NULL
  
  col_name    <- c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4", "id")
  sp          <- dixon_100km[, !names(dixon_100km) %in% col_name]
  sum_area_sp <- colSums(sp)
  sp_nomatch  <- names(sum_area_sp)[sum_area_sp == 0]
  dixon_100km <- dixon_100km[, !names(dixon_100km) %in% sp_nomatch]
  sp_name <- paste(colnames(dixon_100km[, ! names(dixon_100km) %in% col_name]))
  
  cell_expo <- data.frame(do.call(rbind, parallel::mclapply(levels(as.factor(dixon_100km$id)), function(id) {
    
    #id = "cell_1657"
    message(id)
    sub_100 <- dixon_100km[dixon_100km$id == id, ]
    sub_100_sp <- sub_100[, sp_name]
    if(sum(apply(sub_100_sp, 2, sum)) == 0) {return(NULL)}

    sp_expo <- data.frame(do.call(rbind, lapply(colnames(sub_100[, names(sub_100) %in% sp_name]), function(specie) {
      
      #specie = "Turbinaria_reniformis"
      sp <- sub_100[, names(sub_100) %in% c(specie, col_name)]
      sp_range <- sp[sp[, names(sp) %in% specie] == 1, ]
      if(nrow(sp_range) == 0) {return(NULL)}
      sp_area <- nrow(sp_range)
      
      expo_present <- nrow(sp_range[sp_range$present_stress > 0.2, ])
      expo_1.5     <- nrow(sp_range[sp_range$stress_1.5 > 0.2, ])
    
      sp_expo <- data.frame(specie = specie,
                            expo_present = (expo_present * 100) / sp_area,
                            expo_1.5 = (expo_1.5 * 100) / sp_area)
      
      return(sp_expo)
      
    })))
    
    cell_expo <- data.frame(id = id,
                            expo_present = mean(sp_expo$expo_present),
                            expo_1.5 = mean(sp_expo$expo_1.5))
    
  })))
  
  extract_number <- function(x) {
    as.numeric(gsub("[^0-9]", "", x))
  }
  
  number <- sapply(cell_expo$id, extract_number)
  names(number) <- NULL
  cell_expo$number <- number
  cell_expo <- cell_expo[order(cell_expo$number), ]
  
  geom <- sf::st_geometry(millenium_grid_100[millenium_grid_100$id %in% cell_expo$id, ])
  cell_expo$geometry <- geom
  sf::st_geometry(cell_expo) <- "geometry"
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  ggplot2::ggplot()+
    ggplot2::theme_classic()+
    ggplot2::geom_sf(data = cell_expo, ggplot2::aes(fill = expo_1.5))+
    ggplot2::geom_sf(data = world, color = "lightgrey", fill = "lightgrey")+
    ggplot2::scale_fill_gradientn(colors = c("#66CCFF", "#FFFF99", "#FFCC33", "#FF3333"))
    
  
  coord <- sf::st_coordinates(cell_expo)
  
  lat <- coord[, "Y"]
  expo <- c(cell_expo$expo_present, cell_expo$expo_1.5)
  threshold <- c(rep("present", length(cell_expo$expo_present)), rep("1.5", length(cell_expo$expo_1.5)))
    
  lat_expo <- data.frame(expo, threshold, lat = rep(lat, 2))
  
  
  ggplot2::ggplot(lat_expo, ggplot2::aes(y = expo, x = lat, color = threshold))+
    ggplot2::geom_area()
  
}