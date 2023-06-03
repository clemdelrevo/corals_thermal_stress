get_100km_dixon <- function(analyse_dixon, millenium_grid_100) {
  
  #targets::tar_load(analyse_dixon)
  #targets::tar_load(millenium_grid_100)
  
  data_100km <- data.frame(do.call(rbind, parallel::mclapply(1:nrow(millenium_grid_100), function(l) {
    
    #l = 1500
    message(l)
    millenium_line <- millenium_grid_100[l, ]
    
    intersect <- sf::st_intersects(analyse_dixon, millenium_line)
    
    analyse_dixon_sub <- analyse_dixon[sapply(intersect, length) != 0, ]
    analyse_dixon_sub$id <- rep(millenium_line$id, nrow(analyse_dixon_sub))
    
    return(analyse_dixon_sub)
    
  })))  
  
  data_100km
  
}