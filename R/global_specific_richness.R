get_specific_richness_map <- function(final_taxonomy, coral_reef_grid){
  
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(coral_reef_grid)
  
  specific_richnesse_grid <- sf::st_intersects(coral_reef_grid, final_taxonomy)
  
  richness <- sapply(specific_richnesse_grid, length)
  
  coral_reef_grid$spe_richness <- richness
  
  world <- rnaturalearth::ne_countries(scale='medium',returnclass = 'sf')
  world <- sf::st_transform(world, crs = "EPSG:4326")
  
  global_specific_richness_map <- ggplot2::ggplot(data = world)+
    ggplot2::geom_sf()+
    ggplot2::geom_sf(data = coral_reef_grid, ggplot2::aes(fill = spe_richness))+
    ggplot2::scale_fill_viridis_c()
  
  return(global_specific_richness_map)
  
}