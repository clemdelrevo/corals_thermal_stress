# Create global grid with 4km cells that contain coral reef --------------------

get_millenium_grid <- function(millenium_reef, cellsize = 0.06981317) {
  
  #targets::tar_load(millenium_reef)
  
  grid <- sf::st_make_grid(millenium_reef, cellsize = cellsize, what = "polygons",
                           square = TRUE)
  
  grid <- sf::st_as_sf(grid)
  
  reef_inter_grid <- sf::st_intersects(grid, millenium_reef)
  # remove cells doesn't intersect with millenium reef
  sub_grid <- grid[sapply(reef_inter_grid, length) != 0,]
  # give name for each cell
  sub_grid$id <- paste0("cell_", 1:nrow(sub_grid))
  # place name in first
  sub_grid <- dplyr::relocate(sub_grid, id, .before = x)
  
  sf::st_geometry(sub_grid) <- "geometry"
  
  return(sub_grid)
  
}


# Create global grid with ~100km cells that contain coral reef -----------------

get_millenium_grid <- function(millenium_reef, cellsize = 1) {
  
  #targets::tar_load(millenium_reef)
  
  grid <- sf::st_make_grid(millenium_reef, cellsize = cellsize, what = "polygons",
                           square = TRUE)
  
  grid <- sf::st_as_sf(grid)
  
  reef_inter_grid <- sf::st_intersects(grid, millenium_reef)
  # remove cells doesn't intersect with millenium reef
  sub_grid <- grid[sapply(reef_inter_grid, length) != 0,]
  # give name for each cell
  sub_grid$id <- paste0("cell_", 1:nrow(sub_grid))
  # place name in first
  sub_grid <- dplyr::relocate(sub_grid, id, .before = x)
  
  sf::st_geometry(sub_grid) <- "geometry"
  
  return(sub_grid)
  
}