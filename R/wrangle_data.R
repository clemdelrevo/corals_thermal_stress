# Read the data and put some minors transformations ----------------------------

wrangle_thermal_dixon <- function(thermal_dixon_csv){
  
  #targets::tar_load(thermal_dixon_csv)
  
    # read data and rename columns
    thermal_dixon        <- read.csv2(thermal_dixon_csv, header = FALSE, dec = ",", skip = 2)
    names(thermal_dixon) <- c("regions", "longitude", "latitude", "present_stress",
                               "stress_1.5", "stress_2", "stress_3", "stress_4")
    
    # transform in sf object
    thermal_dixon <- sf::st_as_sf(thermal_dixon, coords = c("longitude", "latitude"))
    # put crs
    sf::st_crs(thermal_dixon) <- 4326
    thermal_dixon             <- sf::st_make_valid(thermal_dixon)
    
    return(thermal_dixon)
  }
  
wrangle_thermal_kalmus <- function(thermal_kalmus_nc) {

    
    splits <- strsplit(basename(thermal_kalmus_nc), "_")
    th_data <- data.frame(ssp = sapply(splits, "[", 5),
                          thermal_departure  = sapply(splits, "[", 6),
                          threshold = sapply(strsplit(sapply(splits, "[", 7), "\\."), "[", 1),
                          file = thermal_stress_data)
    th_data$col_name <- apply(th_data[, c("ssp", "thermal_departure", "threshold")], 1, paste, collapse = "_")
    
    #get coords
    d <- stars::read_ncdf(thermal_stress_data[1])
    
    thermal_stress <- data.frame(x  = d$lon, y = d$lat)
    
    depart_years <- round(setNames(data.frame(lapply(1:nrow(th_data), function(l) {
      d <- stars::read_ncdf(th_data[l, "file"])
      d$departure_year
    })), th_data$col_name))
    
    thermal_kalmus <- cbind(thermal_stress, depart_years)
    thermal_kalmus <- sf::st_as_sf(thermal_kalmus, coords = c("x", "y"))
    sf::st_crs(thermal_kalmus) <- sf::st_crs(4326)
    thermal_kalmus <- sf::st_wrap_dateline(thermal_kalmus)
    
    return(thermal_kalmus)
    
}

wrangle_millenium_reef <- function(millenium_reef_shp){
  
  #targets::tar_load(millenium_reef_shp)
  
  # read data
  millenium_reef <- sf::st_read(millenium_reef_shp)
  millenium_reef <- sf::st_make_valid(millenium_reef)
  
  return(millenium_reef)

}

wrangle_iucn_corals_range <- function(corals_range_shp){
  
  #targets::tar_load(corals_range_shp)
  #targets::tar_load(crs)
  
  # read data
  corals_range <- do.call(rbind, lapply(corals_range_shp, sf::st_read))
  corals_range <- sf::st_make_valid(corals_range)  
  # replace space by underscore
  corals_range$sci_name <- gsub(" ", "_", corals_range$sci_name)
  
  return(corals_range)
  
}

wrangle_ecoregions <- function(ecoregions_shp){
  
  # read data
  ecoregions <- sf::st_read(ecoregions_shp)
  ecoregions <- sf:: st_make_valid(ecoregions)
  
  return(ecoregions)
}

wrangle_coral_habitats <- function(coral_habitats, hab_ok = c("Suitable")){
  
  #targets::tar_load(coral_habitats)
  
  # simplification of data by species
  habitat_sp <- sapply(1:length(coral_habitats$coral_habitats), function(n){
    
    #n = 300
    id          <- coral_habitats$coral_habitats[[n]][1]
    id          <- unlist(id)
    habitat     <- coral_habitats$coral_habitats[[n]]$result$habitat
    habitat     <- unlist(habitat)
    suitability <- coral_habitats$coral_habitats[[n]]$result$suitability
    suitability <- unlist(suitability)
    data        <- data.frame(cbind(id, habitat, suitability))
    data$category <- rep(coral_habitats$habitats_coral_sps$result$category[[n]], nrow(data))
    
    return(data)
    
  })
  
  # replace space by underscore
  sp_name       <- gsub(" ", "_", coral_habitats$habitats_coral_sps$result$scientific_name)
  
  names(habitat_sp) <- sp_name
  
  # filtrate by suitable habitat
  habitat_sp <- lapply(habitat_sp, function(n){
    
    #n = habitat_sp[[1]]
    df_filter <- n[n$suitability %in% hab_ok, ]
    df_filter <- na.omit(df_filter)
    
    return(df_filter)
  
  })
  
  return(habitat_sp)
  
}
  
wrangle_coral_traits <- function(coral_traits){
  
  #targets::tar_load(coral_traits)
  
  # read data
  depth_lower  <- read.csv(coral_traits[[1]], dec = ",")
  depth_upper  <- read.csv(coral_traits[[2]], dec = ",")
  
  # merge the two dataframe
  merge_traits <- merge(depth_lower, depth_upper, by = "specie_name")
  
  # selection of interest variables
  merge_traits <- merge_traits |>
    dplyr::select(specie_name, value.x, value.y)
  
  # rename variables
  names(merge_traits) <- c("specie_name", "depth_lower", "depth_upper")
  
  # using higher deep depth and lower shallow depth of species
  coral_traits_final <- merge_traits |>
    dplyr::group_by(specie_name) |>
    dplyr::summarise(depth_lower = max(depth_lower),
                     depth_upper = min(depth_upper))
  
  coral_traits_final <- as.data.frame(coral_traits_final)
  
  # transform depth variables as integer
  coral_traits_final$depth_lower <- as.integer(coral_traits_final$depth_lower)
  coral_traits_final$depth_upper <- as.integer(coral_traits_final$depth_upper)
  # replace space by underscore
  coral_traits_final$specie_name <- gsub(" ", "_", coral_traits_final$specie_name)
  # modify shallow depth living at 1 meter
  coral_traits_final$depth_upper[coral_traits_final$depth_upper == 0] = 1
  
  return(coral_traits_final)
  
}

wrangle_correspondance_aca_iucn <- function(correspondance_aca_iucn_csv){
  
  # read data
  read.csv2(correspondance_aca_iucn_csv,
            check.names = FALSE,
            row.names = 1)
  
}

wrangle_reef_at_risk <- function(reef_at_risk_shp) {
  
  #targets::tar_load(reef_at_risk_shp)
  
  reef_at_risk <- sf::read_sf(reef_at_risk_shp)
  reef_at_risk <- sf::st_make_valid(reef_at_risk)
  reef_at_risk <- sf::st_transform(reef_at_risk, 4326)
  
  return(reef_at_risk)
  
}