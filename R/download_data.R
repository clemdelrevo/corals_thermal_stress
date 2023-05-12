# --- ECOREGIONS ---------------------------------------------------------------

# Download Marine Ecoregions of the World (MEOW) 

# MEOW is a biogeographic classification 
# of the world's coasts and shelves.

download_ecoregions <- function(){
  
  ## download, unzip and drop zip
  ecoregions_url <- paste0("https://files.worldwildlife.org/wwfcmsprod/files/",
                           "Publication/file/7gger8mmke_MEOW_FINAL.zip?_ga=2.",
                           "184401894.1710969610.1677670150-1612025012.1677670137")
  ecoregions_dir <- "data/ecoregions/"
  ecoregions_zip <- "data/ecoregions/MEOW.zip"
  ecoregions_shp <- "data/ecoregions/meow_ecos.shp"
  delete.file    <- "data/ecoregions/MEOW"
  dir.create(ecoregions_dir)
  download.file(url = ecoregions_url,
                destfile = ecoregions_zip,
                method = "curl")
  unzip(ecoregions_zip, exdir = ecoregions_dir)
  unlink(ecoregions_zip)
  files.list <- list.files(file.path(delete.file))
  filesstrings::file.move(file.path(delete.file, files.list), ecoregions_dir)
  file.remove(delete.file)
  
  return(ecoregions_shp)
  
}

# ---- CORALS RANGE MAP --------------------------------------------------------

# Download extend occurence of corals

#This dataset contains distribution information on corals assessed for 
#The IUCN Red List of Threatened Species

download_corals_range <- function(){
  
  #download to https://www.iucnredlist.org/resources/files/88f8addb-7d89-4e2a-9284-5862824797c3
  
  #dir.create("data/iucn_corals")
  corals_range_path <- "data/iucn_corals/"
  corals_range_shp  <- list.files(corals_range_path, pattern = ".shp$", full.names = TRUE)
  return(corals_range_shp)
}

# ---- THERMAL STRESS ---------------------------------------------------------

# download thermal projections

#This dataset contains longitude and latitude area and associate probability 
#of thermal stress > 4°C within 1986-2019 climate, and 1.5°C, 2°C, 3°C and 4°C 
#projections global warming

#this data has been converted from xls to csv

download_thermal_stress <- function(ts_data_source){
  
  if (ts_data_source == "dixon_pclm") {
  
    #download to https://doi.org/10.1371/journal.pclm.0000004.s013
    
    #dir.create("data/thermal_stress")
    thermal_stress_csv <- "data/thermal_stress/dixon_pclm/journal.pclm.0000004.s013.csv"
    return(thermal_stress_csv)
  }
  
  if (ts_data_source =="kalmus_ef") {
    
    return(list.files("data/thermal_stress/kalmus_ef", full.names = TRUE))
    
  }
  
}

# ---- CORAL X HABITATS ASSOCIATIONS -------------------------------------------

# download habitats of corals

# this data associate habitat to each corals species

download_coral_habitats <- function() {
  
  message("# ---- gathering coral X habitats associations")
  
  avail_groups <- rredlist::rl_comp_groups()
  
  if (! "reef_building_corals" %in% avail_groups$result$group_name) stop("Group 'reef_building_corals' not available via 'rredlist'")
  
  coral_sps <- rredlist::rl_comp_groups(group = "reef_building_corals")
  
  coral_habitats <- setNames(lapply(coral_sps$result$taxonid, function(id) {
    message("-- taxonid: ", id)
    Sys.sleep(2)
    rredlist::rl_habitats(id = id)
  }), coral_sps$result$taxonid)
  
  # do.call(rbind,
  # r$result$id <- r$id
  # dplyr::relocate(r$result, id, .before = code)
  
  list(habitats_coral_sps = coral_sps,
       coral_habitats = coral_habitats)
  
}

# --- GLOBAL REEF LOCATION -----------------------------------------------------

# download Millennium Coral Reef Mapping Project

#This dataset shows the global distribution of coral reefs in 
#tropical and subtropical regions

download_millenium_reef <- function(overwrite = TRUE){
  
  # download to https://data.unep-wcmc.org/datasets/1
  millenium_reef_url      <- paste0("https://datadownload-production.s3.us-east-1.",
                               "amazonaws.com/WCMC008_CoralReefs2021_v4_1.zip")
  millenium_reef_path     <- "data/"
  millenium_reef_zip      <- "data/WCMC008_CoralReefs2021_v4_1.zip"
  download.file(url = millenium_reef_url, destfile = millenium_reef_zip,
                method = "curl")
  unzip(millenium_reef_zip, exdir = millenium_reef_path)
  unlink(millenium_reef_zip)
  millenium_reef_old_name <- "data/14_001_WCMC008_CoralReefs2021_v4_1"
  millenium_reef_new_name <- "data/millenium_reef"
  file.rename(millenium_reef_old_name, millenium_reef_new_name)
  millenium_reef_shp      <- paste0("data/millenium_reef/",
                                  "01_Data/WCMC008_CoralReef2021_Py_v4_1.shp")
  
  return(millenium_reef_shp)
  
}

# ---- GEOMORPHIC REEF MAP -----------------------------------------------------

# Download geomorphic data

#this data set contains global geomorphic reef data map

download_geomorphic_reef <- function(){
  
  #dir.create("data/geomorphic_reef")
  geomorphic_reef_path <- "data/geomorphic_reef"
  geomorphic_reef_gpkg <- list.files(geomorphic_reef_path, pattern = ".gpkg",
                                     full.names = TRUE)
  
  return(geomorphic_reef_gpkg)
  
}

# --- BATHYMETRY ---------------------------------------------------------------

# Download GEBCO Bathymetry

# The GEBCO_2022 Grid was published in June 2022 and is a global terrain model
# for ocean and land, providing elevation data, in meters, on a 15 arc-second
# interval grid.

download_gebco <- function(overwrite = TRUE){
  
  # download, unzip and drop zip
  gebco_url <- paste0("https://www.bodc.ac.uk/data/open_download/gebco/",
                      "gebco_2022_sub_ice_topo/zip/")
  gebco_dir <- "data/gebco/"
  gebco_zip <- "data/gebco/gebco_2022_sub_ice_topo.zip"
  gebco_nc  <- "data/gebco/GEBCO_2022_sub_ice_topo.nc"
  dir.create(gebco_dir)
  download.file(url = gebco_url,
                destfile = gebco_zip,
                method = "curl")
  unzip(gebco_zip, exdir = gebco_dir)
  unlink(gebco_zip)
  
  return(gebco_nc)
  
}

# ---- CORALS TRAITS -----------------------------------------------------------

# Download depth corals traits

# The Coral Trait Database is a growing compilation of scleractinian 
# coral life history trait, phylogenetic and biogeographic data

download_coral_traits <- function(){
  
  depth_upper_url   <- "https://coraltraits.org/traits/91.csv"
  depth_lower_url   <- "https://coraltraits.org/traits/92.csv"
  depth_traits_path <- "data/coral_traits"
  depth_traits_csv  <- paste0(depth_traits_path,  "data", format(Sys.Date(), format = "%Y%m%d"))
  depth_upper_csv   <- paste0(depth_traits_path, "/depth_upper.csv")
  depth_lower_csv   <- paste0(depth_traits_path, "/depth_lower.csv")
  
  dir.create(depth_traits_path)
  download.file(url = depth_upper_url, destfile = depth_traits_csv,
                method = "curl")
  file.rename(depth_traits_csv, depth_upper_csv)
  
  download.file(url = depth_lower_url, destfile = depth_traits_csv,
                method = "curl")
  file.rename(depth_traits_csv, depth_lower_csv)
  
  coral_traits <- list.files(depth_traits_path, pattern = ".csv$",
                             full.names = TRUE)
  
  return(coral_traits)
  
}

# ---- IUCN AND ACA HABITATS ---------------------------------------------------

# This dataset was create for standardize corals living habitats between 
# IUCN and ACA data

download_correspondance_aca_iucn <- function(){
  
  correspondance_aca_iucn_csv <- "data/correspondance_aca_iucn/correspondance_aca_iucn.csv"
  
  return(correspondance_aca_iucn_csv)
  
}