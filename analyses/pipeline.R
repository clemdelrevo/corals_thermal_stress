library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 5)

# sf options
sf::sf_use_s2(FALSE)

#pipeline
list(
  
  ## option ---
  tar_target(ts_data_source, "kalmus_ef")
  
  ## download data ---
 ,tar_target(ecoregions_shp, download_ecoregions(), format = "file")
 ,tar_target(corals_range_shp, download_corals_range(), format = "file")
 ,tar_target(thermal_stress_data, download_thermal_stress(ts_data_source), format = "file")
 ,tar_target(coral_habitats, download_coral_habitats())
 ,tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
 ,tar_target(coral_traits, download_coral_traits(), format = "file")
 ,tar_target(correspondance_aca_iucn_csv, download_correspondance_aca_iucn(), format = "file")
 
 ## wrangle_data ---
 ,tar_target(thermal_stress, wrangle_thermal_stress(thermal_stress_data, ts_data_source))
 ,tar_target(millenium_reef, wrangle_millenium_reef(millenium_reef_shp))
 ,tar_target(corals_range, wrangle_iucn_corals_range(corals_range_shp))
 ,tar_target(ecoregions, wrangle_ecoregions(ecoregions_shp))
 ,tar_target(coral_traits_final, wrangle_coral_traits(coral_traits))
 ,tar_target(habitat_sp, wrangle_coral_habitats(coral_habitats))
 ,tar_target(correspondance_aca_iucn, wrangle_correspondance_aca_iucn(correspondance_aca_iucn_csv))

 ## make taxonomy ---
 ,tar_target(taxonomy, make_taxonomy(coral_traits_final, habitat_sp, corals_range))
 
 ## get valid taxonomy database ---
 ,tar_target(valid_ctdb, get_valid_ctdb(taxonomy, coral_traits_final))
 ,tar_target(valid_corals_range, get_valid_coral_range(taxonomy, corals_range)) #, format = "file
 ,tar_target(valid_habitats, get_valid_habitats(taxonomy, habitat_sp, correspondance_aca_iucn, Other = 1))

 ## get final taxonomy ---
 ,tar_target(final_taxonomy, get_final_taxonomy(taxonomy, valid_ctdb, valid_habitats, valid_corals_range))
 
 ## get specific richness map ---
 #,tar_target(global_specific_richness_map, get_specific_richness_map(final_taxonomy, coral_reef_grid))
 

)