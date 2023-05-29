library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 8)

# sf options
sf::sf_use_s2(FALSE)

#pipeline
list(
  
  ## download data ---
  tar_target(ecoregions_shp, download_ecoregions(), format = "file")
 ,tar_target(corals_range_shp, download_corals_range(), format = "file")
 ,tar_target(thermal_dixon_csv, download_thermal_dixon(), format = "file")
 ,tar_target(thermal_kalmus_nc, download_thermal_kalmus(), format = "file")
 ,tar_target(coral_habitats, download_coral_habitats())
 ,tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
 ,tar_target(coral_traits, download_coral_traits(), format = "file")
 ,tar_target(reef_at_risk_shp, download_reef_at_risk(), format = "file")
 
 ## wrangle_data ---
 ,tar_target(thermal_dixon, wrangle_thermal_dixon(thermal_dixon_csv))
 ,tar_target(thermal_kalmus, wrangle_thermal_kalmus(thermal_kalmus_nc))
 ,tar_target(millenium_reef, wrangle_millenium_reef(millenium_reef_shp))
 ,tar_target(corals_range, wrangle_iucn_corals_range(corals_range_shp))
 ,tar_target(ecoregions, wrangle_ecoregions(ecoregions_shp))
 ,tar_target(coral_traits_final, wrangle_coral_traits(coral_traits))
 ,tar_target(habitat_sp, wrangle_coral_habitats(coral_habitats))
 ,tar_target(reef_at_risk, wrangle_reef_at_risk(reef_at_risk_shp))

 ## make taxonomy ---
 ,tar_target(taxonomy, make_taxonomy(coral_traits_final, habitat_sp, corals_range))
 
 ## get valid taxonomy database ---
 ,tar_target(valid_ctdb, get_valid_ctdb(taxonomy, coral_traits_final))
 ,tar_target(valid_corals_range, get_valid_coral_range(taxonomy, corals_range)) #, format = "file
 ,tar_target(valid_habitats, get_valid_habitats(taxonomy, habitat_sp))

 ## get final taxonomy ---
 ,tar_target(final_taxonomy, get_final_taxonomy(taxonomy, valid_ctdb, valid_habitats, valid_corals_range))
 
 ## get grid
 #,tar_target(millenium_grid, get_millenium_grid(millenium_reef, cellsize = 0.06981317))
 
 ## get specific richness map ---
 #,tar_target(global_specific_richness_map, get_specific_richness_map(final_taxonomy, coral_reef_grid))
 
 ## get analyses of dixon's data ---
 ,tar_target(analyse_dixon, get_analyse_dixon(thermal_dixon, final_taxonomy, ecoregions))
 
 ## get impact of thermal stress in dixon data ---
 ,tar_target(final_impacts_global, get_impact_global(analyse_dixon, final_taxonomy))
 ### at different scale ---
 ,tar_target(final_impacts_zee, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, reef_at_risk, scale_impact = 0))
 ,tar_target(final_impacts_ecoregion, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 1))
 ,tar_target(final_impacts_region, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 2))
 ,tar_target(final_impacts_province, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 3))
 ,tar_target(final_impacts_realm, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 4))
 
 ## get dixon boxplot threshold ---
 ,tar_target(boxplot_threshold_family, get_boxplot_impact_family(final_impacts_global))
 ,tar_target(boxplot_threshold_statut, get_boxplot_impact_statut(final_impacts_global))
 ,tar_target(boxplot_threshold_region, get_boxplot_impacts_region(final_impacts_region, final_impacts_global))
 
 ## get dixon plot ---
 ,tar_target(plot_stress_range, get_stress_range(final_impacts_global))
 
 ## get dixon map of impacts ---
 ,tar_target(ecoregion_map, impact_ecoregion_map(final_impacts_ecoregion, ecoregions))
 ,tar_target(province_map, impacts_province_map(final_impacts_province, ecoregions))
 ,tar_target(realm_map, impact_realm_map(final_impacts_realm, ecoregions))
 ,tar_target(inreef_map, impacts_inreef_map(final_impacts_ecoregion, ecoregions))

 ## analyse of kalmus data ---
 ,tar_target(analyse_kalmus, get_analyse_kalmus(thermal_kalmus, final_taxonomy))
 
 ## get impact of thermal stress in kalmus data
 ,tar_target(kalmus_impacts_family, get_impact_family(analyse_kalmus, final_taxonomy))
 
 ## get kalmus plot
 ,tar_target(plot_years_threshold, get_years_threshold(kalmus_impacts_family))
 
)
