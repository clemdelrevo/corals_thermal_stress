library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 7)

# sf options
sf::sf_use_s2(FALSE)

#pipeline
list(
  
  ## download data ---
  tar_target(ecoregions_shp, download_ecoregions(), format = "file")
 ,tar_target(corals_range_shp, download_corals_range(), format = "file")
 ,tar_target(thermal_dixon_csv, download_thermal_dixon(), format = "file")
 ,tar_target(coral_habitats, download_coral_habitats())
 ,tar_target(coral_traits, download_coral_traits(), format = "file")
 #,tar_target(millenium_reef_shp, download_millenium_reef(), format = "file")
 
 ## wrangle_data ---
 ,tar_target(thermal_dixon, wrangle_thermal_dixon(thermal_dixon_csv))
 ,tar_target(corals_range, wrangle_iucn_corals_range(corals_range_shp))
 ,tar_target(ecoregions, wrangle_ecoregions(ecoregions_shp))
 ,tar_target(coral_traits_final, wrangle_coral_traits(coral_traits))
 ,tar_target(habitat_sp, wrangle_coral_habitats(coral_habitats))
 #,tar_target(millenium_reef, wrangle_millenium_reef(millenium_reef_shp))

 ## make taxonomy ---
 ,tar_target(taxonomy, make_taxonomy(coral_traits_final, habitat_sp, corals_range))
 
 ## get valid taxonomy database ---
 ,tar_target(valid_ctdb, get_valid_ctdb(taxonomy, coral_traits_final))
 ,tar_target(valid_corals_range, get_valid_coral_range(taxonomy, corals_range)) #, format = "file
 ,tar_target(valid_habitats, get_valid_habitats(taxonomy, habitat_sp))

 ## get final taxonomy ---
 ,tar_target(final_taxonomy, get_final_taxonomy(taxonomy, valid_ctdb, valid_habitats, valid_corals_range))
 
 ## get analyses of dixon's data ---
 ,tar_target(analyse_dixon, get_analyse_dixon(thermal_dixon, final_taxonomy, ecoregions))
 
 ## get impact of thermal stress in dixon data ---
 ,tar_target(final_impacts_global, get_impact_global(analyse_dixon, final_taxonomy))
 ,tar_target(final_impacts_trait, get_darling_trait(final_impacts_global))
 ### at different scale ---
 ,tar_target(final_impacts_ecoregion, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 1))
 ,tar_target(final_impacts_region, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 2))
 ,tar_target(final_impacts_province, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 3))
 ,tar_target(final_impacts_realm, get_impact_regions(analyse_dixon, final_taxonomy, ecoregions, scale_impact = 4))
 
 ## get dixon boxplot threshold ---
 ,tar_target(boxplot_threshold_family, get_boxplot_impact_family(final_impacts_global))
 ,tar_target(boxplot_threshold_statut, get_boxplot_impact_statut(final_impacts_global))
 ,tar_target(boxplot_threshold_habitat, get_boxplot_impact_habitat(final_impacts_global))
 ,tar_target(boxplot_threshold_trait, get_boxplot_impact_trait(final_impacts_trait))
 
 ## get dixon plot ---
 ,tar_target(histo_sp, get_sp_histo(final_impacts_global))
 ,tar_target(plot_stress_range, get_stress_range(final_impacts_global))
 
 ## get dixon map of impacts ---
 ,tar_target(ecoregion_map, impact_ecoregion_map(final_impacts_ecoregion, ecoregions))
 ,tar_target(province_map, impacts_province_map(final_impacts_province, ecoregions))
 ,tar_target(realm_map, impact_realm_map(final_impacts_realm, ecoregions))
 ,tar_target(richness_map, get_richness_map(final_impacts_ecoregion, ecoregions))
 ,tar_target(hotspot_map, get_hotspot_map(biodiv_stat))
 
 ## get dixon figure ---
 ,tar_target(boxplot_eco_realm, get_boxplot_ecoregion_realm(ecoregions, final_impacts_ecoregion, final_impacts_realm))

 ## get biodiveristy stat ---
 ,tar_target(biodiv_stat, get_biodiv_stat(ecoregion_map, final_impacts_ecoregion, final_taxonomy, ecoregions, analyse_dixon, final_impacts_global))
 
 ## export figure ---
 ,tar_target(export_boxplot, get_export_boxplot(boxplot_threshold_family, boxplot_threshold_statut, boxplot_threshold_habitat, boxplot_threshold_trait))
 
)
