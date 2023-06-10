
get_boxplot_ecoregion_realm <- function(ecoregions, final_impacts_ecoregion, final_impacts_realm) {
  
  #targets::tar_load(ecoregions)
  #targets::tar_load(final_impacts_ecoregion)
  #targets::tar_load(final_impacts_realm)
  
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$area == "area_exposed", ]
  final_impacts_ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$family != "Tubiporidae" & final_impacts_ecoregion$family != "Scleractinia incertae sedis", ]
  final_impacts_realm     <- final_impacts_realm[final_impacts_realm$area == "area_exposed", ]
  final_impacts_realm     <- final_impacts_realm[final_impacts_realm$family != "Tubiporidae" & final_impacts_realm$family != "Scleractinia incertae sedis", ]
  
  realm <- unique(ecoregions$REALM[ecoregions$Lat_Zone == "Tropical"])
  
  boxplot_eco_realm <- setNames(lapply(levels(as.factor(realm)), function(realm) {
  
    #realm = "Central Indo-Pacific"
    ecoregion <- ecoregions$ECOREGION[ecoregions$REALM %in% realm]
    ecoregion <- final_impacts_ecoregion[final_impacts_ecoregion$region %in% ecoregion, ]
    
    n <- ecoregion |> dplyr::group_by(region) |> dplyr::summarise(count = dplyr::n())
    
    boxplot_eco <- ggplot2::ggplot(ecoregion, ggplot2::aes(x = region, y = stress_1.5, fill = region))+
      ggplot2::theme_classic()+
      ggplot2::geom_boxplot(size = 0.2, outlier.size = 0.2)+
      ggplot2::ylim(-5, 100)+
      ggplot2::xlab("")+
      ggplot2::ylab("% d'exposition de l'aire de distribution\n au stress thermique")+
      ggplot2::ggtitle(realm)+
      ggplot2::geom_hline(yintercept = 90, color = "red", linetype = "dashed")+
      ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90),
                     plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                     panel.border = ggplot2::element_rect(linewidth = 1.5, fill = NA),
                     plot.title = ggplot2::element_text(face = "bold", hjust = 0),
                     )+
      ggplot2::geom_text(data = n, ggplot2::aes(x = region, y = -5, label = paste0("n = ", count)), show.legend = FALSE, size = 3, fontface = "italic", angle = 90)
    
    realm_sub <- final_impacts_realm[final_impacts_realm$region == realm, ]
    
    n <- realm_sub |> dplyr::group_by(region) |> dplyr::summarise(count = dplyr::n())
    
    boxplot_realm <- ggplot2::ggplot(realm_sub, ggplot2::aes(x = region, y = stress_1.5, fill = region))+
      ggplot2::theme_classic()+
      ggplot2::geom_boxplot(width = 0.3, outlier.shape = NA)+
      ggplot2::geom_jitter(width = 0.08, alpha = 0.6, size = 0.8)+
      ggplot2::ylim(-2, 100)+
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.line = ggplot2::element_blank(),
                     legend.position = "none",
                     plot.margin = ggplot2::margin(0, 19, 0, 0, "cm"))+
      ggplot2::geom_text(data = n, ggplot2::aes(x = region, y = -2, label = paste0("n = ", count)), show.legend = FALSE, size = 3, fontface = "italic")
  
    cowplot::plot_grid(boxplot_eco, boxplot_realm, nrow = 1, align = "h", axis = "lr", rel_widths = c(1,1))+
      ggplot2::theme(plot.margin = ggplot2::margin(0, -18, 0, 0, "cm"))
    
  }), realm[order(realm)])
  
  dir.create("outputs/figure/boxplot_eco_realm/", showWarnings = FALSE)
  
  ggplot2::ggsave("outputs/figure/boxplot_eco_realm/Tropical_Atlantic.png", plot = boxplot_eco_realm$`Tropical Atlantic`, dpi = 500,
                  width = 12, height = 8)
  
  ggplot2::ggsave("outputs/figure/boxplot_eco_realm/Western_Indo_Pacific.png", plot = boxplot_eco_realm$`Western Indo-Pacific`, dpi = 500,
                  width = 12, height = 8)
  
  ggplot2::ggsave("outputs/figure/boxplot_eco_realm/Central_Indo_Pacific.png", plot = boxplot_eco_realm$`Central Indo-Pacific`, dpi = 500,
                  width = 12, height = 8)
  
  ggplot2::ggsave("outputs/figure/boxplot_eco_realm/Tropical_Eastern_Pacific.png", plot = boxplot_eco_realm$`Tropical Eastern Pacific`, dpi = 500,
                  width = 12, height = 8)
  
  ggplot2::ggsave("outputs/figure/boxplot_eco_realm/Eastern_Indo_Pacific.png", plot = boxplot_eco_realm$`Eastern Indo-Pacific`, dpi = 500,
                  width = 12, height = 8)
  
}


