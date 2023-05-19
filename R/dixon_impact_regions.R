get_impact_regions <- function(analyse_dixon, final_taxonomy, ecoregions, scale_impact){
  
  #targets::tar_load(analyse_dixon)
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(ecoregions)
  
  if(scale_impact == 1) {scale = analyse_dixon$ECOREGION}
  if(scale_impact == 2) {scale = analyse_dixon$regions}
  if(scale_impact == 3) {scale = analyse_dixon$PROVINCE}
  if(scale_impact == 4) {scale = analyse_dixon$REALM}
  
  analyse_dixon_sp     <- analyse_dixon[, !names(analyse_dixon) %in% c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4",
                                                                       "ECOREGION", "PROVINCE", "REALM", "Lat_Zone")]
  names_sp             <- paste(colnames(analyse_dixon_sp), sep = ",")
  sum_area_sp          <- colSums(analyse_dixon_sp)
  sp_nomatch           <- names(sum_area_sp)[sum_area_sp == 0]
  analyse_dixon        <- analyse_dixon[, c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4",
                                     "ECOREGION", "PROVINCE", "REALM", "Lat_Zone", names_sp)]
  
  impact_regions <- parallel::mclapply(levels(as.factor(scale)), function(region){
    
    #region = "Tropical Atlantic"
    message(region)
    if(all(scale == analyse_dixon$ECOREGION)) {region <- analyse_dixon[analyse_dixon$ECOREGION == region, ]}
    if(all(scale == analyse_dixon$regions))   {region <- analyse_dixon[analyse_dixon$regions == region, ]}
    if(all(scale == analyse_dixon$PROVINCE)) {region <- analyse_dixon[analyse_dixon$PROVINCE == region, ]}
    if(all(scale == analyse_dixon$REALM)) {region <- analyse_dixon[analyse_dixon$REALM == region, ]}
    
    sp_impacts <- lapply(11:ncol(region), function(sp){
        
      #sp = 150
      message(sp)
      select_sp    <- region[, c(1:10, sp)]
      sp_name      <- colnames(select_sp)[11]
      sp_family    <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp_name]
      sp_range     <- select_sp[select_sp[, 11] == 1, ]
      if(nrow(sp_range) == 0) {return(NULL)}
      area_range   <- nrow(sp_range)
      heat_stress  <- sp_range[, names(sp_range) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]
      level_stress <- colnames(heat_stress)
      if(all(scale == analyse_dixon$ECOREGION)) {region_name <- unique(select_sp[, names(select_sp) %in% "ECOREGION"])}
      if(all(scale == analyse_dixon$regions)) {region_name <- unique(select_sp[, names(select_sp) %in% "regions"])}
      if(all(scale == analyse_dixon$PROVINCE)) {region_name <- unique(select_sp[, names(select_sp) %in% "PROVINCE"])}
      if(all(scale == analyse_dixon$REALM)) {region_name <- unique(select_sp[, names(select_sp) %in% "REALM"])}
    
   
      area_impacts <- lapply(1:ncol(heat_stress), function(level){
     
        #level = 1
        heat_stress       <- heat_stress[, level]
        area_refuge       <- length(heat_stress[heat_stress <= 0.1])
        area_intermediate <- length(heat_stress[heat_stress > 0.1 & heat_stress <= 0.2])
        area_exposed      <- length(heat_stress[heat_stress > 0.2])
        sp_range          <- length(heat_stress)
        
        area_exposition <- data.frame(rbind(area_refuge, area_intermediate, area_exposed))
        
        area_exposition <- (area_exposition * 100) / sp_range
        
        return(area_exposition)
     
       })
   
      
       sp_area_impacts <- data.frame(do.call(cbind, area_impacts))
       
       colnames(sp_area_impacts) <- level_stress
       sp_area_impacts$region    <- rep(region_name, nrow(sp_area_impacts))
       sp_area_impacts$area      <- c("area_refuge", "area_intermediate", "area_exposed")
       sp_area_impacts$range     <- rep(area_range, nrow(sp_area_impacts))
       sp_area_impacts$specie    <- rep(sp_name, nrow(sp_area_impacts))
       sp_area_impacts$family    <- rep(sp_family, nrow(sp_area_impacts))
       
       sp_area_impacts <- sp_area_impacts[, c("region", "specie", "family", "range", "area", "present_stress", 
                                              "stress_1.5", "stress_2", "stress_3", "stress_4")]
   
       return(sp_area_impacts)
       
    })
    
    sp_exposition <- data.frame(do.call(rbind, sp_impacts))
    
    return(sp_exposition)
    
  })
    
    final_impacts_region <- data.frame(do.call(rbind, impact_regions))
    row.names(final_impacts_region) <- 1:nrow(final_impacts_region)
    
    return(final_impacts_region)
  
}