get_impact_regions <- function(analyse_dixon, final_taxonomy, ecoregions, reef_at_risk, scale_impact){
  
  #targets::tar_load(analyse_dixon)
  #targets::tar_load(final_taxonomy)
  #targets::tar_load(ecoregions)
  #targets::tar_load(reef_at_risk)
  
  if(scale_impact == 0) {
    
    analyse_dixon <- parallel::mclapply(levels(as.factor(reef_at_risk$COUNTRY)), function(country) {
      
      #country = "VIETNAM"
      message(country)
      country      <- reef_at_risk[reef_at_risk$COUNTRY == country, ]
      intersection <- sf::st_intersection(analyse_dixon, country)
      if(nrow(intersection) == 0) {return(NULL)}
      
      return(intersection)
      
    })
    
    analyse_dixon <- do.call(rbind, analyse_dixon)
    scale = analyse_dixon$COUNTRY
    
  }
  
  if(scale_impact == 1 | scale_impact == 2 | scale_impact == 3 | scale_impact == 4) {
  
  analyse_dixon <- parallel::mclapply(levels(as.factor(ecoregions$ECOREGION)), function(region){
    
    #region = "Yellow Sea"
    message(region)
    ecoregion    <- ecoregions[ecoregions$ECOREGION == region, ]
    intersection <- sf::st_intersection(analyse_dixon, ecoregion)
    if(nrow(intersection) == 0) {return(NULL)}
    
    return(intersection)
    
  })
  
  analyse_dixon <- do.call(rbind, analyse_dixon)
  
  }
  
  analyse_dixon <- sf::st_drop_geometry(analyse_dixon)
  
  if(scale_impact == 1) {scale = analyse_dixon$ECOREGION}
  if(scale_impact == 2) {scale = analyse_dixon$regions}
  if(scale_impact == 3) {scale = analyse_dixon$PROVINCE}
  if(scale_impact == 4) {scale = analyse_dixon$REALM}
  
  if(scale_impact == 0) {
    
    col_name <- c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4",
                  "COUNTRY", "Alt_Name", "R_Dep_Indx", "R_Dep_Cat", "Adapt_Indx", "Adapt_Cat", "Vuln_Indx", "Vuln_Cat", "Notes")
    }
  
  if(scale_impact == 1 | scale_impact == 2 | scale_impact == 3 | scale_impact == 4) {
    col_name <- c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4",
                  "ECOREGION", "PROVINCE", "REALM", "Lat_Zone")
  }
  
  if(scale_impact == 1 | scale_impact == 2 | scale_impact == 3 | scale_impact == 4) {
    analyse_dixon <- analyse_dixon[, !names(analyse_dixon) %in% c("ECO_CODE", "PROV_CODE", "RLM_CODE", "ALT_CODE", "ECO_CODE_X")]
  }
  
  analyse_dixon_sp     <- analyse_dixon[, !names(analyse_dixon) %in% col_name]
  sum_area_sp          <- colSums(analyse_dixon_sp)
  sp_nomatch           <- names(sum_area_sp)[sum_area_sp == 0]
  analyse_dixon        <- analyse_dixon[, !names(analyse_dixon) %in% sp_nomatch]
  
  if(scale_impact == 0) {
    sp_name       <- paste(names(analyse_dixon[, !names(analyse_dixon) %in% col_name]))
    analyse_dixon <- analyse_dixon[, c(col_name, sp_name)]
    }
  
  if(scale_impact == 1 | scale_impact == 2 | scale_impact == 3 | scale_impact == 4) {
     sp_name       <- paste(names(analyse_dixon[, !names(analyse_dixon) %in% col_name]))
     analyse_dixon <- analyse_dixon[, c(col_name, sp_name)]
  }
  
  impact_regions <- lapply(levels(as.factor(scale)), function(region){
    
    #region = "AMERICAN SAMOA"
    message(region)
    region_sub <- analyse_dixon[scale == region, ]

    sp_impacts <- parallel::mclapply(colnames(region_sub[, names(region_sub) %in% sp_name]), function(sp){
        
      #sp = "Psammocora_digitata"
      message(sp)
      select_sp    <- region_sub[, names(region_sub) %in% c(col_name, sp)]
      sp_name      <- sp
      sp_family    <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp_name]
      sp_range     <- select_sp[select_sp[, names(select_sp) %in% sp] == 1, ]
      if(nrow(sp_range) == 0) {return(NULL)}
      region_name  <- region
      area_range   <- nrow(sp_range)
      heat_stress  <- sp_range[, names(sp_range) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]
      level_stress <- colnames(heat_stress)
      
      if(any(colnames(region_sub) %in% "COUNTRY")) {
        
        dependance    <- unique(region_sub$R_Dep_Cat)
        adaptability  <- unique(region_sub$Adapt_Cat)
        vulnerability <- unique(region_sub$Vuln_Cat)
        note          <- unique(region_sub$Notes)
        
      }
      
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
       
       if(any(colnames(region_sub) %in% "COUNTRY")) {
         
         sp_area_impacts$dependance   <- rep(dependance, nrow(sp_area_impacts))
         sp_area_impacts$adaptability <- rep(adaptability, nrow(sp_area_impacts))
         sp_area_impacts$vulnerability <- rep(vulnerability, nrow(sp_area_impacts))
         sp_area_impacts$note         <- rep(note, nrow(sp_area_impacts))
         
       } 
       
       if(any(colnames(region_sub) %in% "COUNTRY")) {
         
         sp_area_impacts <- sp_area_impacts[, c("region", "specie", "family", "range", "area", "present_stress", 
                                                "stress_1.5", "stress_2", "stress_3", "stress_4",
                                                "dependance", "adaptability", "vulnerability", "note")]
         
       } else {
         
       sp_area_impacts <- sp_area_impacts[, c("region", "specie", "family", "range", "area", "present_stress", 
                                              "stress_1.5", "stress_2", "stress_3", "stress_4")]
       }
      
       return(sp_area_impacts)
       
    })
    
    sp_exposition <- data.frame(do.call(rbind, sp_impacts))
    
    return(sp_exposition)
    
  })
    
    final_impacts_region <- data.frame(do.call(rbind, impact_regions))
    row.names(final_impacts_region) <- 1:nrow(final_impacts_region)
    
    return(final_impacts_region)
  
}