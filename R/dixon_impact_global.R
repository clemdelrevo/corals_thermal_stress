get_impact_global <- function(analyse_dixon, final_taxonomy){

  #targets::tar_load(analyse_dixon)
  #targets::tar_load(final_taxonomy)
  
  analyse_dixon        <- analyse_dixon[, !names(analyse_dixon) %in% c("regions", "ECOREGION", "PROVINCE", "REALM", "Lat_Zone")]
  analyse_dixon_sp     <- analyse_dixon[, !names(analyse_dixon) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]
  sum_area_sp          <- colSums(analyse_dixon_sp)
  sp_nomatch           <- names(sum_area_sp)[sum_area_sp == 0]
  analyse_dixon        <- analyse_dixon[, ! (names(analyse_dixon) %in% sp_nomatch)]
  
  
  sp_impacts <- parallel::mclapply(6:ncol(analyse_dixon), function(sp){
    
    #sp = 205
    message(sp)
    select_sp    <- analyse_dixon[, c(1:5,sp)]
    sp_name      <- colnames(select_sp)[6]
    sp_family    <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp_name]
    sp_statut    <- final_taxonomy$final_statut[final_taxonomy$final_genus_sp %in% sp_name]
    sp_inreef    <- final_taxonomy$in_reef[final_taxonomy$final_genus_sp %in% sp_name]
    sp_outreef   <- final_taxonomy$out_reef[final_taxonomy$final_genus_sp %in% sp_name]
    sp_range     <- select_sp[select_sp[, 6] == 1, ]
    area_range   <- nrow(sp_range)
    heat_stress  <- sp_range[, names(sp_range) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]
    level_stress <- colnames(heat_stress)
    
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
    
     sp_area_impacts <- do.call(cbind, area_impacts)
     
     colnames(sp_area_impacts) <- level_stress
     sp_area_impacts$area      <- c("area_refuge", "area_intermediate", "area_exposed")
     sp_area_impacts$specie    <- rep(sp_name, nrow(sp_area_impacts))
     sp_area_impacts$range     <- rep(area_range, nrow(sp_area_impacts))
     sp_area_impacts$family    <- rep(sp_family, nrow(sp_area_impacts))
     sp_area_impacts$in_reef   <- rep(sp_inreef, nrow(sp_area_impacts))
     sp_area_impacts$out_reef  <- rep(sp_outreef, nrow(sp_area_impacts))
     sp_area_impacts$statut    <- rep(sp_statut, nrow(sp_area_impacts))
     
     sp_area_impacts <- sp_area_impacts[, c("specie", "family", "range", "area", "present_stress", 
                                            "stress_1.5", "stress_2", "stress_3", "stress_4",
                                            "in_reef", "out_reef", "statut")]
    
     return(sp_area_impacts)
     
  })
  
  final_impacts_global <- data.frame(do.call(rbind, sp_impacts))
  row.names(final_impacts_global) <- 1:nrow(final_impacts_global)
  
  return(final_impacts_global)
  
}
  