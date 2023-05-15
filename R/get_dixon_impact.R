get_impact_sp <- function(analyses_dixon, final_taxonomy){

  #targets::tar_load(analyses_dixon)
  
  analyses_dixon <- sf::st_drop_geometry(analyses_dixon)
  
  sp_impacts <- lapply(7:ncol(analyses_dixon), function(sp){
    
    #sp = 9
    select_sp    <- analyses_dixon[, c(1:6, sp)]
    sp_name      <- colnames(select_sp)[7]
    sp_family    <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp_name]
    sp_range     <- select_sp[select_sp[, 7] == 1, ]
    heat_stress  <- sp_range[, 2:6]
    level_stress <- colnames(heat_stress)
    area_range   <- nrow(sp_range)
    
     area_impacts <- sapply(1:ncol(heat_stress), function(level){
       
       #level = 1
       heat_stress <- heat_stress[, level]
       area_refuge <- length(heat_stress[heat_stress <= 0.1])
       area_intermediate <- length(heat_stress[heat_stress > 0.1 & heat_stress <= 0.2])
       area_exposed <- length(heat_stress[heat_stress > 0.2])
       
       list_area <- list(area_refuge, 
                         area_intermediate, 
                         area_exposed)
       
       return(list_area)
       
     })
    
     sp_area_impacts <- data.frame(do.call(cbind, as.data.frame(area_impacts)))
     colnames(sp_area_impacts) <- level_stress
     sp_area_impacts$area <- c("area_refuge", "area_intermediate", "area_exposed")
     sp_area_impacts$specie <- rep(sp_name, nrow(sp_area_impacts))
     sp_area_impacts$family  <- rep(sp_family, nrow(sp_area_impacts))
     sp_area_impacts <- sp_area_impacts[, c("specie", "family", "area", "present_stress", 
                                            "stress_1.5", "stress_2", "stress_3", "stress_4")]
    
     return(sp_area_impacts)
     
  })
  
  final_impacts <- data.frame(do.call(rbind, sp_impacts))
  final_impacts$present_stress <- as.integer(final_impacts$present_stress)
  final_impacts$stress_1.5 <- as.integer(final_impacts$stress_1.5)
  final_impacts$stress_2   <- as.integer(final_impacts$stress_2)
  final_impacts$stress_3   <- as.integer(final_impacts$stress_3)
  final_impacts$stress_4   <- as.integer(final_impacts$stress_4)
  
  return(final_impacts)
  
}