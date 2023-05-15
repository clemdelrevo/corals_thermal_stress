get_impact_sp <- function(analyse_dixon, final_taxonomy){

  #targets::tar_load(analyse_dixon)
  #targets::tar_load(final_taxonomy)
  
  analyse_dixon        <- sf::st_drop_geometry(analyse_dixon)
  analyse_dixon_sp     <- analyse_dixon[, !names(analyse_dixon) %in% c("regions", "present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]
  sum_area_sp          <- colSums(analyse_dixon_sp)
  sp_nomatch           <- names(sum_area_sp)[sum_area_sp == 0]
  analyse_dixon        <- analyse_dixon[, ! (names(analyse_dixon) %in% sp_nomatch)]
  
  
  sp_impacts <- lapply(7:ncol(analyse_dixon), function(sp){
    
    #sp = 205
    message(sp)
    select_sp    <- analyse_dixon[, c(1:6, sp)]
    sp_name      <- colnames(select_sp)[7]
    sp_family    <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp_name]
    if(length(sp_family) == 0) {sp_family <- 0}
    sp_range     <- select_sp[select_sp[, 7] == 1, ]
    area_range   <- nrow(sp_range)
    heat_stress  <- sp_range[, 2:6]
    level_stress <- colnames(heat_stress)
    
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
  
  sp_percentage_impacts <- lapply(levels(as.factor(final_impacts$specie)), function(sp){
    
    #sp = "Acropora_abrotanoides"
    message(sp)
    specie      <- final_impacts[final_impacts$specie == sp, ]
    sp_name     <- specie$specie
    family_name <- unique(specie$family)
    
    stress <- specie[, 4:8]
    range  <- unique(apply(stress, 2, sum))
    
    percentage_impacts <- specie |>
      dplyr::group_by(area) |>
      dplyr::summarise(present_stress = (present_stress * 100) / range,
                       stress_1.5 = (stress_1.5 * 100) / range,
                       stress_2 = (stress_2 * 100) / range,
                       stress_3 = (stress_3 * 100) / range,
                       stress_4 = (stress_4 * 100) / range)
    
    percentage_impacts <- data.frame(cbind(specie = sp_name,
                                           family = family_name,
                                           percentage_impacts))
    
    return(percentage_impacts)
    
  })
  
  final_impacts <- data.frame(do.call(rbind, sp_percentage_impacts))
  
  return(final_impacts)
  
}