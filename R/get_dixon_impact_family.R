get_dixon_impact_family <- function(final_impacts){
  
  #targets::tar_load(final_impacts)
  
  sp_percentage_impacts <- lapply(levels(as.factor(final_impacts$specie)), function(sp){
    
    #sp = "Acropora_muricata"
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
    
  global_percentage_impacts <- data.frame(do.call(rbind, sp_percentage_impacts))
  global_exposed <- global_percentage_impacts[global_percentage_impacts$area == "area_exposed", ]

  total_nb_sp <- tapply(global_exposed$specie, as.factor(global_exposed$family), length)
  
  
  nb_sp_impacted <- lapply(global_percentage_impacts[4:length(global_percentage_impacts)], function(col){
    
    #col = global_percentage_impacts[4]
    
    global_50 <- subset(global_percentage_impacts, col > 50)
    if(nrow(global_50) == 0) {
      
      nb_sp <- rep(0, length(unique(global_percentage_impacts$family)))
      
    } else {
    nb_sp <- tapply(global_50$specie, global_50$family, length)
    
    }
    
    return(nb_sp)
    
  })
  
  nb_sp_impacted <- data.frame(do.call(cbind, nb_sp_impacted))
  nb_sp_impacted <- data.frame(family = names(total_nb_sp),
                               cbind(total = as.integer(total_nb_sp), nb_sp_impacted))
  row.names(nb_sp_impacted) <- 1:nrow(nb_sp_impacted)
  
  family_impact <- lapply(1:nrow(nb_sp_impacted), function(l){
    
    #l = 1
    family_line   <- nb_sp_impacted[l, ]
    family        <- nb_sp_impacted[l, 1:2]
    family_impact <- (family_line[, 3:7] * 100) / family_line[, 2]
    family_impact <- data.frame(cbind(family, family_impact))
    return(family_impact)
    
  })
  
family_impact <- data.frame(do.call(rbind, family_impact))
#final_family_impact <- data.frame(cbind(family = family_impact$family, apply(family_impact[, 2:6], 2, as.integer)))
family_impact$total <- NULL
final_family_impact <- tidyr::gather(family_impact, threshold, value, -family)
  
dixon_family_impacts <- ggplot2::ggplot(final_family_impact, ggplot2::aes(x = as.factor(family), y = as.integer(value), color = threshold))+
  ggplot2::geom_jitter()

return(dixon_family_impacts)
  
}
