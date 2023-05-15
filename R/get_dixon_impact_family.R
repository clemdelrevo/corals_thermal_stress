get_dixon_impact_family <- function(final_impacts, analyse_dixon){
  
  #targets::tar_load(final_impacts)
  
  global_exposed <- final_impacts[final_impacts$area == "area_exposed", ]

  total_nb_sp <- tapply(global_exposed$specie, as.factor(global_exposed$family), length)
  
  present_50 <- global_exposed[global_exposed$present_stress > 50, ]
  n_present  <- tapply(present_50$specie, as.factor(present_50$family), length)
  stress_1.5 <- global_exposed[global_exposed$stress_1.5 > 50, ]
  n_1.5      <- tapply(stress_1.5$specie, as.factor(stress_1.5$family), length)
  stress_2   <- global_exposed[global_exposed$stress_2 > 50, ]
  n_2        <- tapply(stress_2$specie, as.factor(stress_2$family), length)
  stress_3   <- global_exposed[global_exposed$stress_3 > 50, ]
  n_3        <- tapply(stress_3$specie, as.factor(stress_3$family), length)
  stress_4   <- global_exposed[global_exposed$stress_4 > 50, ]
  n_4        <- tapply(stress_4$specie, as.factor(stress_4$family), length)
  
  sp_impacted <- data.frame(family = names(total_nb_sp), total_sp = total_nb_sp)
  sp_impacted$present_stress <- n_present[sp_impacted$family]
  sp_impacted$stress_1.5 <- n_1.5[sp_impacted$family]
  sp_impacted$stress_2 <- n_2[sp_impacted$family]
  sp_impacted$stress_3 <- n_3[sp_impacted$family]
  sp_impacted$stress_4 <- n_4[sp_impacted$family]
  
  sp_impacted[is.na(sp_impacted)] <- 0
  row.names(sp_impacted) <- 1:nrow(sp_impacted)
  
  family_impact <- lapply(1:nrow(sp_impacted), function(l){
    
    #l = 1
    family_line   <- sp_impacted[l, ]
    family        <- sp_impacted[l, names(family_line) %in% "family"]
    family_impact <- (family_line[, names(family_line) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")] * 100) / family_line[, names(family_line) %in% "total_sp"]
    family_impact <- data.frame(cbind(family, family_impact))
    return(family_impact)
    
  })
  
family_impact <- data.frame(do.call(rbind, family_impact))

final_family_impact <- tidyr::gather(family_impact, threshold, value, -family)

dixon_family_impacts <- ggplot2::ggplot(final_family_impact, ggplot2::aes(x = as.factor(family), y = as.integer(value), color = threshold, shape = threshold))+
  ggplot2::theme_classic()+
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.5), size = 2)+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
  ggplot2::ylab("pourcentage d'espèces exposées sur plus de 50% du range")+
  ggplot2::xlab("")
  

return(dixon_family_impacts)
  
}
