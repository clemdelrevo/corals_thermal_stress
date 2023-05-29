get_impact_family <- function(analyse_kalmus, final_taxonomy) {

  #targets::tar_load(analyse_kalmus)
  #targets::tar_load(final_taxonomy)
  
  analyse_kalmus <- sf::st_drop_geometry(analyse_kalmus)
  
  model   <- names(analyse_kalmus)[1:24]
  species <- names(analyse_kalmus)[25:ncol(analyse_kalmus)]

  
  sp_threshold <- data.frame(do.call(rbind, parallel::mclapply(colnames(analyse_kalmus[, names(analyse_kalmus) %in% species]), function(sp) {
    
    #sp = "Acropora_aculeus"
    message(sp)
    specie <- analyse_kalmus[, names(analyse_kalmus) %in% c(model, sp)]
    specie <- specie[specie[, names(specie) %in% sp] == 1, ]
    
    model_threshold <- lapply(colnames(specie[, names(specie) %in% model]), function(threshold) {
      
      #threshold = "ssp585_10yrs_DHW4"
      year <- specie[, names(specie) %in% threshold]
      length_model <- length(year)
      year_impact  <- table(year)
      n_threshold  <- data.frame(cumul = cumsum(year_impact), year = names(year_impact))
      pourc_model  <- (n_threshold$cumul * 100) / length_model
      family       <- final_taxonomy$final_family[final_taxonomy$final_genus_sp %in% sp]
      name_model   <- names(specie)[names(specie) %in% threshold]
      n_threshold$cumul <- NULL
      
      model_threshold <- cbind(name_model = rep(name_model, length(pourc_model)), pourc_model, n_threshold, specie = rep(sp, length(pourc_model)), 
                               family = rep(family, length(pourc_model)))
      
      return(model_threshold)
      
    })
  
    model_threshold <- do.call(rbind, model_threshold)
    
  }, mc.cores = 8)))
    
  final_impacts_family <- data.frame(do.call(rbind, parallel::mclapply(levels(as.factor(sp_threshold$family)), function(family) {
    
    #family = "Acroporidae"
    message(family)
    family_sub <- sp_threshold[sp_threshold$family == family, ]
    
      model_threshold <- lapply(levels(as.factor(family_sub$name_model)), function(model) {
        
        #model = "ssp585_5yrs_DHW8"
        model_sub <- family_sub[family_sub$name_model == model, ]
      
        year_threshold <- tapply(model_sub$pourc_model, model_sub$year, mean)
        year_threshold <- data.frame(model = rep(model, length(year_threshold)), pourc = year_threshold, year = as.numeric(names(year_threshold)))
        year_threshold <- na.omit(year_threshold)
        year_threshold <- year_threshold[order(year_threshold$year), ]
        year_threshold <- year_threshold[year_threshold$pourc >= cummax(year_threshold$pourc), ]

        return(year_threshold)
      
      })
      
      model_threshold <- data.frame(do.call(rbind, model_threshold))
      model_threshold$family <- rep(family, nrow(model_threshold))
      
      return(model_threshold)
      
    })))
    
  return(final_impacts_family)

}
