get_boxplot_impact_family <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_exposed <- final_impacts_exposed[final_impacts_exposed$family != "Tubiporidae" & final_impacts_exposed$family != "Scleractinia incertae sedis", ]
  
  anova <- setNames(lapply(colnames(final_impacts_exposed[, names(final_impacts_exposed) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]), function(col) {
    
    aov_family <- summary(aov(final_impacts_exposed[, col] ~ final_impacts_exposed$family))
    
  }), colnames(final_impacts_exposed[, names(final_impacts_exposed) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]))
  
  
  data <- setNames(lapply(levels(as.factor(final_impacts_exposed$family)), function(family) {
    
    family <- final_impacts_exposed[final_impacts_exposed$family == family, ]
    mean_thresh <- mean(family$stress_1.5)
    error <- plotrix::std.error(family$stress_1.5)
    
    return(list(mean_thresh, error))
    
  }), levels(as.factor(final_impacts_exposed$family)))
  
  delta <- data.frame(diff = final_impacts_exposed$stress_1.5 - final_impacts_exposed$present_stress,
                      family = final_impacts_exposed$family)
  mean_delta  <- tapply(delta$diff, delta$family, mean)
  error_delta <- tapply(delta$diff, delta$family, plotrix::std.error)
  delta_data <- data.frame(mean_delta, error_delta, family = names(mean_delta))
  delta_data <- delta_data[order(delta_data$mean_delta), ]
  delta_data$family <- delta_data$family |>
    forcats::fct_inorder()
  
  ggplot2::ggplot(delta_data, ggplot2::aes(y = mean_delta, x = family))+
    ggplot2::theme_classic()+
    ggplot2::geom_col(fill = "lightgrey")+
    ggplot2::coord_flip()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::ylab("delta moyen d'exposition entre 1.5°C et l'exposition actuelle")+
    ggplot2::xlab("")+
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_delta - error_delta, ymax = mean_delta + error_delta))
    
  
  
  final_impacts_all <- final_impacts_exposed
  final_impacts_all$family <- "All"
  
  final_impacts <- rbind(final_impacts_all, final_impacts_exposed)

  final_impacts$family <- final_impacts$family |> forcats::fct_relevel(
      c("Rhizangiidae", "Psammocoridae"              
      ,"Poritidae", "Pocilloporidae", "Plesiastreidae"             
      ,"Plerogyridae", "Oulastreidae", "Oculinidae"                 
      ,"Montastraeidae", "Milleporidae", "Merulinidae"                
      ,"Meandrinidae", "Lobophylliidae", "Leptastreidae"              
      ,"Helioporidae", "Fungiidae", "Faviidae"                   
      ,"Euphylliidae", "Diploastraeidae", "Dendrophylliidae"           
      ,"Coscinaraeidae", "Cladocoridae", "Caryophylliidae"            
      ,"Astrocoeniidae", "Astrangiidae"                       
      ,"Agariciidae", "Acroporidae", "All"                 
    ))
  

boxplot_family <- function(x, y, title) {
  
  ggplot2::ggplot(final_impacts, ggplot2::aes(x = .data[[x]], y = .data[[y]]))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 90), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::ggtitle(title)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))+
    ggplot2::coord_flip()
  
  }
    
  present_stress <- boxplot_family(x = "family", y = "present_stress", "Present stress")
  stress_1.5     <- boxplot_family(x = "family", y ="stress_1.5", "1.5°C")
  stress_2       <- boxplot_family(x = "family", y = "stress_2", "2°C")
  stress_3       <- boxplot_family(x = "family", y = "stress_3", "3°C")
  

  boxplot_impact_family <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  

  aov_family <- summary(aov(final_impacts_exposed$stress_1.5 ~ final_impacts_exposed$family))
  
  
  return(list(boxplot_impact_family, aov_family))
  
}


get_boxplot_impacts_region <- function(final_impacts_region, final_impacts_global){
  
  #targets::tar_load(final_impacts_region)
  #targets::tar_load(final_impacts_global)
  
  final_impacts_region <- final_impacts_region[final_impacts_region$area == "area_exposed", ]
  final_impacts_region$in_reef  <- NULL
  final_impacts_region$out_reef <- NULL
  
  anova <- setNames(lapply(colnames(final_impacts_region[, names(final_impacts_region) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]), function(col) {
    
    aov_region <- summary(aov(final_impacts_region[, col] ~ final_impacts_region$region))
    
  }), colnames(final_impacts_region[, names(final_impacts_region) %in% c("present_stress", "stress_1.5", "stress_2", "stress_3", "stress_4")]))
  
  
  data <- data.frame(do.call(rbind, lapply(levels(as.factor(final_impacts_region$region)), function(region) {
    
    region <- final_impacts_region[final_impacts_region$region == region, ]
    total_sp <- nrow(region)
    threshold <- region[region$stress_3 == 100, ]
    pourc <- (nrow(threshold) * 100) / total_sp
    
  })))
  
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
 
  final_impacts_all <- final_impacts_global
  final_impacts_all$region   <- "Global"
  final_impacts_all$in_reef  <- NULL
  final_impacts_all$out_reef <- NULL
  final_impacts_all$statut   <- NULL
  
  final_impacts_region <- rbind(final_impacts_all, final_impacts_region)
  
  final_impacts_region$region <- final_impacts_region$region |>
    forcats::fct_relevel("Global", "Australia")
  
  boxplot_region <- function(x, y, title) {
    
    ggplot2::ggplot(final_impacts_region, ggplot2::aes(x = forcats::fct_rev(.data[[x]]), y = .data[[y]]))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 90), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::ggtitle(title)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))+
    ggplot2::coord_flip()
    
  }
  
  present_stress <- boxplot_region(x = "region", y = "present_stress", "Present stress")
  stress_1.5     <- boxplot_region(x = "region", y = "stress_1.5", "1.5°C")
  stress_2       <- boxplot_region(x = "region", y = "stress_2", "2°C")
  stress_3       <- boxplot_region(x = "region", y = "stress_3", "3°C")
  
  boxplot_impact_region <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  

  return(boxplot_impact_region, anova)
  
}


tri.to.squ<-function(x)
{
  rn <- row.names(x)
  cn <- colnames(x)
  an <- unique(c(cn,rn))
  myval <-  x[!is.na(x)]
  mymat <-  matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
    
  }
  return(mymat)
} 


get_boxplot_impact_statut <- function(final_impacts_global){
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_exposed <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_exposed$statut[is.na(final_impacts_exposed$statut)] <- "DD"
  
  final_impacts_exposed$statut <- final_impacts_exposed$statut |> 
    forcats::fct_relevel("CR", "EN", "VU", "NT", "LC", "DD")
  
  pp    <- pairwise.t.test(final_impacts_exposed$present_stress, final_impacts_exposed$statut)
  mymat <-tri.to.squ(pp$p.value)
  myletters    <-multcompView::multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
  myletters_df <- data.frame(statut=names(myletters$Letters),letter = myletters$Letters )
  
  ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = as.factor(statut), y = present_stress))+
    ggplot2::geom_boxplot()+
    ggplot2::ylim(0, 100)+
    ggplot2::geom_hline(yintercept = 50, color = "black")+
    ggplot2::geom_hline(yintercept = 90, color = "red")+
    ggplot2::xlab("statut de conservation UICN")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))+
    ggplot2::geom_text(data = myletters_df, ggplot2::aes(label = letter, y = 100), colour="black", size=5) 
  
  
  boxplot_statut <- function(x, y, title) {
  
    ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = .data[[x]], y = .data[[y]]))+
      ggplot2::geom_boxplot()+
      ggplot2::ylim(0, 100)+
      ggplot2::geom_hline(yintercept = 50, color = "black")+
      ggplot2::geom_hline(yintercept = 90, color = "red")+
      ggplot2::xlab("statut de conservation UICN")+
      ggplot2::ylab("% d'exposition du range au stress thermique")+
      ggplot2::ggtitle(title)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
    
  }
  
  present_stress <- boxplot_statut(x = "statut", y = "present_stress", "Present stress")
  stress_1.5     <- boxplot_statut(x = "statut", y = "stress_1.5", "1.5°C")
  stress_2       <- boxplot_statut(x = "statut", y = "stress_2", "2°C")
  stress_3       <- boxplot_statut(x = "statut", y = "stress_3", "3°C")
  
  cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(stress_3, x = 20, y = 0, width = 20, height = 20)
  
}

boxplot_habitat <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  
  final_impacts_global <- na.omit(final_impacts_global)
  
  only_reef <- final_impacts_global[final_impacts_global$in_reef == 1 & final_impacts_global$out_reef == 0, ]
  only_reef$habitat <- "in_reef"
  not_only_reef <- final_impacts_global[final_impacts_global$in_reef == 1 & final_impacts_global$out_reef == 1, ]
  not_only_reef$habitat <- 'in_out_reef'
  
  impacts <- data.frame(rbind(only_reef, not_only_reef))
  
  reef_habitat <- tapply(impacts$specie, impacts$habitat, length)
  
  barplot(reef_habitat)
  
  
  boxplot(impacts$stress_1.5 ~ impacts$habitat)
  
}
