# Visualize corals specie thermal exposition through family'taxon

get_boxplot_impact_family <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_family <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_family <- final_impacts_family[final_impacts_family$family != "Tubiporidae" & final_impacts_family$family != "Scleractinia incertae sedis", ]
  
  stat <- lapply(colnames(final_impacts_family[, names(final_impacts_family) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    #threshold = "stress_1.5"
    sub_data <- final_impacts_family[, names(final_impacts_family) %in% c("family", "specie", threshold)]
    
    mean <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$family, mean)
    sd   <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$family, sd)
    
    cbind(mean, sd)
    
  })
  
  stat <- do.call(cbind, stat)
  stat <- data.frame(rownames(stat), stat, row.names = 1:nrow(stat))
  colnames(stat) <- c("family", "mean_present", "sd_present", "mean_1.5", "sd_1.5")
  
  test <- setNames(lapply(colnames(final_impacts_family[, names(final_impacts_family) %in% c("present_stress", "stress_1.5")]), function(threshold) {
  
    #threshold = "present_stress"
    lm <- lm(final_impacts_family[, threshold] ~ final_impacts_family$family)
    norm <- shapiro.test(residuals(lm))
    
    kruskal  <- kruskal.test(final_impacts_family[, threshold] ~ final_impacts_family$family)
    if(threshold == "present_stress") {
    post_hoc <- rstatix::dunn_test(data = final_impacts_family, formula = present_stress ~ family, p.adjust.method = "bonferroni")
    } else {
      post_hoc <- rstatix::dunn_test(data = final_impacts_family, formula = stress_1.5 ~ family, p.adjust.method = "bonferroni")
    }
    
    return(list(non_norm_distri = norm, non_param_test = kruskal, post_hoc_test = post_hoc))
    
  }), c("present_stress", "stress_1.5"))
  
  final_impacts_family$family <- final_impacts_family$family |> forcats::fct_relevel(
      c("Rhizangiidae", "Psammocoridae"              
      ,"Poritidae", "Pocilloporidae", "Plesiastreidae"             
      ,"Plerogyridae", "Oulastreidae", "Oculinidae"                 
      ,"Montastraeidae", "Milleporidae", "Merulinidae"                
      ,"Meandrinidae", "Lobophylliidae", "Leptastreidae"              
      ,"Helioporidae", "Fungiidae", "Faviidae"                   
      ,"Euphylliidae", "Diploastraeidae", "Dendrophylliidae"           
      ,"Coscinaraeidae", "Cladocoridae", "Caryophylliidae"            
      ,"Astrocoeniidae", "Astrangiidae"                       
      ,"Agariciidae", "Acroporidae"               
    ))
  
  present_stress <- final_impacts_family[, names(final_impacts_family) %in% c("specie", "present_stress", "family")]                                    
  present_stress <- present_stress |> tidyr::gather(threshold, value, - c(specie, family))    
  stress_1.5     <- final_impacts_family[, names(final_impacts_family) %in% c("specie", "stress_1.5", "family")]
  stress_1.5     <- stress_1.5 |> tidyr::gather(threshold, value, - c(specie, family))
  
  data_family <- data.frame(rbind(present_stress, stress_1.5))
  
  n <- data_family |> dplyr::group_by(family, threshold) |> dplyr::summarise(count = dplyr::n())
  n <- merge(data_family, n, by = c("family", "threshold"))
  
  boxplot_family <- ggplot2::ggplot(data_family, ggplot2::aes(x = family, y = value, fill = threshold))+
    ggplot2::geom_boxplot(linewidth = 0.4, notch = FALSE)+
    ggplot2::scale_fill_manual(values = c("#00AFBB", "#FC4E07"), label = c("1989-2019", "+1.5°C"))+
    ggplot2::theme_classic()+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition de l'aire de distribution\n au stress thermique")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.text.y = ggplot2::element_text(size = 11, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9))+
    ggplot2::labs(subtitle = paste0("Kruskal-Wallis (1986-2019), Khi2 = ", round(test$present_stress$non_param_test$statistic), ", df = ", test$present_stress$non_param_test$parameter, ", p-value = ", round(test$present_stress$non_param_test$p.value, 4),
                                    " ; Kruskal-Wallis (+1.5°C), Khi2 = ", round(test$stress_1.5$non_param_test$statistic), ", df = ", test$stress_1.5$non_param_test$parameter, ", p-value = ", sprintf("%.4e", test$stress_1.5$non_param_test$p.value)),
                  fill = "Projections climatiques")+
    ggplot2::geom_hline(yintercept = 90, color = "red", linetype = "dashed")+
    ggplot2::coord_flip()+
    ggplot2::geom_segment(ggplot2::aes(y = min(value), yend = max(value),
                     x = family, xend = family),
                 color = "darkgray", linetype = "dashed", position = ggplot2::position_nudge(x = -0.5, y = 0))+
    ggplot2::geom_text(data = n, ggplot2::aes(x = family, y = -4, label = paste0("n = ", count)), show.legend = FALSE, size = 3, fontface = "italic")
  
  return(list(stat = stat, test = test, boxplot = boxplot_family))
  
}

# Visualize corals specie thermal exposure through 
# biogeographical region of Dixon's data

get_boxplot_impact_region <- function(final_impacts_region, final_impacts_global){
  
  #targets::tar_load(final_impacts_region)
  #targets::tar_load(final_impacts_global)
  
  final_impacts_region <- final_impacts_region[final_impacts_region$area == "area_exposed", ]
  final_impacts_region <- final_impacts_region[final_impacts_region$family != "Tubiporidae" & final_impacts_region$family != "Scleractinia incertae sedis", ]
  final_impacts_region$in_reef  <- NULL
  final_impacts_region$out_reef <- NULL
  
  test <- setNames(lapply(colnames(final_impacts_region[, names(final_impacts_region) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    kruskal  <- kruskal.test(final_impacts_region[, threshold] ~ final_impacts_region$region)
    if(threshold == "present_stress") {
      post_hoc <- rstatix::dunn_test(data = final_impacts_region, formula = present_stress ~ region, p.adjust.method = "bonferroni")
    } else {
      post_hoc <- rstatix::dunn_test(data = final_impacts_region, formula = stress_1.5 ~ region, p.adjust.method = "bonferroni")
    }
    return(list(kruskal, post_hoc))
    
  }), c("present_stress", "stress_1.5"))
  
  data <- data.frame(do.call(rbind, lapply(levels(as.factor(final_impacts_region$region)), function(region) {
    
    region <- final_impacts_region[final_impacts_region$region == region, ]
    total_sp <- nrow(region)
    threshold <- region[region$stress_3 == 100, ]
    pourc <- (nrow(threshold) * 100) / total_sp
    
  })))
  
  final_impacts_global <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_global <- final_impacts_global[final_impacts_global$family != "Tubiporidae" & final_impacts_global$family != "Scleractinia incertae sedis", ]
 
  final_impacts_all <- final_impacts_global
  final_impacts_all$region   <- "Global"
  final_impacts_all$in_reef  <- NULL
  final_impacts_all$out_reef <- NULL
  final_impacts_all$statut   <- NULL
  
  final_impacts_region <- rbind(final_impacts_all, final_impacts_region)
  
  final_impacts_region$region <- final_impacts_region$region |>
    forcats::fct_relevel("Global", "Australia")
  
  n <- final_impacts_region |> dplyr::group_by(region) |> dplyr::summarise(count = dplyr::n())
  
  present_stress <- final_impacts_region[, names(final_impacts_region) %in% c("specie", "present_stress", "region")]                                    
  present_stress <- present_stress |> tidyr::gather(threshold, value, - c(specie, region))    
  stress_1.5     <- final_impacts_region[, names(final_impacts_region) %in% c("specie", "stress_1.5", "region")]
  stress_1.5     <- stress_1.5 |> tidyr::gather(threshold, value, - c(specie, region))
  
  data_region <- data.frame(rbind(present_stress, stress_1.5))
  
  final_impacts_region$rank_present <- unlist(lapply(levels(as.factor(final_impacts_region$region)), function(region) {
    
    region <- final_impacts_region[final_impacts_region$region == region, ]
    rank_present <- rank(region$present_stress)
    
    return(rank_present)
    
  }))
  
  final_impacts_region$rank_1.5 <- unlist(lapply(levels(as.factor(final_impacts_region$region)), function(region) {
    
    region <- final_impacts_region[final_impacts_region$region == region, ]
    rank_present <- rank(region$stress_1.5)
    
    return(rank_present)
    
  }))
  
  box <- ggplot2::ggplot(data_region, ggplot2::aes(x = region, y = value, fill = region, color = threshold))+
    ggplot2::geom_boxplot(linewidth = 0.4)+
    ggplot2::scale_fill_brewer(palette = "Set3")+
    ggplot2::scale_color_manual(values = c("#00AFBB", "#FC4E07"))+
    ggplot2::theme_classic()+
    ggplot2::coord_flip()
  
  plot <- ggplot2::ggplot(final_impacts_region, ggplot2::aes(y = rank_1.5, x = rank_present))+
    ggplot2::geom_point(ggplot2::aes(color = region))+
    ggplot2::geom_smooth(method = "lm", color = "red")+
    ggplot2::scale_color_brewer(palette = "Set3")+
    ggplot2::theme_classic()
  
  cowplot::plot_grid(box, plot, align = "hv", ncol = 2)
  
  cowplot::ggdraw(xlim = c(0, 40), ylim = c(0,40))+
    cowplot::draw_plot(present_stress, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(stress_1.5, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(plot, x = 20, y = 0, width = 20, height = 40)
  
  final_impacts_exposed <-  final_impacts_exposed |> 
    dplyr::group_by(statut) |>
    dplyr::mutate(diff = stress_1.5 - present_stress)
  
  box_diff <- ggplot2::ggplot(final_impacts_exposed, ggplot2::aes(x = statut, y = diff, fill = statut))+
    ggplot2::geom_boxplot()+
    ggplot2::scale_fill_ordinal()+
    ggplot2::theme_classic()
  
  cowplot::plot_grid(box, box_diff, align = "hv")
  
  boxplot_region <- function(x, y, title, subtitle) {
    
    ggplot2::ggplot(final_impacts_region, ggplot2::aes(x = forcats::fct_rev(.data[[x]]), y = .data[[y]]))+
      ggplot2::theme_classic()+
      ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 50), fill = "lightblue", alpha = 0.06)+
      ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 50, ymax = 90), fill = "lightgoldenrod", alpha = 0.06)+
      ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 90, ymax = 100), fill = "tomato", alpha = 0.06)+
      ggplot2::geom_boxplot(alpha = 0.5)+
      ggplot2::coord_flip()+
      ggplot2::xlab("")+
      ggplot2::ylab("% d'exposition du range au stress thermique")+
      ggplot2::ggtitle(title)+
      ggplot2::labs(subtitle = subtitle)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = ggplot2::element_text(hjust = 0, face = "italic"))+
      ggplot2::geom_text(data = n, ggplot2::aes(x = region, y = -4, label = paste0("n = ", count)), show.legend = FALSE, size = 3, fontface = "italic")
    
  }
  
  present_stress <- boxplot_region(x = "region", y = "present_stress", "1989-2019", paste0("Kruskal Wallis, p-value = ", round(test$present_stress[[1]]$p.value, 3)))
  stress_1.5     <- boxplot_region(x = "region", y = "stress_1.5", "1.5°C", paste0("Kruskal Wallis, p-value = ", round(test$stress_1.5[[1]]$p.value, 3)))
  
  boxplot_impact_region <- cowplot::plot_grid(present_stress, stress_1.5, ncol = 1, align = "hv")

  return(list(boxplot_impact_region, test))
  
}

get_boxplot_impact_statut <- function(final_impacts_global){
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_statut <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_statut <- final_impacts_statut[final_impacts_statut$family != "Tubiporidae" & final_impacts_statut$family != "Scleractinia incertae sedis", ]
  
  final_impacts_statut$statut[is.na(final_impacts_statut$statut)] <- "DD"
  
  final_impacts_statut$statut <- final_impacts_statut$statut |> 
    forcats::fct_relevel("CR", "EN", "VU", "NT", "LC", "DD")
  
  stat <- lapply(colnames(final_impacts_statut[, names(final_impacts_statut) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    #threshold = "stress_1.5"
    sub_data <- final_impacts_statut[, names(final_impacts_statut) %in% c("family", "specie", "statut", threshold)]
    
    mean <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$statut, mean)
    sd <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$statut, sd)
    
    cbind(mean, sd)
    
  })
  
  stat <- do.call(cbind, stat)
  stat <- data.frame(rownames(stat), stat, row.names = 1:nrow(stat))
  colnames(stat) <- c("statut", "mean_present", "sd_present", "mean_1.5", "sd_1.5")
  
  test <- setNames(lapply(colnames(final_impacts_statut[, names(final_impacts_statut) %in% c("present_stress", "stress_1.5")]), function(threshold) {
  
  lm   <- lm(final_impacts_statut[, threshold] ~ final_impacts_statut$statut) 
  norm <- shapiro.test(residuals(lm))
    
  kruskal  <- kruskal.test(final_impacts_statut[, threshold] ~ final_impacts_statut$statut)
  if(threshold == "present_stress") {
    post_hoc <- rstatix::dunn_test(data = final_impacts_statut, formula = present_stress ~ statut, p.adjust.method = "bonferroni")
  } else {
    post_hoc <- rstatix::dunn_test(data = final_impacts_statut, formula = stress_1.5 ~ statut, p.adjust.method = "bonferroni")
  }
    
    return(list(non_norm_distri = norm, non_param_test = kruskal, post_hoc_test = post_hoc))
    
  }), c("present_stress", "stress_1.5"))
 
  present_stress <- final_impacts_statut[, names(final_impacts_statut) %in% c("specie", "present_stress", "statut")]                                    
  present_stress <- present_stress |> tidyr::gather(threshold, value, - c(specie, statut))    
  stress_1.5     <- final_impacts_statut[, names(final_impacts_statut) %in% c("specie", "stress_1.5", "statut")]
  stress_1.5     <- stress_1.5 |> tidyr::gather(threshold, value, - c(specie, statut))
  
  data_statut <- data.frame(rbind(present_stress, stress_1.5))
  
  final_impacts_statut$rank_present <- rank(final_impacts_statut$present_stress)
  final_impacts_statut$rank_1.5     <- rank(final_impacts_statut$stress_1.5)
  
  n <- data_statut |> dplyr::group_by(statut, threshold) |> dplyr::summarise(count = dplyr::n())
  n <- merge(data_statut, n, by = c("statut", "threshold"))
  
  boxplot_statut <- ggplot2::ggplot(data_statut, ggplot2::aes(x = statut, y = value, fill = statut, color = threshold))+
    ggplot2::geom_boxplot(linewidth = 0.4)+
    ggplot2::theme_classic()+
    ggplot2::scale_fill_ordinal()+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition de l'aire de distribution\n au stress thermique")+
    ggplot2::labs(fill = "Statut UICN", color = "Projections climatiques",
                  subtitle = paste0("Kruskal-Wallis (1986-2019), Khi2 = ", round(test$present_stress$non_param_test$statistic), ", df = ", test$present_stress$non_param_test$parameter, ", p-value = ", sprintf("%.4e", as.numeric(test$present_stress$non_param_test$p.value)),
                                    " ; Kruskal-Wallis (+1.5°C), Khi2 = ", round(test$stress_1.5$non_param_test$statistic), ", df = ", test$stress_1.5$non_param_test$parameter, ", p-value = ", sprintf("%.4e", as.numeric(test$stress_1.5$non_param_test$p.value))))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 13, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 9))+
    ggplot2::scale_color_manual(values = c("lightgray", "black"), label = c("1986-2019", "+1.5°C"))+
    ggplot2::geom_hline(yintercept = 90, color = "red", linetype = "dashed")+
    ggplot2::geom_text(data = n, ggplot2::aes(x = statut, y = -2, label = paste0("n = ", count)), show.legend = FALSE, size = 4, fontface = "italic")

  return(list(stat = stat, test = test, boxplot = boxplot_statut))
  
}



get_boxplot_impact_habitat <- function(final_impacts_global) {
  
  #targets::tar_load(final_impacts_global)
  
  final_impacts_habitat <- final_impacts_global[final_impacts_global$area == "area_exposed", ]
  final_impacts_habitat <- final_impacts_habitat[!is.na(final_impacts_habitat$in_reef) | !is.na(final_impacts_habitat$out_reef), ]
  
  only_reef <- final_impacts_habitat[final_impacts_habitat$in_reef == 1 & final_impacts_habitat$out_reef == 0, ]
  only_reef$habitat <- "in_reef"
  not_only_reef <- final_impacts_habitat[final_impacts_habitat$in_reef == 1 & final_impacts_habitat$out_reef == 1, ]
  not_only_reef$habitat <- 'in_out_reef'
  
  final_impacts_habitat <- data.frame(rbind(only_reef, not_only_reef))
  
  stat <- lapply(colnames(final_impacts_habitat[, names(final_impacts_habitat) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    #threshold = "stress_1.5"
    sub_data <- final_impacts_habitat[, names(final_impacts_habitat) %in% c("family", "specie", "habitat", threshold)]
    
    mean <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$habitat, mean)
    sd   <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$habitat, sd)
    
    cbind(mean, sd)
    
  })
  
  stat <- do.call(cbind, stat)
  stat <- data.frame(rownames(stat), stat, row.names = 1:nrow(stat))
  colnames(stat) <- c("habitat", "mean_present", "sd_present", "mean_1.5", "sd_1.5")
  
  test <- setNames(lapply(colnames(final_impacts_habitat[, names(final_impacts_habitat) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    lm   <- lm(final_impacts_habitat[, threshold] ~ final_impacts_habitat$habitat)
    norm <- shapiro.test(residuals(lm))
    
    #threshold = "stress_1.5"
    wilcox <- wilcox.test(final_impacts_habitat[, threshold] ~ final_impacts_habitat$habitat)
    
    return(list(non_norm_distri = norm, non_param_test = wilcox))
    
  }),  c("present_stress", "stress_1.5"))
  
  final_impacts_habitat$habitat <- final_impacts_habitat$habitat |>
    forcats::fct_relevel("in_reef", "in_out_reef")
  
  present_stress <- final_impacts_habitat[, names(final_impacts_habitat) %in% c("specie", "present_stress", "habitat")]                                    
  present_stress <- present_stress |> tidyr::gather(threshold, value, - c(specie, habitat))    
  stress_1.5     <- final_impacts_habitat[, names(final_impacts_habitat) %in% c("specie", "stress_1.5", "habitat")]
  stress_1.5     <- stress_1.5 |> tidyr::gather(threshold, value, - c(specie, habitat))
  
  data_habitat <- data.frame(rbind(present_stress, stress_1.5))
  
  final_impacts_habitat$rank_present <- rank(final_impacts_habitat$present_stress)
  final_impacts_habitat$rank_1.5     <- rank(final_impacts_habitat$stress_1.5)
  
  n <- data_habitat |> dplyr::group_by(habitat, threshold) |> dplyr::summarise(count = dplyr::n())
  n <- merge(data_habitat, n, by = c("habitat", "threshold"))
  
  boxplot_habitat <- ggplot2::ggplot(data_habitat, ggplot2::aes(x = habitat, y = value, fill = habitat, color = threshold))+
    ggplot2::geom_boxplot(linewidth = 0.4)+
    ggplot2::theme_classic()+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition de l'aire de distribution\n au stress thermique")+
    ggplot2::geom_hline(yintercept = 90, color = "red", linetype = "dashed")+
    ggplot2::labs(fill = "Habitat", color = "Projection climatique",
                  subtitle = paste0("Wilcoxon (1986-2019), U = ", round(test$present_stress$non_param_test$statistic), ", p-value = ", round(test$present_stress$non_param_test$p.value, 4),
                                    " ; Wilcoxon (+1.5°C), U = ", round(test$stress_1.5$non_param_test$statistic), " , p-value = ", round(test$stress_1.5$non_param_test$p.value, 4)))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 13, face = "bold"))+
    ggplot2::scale_color_manual(values = c("lightgray", "black"), label = c("1986-2019", "+1.5°C"))+
    ggplot2::scale_fill_manual(values = c("#999999", "#E69F00"), label = c("inféodé au récif", "non inféodé au récif"))+
    ggplot2::scale_x_discrete(label = c("inféodé au récif", "non inféodé au récif"))+
    ggplot2::geom_text(data = n, ggplot2::aes(x = habitat, y = -2, label = paste0("n = ", count)), show.legend = FALSE, size = 4, fontface = "italic")
  
  
  return(list(stat = stat, test = test, boxplot = boxplot_habitat))
  
}

get_boxplot_impact_trait <- function(final_impacts_trait) {
  
  #targets::tar_load(final_impacts_trait)
  
  stat <- lapply(colnames(final_impacts_trait[, names(final_impacts_trait) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    #threshold = "stress_1.5"
    sub_data <- final_impacts_trait[, names(final_impacts_trait) %in% c("family", "specie", "life_trait", threshold)]
    
    mean <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$life_trait, mean)
    sd <- tapply(sub_data[, names(sub_data) %in% threshold], sub_data$life_trait, sd)
    
    cbind(mean, sd)
    
  })
  
  stat <- do.call(cbind, stat)
  stat <- data.frame(rownames(stat), stat, row.names = 1:nrow(stat))
  colnames(stat) <- c("life_trait", "mean_present", "sd_present", "mean_1.5", "sd_1.5")
  
  
  test <- setNames(lapply(colnames(final_impacts_trait[, names(final_impacts_trait) %in% c("present_stress", "stress_1.5")]), function(threshold) {
    
    lm   <- lm(final_impacts_trait[, threshold] ~ final_impacts_trait$life_trait)
    norm <- shapiro.test(residuals(lm))
    
    kruskal  <- kruskal.test(final_impacts_trait[, threshold] ~ final_impacts_trait$life_trait)
    if(threshold == "present_stress") {
      post_hoc <- rstatix::dunn_test(data = final_impacts_trait, formula = present_stress ~ life_trait, p.adjust.method = "bonferroni")
    } else {
      post_hoc <- rstatix::dunn_test(data = final_impacts_trait, formula = stress_1.5 ~ life_trait, p.adjust.method = "bonferroni")
    }
    return(list(non_norm_distri = norm, non_param_test = kruskal, post_hoc_test = post_hoc))
    
  }), c("present_stress", "stress_1.5"))
  
  present_stress <- final_impacts_trait[, names(final_impacts_trait) %in% c("specie", "present_stress", "life_trait")]                                    
  present_stress <- present_stress |> tidyr::gather(threshold, value, - c(specie, life_trait))    
  stress_1.5     <- final_impacts_trait[, names(final_impacts_trait) %in% c("specie", "stress_1.5", "life_trait")]
  stress_1.5     <- stress_1.5 |> tidyr::gather(threshold, value, - c(specie, life_trait))
  
  data_trait <- data.frame(rbind(present_stress, stress_1.5))
  
  final_impacts_trait$rank_present <- rank(final_impacts_trait$present_stress)
  final_impacts_trait$rank_1.5     <- rank(final_impacts_trait$stress_1.5)
  
  n <- data_trait |> dplyr::group_by(life_trait, threshold) |> dplyr::summarise(count = dplyr::n())
  n <- merge(data_trait, n, by = c("life_trait", "threshold"))
  
  boxplot_trait <- ggplot2::ggplot(data_trait, ggplot2::aes(x = life_trait, y = value, fill = life_trait, color = threshold))+
    ggplot2::geom_boxplot(linewidth = 0.3)+
    ggplot2::theme_classic()+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition de l'aire de distribution\n au stress thermique")+
    ggplot2::geom_hline(yintercept = 90, color = "red", linetype = "dashed")+
    ggplot2::labs(fill = "Trait d'histoire\n de vie", color = "Projection climatique",
                  subtitle = paste0("Kruskal-Wallis (1989-2019), Khi2 = ", round(test$present_stress$non_param_test$statistic), ", df = ", test$present_stress$non_param_test$parameter, ", p-value = ", round(test$present_stress$non_param_test$p.value, 4),
                                    " ; Kruskal-Wallis (+1.5°C), Khi2 = ", round(test$stress_1.5$non_param_test$statistic), ", df = ", test$stress_1.5$non_param_test$parameter, ", p-value = ", round(test$stress_1.5$non_param_test$p.value, 4)))+
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.text.y = ggplot2::element_text(size = 12, face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 13, face = "bold"))+
    ggplot2::scale_color_manual(values = c("lightgray", "black"), label = c("1986-2019", "+1.5°C"))+
    ggplot2::scale_fill_brewer(palette = "Set1", label = c("compétitive", "généraliste", "stress tolérante", "opportuniste"))+
    ggplot2::scale_x_discrete(label = c("compétitive", "généraliste", "stress tolérante", "opportuniste"))+
    ggplot2::geom_text(data = n, ggplot2::aes(x = life_trait, y = -2, label = paste0("n = ", count)), show.legend = FALSE, size = 4, fontface = "italic")
  
  return(list(stat = stat, test = test, boxplot = boxplot_trait))
  
}
