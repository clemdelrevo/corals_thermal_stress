get_boxplot_impact_family <- function(final_impacts) {
  
  #targets::tar_load(final_impacts)
  
  final_impacts <- final_impacts[final_impacts$area == "area_exposed", ]
  
  final_impacts <- na.omit(final_impacts)  
  
  boxplot_impact_present <- ggplot2::ggplot(final_impacts, ggplot2::aes(x = forcats::fct_rev(as.factor(family)), y = present_stress))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
  
  boxplot_impact_1.5 <- ggplot2::ggplot(final_impacts, ggplot2::aes(x = forcats::fct_rev(as.factor(family)), y = stress_1.5))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
  
  
  boxplot_impact_2 <- ggplot2::ggplot(final_impacts, ggplot2::aes(x = forcats::fct_rev(as.factor(family)), y = stress_2))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
  
  boxplot_impact_3 <- ggplot2::ggplot(final_impacts, ggplot2::aes(x = forcats::fct_rev(as.factor(family)), y = stress_3))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
    
  boxplot_impact_4 <- ggplot2::ggplot(final_impacts, ggplot2::aes(x = forcats::fct_rev(as.factor(family)), y = stress_4))+
    ggplot2::theme_classic()+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 10), fill = "lightblue", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 20), fill = "lightgoldenrod", alpha = 0.06)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = 20, ymax = 100), fill = "tomato", alpha = 0.06)+
    ggplot2::geom_boxplot(alpha = 0.5)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))+
    ggplot2::xlab("")+
    ggplot2::ylab("% d'exposition du range au stress thermique")+
    ggplot2::coord_flip()
  
  
  boxplot_threshold <- cowplot::ggdraw(xlim = c(0, 40), ylim = c(0, 40))+
    cowplot::draw_plot(boxplot_impact_present, x = 0, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(boxplot_impact_1.5, x = 20, y = 20, width = 20, height = 20)+
    cowplot::draw_plot(boxplot_impact_2, x = 0, y = 0, width = 20, height = 20)+
    cowplot::draw_plot(boxplot_impact_3, x = 20, y = 0, width = 20, height = 20)
    
   return(boxplot_threshold)
  
}
