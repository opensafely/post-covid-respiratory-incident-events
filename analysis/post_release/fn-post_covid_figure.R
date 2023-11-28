# Define figure function -------------------------------------------------------

post_covid_figure <- function(data, subgroup, preexisting) {
  
  message(paste0("Making: ",subgroup," when preexisting = ",preexisting))
  df_plot <- data
  
  ## Restrict data based on pre-existing condition -----------------------------
  print("Restrict data based on pre-existing condition")
  
  if (preexisting==TRUE) {
    df_plot <- df_plot[grepl("-preexisting",df_plot$name),]
  }
  
  if (preexisting==FALSE) {
    df_plot <- df_plot[grepl("-no_preexisting",df_plot$name),]
  }
  
  ## Filter data ---------------------------------------------------------------
  print("Filter data")
  
  df_plot <- df_plot[grepl("day",df_plot$term) & 
                       df_plot$model=="mdl_max_adj",
                     c("cohort","analysis","outcome","outcome_time_median","term","hr","conf_low","conf_high")]
  
  df_plot <- df_plot[df_plot$term!="days_pre",]
  
  # Make columns numeric -------------------------------------------------------
  print("Make columns numeric")
  
  df_plot <- df_plot %>% 
    dplyr::mutate_at(c("outcome_time_median","hr","conf_low","conf_high"), as.numeric)
  
  ## Add plot labels -----------------------------------------------------------
  print("Add plot labels")
  
  plot_labels <- readr::read_csv("lib/plot_labels.csv",
                                 show_col_types = FALSE)
  
  df_plot <- merge(df_plot, plot_labels, by.x = "outcome", by.y = "term", all.x = TRUE)
  df_plot <- dplyr::rename(df_plot, "outcome_label" = "label")
  
  df_plot <- merge(df_plot, plot_labels, by.x = "analysis", by.y = "term", all.x = TRUE)
  df_plot <- dplyr::rename(df_plot, "analysis_label" = "label")
  
  ## Restrict to plot data -----------------------------------------------------
  print("Restrict to plot data")
  
  if (subgroup %in% c("hospitalised")) {
    df_plot <- df_plot[(df_plot$analysis=="main" | grepl(subgroup,df_plot$analysis)),]
  } else if (subgroup %in% c("sub_covid_history")) {
    df_plot <- df_plot[(df_plot$analysis=="main" | grepl(subgroup,df_plot$analysis)),]
    df_plot$analysis_label <- ifelse(df_plot$analysis_label=="All COVID-19", "No history of COVID-19",df_plot$analysis_label)
    df_plot <- df_plot[df_plot$cohort!="prevax",]
  } else {
    df_plot <- df_plot[grepl(subgroup,df_plot$analysis),]
  }
  
  
  ## Remove extreme HRs --------------------------------------------------------
  print("Remove extreme HRs")
  
  df_plot$conf_low <- ifelse(df_plot$hr < 2^-7 | df_plot$hr > 2^7, NA, df_plot$conf_low)
  df_plot$conf_high <- ifelse(df_plot$hr < 2^-7 | df_plot$hr > 2^7, NA, df_plot$conf_high)
  df_plot$hr <- ifelse(df_plot$hr < 2^-7 | df_plot$hr > 2^7, NA, df_plot$hr)
  
  ## Plot data -----------------------------------------------------------------
  print("Plot data")
  
  ggplot2::ggplot(data = df_plot,
                  mapping = ggplot2::aes(x = outcome_time_median, y = hr, color = cohort)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0)) +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = conf_low, 
                                                  ymax = conf_high,  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 0)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 0)) +
    ggplot2::scale_y_continuous(lim = c(2^-7,2^7), breaks = 2^seq(-8,8,2), labels = display(2^seq(-8,8,2)), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,511), breaks = seq(0,511,56), labels = seq(0,511,56)/7) +
    ggplot2::scale_color_manual(breaks = c("prevax", "vax", "unvax"),
                                labels = c("Pre-vaccination (Jan 1 2020 - Dec 14 2021)",
                                           "Vaccinated (Jun 1 2021 - Dec 14 2021)",
                                           "Unvaccinated (Jun 1 2021 - Dec 14 2021)"),
                                values = c("#d2ac47", "#58764c", "#0018a8")) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval\n") +
    ggplot2::guides(color=ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
    ggplot2::facet_grid(outcome_label ~ analysis_label)
  
  ## Save plot -----------------------------------------------------------------
  print("Save plot")
  
  ggplot2::ggsave(paste0("output/post_release/figure_",gsub("sub_","",subgroup),"-preexisting_",preexisting,".png"), 
                  height = 210, width = 297, 
                  unit = "mm", dpi = 600, scale = 0.8)
  
}