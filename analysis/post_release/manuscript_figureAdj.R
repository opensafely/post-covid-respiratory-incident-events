# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv("output/plot_model_output.csv",
                      show_col_types = FALSE)

# Filter data ------------------------------------------------------------------
print("Filter data")

df$preexisting <- gsub(".*-","",df$name)

df <- df[df$analysis=="main" & 
           grepl("day",df$term),
         c("cohort","preexisting","outcome","outcome_time_median","model","term","hr","conf_low","conf_high")]

df <- df[df$term!="days_pre",]

# Make columns numeric ---------------------------------------------------------
print("Make columns numeric")

df <- df %>% 
  dplyr::mutate_at(c("outcome_time_median","hr","conf_low","conf_high"), as.numeric)

# Add plot labels --------------------------------------------------------------
print("Add plot labels")

plot_labels <- readr::read_csv("lib/plot_labels.csv", show_col_types = FALSE)

df <- merge(df, plot_labels, by.x = "outcome", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "outcome_label" = "label")


# Separate pre-existing --------------------------------------------------------
print("Separate pr-existing")

for (i in c("preexisting","no_preexisting")) {
  
  df_plot <- df[df$preexisting==i,]
  df_plot$conf_low <- ifelse(df_plot$hr > 2^8 | df_plot$hr < 2^-8, NA, df_plot$conf_low)
  df_plot$conf_high <- ifelse(df_plot$hr > 2^8 | df_plot$hr < 2^-8, NA, df_plot$conf_high)
  df_plot$hr <- ifelse(df_plot$hr > 2^8 | df_plot$hr < 2^-8, NA, df_plot$hr)
  
  # Plot data --------------------------------------------------------------------
  print("Plot data")
  
  ggplot2::ggplot(data = df_plot[df_plot$term!="days0_1",],
                  mapping = ggplot2::aes(x = outcome_time_median, y = hr, color = cohort)) +
    ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1), colour = "#A9A9A9") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0)) +
    ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = conf_low, 
                                                  ymax = conf_high,  
                                                  width = 0), 
                           position = ggplot2::position_dodge(width = 0)) +
    ggplot2::geom_line(position = ggplot2::position_dodge(width = 0), ggplot2::aes(linetype=model)) +
    ggplot2::scale_y_continuous(lim = c(0.5,140), breaks = c(0.5,1,2,4,8,16,32,64,128), trans = "log") +
    ggplot2::scale_x_continuous(lim = c(0,511), breaks = seq(0,511,56), labels = seq(0,511,56)/7) +
    ggplot2::scale_linetype_manual(values=c("solid", "dashed"), 
                                   breaks = c("mdl_max_adj", "mdl_age_sex"),
                                   labels = c("Maximally adjusted","Age- and sex- adjusted")) +
    ggplot2::scale_color_manual(breaks = c("prevax", "vax", "unvax"),
                                labels = c("Pre-vaccination (Jan 1 2020 - Dec 14 2021)",
                                           "Vaccinated (Jun 1 2021 - Dec 14 2021)",
                                           "Unvaccinated (Jun 1 2021 - Dec 14 2021)"),
                                values = c("#d2ac47", "#58764c", "#0018a8")) +
    ggplot2::labs(x = "\nWeeks since COVID-19 diagnosis", y = "Hazard ratio and 95% confidence interval\n") +
    ggplot2::guides(color=ggplot2::guide_legend(ncol = 1, byrow = TRUE),
                    linetype=ggplot2::guide_legend(ncol = 1, byrow = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.spacing.x = ggplot2::unit(0.5, "lines"),
                   panel.spacing.y = ggplot2::unit(0, "lines"),
                   legend.key = ggplot2::element_rect(colour = NA, fill = NA),
                   legend.title = ggplot2::element_blank(),
                   legend.position="bottom",
                   plot.background = ggplot2::element_rect(fill = "white", colour = "white")) +
    ggplot2::facet_wrap(outcome_label~., ncol = 2)
  
  # Save plot --------------------------------------------------------------------
  print("Save plot")
  
  flag <- ""
  flag <- ifelse(i=="no_preexisting", "FALSE", flag)
  flag <- ifelse(i=="preexisting", "TRUE", flag)
  
  ggplot2::ggsave(paste0("output/post_release/figureAdj-preexisting_",flag,".png"), 
                  height = (length(unique(df_plot$outcome))*56)+73, width = 210, # height should be 297
                  unit = "mm", dpi = 600, scale = 0.8)
  
}
