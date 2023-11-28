# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv("output/plot_model_output.csv",
                      show_col_types = FALSE)

# Define population ------------------------------------------------------------
print("Define population")

df$population <- gsub(".*-","",df$name)

# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df[grepl("day",df$term) & 
           df$model=="mdl_max_adj",
         c("analysis","cohort","population","outcome","term","hr","conf_low","conf_high")]

df <- df[df$term!="days_pre",]

# Make columns numeric ---------------------------------------------------------
print("Make columns numeric")

df <- df %>% 
  dplyr::mutate_at(c("hr","conf_low","conf_high"), as.numeric)

# Add plot labels ---------------------------------------------------------
print("Add plot labels")

plot_labels <- readr::read_csv("lib/plot_labels.csv",
                               show_col_types = FALSE)

df <- merge(df, plot_labels, by.x = "outcome", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "outcome_label" = "label")

df <- merge(df, plot_labels, by.x = "analysis", by.y = "term", all.x = TRUE)
df <- dplyr::rename(df, "analysis_label" = "label")
df$analysis_label <- ifelse(df$analysis_label=="All COVID-19","Primary",df$analysis_label)

# Tidy estimate ----------------------------------------------------------------
print("Tidy estimate")

df$estimate <- paste0(display(df$hr)," (",display(df$conf_low),"-",display(df$conf_high),")")

# Tidy term --------------------------------------------------------------------
print("Tidy term")

df$weeks <- ""
df$weeks <- ifelse(df$term=="days0_28", "1-4", df$weeks)
df$weeks <- ifelse(df$term=="days28_197", "5-28", df$weeks)
df$weeks <- ifelse(df$term=="days197_365", "29-52", df$weeks)
df$weeks <- ifelse(df$term=="days365_714", "53-102", df$weeks)

df$weeks <- factor(df$weeks, levels = c("1-4","5-28","29-52","53-102"))

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- df[,c("analysis_label","cohort","population","outcome_label","weeks","estimate")]

df <- tidyr::pivot_wider(df, 
                         id_cols = c("analysis_label","population","outcome_label","weeks"),
                         names_from = "cohort",
                         values_from = "estimate")

# Order analyses ---------------------------------------------------------------
print("Order analyses")

df$analysis_label <- factor(df$analysis_label,
                            levels = c("Primary",
                                       "Hospitalised COVID-19",
                                       "Non-hospitalised COVID-19",
                                       "History of COVID-19",
                                       "Smoking status: Current",
                                       "Smoking status: Former",
                                       "Smoking status: Never",
                                       "Age group: 18-39",
                                       "Age group: 40-59",
                                       "Age group: 60-79",
                                       "Age group: 80-110",
                                       "Sex: Female",                                   
                                       "Sex: Male",
                                       "Ethnicity: White",
                                       "Ethnicity: South Asian",
                                       "Ethnicity: Black",
                                       "Ethnicity: Other",                       
                                       "Ethnicity: Mixed"))

# Tidy table -------------------------------------------------------------------
print("Tidy table")

df <- df[order(df$analysis_label,df$outcome_label,df$weeks),
         c("analysis_label","outcome_label","weeks","prevax","vax","unvax","population")]

df <- dplyr::rename(df,
                    "Analysis" = "analysis_label",
                    "Outcome" = "outcome_label",
                    "Weeks since COVID-19" = "weeks",
                    "Pre-vaccination cohort" = "prevax",
                    "Vaccinated cohort" = "vax",
                    "Unvaccinated cohort" = "unvax")

# Save table -------------------------------------------------------------------
print("Save table")

df$`Pre-vaccination cohort` <- ifelse(df$`Pre-vaccination cohort`=="NA (NA-NA)","Redacted",df$`Pre-vaccination cohort`)
df$`Vaccinated cohort` <- ifelse(df$`Vaccinated cohort`=="NA (NA-NA)","Redacted",df$`Vaccinated cohort`)
df$`Unvaccinated cohort` <- ifelse(df$`Unvaccinated cohort`=="NA (NA-NA)","Redacted",df$`Unvaccinated cohort`)

# Save table -------------------------------------------------------------------
print("Save table")

for (population in c("no_preexisting", "preexisting")) {
  
  tmp <- df[df$population==population,]
  tmp$population <- NULL
  
  flag <- ""
  flag <- ifelse(population=="no_preexisting", "FALSE", flag)
  flag <- ifelse(population=="preexisting", "TRUE", flag)
  
  readr::write_csv(tmp, 
                   paste0("output/post_release/table3-preexisting_",flag,".csv"), 
                   na = "-")
  
}