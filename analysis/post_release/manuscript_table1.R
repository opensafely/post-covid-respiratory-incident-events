# Load data --------------------------------------------------------------------
print("Load data")

files <- list.files(path = path_table1, pattern = "table1_", full.names = TRUE)

df <- NULL

for (f in files) {
  tmp <- readr::read_csv(f, show_col_types = FALSE)
  tmp$source <- gsub(".*/","",f)
  tmp$split <- gsub("table1_","",gsub("_pre_existing_condition","",gsub("_covid_history","",gsub("_rounded.csv","",tmp$source))))
  tmp <- tidyr::separate(tmp, split, into = c("cohort","pre_existing_condition","covid_history"))
  df <- rbind(df, tmp)
}

# Pivot table ------------------------------------------------------------------
print("Pivot table")

df <- tidyr::pivot_wider(df, 
                         id_cols = c("Characteristic","Subcharacteristic","pre_existing_condition","covid_history"),
                         names_from = "cohort",
                         values_from = c("N (%)","COVID-19 diagnoses"))

# Save table -------------------------------------------------------------------
print("Save table")

for (covid_history in c("TRUE","FALSE")) {
  for (pre_existing_condition in c("TRUE","FALSE")) {
    
    tmp <- df[df$pre_existing_condition==pre_existing_condition &
              df$covid_history==covid_history,
              c("Characteristic","Subcharacteristic",
                 paste0(c("N (%)","COVID-19 diagnoses"),"_prevax"),
                 paste0(c("N (%)","COVID-19 diagnoses"),"_vax"),
                 paste0(c("N (%)","COVID-19 diagnoses"),"_unvax"))]

    readr::write_csv(tmp, 
                     paste0("output/post_release/table1-preexisting_",pre_existing_condition,"-covid_history_",covid_history,".csv"), 
                     na = "-")
    
  }
}
