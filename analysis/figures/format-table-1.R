

files <- list.files("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives", pattern = "table1")
files <- unique(gsub("table1_prevax_|table1_vax_|table1_unvax_","",files))
f=files[1]
for (f in files) {
  save_name <- paste0("table-1-",gsub("_rounded.csv","",f))
  
  prevax <- readr::read_csv(paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table1_prevax_",f))
  vax <- readr::read_csv(paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table1_vax_",f))
  unvax <- readr::read_csv(paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table1_unvax_",f))
  
  colnames(prevax) <- c("Characteristic","Subcharacteristic","Pre-vaccination cohort (1 Jan 2020 to 14 Dec 2021) N (%)","Pre-vaccination cohort (1 Jan 2020 to 14 Dec 2021) COVID-19 diagnoses")
  colnames(vax) <- c("Characteristic","Subcharacteristic","Vaccinated cohort (1 June to 14 Dec 2021) N (%)","Vaccinated cohort (1 June to 14 Dec 2021) COVID-19 diagnoses")
  colnames(unvax) <- c("Characteristic","Subcharacteristic","Unvaccinated cohort (1 June to 14 Dec 2021) N (%)","Unvaccinated cohort (1 June to 14 Dec 2021) COVID-19 diagnoses")
  
  prevax <- prevax %>% left_join(vax, by = c("Characteristic"="Characteristic","Subcharacteristic"="Subcharacteristic"))
  prevax <- prevax %>% left_join(unvax, by = c("Characteristic"="Characteristic","Subcharacteristic"="Subcharacteristic"))
  
  prevax <- rbind(c("Characteristic","Subcharacteristic",rep(c("N (%)","COVID-19 diagnoses"),3)),prevax)
  
  colnames(prevax) <- c("Characteristic","Subcharacteristic","Pre-vaccination cohort (1 Jan 2020 to 14 Dec 2021)","Pre-vaccination cohort (1 Jan 2020 to 14 Dec 2021)",
                        "Vaccinated cohort (1 June to 14 Dec 2021)","Vaccinated cohort (1 June to 14 Dec 2021)",
                        "Unvaccinated cohort (1 June to 14 Dec 2021)","Unvaccinated cohort (1 June to 14 Dec 2021)")
  
  write.csv(prevax,paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/generated-tables/",save_name,".csv"),row.names = F)

}
