library(broman)
library(dplyr)

# Read in table 2
prevax <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table2_prevax_rounded.csv")
vax <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table2_vax_rounded.csv")
unvax <- readr::read_csv("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table2_unvax_rounded.csv")

table2 <- rbind(prevax,vax,unvax)
rm(prevax,vax,unvax)                         

table2[c("name","exposure","total_events","day0_events","total_exposed","total_person_days","sample_size")] <- NULL
table2 <- table2[table2$analysis %in% c("sub_covid_hospitalised","sub_covid_nonhospitalised"),]

# Unexposed person days and event counts should be the same however currently are not. For now filter to just non-hosp results to 
# formatting works (otherwise it won't format correctly)
table2_pre_expo <- table2 %>% select(cohort,outcome,analysis,population, unexposed_person_days, unexposed_events) %>%
  filter(analysis == "sub_covid_nonhospitalised")
table2_pre_expo$period <- "unexposed"
table2_pre_expo <- table2_pre_expo %>% rename(event_count = unexposed_events,
                                              person_days = unexposed_person_days)

table2_post_expo <- table2 %>% select(cohort,outcome,analysis,population, exposed_person_days, exposed_events)
table2_post_expo$period <- "post_expo"
table2_post_expo <- table2_post_expo %>% rename(event_count = exposed_events,
                                                person_days = exposed_person_days )

table2 <- rbind(table2_pre_expo,table2_post_expo)
rm(table2_pre_expo,table2_post_expo)

table2 <- table2 %>% mutate(across(c("event_count","person_days"), as.numeric))

# Add in incidence rate
table2[,"Incidence rate (per 100,000 person-years)"] <- add_commas(round((table2$event_count/(table2$person_days/365.25))*100000))
table2[,"Event counts/100,000 person-years"] <- paste0(add_commas(table2$event_count), "/", add_commas(round((table2$person_days/365.25))))

table2$period <- ifelse(table2$period=="unexposed","No COVID-19",table2$period)
table2$period <- ifelse(table2$period=="post_expo" & table2$analysis == "sub_covid_hospitalised","After hospitalised COVID-19",table2$period)
table2$period <- ifelse(table2$period=="post_expo" & table2$analysis == "sub_covid_nonhospitalised","After non-hospitalised COVID-19",table2$period)

table2[,c("analysis","person_days","event_count")] <- NULL

# Pivot table
table2 <- tidyr::pivot_wider(table2, names_from = "cohort", values_from = c("Event counts/100,000 person-years","Incidence rate (per 100,000 person-years)"))

table2 <- table2[,c("outcome","population","period","Event counts/100,000 person-years_prevax","Incidence rate (per 100,000 person-years)_prevax",
                                          "Event counts/100,000 person-years_vax","Incidence rate (per 100,000 person-years)_vax",
                                          "Event counts/100,000 person-years_unvax","Incidence rate (per 100,000 person-years)_unvax")]

# Format outcome names
table2 <- table2 %>% mutate(outcome = case_when(outcome == "out_date_pneumonia" ~ "Pneumonia",
                                                outcome == "out_date_asthma" ~ "Asthma",
                                                outcome == "out_date_copd" ~ "COPD",
                                                outcome == "out_date_pulmonary_fibrosis" ~ "Pulmonary Fibrosis"))

table2$outcome <- factor(table2$outcome, levels = c("Pneumonia",
                                                    "Asthma",
                                                    "COPD",
                                                    "Pulmonary Fibrosis"))


table2$period <- factor(table2$period, levels = c("No COVID-19",
                                                  "After hospitalised COVID-19",
                                                  "After non-hospitalised COVID-19"))

table2 <- table2[order(table2$outcome, table2$period),]

table2_no_prexisting <- table2 %>% filter(population == "no_preexisting") %>% select(!population)
table2_no_prexisting <- rbind(c(NA,NA,rep(c("Event/person-years","Incidence rate*"),3)),table2_no_prexisting)

table2_prexisting <- table2 %>% filter(population == "preexisting") %>% select(!population)
table2_prexisting <- rbind(c(NA,NA,rep(c("Event/person-years","Incidence rate*"),3)),table2_prexisting)

# Get population numbers from table 1
for (cohort in c("prevax","vax","unvax")) {
  for (preexisting in c("TRUE","FALSE")) {
    df <- readr::read_csv(paste0("C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/descriptives/table1_",cohort,"_pre_existing_condition_",preexisting,"_covid_history_FALSE_rounded.csv"))
    assign(paste0(cohort,"_preexisting_",preexisting),as.numeric(df$`N (%)`[1]))
  }
}

# Add population numbers to colnames
colnames(table2_no_prexisting) <- c("Outcome","period",paste0("Pre-vaccination cohort (N=",add_commas(prevax_preexisting_FALSE),")"),paste0("Pre-vaccination cohort (N=",add_commas(prevax_preexisting_FALSE),")"),
                                    paste0("Vaccinated (N=",add_commas(vax_preexisting_FALSE),")"),paste0("Vaccinated (N=",add_commas(vax_preexisting_FALSE),")"),
                                    paste0("Unvaccinated (N=",add_commas(unvax_preexisting_FALSE),")"),paste0("Unvaccinated (N=",add_commas(unvax_preexisting_FALSE),")")) 

colnames(table2_prexisting) <- c("Outcome","period",paste0("Pre-vaccination cohort (N=",add_commas(prevax_preexisting_TRUE),")"),paste0("Pre-vaccination cohort (N=",add_commas(prevax_preexisting_TRUE),")"),
                                    paste0("Vaccinated (N=",add_commas(vax_preexisting_TRUE),")"),paste0("Vaccinated (N=",add_commas(vax_preexisting_TRUE),")"),
                                    paste0("Unvaccinated (N=",add_commas(unvax_preexisting_TRUE),")"),paste0("Unvaccinated (N=",add_commas(unvax_preexisting_TRUE),")")) 

write.csv(table2_no_prexisting,"C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/generated-tables/table-2-pre_existing_condition_FALSE.csv",row.names = F)
write.csv(table2_prexisting,"C:/Users/zy21123/OneDrive - University of Bristol/Documents - grp-EHR/Projects/post-covid-respiratory/incident-events/results/generated-tables/table-2-pre_existing_condition_TRUE.csv",row.names = F)
