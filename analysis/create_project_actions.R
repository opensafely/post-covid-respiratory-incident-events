library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)


###########################
# Load information to use #
###########################

## defaults ----
defaults_list <- list(
  version = "3.0",
  expectations= list(population_size=10000L)
)

# Define active analyses -------------------------------------------------------

active_analyses <- read_rds("lib/active_analyses.rds")
active_analyses <- active_analyses[order(active_analyses$analysis,active_analyses$cohort,active_analyses$outcome),]
active_analyses <- active_analyses[active_analyses$cohort %in% c("prevax","unvax","vax"),]
cohorts <- unique(active_analyses$cohort)

# Specify active analyses requiring Stata --------------------------------------

run_stata <- c("cohort_prevax-sub_age_60_79-pneumonia-no_preexisting",
               "cohort_prevax-sub_covid_hospitalised-pneumonia-preexisting",
               "cohort_prevax-sub_covid_hospitalised-pneumonia-no_preexisting",
               "cohort_prevax-sub_ethnicity_mixed-pneumonia-no_preexisting",
               "cohort_prevax-sub_ethnicity_other-pneumonia-no_preexisting",
               "cohort_prevax-sub_covid_hospitalised-copd-no_preexisting",
               "cohort_prevax-sub_covid_hospitalised-pulmonary_fibrosis-no_preexisting",
               "cohort_prevax-main-pneumonia-preexisting",
               "cohort_prevax-sub_covid_nonhospitalised-pneumonia-preexisting",
               "cohort_prevax-sub_covid_hospitalised-pulmonary_fibrosis-no_preexisting",
#               "cohort_unvax-main-copd-no_preexisting",
               "cohort_unvax-sub_age_60_79-pneumonia-no_preexisting",
               "cohort_unvax-sub_covid_hospitalised-pneumonia-preexisting",
               "cohort_unvax-sub_covid_hospitalised-pneumonia-no_preexisting",
               "cohort_unvax-sub_ethnicity_asian-pneumonia-no_preexisting",
#               "cohort_unvax-main-pulmonary_fibrosis-no_preexisting",
               "cohort_unvax-main-pneumonia-preexisting",
#               "cohort_unvax-main-pulmonary_fibrosis-no_preexisting",
#               "cohort_vax-sub_covid_hospitalised-asthma-no_preexisting",
#               "cohort_vax-sub_covid_hospitalised-copd-no_preexisting",
               "cohort_vax-sub_covid_hospitalised-pneumonia-preexisting",
               "cohort_vax-sub_covid_hospitalised-pneumonia-no_preexisting",
#               "cohort_vax-sub_covid_hospitalised-pulmonary_fibrosis-no_preexisting",
               "cohort_vax-main-pulmonary_fibrosis-no_preexisting"
#               "cohort_unvax-sub_ethnicity_other-pulmonary_fibrosis-preexisting"
                )

stata <- active_analyses[active_analyses$name %in% run_stata,]
stata$save_analysis_ready <- TRUE
stata$day0 <- grepl("1;",stata$cut_points)


# create action functions ----

############################
## generic action function #
############################


action <- function(
  name,
  run,
  dummy_data_file=NULL,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL
){
  
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL
  
  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  action[sapply(action, is.null)] <- NULL
  
  action_list <- list(name = action)
  names(action_list) <- name
  
  action_list
}


## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


 #################################################
 ## Function for typical actions to analyse data #
 #################################################
 # Updated to a typical action running Cox models for one outcome
 apply_model_function <- function(name, cohort, analysis, ipw, strata, 
                                 covariate_sex, covariate_age, covariate_other, 
                                 cox_start, cox_stop, study_start, study_stop,
                                 cut_points, controls_per_case,
                                 total_event_threshold, episode_event_threshold,
                                 covariate_threshold, age_spline){
  
  splice(
    action(
      name = glue("make_model_input-{name}"),
      run = glue("r:latest analysis/model/make_model_input.R {name}"),
      needs = list("stage1_data_cleaning_all"),
      highly_sensitive = list(
        model_input = glue("output/model_input-{name}.rds")
      )
    ),

  action(
      name = glue("cox_ipw-{name}"),
      run = glue("cox-ipw:v0.0.21 --df_input=model_input-{name}.rds --ipw={ipw} --exposure=exp_date --outcome=out_date --strata={strata} --covariate_sex={covariate_sex} --covariate_age={covariate_age} --covariate_other={covariate_other} --cox_start={cox_start} --cox_stop={cox_stop} --study_start={study_start} --study_stop={study_stop} --cut_points={cut_points} --controls_per_case={controls_per_case} --total_event_threshold={total_event_threshold} --episode_event_threshold={episode_event_threshold} --covariate_threshold={covariate_threshold} --age_spline={age_spline} --df_output=model_output-{name}.csv"),
      needs = list(glue("make_model_input-{name}")),
      moderately_sensitive = list(
        model_output = glue("output/model_output-{name}.csv"))
    )
  )
}

# Create function to make Table 1 ----------------------------------------------

table1 <- function(cohort, pre_existing_condition, covid_history){
  splice(
    comment(glue("Table 1 - {cohort} pre_existing_condition {pre_existing_condition} covid_history {covid_history}")),
    action(
      name = glue("table1_{cohort}_pre_existing_condition_{pre_existing_condition}_covid_history_{covid_history}"),
      run = "r:latest analysis/descriptives/table1.R",
      arguments = c(cohort,pre_existing_condition,covid_history),
      needs = list(glue("stage1_data_cleaning_all")),
      moderately_sensitive = list(
        table1 = glue("output/review/descriptives/table1_{cohort}_pre_existing_condition_{pre_existing_condition}_covid_history_{covid_history}.csv"),
        table1_rounded = glue("output/review/descriptives/table1_{cohort}_pre_existing_condition_{pre_existing_condition}_covid_history_{covid_history}_rounded.csv")
      )
    )
  )
}

# Create function to make Table 2 ----------------------------------------------

table2 <- function(cohort){
  
  table2_names <- gsub("out_date_","",unique(active_analyses[active_analyses$cohort=={cohort},]$name))
  
  splice(
    comment(glue("Table 2 - {cohort}")),
    action(
      name = glue("table2_{cohort}"),
      run = "r:latest analysis/descriptives/table2.R",
      arguments = c(cohort),
      needs = c(as.list(paste0("make_model_input-",table2_names))),
      moderately_sensitive = list(
        table2 = glue("output/table2_{cohort}.csv"),
        table2_rounded = glue("output/table2_{cohort}_rounded.csv")
      )
    )
  )
}

# Create function to make Stata models -----------------------------------------

apply_stata_model_function <- function(name, cohort, analysis, ipw, strata, 
                                       covariate_sex, covariate_age, covariate_other, 
                                       cox_start, cox_stop, study_start, study_stop,
                                       cut_points, controls_per_case,
                                       total_event_threshold, episode_event_threshold,
                                       covariate_threshold, age_spline, day0){
  splice(
    action(
      name = glue("ready-{name}"),
      run = glue("cox-ipw:v0.0.27 --df_input=model_input-{name}.rds --ipw={ipw} --exposure=exp_date --outcome=out_date --strata={strata} --covariate_sex={covariate_sex} --covariate_age={covariate_age} --covariate_other={covariate_other} --cox_start={cox_start} --cox_stop={cox_stop} --study_start={study_start} --study_stop={study_stop} --cut_points={cut_points} --controls_per_case={controls_per_case} --total_event_threshold={total_event_threshold} --episode_event_threshold={episode_event_threshold} --covariate_threshold={covariate_threshold} --age_spline={age_spline} --save_analysis_ready=TRUE --run_analysis=FALSE --df_output=model_output-{name}.csv"),
      needs = list(glue("make_model_input-{name}")),
      highly_sensitive = list(ready = glue("output/ready-{name}.csv.gz"))
    ),
    action(
      name = glue("stata_cox_ipw-{name}"),
      run = "stata-mp:latest analysis/cox_model.do",
      arguments = c(name, day0),
      needs = c(as.list(glue("ready-{name}"))),
      moderately_sensitive = list(
        stata_fup = glue("output/stata_fup-{name}.csv"),
        stata_model_output = glue("output/stata_model_output-{name}.txt")
      )
    )
  )
}

##########################################################
## Define and combine all actions into a list of actions #
##########################################################
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  comment("Generate vaccination eligibility information"),
  action(
    name = glue("vax_eligibility_inputs"),
    run = "r:latest analysis/metadates.R",
    highly_sensitive = list(
      study_dates_json = glue("output/study_dates.json"),
      vax_jcvi_groups= glue("output/vax_jcvi_groups.csv"),
      vax_eligible_dates= ("output/vax_eligible_dates.csv")
    )
  ),

  comment("Generate dummy data for study_definition - population_prelim"),
  action(
    name = "generate_study_population_prelim",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prelim --output-format feather",
    needs = list("vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_prelim.feather")
    )
  ),
  
  comment("Generate dates for all study cohorts"),
  action(
    name = "generate_index_dates",
    run = "r:latest analysis/prelim.R",
    needs = list("vax_eligibility_inputs","generate_study_population_prelim"),
    highly_sensitive = list(
      index_dates = glue("output/index_dates.csv")
    )
  ),

  comment("Generate dummy data for study_definition - prevax"),
  action(
    name = "generate_study_population_prevax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_prevax --output-format csv.gz",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.csv.gz")
    )
  ),

  comment("Generate dummy data for study_definition - vax"),
  action(
    name = "generate_study_population_vax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_vax --output-format csv.gz",
    needs = list("generate_index_dates","vax_eligibility_inputs"),
    highly_sensitive = list(
      cohort = glue("output/input_vax.csv.gz")
    )
  ),

  comment("Generate dummy data for study_definition - unvax"),
  action(
    name = "generate_study_population_unvax",
    run = "cohortextractor:latest generate_cohort --study-definition study_definition_unvax --output-format csv.gz",
    needs = list("vax_eligibility_inputs","generate_index_dates"),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.csv.gz")
    )
  ),
  
  comment("Preprocess data -prevax"),
  action(
    name = "preprocess_data_prevax",
    run = "r:latest analysis/preprocess/preprocess_data.R prevax",
    needs = list( "generate_index_dates","generate_study_population_prevax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_prevax_stage0.txt"),
      describe_venn = glue("output/not-for-review/describe_venn_prevax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_prevax.rds"),
      venn = glue("output/venn_prevax.rds")
    )
  ),
  
  comment("Preprocess data - vax"),
  action(
    name = "preprocess_data_vax",
    run = "r:latest analysis/preprocess/preprocess_data.R vax",
    needs = list("generate_index_dates","generate_study_population_vax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_vax_stage0.txt"),
      descrive_venn = glue("output/not-for-review/describe_venn_vax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_vax.rds"),
      venn = glue("output/venn_vax.rds")
    )
  ), 
  
  comment("Preprocess data -unvax"),
  action(
    name = "preprocess_data_unvax",
    run = "r:latest analysis/preprocess/preprocess_data.R unvax",
    needs = list("generate_index_dates", "generate_study_population_unvax"),
    moderately_sensitive = list(
      describe = glue("output/not-for-review/describe_input_unvax_stage0.txt"),
      describe_venn = glue("output/not-for-review/describe_venn_unvax.txt")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_unvax.rds"),
      venn = glue("output/venn_unvax.rds")
    )
  ),

  comment("Stage 1 - Data cleaning - all cohorts"),
  action(
    name = "stage1_data_cleaning_all",
    run = "r:latest analysis/preprocess/Stage1_data_cleaning.R all",
    needs = list("preprocess_data_prevax","preprocess_data_vax", "preprocess_data_unvax","vax_eligibility_inputs"),
    moderately_sensitive = list(
      refactoring = glue("output/not-for-review/meta_data_factors_*.csv"),
      QA_rules = glue("output/review/descriptives/QA_summary_*.csv"),
      IE_criteria = glue("output/review/descriptives/Cohort_flow_*.csv"),
      histograms = glue("output/not-for-review/numeric_histograms_*.svg")
    ),
    highly_sensitive = list(
      cohort = glue("output/input_*.rds")
    )
  ),

  action(
    name = glue("describe_file-input_prevax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_prevax_stage1 rds"),
    needs = list("stage1_data_cleaning_all"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_prevax_stage1.txt")
    )
  ),
  
  action(
    name = glue("describe_file-input_vax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_vax_stage1 rds"),
    needs = list("stage1_data_cleaning_all"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_vax_stage1.txt")
    )
  ),
  
  action(
    name = glue("describe_file-input_unvax_stage1"),
    run = glue("r:latest analysis/describe_file.R input_unvax_stage1 rds"),
    needs = list("stage1_data_cleaning_all"),
    moderately_sensitive = list(
      describe_model_input = glue("output/describe-input_unvax_stage1.txt")
    )
  ),

  comment("Stage 5 - Run models"),
  splice(
    # over outcomes
    unlist(lapply(1:nrow(active_analyses), 
                  function(x) apply_model_function(name = active_analyses$name[x],
                                                   cohort = active_analyses$cohort[x],
                                                   analysis = active_analyses$analysis[x],
                                                   ipw = active_analyses$ipw[x],
                                                   strata = active_analyses$strata[x],
                                                   covariate_sex = active_analyses$covariate_sex[x],
                                                   covariate_age = active_analyses$covariate_age[x],
                                                   covariate_other = active_analyses$covariate_other[x],
                                                   cox_start = active_analyses$cox_start[x],
                                                   cox_stop = active_analyses$cox_stop[x],
                                                   study_start = active_analyses$study_start[x],
                                                   study_stop = active_analyses$study_stop[x],
                                                   cut_points = active_analyses$cut_points[x],
                                                   controls_per_case = active_analyses$controls_per_case[x],
                                                   total_event_threshold = active_analyses$total_event_threshold[x],
                                                   episode_event_threshold = active_analyses$episode_event_threshold[x],
                                                   covariate_threshold = active_analyses$covariate_threshold[x],
                                                   age_spline = active_analyses$age_spline[x])), recursive = FALSE
    )
  ),
  
  splice(
    unlist(lapply(1:nrow(stata), 
                  function(x) apply_stata_model_function(name = stata$name[x],
                                                         cohort = stata$cohort[x],
                                                         analysis = stata$analysis[x],
                                                         ipw = stata$ipw[x],
                                                         strata = stata$strata[x],
                                                         covariate_sex = stata$covariate_sex[x],
                                                         covariate_age = stata$covariate_age[x],
                                                         covariate_other = stata$covariate_other[x],
                                                         cox_start = stata$cox_start[x],
                                                         cox_stop = stata$cox_stop[x],
                                                         study_start = stata$study_start[x],
                                                         study_stop = stata$study_stop[x],
                                                         cut_points = stata$cut_points[x],
                                                         controls_per_case = stata$controls_per_case[x],
                                                         total_event_threshold = stata$total_event_threshold[x],
                                                         episode_event_threshold = stata$episode_event_threshold[x],
                                                         covariate_threshold = stata$covariate_threshold[x],
                                                         age_spline = stata$age_spline[x],
                                                         day0 = stata$day0[x])), 
           recursive = FALSE
    )
  ),
  
  comment("Make Table 1"),
  splice(
    # over outcomes
    unlist(lapply(unique(active_analyses$cohort), function(x) 
      splice(unlist(lapply(c("TRUE","FALSE"), function(y)
        splice(unlist(lapply(c("TRUE","FALSE"), function(z) table1(cohort = x, pre_existing_condition = y, covid_history = z)),recursive = FALSE))
        ), recursive = FALSE))
    ),recursive = FALSE)),
  

  comment("Make Table 2"),
  splice(
    unlist(lapply(unique(active_analyses$cohort), 
                  function(x) table2(cohort = x)), 
           recursive = FALSE
    )
  ),

  action(
    name = "make_model_output",
    run = "r:latest analysis/model/make_model_output.R",
    needs = as.list(paste0("cox_ipw-",
                           setdiff(active_analyses$name,stata$name))),
    moderately_sensitive = list(
      model_output = glue("output/model_output.csv"),
      model_output_midpoint6 = glue("output/model_output_midpoint6.csv")
    )
  ),
  
  action(
    name = "make_stata_model_output",
    run = "r:latest analysis/model/make_stata_model_output.R",
    needs = as.list(paste0("stata_cox_ipw-",stata$name)),
    moderately_sensitive = list(
      stata_model_output = glue("output/stata_model_output.csv"),
      stata_model_output_midpoint6 = glue("output/stata_model_output_midpoint6.csv")
    )
  )
)


## combine everything ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)
  
#####################################################################################
## convert list to yaml, reformat comments and white space, and output a .yaml file #
#####################################################################################
as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")
