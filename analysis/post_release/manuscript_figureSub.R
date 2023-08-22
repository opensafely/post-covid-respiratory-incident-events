source("analysis/post_release/fn-post_covid_figure.R")

# Load data ------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(path_model_output, show_col_types = FALSE)
df <- df[df$hr!="[redact]",]

# Run all subgroup plots -------------------------------------------------------

for (y in c(TRUE,FALSE)) {
  for (x in c("hospitalised","sub_covid_history","sub_age","sub_sex","sub_ethnicity")) {
    post_covid_figure(data = df, subgroup = x, preexisting = y)
  }
}
