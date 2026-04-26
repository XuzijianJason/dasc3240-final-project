library(dplyr)
library(readr)

data_path <- file.path("data", "Teen_Mental_Health_Dataset.csv")
out_dir <- "analysis_outputs"
dir.create(out_dir, showWarnings = FALSE)

df <- read_csv(data_path, show_col_types = FALSE) |>
  mutate(
    gender_label = recode(gender, female = "Female", male = "Male"),
    usage_band = ntile(daily_social_media_hours, 4)
  )

numeric_cols <- c(
  "age",
  "daily_social_media_hours",
  "sleep_hours",
  "screen_time_before_sleep",
  "academic_performance",
  "physical_activity",
  "stress_level",
  "anxiety_level"
)

summary_table <- df |>
  summarise(
    teens = n(),
    avg_age = mean(age),
    avg_social_media_hours = mean(daily_social_media_hours),
    avg_sleep_hours = mean(sleep_hours),
    avg_stress = mean(stress_level),
    avg_anxiety = mean(anxiety_level),
    avg_gpa = mean(academic_performance),
    high_depression_risk_share = mean(depression_risk == "high"),
    missing_values = sum(is.na(across(everything())))
  )

usage_band_summary <- df |>
  group_by(usage_band) |>
  summarise(
    teens = n(),
    min_social_hours = min(daily_social_media_hours),
    max_social_hours = max(daily_social_media_hours),
    avg_social_hours = mean(daily_social_media_hours),
    avg_stress = mean(stress_level),
    avg_anxiety = mean(anxiety_level),
    avg_sleep = mean(sleep_hours),
    avg_gpa = mean(academic_performance),
    high_depression_risk_share = mean(depression_risk == "high"),
    .groups = "drop"
  )

segment_summary <- df |>
  group_by(platform_usage, depression_risk) |>
  summarise(
    teens = n(),
    avg_social_hours = mean(daily_social_media_hours),
    avg_stress = mean(stress_level),
    avg_anxiety = mean(anxiety_level),
    avg_sleep = mean(sleep_hours),
    avg_gpa = mean(academic_performance),
    .groups = "drop"
  )

corr_matrix <- as.data.frame(cor(df[, numeric_cols], use = "complete.obs"))
corr_matrix <- cbind(variable = rownames(corr_matrix), corr_matrix)
rownames(corr_matrix) <- NULL

stress_model <- lm(
  stress_level ~ daily_social_media_hours + sleep_hours +
    screen_time_before_sleep + physical_activity + age +
    gender_label + platform_usage + social_interaction_level,
  data = df
)

anxiety_model <- lm(
  anxiety_level ~ daily_social_media_hours + sleep_hours +
    screen_time_before_sleep + physical_activity + age +
    gender_label + platform_usage + social_interaction_level,
  data = df
)

model_to_table <- function(model, model_name) {
  coefs <- summary(model)$coefficients
  data.frame(
    model = model_name,
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    p_value = coefs[, "Pr(>|t|)"],
    r_squared = summary(model)$r.squared,
    row.names = NULL
  )
}

model_table <- bind_rows(
  model_to_table(stress_model, "stress_level"),
  model_to_table(anxiety_model, "anxiety_level")
)

write_csv(summary_table, file.path(out_dir, "overall_summary.csv"))
write_csv(usage_band_summary, file.path(out_dir, "usage_band_summary.csv"))
write_csv(segment_summary, file.path(out_dir, "platform_risk_summary.csv"))
write_csv(corr_matrix, file.path(out_dir, "correlation_matrix.csv"))
write_csv(model_table, file.path(out_dir, "model_coefficients.csv"))
