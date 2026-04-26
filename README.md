# Teen Social Media and Mental Health

This Shiny app explores how daily social media use is associated with teen stress, anxiety, sleep, academic performance, and depression risk in a synthetic Kaggle dataset.

## Project question

Do teens with heavier daily social media use show higher stress and anxiety, shorter sleep, lower academic performance, and higher depression risk?

## Dataset

The project uses `data/Teen_Mental_Health_Dataset.csv`.

- Source: [Teen Social Media Usage & Mental Health](https://www.kaggle.com/datasets/sureshbeekhani/teen-social-media-usage-and-mental-health/data) by SURESH BEEKHANI on Kaggle
- License: MIT
- Kaggle last updated date: 2026-04-24
- Rows: 2,500 synthetic teen-level records
- Original variables: `age`, `gender`, `daily_social_media_hours`, `platform_usage`, `sleep_hours`, `screen_time_before_sleep`, `academic_performance`, `physical_activity`, `social_interaction_level`, `stress_level`, `anxiety_level`, `depression_risk`
- Derived variables in the app: usage quartile and display labels for categorical fields
- Missing values: none detected

Important caveat: the Kaggle page states that this is a synthetic dataset generated for educational and research purposes. It does not represent real individuals. The app therefore presents the findings as simulation-based relationships, not real-world causal evidence.

## Data preparation

The app performs lightweight preparation:

- Converts gender, platform, depression risk, and social interaction into categorical fields.
- Creates quartiles of daily social media use for story-driven comparisons.
- Preserves the original numeric units for stress, anxiety, sleep, academic performance, screen time before sleep, and physical activity.
- Does not remove any records because no missing values were detected.

## Key findings from the full dataset

- Average daily social media use is about 4.46 hours.
- Average stress level is about 5.60 on a 1-10 scale.
- Average anxiety level is about 5.17 on a 1-10 scale.
- Average sleep is about 6.19 hours.
- Daily social media use is strongly positively correlated with stress (`r` about 0.91).
- Daily social media use is strongly positively correlated with anxiety (`r` about 0.87).
- Daily social media use is strongly negatively correlated with sleep (`r` about -0.77).
- Daily social media use is strongly negatively correlated with academic performance (`r` about -0.86).
- In a regression model for stress, daily social media use is statistically significant at the 0.05 level (`p < 0.001`) after controlling for sleep, bedtime screen use, physical activity, age, gender, platform, and social interaction.

Presentation interpretation: the data tells a clear story, but because it is synthetic and relationship logic is built in, the responsible conclusion is not "social media causes teen mental health problems." A better conclusion is: "In this educational simulation, heavier social media use is strongly associated with higher stress and anxiety, shorter sleep, and higher depression risk."

## Run locally

From the parent folder:

```r
install.packages(c("shiny", "bslib", "dplyr", "plotly", "DT", "readr", "scales"))
shiny::runApp("teen_social_media_mental_health_shiny")
```

From inside this folder:

```r
shiny::runApp()
```

## Reproduce summary outputs

```r
source("analysis.R")
```

This writes summary CSV files to `analysis_outputs/`.

## App structure

```text
teen_social_media_mental_health_shiny/
  app.R
  analysis.R
  README.md
  data/
    SOURCE.md
    Teen_Mental_Health_Dataset.csv
  www/
    styles.css
  analysis_outputs/
```

## Deployment

After uploading this folder to a public GitHub repository, other users can run it with:

```r
shiny::runGitHub("<repo>", "<username>")
```

It can also be deployed to shinyapps.io or another Shiny-compatible host.
