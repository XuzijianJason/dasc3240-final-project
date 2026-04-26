library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(readr)
library(scales)

data_path <- file.path("data", "Teen_Mental_Health_Dataset.csv")
raw_data <- read_csv(data_path, show_col_types = FALSE)

app_data <- raw_data |>
  mutate(
    gender_label = factor(
      recode(gender, female = "Female", male = "Male"),
      levels = c("Female", "Male")
    ),
    platform_usage = factor(platform_usage, levels = c("Instagram", "TikTok", "Both", "Other")),
    social_interaction_level = factor(
      social_interaction_level,
      levels = c("low", "medium", "high"),
      labels = c("Low", "Medium", "High")
    ),
    depression_risk = factor(
      depression_risk,
      levels = c("low", "medium", "high"),
      labels = c("Low", "Medium", "High")
    ),
    usage_band = factor(
      ntile(daily_social_media_hours, 4),
      levels = 1:4,
      labels = c("Lowest", "Lower middle", "Upper middle", "Highest")
    )
  )

metric_choices <- c(
  "Stress level" = "stress_level",
  "Anxiety level" = "anxiety_level",
  "Sleep hours" = "sleep_hours",
  "Academic performance" = "academic_performance"
)

metric_labels <- c(
  stress_level = "Stress level",
  anxiety_level = "Anxiety level",
  sleep_hours = "Sleep hours",
  academic_performance = "Academic performance (GPA)"
)

metric_units <- c(
  stress_level = "points",
  anxiety_level = "points",
  sleep_hours = "hours",
  academic_performance = "GPA points"
)

axis_labels <- c(
  age = "Age",
  daily_social_media_hours = "Daily social media use (hours)",
  sleep_hours = "Sleep hours",
  screen_time_before_sleep = "Screen time before sleep (hours)",
  academic_performance = "Academic performance (GPA)",
  physical_activity = "Physical activity (hours)",
  stress_level = "Stress level",
  anxiety_level = "Anxiety level"
)

term_labels <- c(
  "(Intercept)" = "Baseline",
  daily_social_media_hours = "Daily social media use (per hour)",
  sleep_hours = "Sleep hours (per hour)",
  screen_time_before_sleep = "Screen time before sleep (per hour)",
  physical_activity = "Physical activity (per hour)",
  age = "Age (per year)",
  gender_labelMale = "Gender: Male vs Female",
  platform_usageTikTok = "Platform: TikTok vs Instagram",
  platform_usageBoth = "Platform: Both vs Instagram",
  platform_usageOther = "Platform: Other vs Instagram",
  social_interaction_levelMedium = "Social interaction: Medium vs Low",
  social_interaction_levelHigh = "Social interaction: High vs Low"
)

color_choices <- c(
  "Depression risk" = "depression_risk",
  "Platform" = "platform_usage",
  "Gender" = "gender_label",
  "Social interaction" = "social_interaction_level"
)

plot_palette <- c(
  "#1f7a8c",
  "#d95f0e",
  "#3f7f4f",
  "#6b5b95",
  "#b56576",
  "#3d5a80",
  "#9a6700",
  "#4f6f52"
)

risk_palette <- c(
  Low = "#3f7f4f",
  Medium = "#d99a2b",
  High = "#c2410c"
)

standard_margin <- list(l = 78, r = 32, t = 32, b = 130)

filters <- function() {
  sidebar(
    title = "Filters",
    width = 285,
    selectInput(
      "platforms",
      "Platform",
      choices = levels(app_data$platform_usage),
      selected = levels(app_data$platform_usage),
      multiple = TRUE
    ),
    checkboxGroupInput(
      "genders",
      "Gender",
      choices = levels(app_data$gender_label),
      selected = levels(app_data$gender_label),
      inline = TRUE
    ),
    checkboxGroupInput(
      "risk_levels",
      "Depression risk",
      choices = levels(app_data$depression_risk),
      selected = levels(app_data$depression_risk),
      inline = TRUE
    ),
    checkboxGroupInput(
      "interaction_levels",
      "Social interaction",
      choices = levels(app_data$social_interaction_level),
      selected = levels(app_data$social_interaction_level)
    ),
    sliderInput(
      "age_range",
      "Age",
      min = min(app_data$age),
      max = max(app_data$age),
      value = range(app_data$age),
      step = 1,
      sep = ""
    ),
    sliderInput(
      "usage_range",
      "Daily social media use",
      min = min(app_data$daily_social_media_hours),
      max = max(app_data$daily_social_media_hours),
      value = range(app_data$daily_social_media_hours),
      step = 0.1
    ),
    selectInput("metric", "Outcome", choices = metric_choices),
    radioButtons("color_by", "Segment", choices = color_choices)
  )
}

stat_card <- function(label, value, note = NULL) {
  div(
    class = "stat-card",
    div(class = "stat-label", label),
    div(class = "stat-value", value),
    if (!is.null(note)) div(class = "stat-note", note)
  )
}

ui <- page_navbar(
  title = "Teen Mental Health",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  header = tags$head(
    tags$link(rel = "icon", href = "data:,"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebar = filters(),
  nav_panel(
    "Story",
    div(
      class = "hero",
      div(
        class = "hero-copy",
        div(class = "hero-kicker", "Interactive Shiny Analysis"),
        h1("Teen Social Media and Mental Health"),
        p(
          "How strongly does daily social media use track stress, anxiety, sleep, and academic performance in a synthetic dataset of 2,500 teen records?"
        )
      )
    ),
    uiOutput("kpis"),
    div(class = "section-title", "Main pattern"),
    uiOutput("main_finding"),
    div(class = "chart-block", plotlyOutput("scatter_plot", height = "620px"))
  ),
  nav_panel(
    "Explore",
    div(class = "section-title", "Usage bands"),
    div(class = "chart-block", plotlyOutput("band_plot", height = "560px")),
    div(class = "section-title", "Depression risk ladder"),
    div(class = "chart-block", plotlyOutput("risk_plot", height = "540px")),
    div(class = "section-title", "Correlation map"),
    div(class = "chart-block", plotlyOutput("heatmap_plot", height = "680px")),
    div(class = "section-title", "Segment summary"),
    DTOutput("segment_table")
  ),
  nav_panel(
    "Explain",
    div(class = "section-title", "Answer"),
    uiOutput("explanation"),
    div(class = "section-title", "Model check"),
    uiOutput("model_finding"),
    div(class = "table-wrap", tableOutput("model_table")),
    div(class = "section-title", "Outcome distribution"),
    div(class = "chart-block", plotlyOutput("distribution_plot", height = "540px"))
  ),
  nav_panel(
    "Data",
    fluidPage(
      div(class = "section-title", "Dataset, source, and preparation"),
      fluidRow(
        column(
          4,
          div(
            class = "method-card",
            h3("Dataset"),
            p("Source: Kaggle dataset by SURESH BEEKHANI."),
            tags$a(
              "Teen Social Media Usage & Mental Health",
              href = "https://www.kaggle.com/datasets/sureshbeekhani/teen-social-media-usage-and-mental-health/data",
              target = "_blank"
            ),
            p("Rows: 2,500"),
            p("Variables: 12 original variables plus derived usage bands and display labels."),
            p("Unit of analysis: one synthetic teen-level record.")
          )
        ),
        column(
          4,
          div(
            class = "method-card",
            h3("Preparation"),
            p("No missing values were detected."),
            p("Gender, platform, depression risk, and social interaction were converted to categorical fields."),
            p("Daily social media use bands are quartiles used for storytelling and comparison.")
          )
        ),
        column(
          4,
          div(
            class = "method-card warning",
            h3("License and caveat"),
            p("Kaggle lists the dataset license as MIT."),
            p("The Kaggle page states that this is a synthetic dataset generated for educational and research purposes."),
            p("The app therefore interprets patterns as designed simulation relationships, not evidence about real individuals.")
          )
        )
      ),
      div(class = "section-title", "Data preview"),
      DTOutput("data_preview")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$platforms, input$genders, input$risk_levels, input$interaction_levels)
    app_data |>
      filter(
        platform_usage %in% input$platforms,
        gender_label %in% input$genders,
        depression_risk %in% input$risk_levels,
        social_interaction_level %in% input$interaction_levels,
        age >= input$age_range[1],
        age <= input$age_range[2],
        daily_social_media_hours >= input$usage_range[1],
        daily_social_media_hours <= input$usage_range[2]
      )
  })

  metric_name <- reactive(metric_labels[[input$metric]])

  model_result <- reactive({
    df <- filtered_data()
    req(nrow(df) > 25)
    base_predictors <- c(
      "daily_social_media_hours",
      "sleep_hours",
      "screen_time_before_sleep",
      "physical_activity",
      "age",
      "gender_label",
      "platform_usage",
      "social_interaction_level"
    )
    predictors <- setdiff(base_predictors, input$metric)
    model <- lm(reformulate(predictors, response = input$metric), data = df)
    list(model = model, summary = summary(model), predictors = predictors)
  })

  output$kpis <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 1)
    corr_value <- cor(df$daily_social_media_hours, df[[input$metric]], use = "complete.obs")
    high_risk_share <- mean(df$depression_risk == "High")
    fluidRow(
      column(3, stat_card("Teens", comma(nrow(df)), "Filtered records")),
      column(3, stat_card("Avg social use", number(mean(df$daily_social_media_hours), accuracy = 0.01), "Hours per day")),
      column(3, stat_card(metric_name(), number(mean(df[[input$metric]]), accuracy = 0.01), "Filtered mean")),
      column(3, stat_card("High risk", percent(high_risk_share, accuracy = 0.1), "Depression risk share"))
    )
  })

  output$main_finding <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 2)
    corr_value <- cor(df$daily_social_media_hours, df[[input$metric]], use = "complete.obs")
    band_summary <- df |>
      group_by(usage_band) |>
      summarise(mean_metric = mean(.data[[input$metric]], na.rm = TRUE), .groups = "drop") |>
      arrange(usage_band)
    low <- band_summary$mean_metric[1]
    high <- band_summary$mean_metric[nrow(band_summary)]
    low_band <- as.character(band_summary$usage_band[1])
    high_band <- as.character(band_summary$usage_band[nrow(band_summary)])
    delta <- high - low
    signal <- if (abs(corr_value) >= 0.7) {
      "The relationship is strong in this filtered view."
    } else if (abs(corr_value) >= 0.3) {
      "The relationship is moderate in this filtered view."
    } else {
      "The relationship is weak in this filtered view."
    }
    div(
      class = "finding-box",
      strong(signal),
      p(paste0(
        "Correlation between daily social media use and ", metric_name(),
        " is r = ", number(corr_value, accuracy = 0.001), "."
      )),
      p(paste0(
        "The mean difference from the ", low_band, " to ", high_band,
        " usage band is ", number(delta, accuracy = 0.01),
        " ", metric_units[[input$metric]], "."
      ))
    )
  })

  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 2)
    color_var <- input$color_by
    plot_df <- df |>
      mutate(
        metric_value = .data[[input$metric]],
        color_value = as.character(.data[[color_var]]),
        tooltip = paste0(
          "Age: ", age,
          "<br>Gender: ", gender_label,
          "<br>Platform: ", platform_usage,
          "<br>Depression risk: ", depression_risk,
          "<br>Social media hours: ", daily_social_media_hours,
          "<br>", metric_name(), ": ", round(metric_value, 2)
        )
      )
    trend_model <- lm(metric_value ~ daily_social_media_hours, data = plot_df)
    trend <- data.frame(
      daily_social_media_hours = seq(
        min(plot_df$daily_social_media_hours),
        max(plot_df$daily_social_media_hours),
        length.out = 120
      )
    )
    trend$metric_value <- predict(trend_model, newdata = trend)

    plot_ly(
      plot_df,
      x = ~daily_social_media_hours,
      y = ~metric_value,
      color = ~color_value,
      colors = plot_palette,
      type = "scatter",
      mode = "markers",
      text = ~tooltip,
      hoverinfo = "text",
      marker = list(size = 8, opacity = 0.55)
    ) |>
      add_lines(
        data = trend,
        x = ~daily_social_media_hours,
        y = ~metric_value,
        inherit = FALSE,
        name = "Linear trend",
        line = list(color = "#263238", width = 3),
        hoverinfo = "skip"
      ) |>
      layout(
        xaxis = list(title = "Daily social media use (hours)"),
        yaxis = list(title = metric_name()),
        legend = list(
          title = list(text = names(color_choices[color_choices == color_var])),
          orientation = "h",
          x = 0,
          y = -0.22
        ),
        margin = standard_margin
      )
  })

  output$band_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 2)
    color_var <- input$color_by
    plot_df <- df |>
      mutate(
        metric_value = .data[[input$metric]],
        color_value = as.character(.data[[color_var]]),
        tooltip = paste0(
          "Usage band: ", usage_band,
          "<br>", names(color_choices[color_choices == color_var]), ": ", color_value,
          "<br>", metric_name(), ": ", round(metric_value, 2)
        )
      )
    plot_ly(
      plot_df,
      x = ~usage_band,
      y = ~metric_value,
      color = ~color_value,
      colors = plot_palette,
      type = "box",
      boxpoints = "outliers",
      text = ~tooltip,
      hoverinfo = "text",
      marker = list(opacity = 0.35)
      ) |>
      layout(
        xaxis = list(title = "Daily social media use quartile"),
        yaxis = list(title = metric_name()),
        legend = list(
          title = list(text = names(color_choices[color_choices == color_var])),
          orientation = "h",
          x = 0,
          y = -0.22
        ),
        margin = standard_margin
      )
  })

  output$risk_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 2)
    risk_summary <- df |>
      count(usage_band, depression_risk, name = "teens") |>
      group_by(usage_band) |>
      mutate(pct = teens / sum(teens)) |>
      ungroup()
    plot_ly(
      risk_summary,
      x = ~usage_band,
      y = ~pct,
      color = ~depression_risk,
      colors = risk_palette,
      type = "bar",
      text = ~paste0(
        "Usage band: ", usage_band,
        "<br>Risk: ", depression_risk,
        "<br>Teens: ", teens,
        "<br>Share: ", percent(pct, accuracy = 0.1)
      ),
      hoverinfo = "text"
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "Daily social media use quartile"),
        yaxis = list(title = "Share of teens", tickformat = ".0%"),
        legend = list(title = list(text = "Depression risk"), orientation = "h", x = 0, y = -0.22),
        margin = standard_margin
      )
  })

  output$heatmap_plot <- renderPlotly({
    df <- filtered_data()
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
    req(nrow(df) > 2)
    corr_matrix <- cor(df[, numeric_cols], use = "complete.obs")
    labels <- axis_labels[colnames(corr_matrix)]
    plot_ly(
      x = labels,
      y = labels,
      z = corr_matrix,
      type = "heatmap",
      colors = colorRamp(c("#2b8cbe", "#f7f7f7", "#d95f0e")),
      zmin = -1,
      zmax = 1,
      hovertemplate = "%{y} vs %{x}<br>r = %{z:.3f}<extra></extra>"
    ) |>
      layout(
        xaxis = list(tickangle = 22, automargin = TRUE),
        yaxis = list(autorange = "reversed"),
        margin = list(l = 210, r = 40, t = 35, b = 190)
      )
  })

  output$segment_table <- renderDT({
    df <- filtered_data()
    color_var <- input$color_by
    df |>
      group_by(.data[[color_var]], usage_band) |>
      summarise(
        teens = n(),
        avg_social_hours = mean(daily_social_media_hours),
        avg_stress = mean(stress_level),
        avg_anxiety = mean(anxiety_level),
        avg_sleep = mean(sleep_hours),
        avg_gpa = mean(academic_performance),
        high_risk_share = mean(depression_risk == "High"),
        .groups = "drop"
      ) |>
      arrange(usage_band) |>
      datatable(rownames = FALSE, options = list(pageLength = 8, dom = "tip")) |>
      formatRound(c("avg_social_hours", "avg_stress", "avg_anxiety", "avg_sleep", "avg_gpa"), 2) |>
      formatPercentage("high_risk_share", 1)
  })

  output$explanation <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 2)
    corr_vars <- setdiff(
      c("daily_social_media_hours", "sleep_hours", "screen_time_before_sleep", "physical_activity", "academic_performance"),
      input$metric
    )
    corr_values <- vapply(
      corr_vars,
      function(var) cor(df[[var]], df[[input$metric]], use = "complete.obs"),
      numeric(1)
    )
    strongest_var <- names(which.max(abs(corr_values)))
    strongest_label <- axis_labels[[strongest_var]]
    strongest_value <- corr_values[[strongest_var]]
    strength <- if (abs(strongest_value) >= 0.7) {
      "strong"
    } else if (abs(strongest_value) >= 0.3) {
      "moderate"
    } else {
      "weak"
    }
    div(
      class = "finding-box",
      p(paste0(
        "For ", metric_name(), ", the strongest simple relationship among the comparable behavior variables is ",
        strongest_label, " (r = ", number(strongest_value, accuracy = 0.001), "), which is ", strength, "."
      )),
      p(
        "The visible pattern is clear: heavier social media use is tied to higher stress and anxiety, shorter sleep, lower GPA, and higher depression risk in this synthetic dataset."
      ),
      p(
        "Because the Kaggle page states that the records are synthetic and built with relationship logic, these results should be presented as an educational simulation, not as real-world causal evidence."
      )
    )
  })

  output$model_finding <- renderUI({
    result <- model_result()
    coefs <- result$summary$coefficients
    r2 <- result$summary$r.squared
    social_term <- "daily_social_media_hours"
    social_estimate <- coefs[social_term, "Estimate"]
    social_p <- coefs[social_term, "Pr(>|t|)"]
    p_text <- if (social_p < 0.001) "< 0.001" else number(social_p, accuracy = 0.001)
    direction <- if (social_estimate < 0) "lower" else "higher"
    significance_text <- if (social_p < 0.05) {
      "statistically significant at the 0.05 level"
    } else {
      "not statistically significant at the 0.05 level"
    }
    div(
      class = "finding-box compact",
      p(paste0(
        "Holding sleep, bedtime screen use, physical activity, age, gender, platform, and social interaction constant when applicable, each additional hour of daily social media use is associated with ",
        direction, " ", metric_name(), " by ",
        number(abs(social_estimate), accuracy = 0.001),
        " ", metric_units[[input$metric]], ". The p-value is ", p_text,
        ", so this estimate is ", significance_text, "."
      )),
      p(paste0(
        "The model explains ", percent(r2, accuracy = 0.1),
        " of the variation in the selected outcome. This is useful for explanation inside the app, but it is still not causal evidence."
      ))
    )
  })

  output$model_table <- renderTable({
    result <- model_result()
    coefs <- result$summary$coefficients
    raw_terms <- rownames(coefs)
    data.frame(
      Term = unname(ifelse(raw_terms %in% names(term_labels), term_labels[raw_terms], raw_terms)),
      Estimate = number(coefs[, "Estimate"], accuracy = 0.0001),
      `Std. Error` = number(coefs[, "Std. Error"], accuracy = 0.0001),
      `p-value` = ifelse(coefs[, "Pr(>|t|)"] < 0.001, "<0.001", number(coefs[, "Pr(>|t|)"], accuracy = 0.0001)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$distribution_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 2)
    plot_df <- df |>
      mutate(metric_value = .data[[input$metric]])
    plot_ly(
      plot_df,
      x = ~metric_value,
      color = ~depression_risk,
      colors = risk_palette,
      type = "histogram",
      nbinsx = 24,
      opacity = 0.72
    ) |>
      layout(
        barmode = "overlay",
        xaxis = list(title = metric_name()),
        yaxis = list(title = "Teens"),
        legend = list(title = list(text = "Depression risk"), orientation = "h", x = 0, y = -0.22),
        margin = standard_margin
      )
  })

  output$data_preview <- renderDT({
    app_data |>
      select(
        age,
        gender = gender_label,
        platform_usage,
        usage_band,
        daily_social_media_hours,
        sleep_hours,
        screen_time_before_sleep,
        academic_performance,
        physical_activity,
        social_interaction_level,
        stress_level,
        anxiety_level,
        depression_risk
      ) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      ) |>
      formatRound(
        c(
          "daily_social_media_hours",
          "sleep_hours",
          "screen_time_before_sleep",
          "academic_performance",
          "physical_activity"
        ),
        2
      )
  })
}

shinyApp(ui, server)
