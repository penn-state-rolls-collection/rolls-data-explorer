library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(DT)

data_path <- "data/study-sociofeed_date-1990_data.csv"

if (!file.exists(data_path)) {
  stop(
    paste0(
      "The Sociofeed CSV was not found.\n\n",
      "Place it here:\n",
      "app/data/study-sociofeed_date-1990_data.csv"
    )
  )
}

sociofeed <- read_csv(data_path, show_col_types = FALSE)

required_columns <- c("id", "cond")
missing_required <- setdiff(required_columns, names(sociofeed))

if (length(missing_required) > 0) {
  stop(
    paste(
      "The Sociofeed file is missing these required columns:",
      paste(missing_required, collapse = ", ")
    )
  )
}

sociofeed <- sociofeed %>%
  mutate(
    across(
      -any_of(c("id", "cond")),
      ~ suppressWarnings(as.numeric(.x))
    ),
    cond_label = case_when(
      cond == "soc" ~ "Social eating",
      cond == "ind" ~ "Individual eating",
      TRUE ~ as.character(cond)
    ),
    sex_label = case_when(
      sex == 1 ~ "Sex 1",
      sex == 2 ~ "Sex 2",
      TRUE ~ as.character(sex)
    )
  )

blue <- "#0072B2"
orange <- "#E69F00"
sky_blue <- "#56B4E9"
bluish_green <- "#009E73"

condition_palette <- c(
  "Social eating" = blue,
  "Individual eating" = orange
)

plot_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.title = element_blank()
  )

safe_stat <- function(x, fun) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  fun(x, na.rm = TRUE)
}

summary_by_variable <- function(dat, selected_vars) {
  selected_vars <- selected_vars[selected_vars %in% names(dat)]
  
  map_dfr(selected_vars, function(v) {
    dat %>%
      group_by(cond_label) %>%
      summarise(
        Variable = v,
        n = sum(!is.na(.data[[v]])),
        Mean = round(safe_stat(.data[[v]], mean), 2),
        Median = round(safe_stat(.data[[v]], median), 2),
        SD = round(safe_stat(.data[[v]], sd), 2),
        Min = round(safe_stat(.data[[v]], min), 2),
        Max = round(safe_stat(.data[[v]], max), 2),
        Missing = sum(is.na(.data[[v]])),
        .groups = "drop"
      ) %>%
      relocate(Variable, cond_label)
  })
}

make_violin_plot <- function(dat, variable, title, x_label) {
  plot_data <- dat %>%
    transmute(
      cond_label,
      Value = suppressWarnings(as.numeric(.data[[variable]]))
    ) %>%
    filter(!is.na(Value), !is.na(cond_label))
  
  validate(need(nrow(plot_data) > 0, "No usable numeric values are available."))
  
  ggplot(
    plot_data,
    aes(x = Value, y = cond_label, fill = cond_label)
  ) +
    geom_violin(
      trim = FALSE,
      alpha = 0.45,
      color = "gray35",
      orientation = "y"
    ) +
    geom_point(
      position = position_jitter(width = 0, height = 0.12),
      size = 2,
      alpha = 0.70
    ) +
    scale_fill_manual(values = condition_palette) +
    labs(
      title = title,
      subtitle = "Each point represents a participant observation.",
      x = x_label,
      y = NULL
    ) +
    plot_theme +
    theme(legend.position = "none")
}

sociofeed_prepost <- tibble(
  Measure = c(
    "Alert",
    "Hunger",
    "Anxiety",
    "Fullness",
    "Relaxed",
    "Thirst",
    "Tense",
    "Prospective consumption",
    "Sleepy",
    "Nauseous"
  ),
  Pre = c(
    "alert_pre",
    "hunger_pre",
    "anxiety_pre",
    "fullness_pre",
    "relaxed_pre",
    "thirst_pre",
    "tense_pre",
    "prosp_cons_pre",
    "sleepy_pre",
    "nauseous_pre"
  ),
  Post = c(
    "alert_post",
    "hunger_post",
    "anxiety_post",
    "fullness_post",
    "relaxed_post",
    "thirst_post",
    "tense_post",
    "prosp_cons_post",
    "sleepy_post",
    "nauseous_post"
  )
) %>%
  filter(Pre %in% names(sociofeed), Post %in% names(sociofeed))

make_prepost_long <- function(dat, lookup) {
  if (nrow(lookup) == 0) {
    return(tibble())
  }
  
  map_dfr(seq_len(nrow(lookup)), function(i) {
    dat %>%
      transmute(
        id,
        cond_label,
        Measure = lookup$Measure[i],
        Pre = .data[[lookup$Pre[i]]],
        Post = .data[[lookup$Post[i]]],
        Change = Post - Pre
      )
  })
}

sociofeed_change_long <- make_prepost_long(
  sociofeed,
  sociofeed_prepost
)

sociofeed_eating_vars <- c(
  "total_cal" = "Total calories",
  "dsrt_cal" = "Dessert calories",
  "time" = "Meal time",
  "dsrt_perc_din" = "Dessert percent of dinner",
  "total_g" = "Total grams"
)

sociofeed_eating_vars <- sociofeed_eating_vars[
  names(sociofeed_eating_vars) %in% names(sociofeed)
]

nutrition_choices <- c(
  "Total Calories" = "total_cal",
  "Dessert Calories" = "dsrt_cal",
  "Protein Calories" = "total_pro_cal",
  "Carbohydrate Calories" = "total_cho_cal",
  "Fat Calories" = "total_fat_cal",
  "Protein Percent" = "pro_perc_cal",
  "Carbohydrate Percent" = "cho_perc_cal",
  "Fat Percent" = "fat_perc_cal",
  "Total Fat" = "total_fat",
  "Total Carbohydrates" = "total_cho",
  "Total Protein" = "total_pro"
)

nutrition_choices <- nutrition_choices[
  nutrition_choices %in% names(sociofeed)
]

food_items <- c(
  "spaghetti",
  "meat_sauce",
  "veg_sauce",
  "lettuce",
  "tomato",
  "cucumber",
  "lowcal_dressing",
  "dressing",
  "roll",
  "margarine",
  "cookie",
  "sorbet",
  "icecream"
)

food_choices <- setNames(
  food_items[food_items %in% names(sociofeed)],
  str_to_title(
    str_replace_all(
      food_items[food_items %in% names(sociofeed)],
      "_",
      " "
    )
  )
)

food_metric_suffixes <- c(
  "Amount" = "",
  "Calories" = "_cal",
  "Fat" = "_fat",
  "Carbohydrates" = "_cho",
  "Protein" = "_pro"
)

numeric_variables <- names(sociofeed)[
  vapply(sociofeed, is.numeric, logical(1)) &
    !names(sociofeed) %in% c("id")
]

app_css <- "
body {
  background: #ffffff;
}

.navbar {
  background-color: #0072B2 !important;
}

.navbar .navbar-brand,
.navbar .nav-link {
  color: #ffffff !important;
}

.well,
.card-like {
  background: #ffffff;
  border: 1px solid #d6e4ef;
  border-radius: 14px;
  padding: 18px;
}

a:focus,
button:focus,
input:focus,
select:focus {
  outline: 3px solid #0072B2 !important;
  outline-offset: 2px;
}

.dashboard-intro {
  border: 1px solid #d6e4ef;
  border-radius: 14px;
  padding: 20px;
  margin-bottom: 18px;
  background: #ffffff;
}

.dashboard-intro h2 {
  color: #0072B2;
  margin-top: 0;
}
"

ui <- navbarPage(
  title = "Rolls Collection",
  id = "main_navigation",
  header = tags$head(tags$style(HTML(app_css))),
  
  tabPanel(
    "Home",
    fluidPage(
      div(
        class = "dashboard-intro",
        h2("Sociofeed 1990 Interactive Dashboard"),
        p(
          "Explore study structure, participant ratings, food intake, ",
          "nutrition variables, and data quality."
        )
      ),
      
      fluidRow(
        column(
          width = 4,
          wellPanel(
            h4("File loaded"),
            p(strong(basename(data_path))),
            p(paste("Rows:", nrow(sociofeed))),
            p(paste("Columns:", ncol(sociofeed)))
          )
        ),
        
        column(
          width = 4,
          wellPanel(
            h4("Participants"),
            p(paste("Unique participant IDs:", n_distinct(sociofeed$id))),
            p(paste("Conditions:", paste(unique(sociofeed$cond_label), collapse = ", ")))
          )
        ),
        
        column(
          width = 4,
          wellPanel(
            h4("How to use"),
            p("Use the tabs above to choose variables, compare conditions, and review missing data.")
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Dataset Explorer",
    fluidPage(
      fluidRow(
        column(
          width = 4,
          h3("Dataset overview"),
          DTOutput("dataset_overview")
        ),
        column(
          width = 8,
          h3("Condition counts"),
          DTOutput("condition_counts")
        )
      ),
      hr(),
      h3("Variable directory"),
      DTOutput("variable_directory")
    )
  ),
  
  tabPanel(
    "Variable Explorer",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_variable",
            label = "Choose a numeric variable:",
            choices = numeric_variables,
            selected = numeric_variables[1]
          )
        ),
        mainPanel(
          plotOutput("selected_variable_plot", height = "520px"),
          h3("Summary by eating condition"),
          DTOutput("selected_variable_summary")
        )
      )
    )
  ),
  
  tabPanel(
    "Pre/Post Explorer",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "prepost_measure",
            label = "Choose a pre/post rating:",
            choices = sociofeed_prepost$Measure,
            selected = if (nrow(sociofeed_prepost) > 0) {
              sociofeed_prepost$Measure[1]
            } else {
              character(0)
            }
          )
        ),
        mainPanel(
          plotOutput("prepost_plot", height = "520px"),
          h3("Average change by eating condition"),
          DTOutput("prepost_summary")
        )
      )
    )
  ),
  
  tabPanel(
    "Eating Measures",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "eating_variable",
            label = "Choose an eating measure:",
            choices = sociofeed_eating_vars,
            selected = if (length(sociofeed_eating_vars) > 0) {
              names(sociofeed_eating_vars)[1]
            } else {
              character(0)
            }
          )
        ),
        mainPanel(
          plotOutput("eating_plot", height = "520px"),
          h3("Summary by eating condition"),
          DTOutput("eating_summary")
        )
      )
    )
  ),
  
  tabPanel(
    "Nutrition Explorer",
    fluidPage(
      fluidRow(
        column(
          width = 6,
          wellPanel(
            selectizeInput(
              inputId = "nutrition_variables",
              label = "Choose one or more nutrition variables:",
              choices = nutrition_choices,
              selected = head(unname(nutrition_choices), 3),
              multiple = TRUE
            )
          ),
          DTOutput("nutrition_summary")
        ),
        column(
          width = 6,
          wellPanel(
            selectInput(
              inputId = "selected_food",
              label = "Choose a food:",
              choices = food_choices,
              selected = if (length(food_choices) > 0) {
                unname(food_choices)[1]
              } else {
                character(0)
              }
            )
          ),
          DTOutput("food_summary")
        )
      )
    )
  ),
  
  tabPanel(
    "Data Quality",
    fluidPage(
      h3("Missingness and descriptive statistics"),
      DTOutput("data_quality")
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  
  output$dataset_overview <- renderDT({
    overview <- tibble(
      Metric = c(
        "Rows",
        "Columns",
        "Unique participants",
        "Numeric variables",
        "Variables with missing values"
      ),
      Value = c(
        nrow(sociofeed),
        ncol(sociofeed),
        n_distinct(sociofeed$id),
        sum(vapply(sociofeed, is.numeric, logical(1))),
        sum(vapply(sociofeed, function(x) any(is.na(x)), logical(1)))
      )
    )
    
    datatable(
      overview,
      rownames = FALSE,
      options = list(dom = "t")
    )
  })
  
  output$condition_counts <- renderDT({
    sociofeed %>%
      count(cond_label, name = "Participants") %>%
      datatable(
        rownames = FALSE,
        options = list(dom = "t")
      )
  })
  
  output$variable_directory <- renderDT({
    variable_directory <- tibble(
      Variable = names(sociofeed),
      Type = vapply(sociofeed, function(x) class(x)[1], character(1)),
      Missing = vapply(sociofeed, function(x) sum(is.na(x)), numeric(1)),
      Percent_Missing = round(
        vapply(sociofeed, function(x) sum(is.na(x)), numeric(1)) /
          nrow(sociofeed) * 100,
        1
      )
    )
    
    datatable(
      variable_directory,
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 12, scrollX = TRUE)
    )
  })
  
  output$selected_variable_plot <- renderPlot({
    req(input$selected_variable)
    
    make_violin_plot(
      sociofeed,
      input$selected_variable,
      paste(input$selected_variable, "by eating condition"),
      input$selected_variable
    )
  })
  
  output$selected_variable_summary <- renderDT({
    req(input$selected_variable)
    
    datatable(
      summary_by_variable(sociofeed, input$selected_variable),
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  
  output$prepost_plot <- renderPlot({
    req(input$prepost_measure)
    validate(
      need(
        nrow(sociofeed_change_long) > 0,
        "No matching pre/post columns were found in the CSV."
      )
    )
    
    plot_data <- sociofeed_change_long %>%
      filter(Measure == input$prepost_measure, !is.na(Change))
    
    validate(
      need(
        nrow(plot_data) > 0,
        "No usable values are available for this pre/post measure."
      )
    )
    
    ggplot(
      plot_data,
      aes(x = Change, y = cond_label, fill = cond_label)
    ) +
      geom_violin(
        trim = FALSE,
        alpha = 0.45,
        color = "gray35",
        orientation = "y"
      ) +
      geom_point(
        position = position_jitter(width = 0, height = 0.12),
        size = 2,
        alpha = 0.70
      ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed"
      ) +
      scale_fill_manual(values = condition_palette) +
      coord_cartesian(xlim = c(-100, 100)) +
      labs(
        title = paste(input$prepost_measure, "post minus pre change"),
        subtitle = "Positive values mean the post-rating was higher than the pre-rating.",
        x = "Post - pre change",
        y = NULL
      ) +
      plot_theme +
      theme(legend.position = "none")
  })
  
  output$prepost_summary <- renderDT({
    req(input$prepost_measure)
    
    summary_table <- sociofeed_change_long %>%
      filter(Measure == input$prepost_measure) %>%
      group_by(cond_label) %>%
      summarise(
        n = sum(!is.na(Change)),
        Mean_Change = round(safe_stat(Change, mean), 2),
        Median_Change = round(safe_stat(Change, median), 2),
        SD_Change = round(safe_stat(Change, sd), 2),
        .groups = "drop"
      )
    
    datatable(
      summary_table,
      rownames = FALSE,
      options = list(dom = "t")
    )
  })
  
  output$eating_plot <- renderPlot({
    req(input$eating_variable)
    
    label <- unname(
      sociofeed_eating_vars[input$eating_variable]
    )
    
    make_violin_plot(
      sociofeed,
      input$eating_variable,
      paste(label, "by eating condition"),
      label
    )
  })
  
  output$eating_summary <- renderDT({
    req(input$eating_variable)
    
    datatable(
      summary_by_variable(sociofeed, input$eating_variable),
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
    )
  })
  
  output$nutrition_summary <- renderDT({
    req(input$nutrition_variables)
    
    datatable(
      summary_by_variable(sociofeed, input$nutrition_variables),
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$food_summary <- renderDT({
    req(input$selected_food)
    
    selected_food <- input$selected_food
    metric_variables <- paste0(
      selected_food,
      unname(food_metric_suffixes)
    )
    
    metric_variables <- metric_variables[
      metric_variables %in% names(sociofeed)
    ]
    
    validate(
      need(
        length(metric_variables) > 0,
        "No matching food variables were found."
      )
    )
    
    food_summary <- map_dfr(metric_variables, function(variable) {
      suffix_position <- match(
        variable,
        paste0(selected_food, unname(food_metric_suffixes))
      )
      
      metric_name <- names(food_metric_suffixes)[suffix_position]
      
      sociofeed %>%
        group_by(cond_label) %>%
        summarise(
          Food = str_to_title(str_replace_all(selected_food, "_", " ")),
          Metric = metric_name,
          Variable = variable,
          n = sum(!is.na(.data[[variable]])),
          Mean = round(safe_stat(.data[[variable]], mean), 2),
          Median = round(safe_stat(.data[[variable]], median), 2),
          SD = round(safe_stat(.data[[variable]], sd), 2),
          Missing = sum(is.na(.data[[variable]])),
          .groups = "drop"
        )
    })
    
    datatable(
      food_summary,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$data_quality <- renderDT({
    missing_values <- vapply(
      sociofeed,
      function(x) sum(is.na(x)),
      numeric(1)
    )
    
    quality_table <- tibble(
      Variable = names(sociofeed),
      Type = vapply(sociofeed, function(x) class(x)[1], character(1)),
      Missing = missing_values,
      Percent_Missing = round(missing_values / nrow(sociofeed) * 100, 1),
      Nonmissing = nrow(sociofeed) - missing_values,
      Mean = vapply(
        sociofeed,
        function(x) {
          if (is.numeric(x)) round(safe_stat(x, mean), 2) else NA_real_
        },
        numeric(1)
      ),
      SD = vapply(
        sociofeed,
        function(x) {
          if (is.numeric(x)) round(safe_stat(x, sd), 2) else NA_real_
        },
        numeric(1)
      ),
      Min = vapply(
        sociofeed,
        function(x) {
          if (is.numeric(x)) round(safe_stat(x, min), 2) else NA_real_
        },
        numeric(1)
      ),
      Max = vapply(
        sociofeed,
        function(x) {
          if (is.numeric(x)) round(safe_stat(x, max), 2) else NA_real_
        },
        numeric(1)
      )
    ) %>%
      arrange(desc(Percent_Missing), Variable)
    
    datatable(
      quality_table,
      filter = "top",
      rownames = FALSE,
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shinyApp(ui = ui, server = server)