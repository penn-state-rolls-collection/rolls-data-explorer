library(shiny)
library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(DT)

# -----------------------------
# Load and prepare Sociofeed data
# -----------------------------

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

wholevparts <- read_csv(
  "data/study-wholevparts_date-1988_data.csv",
  show_col_types = FALSE
)

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

# -----------------------------
# Colorblind-friendly palette
# -----------------------------

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

# -----------------------------
# Helper functions
# -----------------------------

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


# This lookup is written so additional datasets can be added later.
study_lookup <- list(
  "Sociofeed 1990" = sociofeed,
  "Wholes vs Parts 1988" = wholevparts
)

all_dataset_variables <- sort(unique(unlist(
  lapply(study_lookup, names),
  use.names = FALSE
)))

# -----------------------------
# User interface
# -----------------------------

app_css <- "
body {
  background: #ffffff;
}

.navbar {
  background-color: #eaf4fb !important;
  border-bottom: 1px solid #b8d6ea !important;
  min-height: 64px;
}

.navbar .navbar-brand {
  color: #005b8f !important;
  font-size: 22px !important;
  font-weight: 700 !important;
  padding-top: 18px !important;
  padding-bottom: 18px !important;
}

.navbar .nav-link {
  background-color: #ffffff !important;
  color: #005b8f !important;
  font-size: 17px !important;
  font-weight: 700 !important;
  margin: 8px 4px !important;
  padding: 12px 16px !important;
  border: 1px solid #b8d6ea !important;
  border-radius: 8px !important;
}

.navbar .nav-link:hover,
.navbar .nav-link:focus,
.navbar .nav-item.active .nav-link,
.navbar .nav-link.active {
  background-color: #ffffff !important;
  color: #005b8f !important;
  border-color: #005b8f !important;
  box-shadow: none !important;
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

.section-title {
  color: #005b8f;
  font-weight: 700;
  margin-bottom: 16px;
}
"

ui <- navbarPage(
  title = "Rolls Collection",
  id = "main_navigation",
  header = tags$head(tags$style(HTML(app_css))),
  
  tabPanel(
    "Home",
    fluidPage(
      div(style = "height: 35px;")
    )
  ),
  
  tabPanel(
    "Dataset Explorer",
    fluidPage(
      fluidRow(
        column(
          width = 6,
          h3(class = "section-title", "Variables by dataset"),
          wellPanel(
            selectInput(
              inputId = "directory_dataset",
              label = "Choose a dataset:",
              choices = names(study_lookup),
              selected = names(study_lookup)[1]
            )
          ),
          DTOutput("variables_for_dataset")
        ),
        column(
          width = 6,
          h3(class = "section-title", "Datasets by variable"),
          wellPanel(
            selectizeInput(
              inputId = "directory_variable",
              label = "Choose a variable:",
              choices = all_dataset_variables,
              selected = all_dataset_variables[1],
              multiple = FALSE,
              options = list(placeholder = "Search for a variable")
            )
          ),
          DTOutput("datasets_for_variable")
        )
      )
    )
  ),
  
  tabPanel(
    "Variable Explorer",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "explorer_dataset",
            label = "Choose a dataset:",
            choices = names(study_lookup),
            selected = names(study_lookup)[1]
          ),
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
  
  selected_explorer_data <- reactive({
    req(input$explorer_dataset)
    study_lookup[[input$explorer_dataset]]
  })
  
  observeEvent(input$explorer_dataset, {
    dat <- selected_explorer_data()
    numeric_choices <- names(dat)[vapply(dat, is.numeric, logical(1))]
    
    updateSelectInput(
      session,
      "selected_variable",
      choices = numeric_choices,
      selected = if (length(numeric_choices) > 0) numeric_choices[1] else character(0)
    )
  }, ignoreInit = FALSE)
  
  output$variables_for_dataset <- renderDT({
    req(input$directory_dataset)
    
    dat <- study_lookup[[input$directory_dataset]]
    
    variable_table <- tibble(
      Variable = names(dat),
      Type = vapply(dat, function(x) class(x)[1], character(1))
    )
    
    datatable(
      variable_table,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  output$datasets_for_variable <- renderDT({
    req(input$directory_variable)
    
    matching_datasets <- names(study_lookup)[
      vapply(
        study_lookup,
        function(dat) input$directory_variable %in% names(dat),
        logical(1)
      )
    ]
    
    result <- tibble(
      Variable = input$directory_variable,
      Dataset = matching_datasets
    )
    
    datatable(
      result,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE)
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
