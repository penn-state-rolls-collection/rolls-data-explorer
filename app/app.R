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

wholevparts <- read_csv(
  "data/study-wholevparts_date-1988_data.csv",
  show_col_types = FALSE
)

# Study-level overview file used by the Study Overview tab.
overview_path <- "data/rolls_collection_overview(Overview).csv"

if (!file.exists(overview_path)) {
  stop(
    paste0(
      "The Rolls Collection overview CSV was not found.\n\n",
      "Place it here:\n",
      "app/data/rolls_collection_overview(Overview).csv"
    )
  )
}

overview <- read_csv(
  overview_path,
  locale = locale(encoding = "Windows-1252"),
  show_col_types = FALSE,
  na = c("", "NA")
)

overview_original_columns <- names(overview)

overview_required <- c(
  "study", "year", "location", "sample_age", "sample_sex",
  "dataset", "study_n", "sub_n", "pub_doi"
)

overview_missing <- setdiff(overview_required, names(overview))

if (length(overview_missing) > 0) {
  stop(
    paste(
      "The overview file is missing these required columns:",
      paste(overview_missing, collapse = ", ")
    )
  )
}

# Variables marked 1 are treated as present. Blank cells, 0, 2, and text such
# as "missing" do not count as a positive occurrence in the filters.
positive_flag <- function(x) {
  numeric_x <- suppressWarnings(as.numeric(as.character(x)))
  !is.na(numeric_x) & numeric_x == 1
}

# Standardize condition-group naming across curated datasets.
# New curated files should use cond_label. Older files with cond still work.
standardize_cond_label <- function(dat) {
  if (!"cond_label" %in% names(dat) && "cond" %in% names(dat)) {
    dat <- dat %>% rename(cond_label = cond)
  }
  
  if ("cond_label" %in% names(dat)) {
    dat <- dat %>%
      mutate(cond_label = as.character(cond_label))
  }
  
  dat
}

format_doi_links <- function(x, study_name) {
  if (is.na(x) || !nzchar(str_trim(as.character(x)))) {
    return("")
  }
  
  doi_values <- str_split(as.character(x), "\\s*;\\s*", simplify = FALSE)[[1]]
  doi_values <- str_trim(doi_values)
  doi_values <- doi_values[nzchar(doi_values)]
  
  study_word <- str_split(
    str_trim(str_to_title(str_replace_all(as.character(study_name), "_", " "))),
    "\\s+",
    simplify = FALSE
  )[[1]][1]
  
  links <- vapply(
    seq_along(doi_values),
    function(i) {
      doi_value <- doi_values[i]
      link_url <- doi_value
      
      if (!str_detect(link_url, regex("^https?://", ignore_case = TRUE))) {
        link_url <- paste0(
          "https://doi.org/",
          str_remove(link_url, regex("^doi:\\s*", ignore_case = TRUE))
        )
      }
      
      link_text <- if (length(doi_values) > 1) {
        paste(study_word, "DOI", i)
      } else {
        paste(study_word, "DOI")
      }
      
      paste0(
        '<a href="',
        htmltools::htmlEscape(link_url, attribute = TRUE),
        '" target="_blank" rel="noopener noreferrer">',
        htmltools::htmlEscape(link_text),
        '</a>'
      )
    },
    character(1)
  )
  
  paste(links, collapse = "<br>")
}

numeric_like_variables <- function(dat) {
  names(dat)[vapply(
    dat,
    function(x) {
      if (is.numeric(x)) {
        return(TRUE)
      }
      
      x_chr <- as.character(x)
      nonmissing <- !is.na(x_chr) & nzchar(str_trim(x_chr))
      
      if (sum(nonmissing) == 0) {
        return(FALSE)
      }
      
      converted <- suppressWarnings(as.numeric(x_chr[nonmissing]))
      mean(!is.na(converted)) >= 0.95
    },
    logical(1)
  )]
}

numeric_values <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  suppressWarnings(as.numeric(as.character(x)))
}

study_method_choices <- c(
  "Sensory-specific satiety" = "sss",
  "Preload before measured intake" = "preload_used",
  "Energy-density manipulation" = "ed_manipulation",
  "Portion-size manipulation" = "ps_manipulation",
  "Volume manipulation" = "volume_manipulation",
  "Fat-content manipulation" = "fat_manipulation",
  "Food-form manipulation" = "food_form",
  "Microstructure / coded videos" = "microstructure",
  "Eating-disorder sample" = "eating_disorder",
  "Weight-loss sample or intervention" = "weight_loss",
  "Obesity sample" = "obesity",
  "Social-context manipulation" = "social_context"
)

study_method_choices <- study_method_choices[
  study_method_choices %in% names(overview)
]

demographic_choices <- c(
  "Age" = "age",
  "Sex" = "sex",
  "Race" = "race",
  "Socioeconomic status" = "ses",
  "BMI" = "bmi"
)

demographic_choices <- demographic_choices[
  demographic_choices %in% names(overview)
]

intake_measure_choices <- c(
  "Recall intake" = "recall_intake",
  "Measured intake" = "measured_intake",
  "Pre/post-meal questions" = "pre_post_meal_q",
  "Food preference" = "food_preference",
  "Eating duration" = "eat_duration",
  "Substance use" = "substance_use",
  "Current-status questions" = "current_status_q"
)

intake_measure_choices <- intake_measure_choices[
  intake_measure_choices %in% names(overview)
]

nutrient_columns <- intersect(
  c("cho_intake", "fat_intake", "pro_intake", "fiber_intake"),
  names(overview)
)

questionnaire_choices <- c(
  "Zung" = "zung",
  "EAT" = "eat",
  "EDI" = "edi",
  "EI" = "ei",
  "Beck" = "beck",
  "BSQ" = "bsq",
  "QEWP-R" = "qewp-r",
  "DEBQ" = "debq",
  "PFS" = "pfs",
  "CFQ" = "cfq",
  "UPSIT" = "upsit"
)

questionnaire_choices <- questionnaire_choices[
  questionnaire_choices %in% names(overview)
]

overview <- overview %>%
  filter(!is.na(study) & nzchar(str_trim(as.character(study)))) %>%
  mutate(
    study = as.character(study),
    dataset = as.character(dataset),
    study_display = str_to_title(str_replace_all(study, "_", " ")),
    dataset_display = if_else(
      is.na(dataset) | !nzchar(str_trim(dataset)),
      "",
      str_to_title(str_replace_all(dataset, "_", " "))
    )
  ) %>%
  add_count(study, name = "rows_in_study") %>%
  mutate(
    sample_size = if_else(
      rows_in_study > 1 & !is.na(suppressWarnings(as.numeric(sub_n))),
      suppressWarnings(as.numeric(sub_n)),
      suppressWarnings(as.numeric(study_n))
    )
  )

location_choices <- sort(unique(na.omit(as.character(overview$location))))
year_choices <- sort(unique(na.omit(overview$year)))
age_group_choices <- sort(unique(na.omit(as.character(overview$sample_age))))
sex_composition_choices <- sort(unique(na.omit(as.character(overview$sample_sex))))

# Standardize the condition column across datasets. Curated files should
# ultimately use cond_label directly; older files using cond are supported.
sociofeed <- standardize_cond_label(sociofeed)
wholevparts <- standardize_cond_label(wholevparts)

required_columns <- c("id", "cond_label")
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
      -any_of(c("id", "cond_label")),
      ~ suppressWarnings(as.numeric(.x))
    ),
    cond_label = case_when(
      cond_label == "soc" ~ "Social eating",
      cond_label == "ind" ~ "Individual eating",
      TRUE ~ as.character(cond_label)
    ),
    sex_label = case_when(
      sex == 1 ~ "Sex 1",
      sex == 2 ~ "Sex 2",
      TRUE ~ as.character(sex)
    )
  )

# Keep Wholes vs Parts condition values as they appear in the curated data
# (currently a and b), but ensure the grouping variable is cond_label.
if ("cond_label" %in% names(wholevparts)) {
  wholevparts <- wholevparts %>%
    mutate(cond_label = as.character(cond_label))
}

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

numeric_variables <- setdiff(
  numeric_like_variables(sociofeed),
  "id"
)


# This lookup is written so additional datasets can be added later.
study_lookup <- list(
  "Sociofeed 1990" = sociofeed,
  "Wholes vs Parts 1988" = wholevparts
)

# Variable group definitions used by the Data Quality tab.
# These rules are intentionally name-based so the same workflow can extend
# to additional curated datasets that follow consistent variable naming.
data_quality_variable_groups <- function(dat) {
  vars <- names(dat)
  analysis_vars <- setdiff(vars, c("id", "cond_label", "session"))
  
  intake_pattern <- regex(
    "intake|kcal|calorie|(^|_)(fat|cho|carb|carbohydrate|pro|protein|fiber)($|_)|_g$|sandwich|sandwhich|water|spaghetti|sauce|lettuce|tomato|cucumber|dressing|roll|margarine|cookie|sorbet|icecream|dessert",
    ignore_case = TRUE
  )
  
  questionnaire_pattern <- regex(
    "(_pre$|_post$|_screen$)|hunger|thirst|fullness|desire|much_eat|alert|anxiety|relaxed|tense|sleepy|nauseous|prosp_cons|taste|zung|(^|_)eat($|_)|edi|(^|_)ei($|_)|beck|bsq|qewp|debq|pfs|cfq|upsit|question",
    ignore_case = TRUE
  )
  
  intake_vars <- analysis_vars[str_detect(analysis_vars, intake_pattern)]
  questionnaire_vars <- analysis_vars[str_detect(analysis_vars, questionnaire_pattern)]
  
  total_intake_vars <- intake_vars[
    str_detect(intake_vars, regex("(^|_)total($|_)|^total_", ignore_case = TRUE))
  ]
  
  list(
    "All data" = analysis_vars,
    "Intake data" = intake_vars,
    "Questionnaire data" = questionnaire_vars,
    "Total intake variables" = total_intake_vars
  )
}

intake_macros_computed <- function(dat) {
  vars <- names(dat)
  
  has_carb <- any(str_detect(
    vars,
    regex("(^|_)(cho|carb|carbohydrate)($|_)", ignore_case = TRUE)
  ))
  has_fat <- any(str_detect(
    vars,
    regex("(^|_)fat($|_)", ignore_case = TRUE)
  ))
  has_protein <- any(str_detect(
    vars,
    regex("(^|_)(pro|protein)($|_)", ignore_case = TRUE)
  ))
  
  if (has_carb && has_fat && has_protein) "Yes" else "No"
}

summarize_data_quality_group <- function(dat, study_name, category, vars) {
  vars <- intersect(vars, names(dat))
  
  if (length(vars) == 0) {
    return(tibble(
      Study = study_name,
      `Data group` = category,
      `Total missing` = 0L,
      `% missing overall` = NA_real_,
      `Mean % missing by participant` = NA_real_,
      `Range of % missing by participant` = "N/A",
      `# complete cases` = NA_integer_,
      `# participants >=85% complete` = NA_integer_,
      `Intake macros computed` = intake_macros_computed(dat)
    ))
  }
  
  data_matrix <- dat[, vars, drop = FALSE]
  total_cells <- nrow(data_matrix) * length(vars)
  total_missing <- sum(is.na(data_matrix))
  overall_missing_pct <- if (total_cells > 0) {
    100 * total_missing / total_cells
  } else {
    NA_real_
  }
  
  if ("id" %in% names(dat)) {
    participant_id <- as.character(dat$id)
  } else {
    participant_id <- as.character(seq_len(nrow(dat)))
  }
  
  participant_missing <- tibble(
    participant_id = participant_id,
    row_missing = rowSums(is.na(data_matrix)),
    row_cells = length(vars)
  ) %>%
    group_by(participant_id) %>%
    summarise(
      missing_cells = sum(row_missing),
      total_cells = sum(row_cells),
      .groups = "drop"
    ) %>%
    mutate(
      percent_missing = 100 * missing_cells / total_cells,
      percent_complete = 100 - percent_missing
    )
  
  range_missing <- range(participant_missing$percent_missing, na.rm = TRUE)
  
  tibble(
    Study = study_name,
    `Data group` = category,
    `Total missing` = total_missing,
    `% missing overall` = round(overall_missing_pct, 1),
    `Mean % missing by participant` = round(mean(participant_missing$percent_missing, na.rm = TRUE), 1),
    `Range of % missing by participant` = paste0(
      round(range_missing[1], 1), "% - ", round(range_missing[2], 1), "%"
    ),
    `# complete cases` = sum(participant_missing$percent_missing == 0, na.rm = TRUE),
    `# participants >=85% complete` = sum(participant_missing$percent_complete >= 85, na.rm = TRUE),
    `Intake macros computed` = intake_macros_computed(dat)
  )
}

build_data_quality_table <- function(study_lookup) {
  map_dfr(
    names(study_lookup),
    function(study_name) {
      dat <- study_lookup[[study_name]]
      groups <- data_quality_variable_groups(dat)
      
      map_dfr(
        names(groups),
        function(category) {
          summarize_data_quality_group(
            dat = dat,
            study_name = study_name,
            category = category,
            vars = groups[[category]]
          )
        }
      )
    }
  )
}

all_dataset_variables <- sort(unique(unlist(
  lapply(study_lookup, names),
  use.names = FALSE
)))

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

.home-hero {
  max-width: 1050px;
  margin: 35px auto 0 auto;
}

.home-photo {
  width: 100%;
  max-width: 310px;
  height: auto;
  border-radius: 12px;
  border: 1px solid #d6e4ef;
  display: block;
  margin: 0 auto 24px auto;
}

.home-copy {
  font-size: 17px;
  line-height: 1.65;
}

.home-link {
  display: inline-block;
  margin-top: 10px;
  font-weight: 700;
}

.home-contact {
  margin-top: 35px;
  padding-top: 20px;
  border-top: 1px solid #d6e4ef;
  font-size: 15px;
  line-height: 1.6;
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
        class = "home-hero",
        fluidRow(
          column(
            width = 4,
            tags$img(
              src = "Barbara.jpg",
              alt = "Barbara J. Rolls",
              class = "home-photo"
            )
          ),
          column(
            width = 8,
            h1(class = "section-title", "Rolls Collection"),
            div(
              class = "home-copy",
              p(
                "The Rolls Collection brings together curated data and documentation ",
                "from studies led by Barbara J. Rolls and the Laboratory for the Study ",
                "of Human Ingestive Behavior. The collection includes information on ",
                "study design, participants, questionnaires, eating behavior, and food intake."
              ),
              p(
                "Use the Study Overview tab to find studies with particular characteristics. ",
                "The other tabs provide more detailed ways to explore the datasets currently ",
                "included in the dashboard."
              ),
              tags$a(
                href = "https://scholarsphere.psu.edu/resources/52cfbdb6-d420-4a5c-a85a-4e5aa099e519",
                target = "_blank",
                rel = "noopener noreferrer",
                class = "home-link",
                "View the Rolls Collection in ScholarSphere"
              )
            )
          )
        ),
        div(
          class = "home-contact",
          p(
            tags$strong("Questions about the collection: "),
            tags$a(
              href = "mailto:researchdatastewardshipprogram@PennStateOffice365.onmicrosoft.com",
              "researchdatastewardshipprogram@PennStateOffice365.onmicrosoft.com"
            )
          ),
          p(
            tags$strong("Dashboard curated and maintained by Alaina Pearce: "),
            tags$a(
              href = "mailto:azp271@psu.edu",
              "azp271@psu.edu"
            )
          ),
          p(
            tags$strong("Barbara J. Rolls, Ph.D.: "),
            tags$a(
              href = "mailto:bjr4@psu.edu",
              "bjr4@psu.edu"
            )
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Study Overview",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Find studies"),
          textInput(
            inputId = "overview_search",
            label = "Search studies:",
            placeholder = "Study name, dataset, location, year, or DOI"
          ),
          checkboxGroupInput(
            inputId = "overview_methods",
            label = "Study methods and sample characteristics:",
            choices = study_method_choices
          ),
          radioButtons(
            inputId = "overview_nutrients",
            label = "Macronutrients assessed:",
            choices = c(
              "Show all" = "all",
              "Yes" = "yes",
              "No" = "no"
            ),
            selected = "all"
          ),
          checkboxGroupInput(
            inputId = "overview_demographics",
            label = "Demographic variables present:",
            choices = demographic_choices
          ),
          checkboxGroupInput(
            inputId = "overview_questionnaires",
            label = "Questionnaires and related measures:",
            choices = questionnaire_choices
          ),
          actionButton(
            inputId = "clear_overview_filters",
            label = "Clear filters",
            class = "btn-default"
          )
        ),
        mainPanel(
          h3(class = "section-title", "Study Overview"),
          DTOutput("study_overview_table")
        )
      )
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
          h3("Summary by condition"),
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
      h3(class = "section-title", "Data Quality by Study"),
      DTOutput("data_quality")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$clear_overview_filters, {
    updateTextInput(session, "overview_search", value = "")
    updateCheckboxGroupInput(session, "overview_methods", selected = character(0))
    updateRadioButtons(session, "overview_nutrients", selected = "all")
    updateCheckboxGroupInput(session, "overview_demographics", selected = character(0))
    updateCheckboxGroupInput(session, "overview_questionnaires", selected = character(0))
  })
  
  overview_filtered <- reactive({
    dat <- overview
    
    search_text <- str_trim(if (is.null(input$overview_search)) "" else input$overview_search)
    if (nzchar(search_text)) {
      searchable_columns <- intersect(
        c("study", "dataset", "citation", "location", "year", "sample_age",
          "sample_sex", "description", "related_publications", "pub_doi"),
        names(dat)
      )
      search_matches <- Reduce(
        `|`,
        lapply(searchable_columns, function(column_name) {
          str_detect(
            coalesce(as.character(dat[[column_name]]), ""),
            regex(search_text, ignore_case = TRUE)
          )
        })
      )
      dat <- dat[search_matches, , drop = FALSE]
    }
    
    selected_characteristics <- c(
      input$overview_methods,
      input$overview_demographics,
      input$overview_questionnaires
    )
    selected_characteristics <- selected_characteristics[
      selected_characteristics %in% names(dat)
    ]
    
    # Retain rows marked 1 for every selected characteristic. Users can select
    # multiple items within a section or combine selections across sections.
    if (length(selected_characteristics) > 0) {
      characteristic_matches <- as.data.frame(
        lapply(dat[selected_characteristics], positive_flag),
        check.names = FALSE
      )
      keep <- rowSums(characteristic_matches, na.rm = TRUE) ==
        length(selected_characteristics)
      dat <- dat[keep, , drop = FALSE]
    }
    
    if (!is.null(input$overview_nutrients) &&
        input$overview_nutrients != "all" &&
        length(nutrient_columns) > 0) {
      nutrient_matches <- as.data.frame(
        lapply(dat[nutrient_columns], positive_flag),
        check.names = FALSE
      )
      any_nutrient_assessed <- rowSums(nutrient_matches, na.rm = TRUE) > 0
      
      if (input$overview_nutrients == "yes") {
        dat <- dat[any_nutrient_assessed, , drop = FALSE]
      } else if (input$overview_nutrients == "no") {
        dat <- dat[!any_nutrient_assessed, , drop = FALSE]
      }
    }
    
    dat
  })
  
  output$study_overview_table <- renderDT({
    table_data <- overview_filtered() %>%
      transmute(
        Study = study_display,
        Dataset = dataset_display,
        Location = location,
        Year = year,
        `Sample age` = sample_age,
        `Sample sex` = sample_sex,
        `Sample size` = sample_size,
        `Publication DOI` = mapply(
          format_doi_links,
          pub_doi,
          study,
          USE.NAMES = FALSE
        )
      )
    
    datatable(
      table_data,
      escape = FALSE,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  selected_explorer_data <- reactive({
    req(input$explorer_dataset)
    study_lookup[[input$explorer_dataset]]
  })
  
  observeEvent(input$explorer_dataset, {
    dat <- selected_explorer_data()
    numeric_choices <- setdiff(
      numeric_like_variables(dat),
      "id"
    )
    
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
    req(input$selected_variable, input$explorer_dataset)
    
    dat <- selected_explorer_data()
    req(input$selected_variable %in% names(dat))
    
    plot_data <- tibble(
      Value = numeric_values(dat[[input$selected_variable]])
    )
    
    if ("cond_label" %in% names(dat)) {
      plot_data$Group <- as.character(dat$cond_label)
    } else {
      plot_data$Group <- "All observations"
    }
    
    plot_data <- plot_data %>%
      filter(!is.na(Value), !is.na(Group))
    
    validate(
      need(
        nrow(plot_data) >= 2,
        "This variable does not contain enough usable numeric values for a violin plot."
      )
    )
    
    ggplot(
      plot_data,
      aes(x = Value, y = Group, fill = Group)
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
      scale_fill_manual(
        values = setNames(
          rep(c(blue, orange, sky_blue, bluish_green), length.out = length(unique(plot_data$Group))),
          unique(plot_data$Group)
        )
      ) +
      labs(
        title = paste(input$selected_variable, "-", input$explorer_dataset),
        subtitle = if ("cond_label" %in% names(dat)) {
          "Values are shown by condition."
        } else {
          "Each point represents one available observation."
        },
        x = input$selected_variable,
        y = NULL
      ) +
      plot_theme +
      theme(legend.position = "none")
  })
  
  output$selected_variable_summary <- renderDT({
    req(input$selected_variable, input$explorer_dataset)
    
    dat <- selected_explorer_data()
    req(input$selected_variable %in% names(dat))
    
    values <- numeric_values(dat[[input$selected_variable]])
    
    if ("cond_label" %in% names(dat)) {
      summary_data <- tibble(
        Group = as.character(dat$cond_label),
        Value = values
      ) %>%
        group_by(Group) %>%
        summarise(
          n = sum(!is.na(Value)),
          Mean = round(safe_stat(Value, mean), 2),
          Median = round(safe_stat(Value, median), 2),
          SD = round(safe_stat(Value, sd), 2),
          Min = round(safe_stat(Value, min), 2),
          Max = round(safe_stat(Value, max), 2),
          Missing = sum(is.na(Value)),
          .groups = "drop"
        )
    } else {
      summary_data <- tibble(
        Dataset = input$explorer_dataset,
        Variable = input$selected_variable,
        n = sum(!is.na(values)),
        Mean = round(safe_stat(values, mean), 2),
        Median = round(safe_stat(values, median), 2),
        SD = round(safe_stat(values, sd), 2),
        Min = round(safe_stat(values, min), 2),
        Max = round(safe_stat(values, max), 2),
        Missing = sum(is.na(values))
      )
    }
    
    datatable(
      summary_data,
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
    quality_table <- build_data_quality_table(study_lookup)
    
    datatable(
      quality_table,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
}

shinyApp(ui = ui, server = server)