# ====================================
# Load required libraries
# ====================================

# ===== Package Loading =====
# List of required packages
packages <- c("shiny", "shinythemes", "shinyWidgets", "ggplot2", "dplyr", "tidyr", "colourpicker",
              "hrbrthemes", "ggpp", "scales", "viridis", "ggthemes",
              "RColorBrewer", "ggsci", "haven", "readxl", "readr", "DT", "ggrain")

# Install any packages that are not already installed
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load all the packages
lapply(packages, require, character.only = TRUE)

# ------------------------------------
# Theme and Palette Functions (common)
# ------------------------------------
theme_nice <- function(base_size = 14) {
  theme_bw(base_size = base_size) +
    theme(panel.grid = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = base_size))
}
choose_theme <- function(theme_choice, base_size_val) {
  if(theme_choice == "nice") {
    return(theme_nice(base_size = base_size_val))
  } else if(theme_choice == "bw") {
    return(theme_bw(base_size = base_size_val))
  } else if(theme_choice == "minimal") {
    return(theme_minimal(base_size = base_size_val))
  } else if(theme_choice == "grey") {
    return(theme_grey(base_size = base_size_val))
  } else if(theme_choice == "ipsum") {
    return(theme_ipsum(base_size = base_size_val))
  } else if(theme_choice == "classic") {
    return(theme_classic(base_size = base_size_val))
  } else if(theme_choice == "economist") {
    return(theme_economist(base_size = base_size_val))
  } else if(theme_choice == "stata") {
    return(theme_stata(base_size = base_size_val))
  } else {
    return(theme_nice(base_size = base_size_val))
  }
}
get_palette_funcs <- function(palette_name) {
  col_fun <- switch(palette_name,
                    "uchicago" = scale_color_uchicago,
                    "npg" = scale_color_npg,
                    "lancet" = scale_color_lancet,
                    "jco" = scale_color_jco,
                    "nejm" = scale_color_nejm,
                    "d3" = scale_color_d3,
                    "simpsons" = scale_color_simpsons,
                    "tron" = scale_color_tron,
                    "futurama" = scale_color_futurama,
                    "rickandmorty" = scale_color_rickandmorty,
                    scale_color_brewer(palette = "Set2"))
  fill_fun <- switch(palette_name,
                     "uchicago" = scale_fill_uchicago,
                     "npg" = scale_fill_npg,
                     "lancet" = scale_fill_lancet,
                     "jco" = scale_fill_jco,
                     "nejm" = scale_fill_nejm,
                     "d3" = scale_fill_d3,
                     "simpsons" = scale_fill_simpsons,
                     "tron" = scale_fill_tron,
                     "futurama" = scale_fill_futurama,
                     "rickandmorty" = scale_fill_rickandmorty,
                     scale_fill_brewer(palette = "Set2"))
  list(col_fun = col_fun, fill_fun = fill_fun)
}
get_seq_palette_funcs <- function(palette_name) {
  if(palette_name %in% c("Greens", "Purples", "Blues", "Greys", "Oranges", "Reds")){
    pal_fun_fill <- function(...) scale_fill_brewer(palette = palette_name, ...)
    pal_fun_colour <- function(...) scale_color_brewer(palette = palette_name, ...)
  } else if(tolower(palette_name) %in% c("viridis", "magma", "plasma", "inferno")){
    pal_fun_fill <- function(...) scale_fill_viridis_d(option = tolower(palette_name), ...)
    pal_fun_colour <- function(...) scale_color_viridis_d(option = tolower(palette_name), ...)
  } else if(palette_name == "Cividis"){
    pal_fun_fill <- function(...) scale_fill_viridis_d(option = "cividis", ...)
    pal_fun_colour <- function(...) scale_color_viridis_d(option = "cividis", ...)
  } else {
    pal_fun_fill <- function(...) scale_fill_brewer(palette = "Blues", ...)
    pal_fun_colour <- function(...) scale_color_brewer(palette = "Blues", ...)
  }
  list(fill_fun = pal_fun_fill, colour_fun = pal_fun_colour)
}

# ------------------------------------
# Define EQ-5D Dimension Labels (for facets)
# ------------------------------------
eq5d_dim_eng <- c("MO" = "Mobility (MO)",
                  "SC" = "Self-Care (SC)",
                  "UA" = "Usual Activities (UA)",
                  "PD" = "Pain/Discomfort (PD)",
                  "AD" = "Anxiety/Depression (AD)")
eq5d_dim_nor <- c("MO" = "Mobilitet (MO)",
                  "SC" = "Egenomsorg (EO)",
                  "UA" = "Vanlige aktiviteter (VA)",
                  "PD" = "Smerte/ubehag (SU)",
                  "AD" = "Angst/Depresjon (AD)")

# ------------------------------------
# Define EQ-5D Response Level Labels
# ------------------------------------
EQ5D_LEVELS <- c("1", "2", "3", "4", "5")
EQ5D_LABELS_ENG <- c("No problem", "Slight problem", "Moderate problems", "Severe problems", "Extreme problems")
EQ5D_LABELS_NOR <- c("Ingen problemer", "Litt problemer", "Moderat problemer", "Alvorlige problemer", "Ekstreme problemer")


# ------------------------------------
# Simulated Data Generation (with Ordered Factors)
# ------------------------------------
set.seed(123)
n <- 2000
gender <- sample(c("male", "female"), n, replace = TRUE)
age_numeric <- sample(18:100, n, replace = TRUE)
age_breaks <- c(18, 30, 40, 50, 60, 70, 80, 90, 101)
age_labels <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
age <- cut(age_numeric, breaks = age_breaks, labels = age_labels, right = FALSE)
treatment <- sample(c("pre", "post"), n, replace = TRUE)
treatment <- factor(treatment, levels = c("pre", "post"), ordered = TRUE)
marital_status <- sample(c("married", "divorced/separated", "single", "widowed"), n,
                         replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1))
marital_status <- factor(marital_status, levels = c("married", "divorced/separated", "single", "widowed"), ordered = TRUE)
smoking <- sample(c("yes", "no"), n, replace = TRUE, prob = c(0.3, 0.7))
education <- sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
education <- factor(education, levels = c("Low", "Medium", "High"), ordered = TRUE)
income <- sample(c("Low", "Medium", "High"), n, replace = TRUE, prob = c(0.3, 0.4, 0.3))
income <- factor(income, levels = c("Low", "Medium", "High"), ordered = TRUE)
simulate_dim <- function(mean_target, smoker, age, education) {
  base <- round(rnorm(1, mean = mean_target, sd = 1))
  if(smoker == "yes") base <- base + 2.0
  if(as.character(age) %in% c("60-69", "70-79", "80-89", "90+")) {
    base <- base + 1.0
  } else if(as.character(age) %in% c("18-29", "30-39")) {
    base <- base - 1.0
  }

  if(education == "Low") {
    base <- base + 1
  } else if(education == "High") {
    base <- base - 1
  }

  round(min(max(base, 1), 5))
}

MO <- mapply(simulate_dim, mean_target = 2.5, smoker = smoking, age = age, education = education)
SC <- mapply(simulate_dim, mean_target = 2.8, smoker = smoking, age = age, education = education)
UA <- mapply(simulate_dim, mean_target = 3.1, smoker = smoking, age = age, education = education)
PD <- mapply(simulate_dim, mean_target = 3.4, smoker = smoking, age = age, education = education)
AD <- mapply(simulate_dim, mean_target = 3.8, smoker = smoking, age = age, education = education)

simulate_vas <- function(baseline, gender, income, education, marital_status, treatment, smoker, age) {
  # Generate a normal-like distribution with fewer extreme values
  base <- qnorm(runif(1, pnorm(-1.2), pnorm(1.2))) * 8 + baseline  # SD is now 8 instead of 10 to reduce spread
  # Apply group-based adjustments
  if(gender == "male") {
    base <- base + 3  # Instead of +5, we make the shift smaller
    base <- min(max(base, 15), 85)  # Males now have an even tighter range (15-85)
  }

  if(gender == "female") {
    base <- base - 3  # Keeping females lower
    base <- min(max(base, 10), 90)  # Keep females within a reasonable range
  }

  if(income == "Low") base <- base - 15
  if(income == "High") base <- base + 15
  if(education == "Low") base <- base - 10
  if(education == "High") base <- base + 7
  if(marital_status == "married") base <- base + 8
  if(marital_status == "divorced/separated") base <- base - 7
  if(marital_status == "widowed") base <- base - 10
  if(treatment == "post") base <- base + 10
  if(smoker == "yes") base <- base - 10

  # Adjust for age
  if(as.character(age) %in% c("60-69", "70-79", "80-89", "90+")) base <- base - 8

  # Final bounding to prevent extreme values
  round(min(max(base, 10), 90))
}

EQ_VAS <- mapply(simulate_vas, baseline = 60, gender = gender, income = income,
                 education = education, marital_status = marital_status,
                 treatment = treatment, smoker = smoking, age = age)

simulated_eq5d <- data.frame(MO, SC, UA, PD, AD, gender, age, EQ_VAS, treatment,
                             marital_status, smoking, education, income)

# Data preparation for Profiles (pivot wide to long)
prepareProfileData <- function(df) {
  df %>% pivot_longer(
    cols = c(MO, SC, UA, PD, AD),
    names_to = "Dimension",
    values_to = "Response"
  )
}

#Function to set x and y limits based on coord cartesian
parse_limits <- function(limits_str) {
  if (is.null(limits_str) || limits_str == "") return(NULL)
  limits_vec <- strsplit(limits_str, ",")[[1]]
  limits_num <- as.numeric(trimws(limits_vec))
  if(length(limits_num) != 2 || any(is.na(limits_num))){
    warning("Invalid limits provided. Using default limits.")
    return(NULL)
  }
  limits_num
}

# ------------------------------
# UI
# ------------------------------
ui <- navbarPage(
  title = "EQ-5D Data Visualization",
  id = "navbar",
  theme = shinytheme("cerulean"),
  inverse = T,

  tabPanel("Instructions", value = "instructions",
           tags$div(
             tags$h3("About"),
             tags$p("This app is developed to quickly create a variety of professional data visualizations for the EQ‐5D instrument.
                    It is designed for researchers, clinicians, and anyone needing high-quality plots for reports, research articles, or exploratory analysis."),

             tags$h4("The EQ‐5D Instrument"),
             tags$p("The EQ‐5D consists of two main components:"),
             tags$ul(
               style = "margin-bottom: 5px;",  # Reduces space below the list
               tags$li(tags$strong("Descriptive System:"), " Includes five dimensions:"),
               tags$ul(
                 style = "margin-bottom: 3px;",  # Reduces space between list items
                 tags$li(tags$strong("Mobility (MO):"), " Ability to move around."),
                 tags$li(tags$strong("Self-Care (SC):"), " Capacity to care for oneself."),
                 tags$li(tags$strong("Usual Activities (UA):"), " Performance in daily tasks."),
                 tags$li(tags$strong("Pain/Discomfort (PD):"), " Level of pain or discomfort."),
                 tags$li(tags$strong("Anxiety/Depression (AD):"), " Mental health status.")
               ),
               tags$li(tags$strong("EQ Visual Analogue Scale (EQ VAS):"),
                       " A 0-100 scale rating overall health.")
             ),

             tags$h4("Uploading Your Own Data"),
             tags$p("You can either use the built-in simulated data or upload your own dataset via the 'Data' tab. Accepted file formats are: SPSS (.sav), Stata (.dta), Excel (.xls, .xlsx), and CSV (.csv).
                    Your data should be in standard wide format – each EQ-5D dimension must be stored as its own variable (column)."),
             tags$p("For the app to function properly, your data must include the following naming conventions of the EQ-5D variables:"),
             tags$table(
               class = "table table-bordered table-condensed table-striped table-hover",
               style = "background-color: #ffffff; padding: 2px; border-radius: 3px;
           max-width: 400px; margin-left: 0; border-collapse: collapse;
           font-size: 12px; line-height: 1.1;",
               tags$thead(
                 style = "background-color: #f0f0f0; font-weight: bold; text-align: left;",
                 tags$tr(
                   tags$th(style = "width: 25%; padding: 2px;", "Variable Name"),
                   tags$th(style = "width: 75%; padding: 2px;", "Full Domain Name")
                 )
               ),
               tags$tbody(
                 lapply(list(
                   c("MO", "Mobility"),
                   c("SC", "Self-Care"),
                   c("UA", "Usual Activities"),
                   c("PD", "Pain/Discomfort"),
                   c("AD", "Anxiety/Depression"),
                   c("EQ_VAS", "Visual Analogue Scale (0-100)")
                 ), function(row) {
                   tags$tr(
                     tags$td(style = "padding: 2px; text-align: left;", row[[1]]),
                     tags$td(style = "padding: 2px;", row[[2]])
                   )
                 })
               )
             ),
             tags$p("The app automatically converts these EQ-5D variables from wide to long format to facilitate plotting multiple dimensions in one graph. It also converts any labelled variables (from SPSS or Stata) into factors, and automatically identifies categorical variables for group comparisons and faceting."),

             tags$h4("Navigating the App"),
             tags$p("The app is organized into several tabs:"),
             tags$ul(
               tags$li(tags$strong("'Data' Tab:"), " Preview your dataset."),
               tags$li(tags$strong("'EQ-5D Profiles' Tab:"), " Visualize responses across the five dimensions using Bar, Density, or Stacked,  plots. Customize appearance options such as labels, themes, and color palettes."),
               tags$li(tags$strong("'EQ VAS' Tab:"), " Explore overall health ratings with Histogram, Density, Raincloud, Boxplot, Violin, or Trend Line plots. Add reference lines, apply facets, and adjust various appearance settings.")
             ),
             tags$p("Once customized, click ",
                    tags$strong("Download Figure"), " ",
                    shiny::icon("download"), " to save as PDF or PNG.")
           )
  ),



  # Data Tab
  tabPanel("Data", value = "data",
           sidebarLayout(
             sidebarPanel(
               radioButtons("data_source", "Select Data Source:",
                            choices = c("Simulated Data" = "simulated",
                                        "Upload Your Own Data" = "upload"),
                            selected = "simulated"),
               conditionalPanel(
                 condition = "input.data_source == 'upload'",
                 fileInput("uploaded_file", "Upload File",
                           accept = c(".sav", ".dta", ".xls", ".xlsx", ".csv"))
               )
             ),
             mainPanel(
               h4("Data Preview (first 10 rows)"),
               DT::dataTableOutput("data_preview")
             )
           )
  ),

  # EQ-5D Profiles Tab
  tabPanel("EQ-5D Profiles", value = "profiles",
           sidebarLayout(
             sidebarPanel(
               selectInput("dimension_choice", "Choose EQ-5D Dimension:",
                           choices = c("All" = "All",
                                       "Mobility (MO)" = "MO",
                                       "Self-Care (SC)" = "SC",
                                       "Usual Activities (UA)" = "UA",
                                       "Pain/Discomfort (PD)" = "PD",
                                       "Anxiety/Depression (AD)" = "AD"),
                           selected = "All"),
               selectInput("profile_plot_type", "Choose Plot Type:",
                           choices = c("Bar Chart" = "bar",
                                       "Density Distribution" = "density",
                                       "Stacked Bar Chart" = "stacked"),
                           selected = "bar"),
               selectInput("profile_group", "Compare Groups:",
                           choices = c("None" = "None"), selected = "None"),

               dropdownButton(
                 label = "Labels",
                 icon = icon("font"),
                 status = "primary",
                 circle = FALSE,
                 width = "300px",
                 textInput("summary_title", "Plot Title:", value = ""),
                 textInput("summary_subtitle", "Plot Subtitle:", value = ""),
                 textInput("summary_xlabel", "X-axis Label (leave blank for default):", value = ""),
                 textInput("summary_ylabel", "Y-axis Label (leave blank for default):", value = ""),
                 textInput("summary_caption", "Plot Caption (leave blank for default):", value = ""),
                 textInput("summary_legend_title", "Legend Title:", value = "Group"),
                 numericInput("summary_base_size", "Size Option:", value = 14, min = 8, max = 50, step = 1)
               ),

               dropdownButton(
                 label = "Appearance",
                 icon = icon("paint-brush"),
                 status = "primary",
                 circle = FALSE,
                 width = "300px",
                 selectInput("profile_theme", "Choose ggplot Theme:",
                             choices = c("nice", "ipsum", "minimal", "grey", "bw", "classic", "economist", "stata"),
                             selected = "nice"),
                 radioButtons("profile_color_mode", "Choose Color Mode:",
                              choices = c("Sequential" = "seq", "Qualitative" = "qual"),
                              selected = "seq", inline = TRUE),
                 conditionalPanel(
                   condition = "input.profile_color_mode == 'qual'",
                   selectInput("profile_qual_palette", "Qualitative Palette:",
                               choices = c("uchicago", "npg", "lancet", "jco", "nejm",
                                           "d3", "simpsons", "tron", "futurama", "rickandmorty"),
                               selected = "uchicago")
                 ),
                 conditionalPanel(
                   condition = "input.profile_color_mode == 'seq'",
                   selectInput("profile_seq_palette", "Sequential Palette:",
                               choices = c("Greens", "Purples", "Blues", "Viridis", "Magma", "Plasma", "Inferno", "Cividis", "Greys"),
                               selected = "Blues")
                 ),
                 sliderInput("profile_alpha", "Transparency (Alpha):",
                             min = 0, max = 1, value = 1, step = 0.1),
                 checkboxInput("profile_coord_flip", "Flip Coordinates (All)", value = FALSE),
                 conditionalPanel(
                   condition = "input.profile_group != 'None'",
                   checkboxInput("profile_group_n_caption",
                                 "Display group counts in caption?",
                                 value = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.profile_plot_type == 'bar' || input.profile_plot_type == 'stacked'",
                   checkboxInput("profile_show_pct", "Show Percentage Labels", value = FALSE),
                   conditionalPanel(
                     condition = "input.profile_show_pct == true",
                     textInput("profile_label_color", "Percentage Label Color:", value = "black")
                   )
                 ),
                 textInput("profile_xlim", "X-axis Limits (e.g., 0,100):", value = ""),
                 textInput("profile_ylim", "Y-axis Limits (e.g., 0,100):", value = ""),
                 numericInput("profile_base_size", "Theme Size Option:", value = 14, min = 8, max = 50, step = 1)
               )
             ),
             mainPanel(
               plotOutput("profile_plot", height = "70vh"),
               br(),
               absolutePanel(
                 id = "languagePanel", fixed = TRUE, draggable = FALSE,
                 bottom = 10, left = 10, width = 300,
                 style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 8px;",
                 h5("Labels English/Norwegian"),
                 p("Select the language for facet labels and response levels:"),
                 radioButtons("language", NULL,
                              choices = c("English", "Norwegian"),
                              selected = "English", inline = TRUE)
               ),
               actionButton("download_btn", "Download Figure", icon = icon("download"))
             )
           )
  ),

  # EQ VAS Tab – Modified Appearance Section and Plot Options
  tabPanel("EQ VAS", value = "eqvas",
           sidebarLayout(
             sidebarPanel(
               selectInput("vas_plot_type", "Choose Plot Type:",
                           choices = c("Histogram", "Density", "Raincloud", "Boxplot", "Violin", "Trend Line"),
                           selected = "Histogram"),
               selectInput("vas_group", "Compare Groups:",
                           choices = c("None" = "None"), selected = "None"),
               selectInput("vas_facet", "Choose Facet:",
                           choices = c("None" = "None"), selected = "None"),
               conditionalPanel(
                 condition = "input.vas_facet != 'None'",
                 radioButtons("vas_facet_scales", "Facet Scale:",
                              choices = c("Fixed" = "fixed", "Free Y" = "free_y", "Free X" = "free_x"),
                              selected = "fixed", inline = TRUE),
                 splitLayout(
                   cellWidths = c("80px", "80px"),
                   numericInput("vas_facet_nrow", "Rows:", value = 0, min = 0, width = "70px"),
                   numericInput("vas_facet_ncol", "Columns:", value = 0, min = 0, width = "70px")
                 ),
                 helpText(tags$small("'Fixed' applies the same scales across facets, while 'Free Y' or 'Free X' allow independent y- or x-axis scales. Adjust Rows/Columns to control the layout."))
               ),

               conditionalPanel(
                 condition = "input.vas_plot_type == 'Trend Line'",
                 selectInput("vas_trend_type", "Choose Trend Method:",
                             choices = c("Observed Means" = "observed",
                                         "Smooth (lm)" = "lm",
                                         "Smooth (loess)" = "loess"),
                             selected = "observed"),
                 selectInput("vas_xvar", "X Variable for Trend:", choices = NULL)
               ),

               dropdownButton(
                 label = "Reference Lines",
                 icon = icon("sliders-h"),
                 status = "primary",
                 circle = FALSE,
                 width = "300px",
                 radioButtons("ref_statistic", "Reference Statistic:",
                              choices = c("Mean" = "mean", "Median" = "median"),
                              selected = "mean"),
                 checkboxInput("ref_show_overall", "Show Grand Reference Line", value = FALSE),
                 conditionalPanel(
                   condition = "input.ref_show_overall == true",
                   colourInput("ref_overall_color", "Grand Line Color:", value = "#000000"),
                   numericInput("ref_overall_linewidth", "Grand Line Width:", value = 1, min = 0.1, step = 0.1),
                   selectInput("ref_overall_linetype", "Grand Line Linetype:",
                               choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                               selected = "dashed")
                 ),
                 checkboxInput("ref_show_groups", "Show Group Reference Lines", value = FALSE),
                 conditionalPanel(
                   condition = "input.ref_show_groups == true",
                   numericInput("ref_group_linewidth", "Group Line Width:", value = 1, min = 0.1, step = 0.1),
                   selectInput("ref_group_linetype", "Group Line Linetype:",
                               choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                               selected = "dashed")
                 ),
                 checkboxInput("ref_custom", "Show Custom Reference Lines", value = FALSE),
                 conditionalPanel(
                   condition = "input.ref_custom == true",
                   colourInput("ref_custom_color", "Custom Line Color:", value = "#000000"),
                   numericInput("ref_custom_linewidth", "Custom Line Width:", value = 1, min = 0.1, step = 0.1),
                   selectInput("ref_custom_linetype", "Custom Line Linetype:",
                               choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                               selected = "dashed"),
                   textInput("ref_custom_values", "Custom Reference Line Values (comma-separated):", value = "")
                 )
               ),

               dropdownButton(
                 label = "Labels",
                 icon = icon("font"),
                 status = "primary",
                 circle = FALSE,
                 width = "300px",
                 textInput("vas_summary_title", "Plot Title:", value = ""),
                 textInput("vas_summary_subtitle", "Plot Subtitle:", value = ""),
                 textInput("vas_summary_xlabel", "X-axis Label (leave blank for default):", value = ""),
                 textInput("vas_summary_ylabel", "Y-axis Label (leave blank for default):", value = ""),
                 textInput("vas_summary_caption", "Plot Caption (leave blank for default):", value = ""),
                 textInput("vas_summary_legend_title", "Legend Title:", value = "Group"),
                 numericInput("vas_summary_base_size", "Size Option:", value = 14, min = 8, max = 50, step = 1)
               ),

               dropdownButton(
                 label = "Appearance",
                 icon = icon("paint-brush"),
                 status = "primary",
                 circle = FALSE,
                 width = "300px",
                 selectInput("vas_theme", "Choose ggplot Theme:",
                             choices = c("nice", "ipsum", "minimal", "grey", "bw", "classic", "economist", "stata"),
                             selected = "nice"),
                 conditionalPanel(condition = "input.vas_group != 'None'",
                   checkboxInput("vas_group_n_caption",
                                 "Display group counts in caption?",
                                      FALSE)),
                 conditionalPanel(
                   condition = "input.vas_group != 'None'",
                   radioButtons("vas_color_mode", "Choose Color Mode:",
                                choices = c("Sequential" = "seq", "Qualitative" = "qual"),
                                selected = "qual", inline = TRUE),
                   conditionalPanel(
                     condition = "input.vas_color_mode == 'qual'",
                     selectInput("vas_qual_palette", "Qualitative Palette:",
                                 choices = c("uchicago", "npg", "lancet", "jco", "nejm",
                                             "d3", "simpsons", "tron", "futurama", "rickandmorty"),
                                 selected = "uchicago")
                   ),
                   conditionalPanel(
                     condition = "input.vas_color_mode == 'seq'",
                     selectInput("vas_seq_palette", "Sequential Palette:",
                                 choices = c("Greens", "Purples", "Blues", "Viridis", "Magma", "Plasma", "Inferno", "Cividis", "Greys"),
                                 selected = "Blues")
                   )
                 ),
                 conditionalPanel(
                   condition = "input.vas_group == 'None'",
                   tags$div(
                     tags$small("Manual Color Settings"),
                     colourInput("vas_manual_fill", "Fill Color:", value = "#A4A9AD")
                   )
                 ),

                 conditionalPanel(
                   condition = "input.vas_plot_type == 'Histogram'",
                   sliderInput("vas_binwidth", "Bin Width:", min = 1, max = 50, value = 2)
                 ),
                 conditionalPanel(
                   condition = "!(input.vas_plot_type == 'Trend Line' && (input.vas_trend_type == 'lm' || input.vas_trend_type == 'loess'))",
                   sliderInput("vas_alpha", "Transparency (Alpha):",
                               min = 0, max = 1, value = 1, step = 0.1)
                 ),
                 checkboxInput("vas_coord_flip_all", "Flip Coordinates (All)", value = FALSE),
                 conditionalPanel(
                   condition = "input.vas_plot_type == 'Trend Line'",
                   sliderInput("vas_line_width", "Trend Line Width:",
                               min = 0.1, max = 5, value = 1, step = 0.1),
                   conditionalPanel(
                     condition = "input.vas_trend_type == 'observed'",
                     sliderInput("vas_point_size", "Trend Point Size:",
                                 min = 1, max = 10, value = 2, step = 0.5)
                   ),
                   checkboxInput("vas_show_ci", "Show Confidence Interval", value = TRUE),
                   conditionalPanel(
                     condition = "input.vas_show_ci == true",
                     sliderInput("vas_ci_alpha", "CI Transparency (Alpha):",
                                 min = 0, max = 1, value = 0.2, step = 0.1)
                   ),
                   checkboxInput("vas_show_points", "Show Raw Data Points", value = FALSE)
                 ),

                 textInput("vas_xlim", "X-axis Limits (e.g., 0,100):", value = ""),
                 textInput("vas_ylim", "Y-axis Limits (e.g., 0,100):", value = "")
               )
             ),
             mainPanel(
               plotOutput("vas_plot", height = "70vh"),
               br(),
               actionButton("download_btn", "Download Figure", icon = icon("download"))
             )
           )
  ),

  # About Tab
  tabPanel("About", value = "about",
           tags$div(
             tags$h3("About This Application"),
             tags$p(
               "This Shiny application was created by Senior Researcher ",
               tags$strong("Sondre Aasen Nilsen"),
               " on behalf of the ",
               tags$strong("Center for Patient-Reported Data at Haukeland University Hospital, Bergen, Norway"),
               "."
             ),
             tags$p("For any inquiries, suggestions, or bug reports, please send an e-mail to: sondre.aa.nilsen@gmail.com"),
             tags$p("The app leverages the following amazing open-source R packages for data manipulation and visualization:"),
             tags$ul(
               tags$li(
                 tags$strong("shiny"), ": Chang, W., et al. (2024). ",
                 tags$em("shiny: Web Application Framework for R."),
                 " R package version 1.10.0. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny")
               ),
               tags$li(
                 tags$strong("shinythemes"), ": Chang, W. (2021). ",
                 tags$em("shinythemes: Themes for Shiny."),
                 " R package version 1.2.0. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=shinythemes", "https://CRAN.R-project.org/package=shinythemes")
               ),
               tags$li(
                 tags$strong("shinyWidgets"), ": Perrier, V., Meyer, F., & Granjon, D. (2024). ",
                 tags$em("shinyWidgets: Custom Inputs Widgets for Shiny."),
                 " R package version 0.8.7. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=shinyWidgets", "https://CRAN.R-project.org/package=shinyWidgets")
               ),
               tags$li(
                 tags$strong("ggplot2"), ": Wickham, H. (2016). ",
                 tags$em("ggplot2: Elegant Graphics for Data Analysis."),
                 " Springer-Verlag New York. Retrieved from ",
                 tags$a(href="https://ggplot2.tidyverse.org", "https://ggplot2.tidyverse.org")
               ),
               tags$li(
                 tags$strong("dplyr"), ": Wickham, H., et al. (2023). ",
                 tags$em("dplyr: A Grammar of Data Manipulation."),
                 " R package version 1.1.4. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=dplyr", "https://CRAN.R-project.org/package=dplyr")
               ),
               tags$li(
                 tags$strong("tidyr"), ": Wickham, H., et al. (2024). ",
                 tags$em("tidyr: Tidy Messy Data."),
                 " R package version 1.3.1. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=tidyr", "https://CRAN.R-project.org/package=tidyr")
               ),
               tags$li(
                 tags$strong("RColorBrewer"), ": Neuwirth, E. (2022). ",
                 tags$em("RColorBrewer: ColorBrewer Palettes."),
                 " R package version 1.1-3. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=RColorBrewer", "https://CRAN.R-project.org/package=RColorBrewer")
               ),
               tags$li(
                 tags$strong("ggsci"), ": Xiao, N. (2024). ",
                 tags$em("ggsci: Scientific Journal and Sci-Fi Themed Color Palettes for 'ggplot2'."),
                 " R package version 3.2.0. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=ggsci", "https://CRAN.R-project.org/package=ggsci")
               ),
               tags$li(
                 tags$strong("ggthemes"), ": Jeffrey B. Arnold (2024). ",
                 tags$em("ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'."),
                 " R package version 5.1.0. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=ggthemes", "https://CRAN.R-project.org/package=ggthemes")
               ),
               tags$li(
                 tags$strong("haven"), ": Wickham, H., et al. (2023). ",
                 tags$em("haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files."),
                 " R package version 2.5.4. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=haven", "https://CRAN.R-project.org/package=haven")
               ),
               tags$li(
                 tags$strong("readxl"), ": Wickham, H. & Bryan, J. (2023). ",
                 tags$em("readxl: Read Excel Files."),
                 " R package version 1.4.3. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=readxl", "https://CRAN.R-project.org/package=readxl")
               ),
               tags$li(
                 tags$strong("readr"), ": Wickham, H., et al. (2024). ",
                 tags$em("readr: Read Rectangular Text Data."),
                 " R package version 2.1.5. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=readr", "https://CRAN.R-project.org/package=readr")
               ),
               tags$li(
                 tags$strong("DT"), ": Xie, Y., et al. (2024). ",
                 tags$em("DT: A Wrapper of the JavaScript Library 'DataTables'."),
                 " R package version 0.33. Retrieved from ",
                 tags$a(href="https://CRAN.R-project.org/package=DT", "https://CRAN.R-project.org/package=DT")
               ),
               tags$li(
                 tags$strong("ggrain"), ": Allen, M., et al. (2021). ",
                 tags$em("Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved]."),
                 " Wellcome Open Research, 4(63). https://doi.org/10.12688/wellcomeopenres.15191.2"
               )
             ),
             tags$p("All these packages are licensed under open-source terms, enabling us to build this app and share it freely with you.")
           )
  )
)

# ------------------------------
# SERVER
# ------------------------------
server <- function(input, output, session) {

  current_plot <- reactiveVal(NULL)

  current_data <- reactive({
    if(input$data_source == "simulated"){
      simulated_eq5d
    } else {
      inFile <- input$uploaded_file
      if(is.null(inFile)) return(NULL)
      ext <- tools::file_ext(inFile$name)
      if(! ext %in% c("sav", "dta", "xls", "xlsx", "csv")) {
        showNotification("Unsupported file type!", type = "error")
        return(NULL)
      }
      df <- tryCatch({
        switch(ext,
               "sav" = haven::read_sav(inFile$datapath),
               "dta" = haven::read_dta(inFile$datapath),
               "xls" = readxl::read_excel(inFile$datapath),
               "xlsx" = readxl::read_excel(inFile$datapath),
               "csv" = readr::read_csv(inFile$datapath),
               NULL)
      }, error = function(e) {
        showNotification("Error reading file. Please check the file format.", type = "error")
        return(NULL)
      })
      if(is.null(df)) return(NULL)
      df <- df %>% mutate(across(everything(), ~ if(inherits(., "haven_labelled") || inherits(., "labelled"))
        haven::as_factor(.) else .))
      return(df)
    }
  })


  observe({
    df <- current_data()
    if(is.null(df)) return()
    cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "profile_group",
                      choices = c("None" = "None", setNames(cat_vars, cat_vars)),
                      selected = "None")
    updateSelectInput(session, "vas_group",
                      choices = c("None" = "None", setNames(cat_vars, cat_vars)),
                      selected = "None")
    updateSelectInput(session, "vas_xvar", choices = names(df),
                      selected = if("age_numeric" %in% names(df)) "age_numeric" else names(df)[1])
    updateSelectInput(session, "vas_facet",
                      choices = c("None" = "None", setNames(cat_vars, cat_vars)),
                      selected = "None")
  })

  output$data_preview <- DT::renderDT({
    head(current_data(), 10)
  })

  # ------------------------------
  # EQ-5D Profiles Plot (Single renderPlot)
  # ------------------------------
  output$profile_plot <- renderPlot({
    df <- current_data()
    req(df)
    df_long <- prepareProfileData(df)
    if(input$dimension_choice != "All"){
      df_long <- df_long %>% filter(Dimension == input$dimension_choice)
    }

    #compute per‐facet sample size
    facet_counts <- df_long %>%
      group_by(Dimension) %>%
      summarise(n = n(), .groups = "drop")

    # choose the base labels based on language
    dim_labels <- if (input$language == "Norwegian") eq5d_dim_nor else eq5d_dim_eng

    # custom labeller: e.g. "Mobility (MO) (n = 345)"
    custom_dim_labeller <- as_labeller(function(x) {
      paste0(dim_labels[x], " (n = ", facet_counts$n[match(x, facet_counts$Dimension)], ")")
    })

    # Determine language-specific response labels
    response_labels <- if(!is.null(input$language) && input$language == "Norwegian") EQ5D_LABELS_NOR else EQ5D_LABELS_ENG
    groupVar <- input$profile_group
    compareGroups <- (groupVar != "None")
    plotType <- input$profile_plot_type

    # Set default plot labels based on plot type and dimension choice
    if(plotType %in% c("bar", "density", "stacked")){
      default_title <- if(input$dimension_choice == "All") "Distribution of EQ-5D Responses" else paste("Dimension:", input$dimension_choice)
      default_subtitle <- if(input$dimension_choice == "All") "Faceted by Dimension" else paste("Dimension:", input$dimension_choice)
      default_x <- ""
      default_y <- "Proportion"
    }

    get_plot_labels <- function(defaults) {
      list(
        title = if(nzchar(input$summary_title)) input$summary_title else defaults$title,
        subtitle = if(nzchar(input$summary_subtitle)) input$summary_subtitle else defaults$subtitle,
        x = if(nzchar(input$summary_xlabel)) input$summary_xlabel else defaults$x,
        y = if(nzchar(input$summary_ylabel)) input$summary_ylabel else defaults$y,
        caption = if(nzchar(input$summary_caption)) input$summary_caption else NULL,
        fill = input$summary_legend_title,
        base_size = input$summary_base_size
      )
    }
    labels <- get_plot_labels(list(title = default_title, subtitle = default_subtitle, x = default_x, y = default_y))
    profile_theme <- choose_theme(input$profile_theme, input$profile_base_size)
    profile_alpha <- input$profile_alpha
    xlim_vals <- parse_limits(input$profile_xlim)
    ylim_vals <- parse_limits(input$profile_ylim)

    if(input$profile_color_mode == "qual"){
      pal_fun_fill <- get_palette_funcs(input$profile_qual_palette)$fill_fun
      pal_fun_colour <- get_palette_funcs(input$profile_qual_palette)$col_fun
    } else {
      pal_seq <- get_seq_palette_funcs(input$profile_seq_palette)
      pal_fun_fill <- pal_seq$fill_fun
      pal_fun_colour <- pal_seq$colour_fun
    }

    # Build the plot based on selected plot type
    if(plotType == "bar"){
      if(compareGroups){
        df_prop <- df_long %>%
          group_by(Dimension, Response, grp = .data[[groupVar]]) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(Dimension, grp) %>%
          mutate(proportion = count / sum(count)) %>%
          ungroup()

        p <- ggplot(df_prop, aes(x = factor(Response, levels = EQ5D_LEVELS, labels = response_labels),
                                 y = proportion, fill = grp)) +
          geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = profile_alpha) +
          scale_y_continuous(labels = scales::percent_format()) +
          scale_x_discrete(name = "", labels = scales::wrap_format(10)) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = labels$x, y = labels$y,
               fill = labels$fill,
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill()

        if(input$profile_show_pct){
          df_prop <- df_prop %>%
            group_by(Dimension, grp) %>%
            mutate(cum_prop = cumsum(proportion) - 0.5 * proportion) %>%
            ungroup()
          p <- p + geom_text(data = df_prop,
                             aes(label = paste0(round(proportion * 100, 1), "%"), y = proportion + 0.01),
                             position = position_dodge(width = 0.9),
                             color = input$profile_label_color,
                             size = 4)
        }
        if(input$dimension_choice == "All"){
          lab <- if(!is.null(input$language) && input$language == "Norwegian") {
            labeller(Dimension = eq5d_dim_nor)
          } else {
            labeller(Dimension = eq5d_dim_eng)
          }
          p <- p + facet_wrap(~ Dimension, labeller = custom_dim_labeller)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
      } else {
        df_prop <- df_long %>%
          group_by(Dimension, Response) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(Dimension) %>%
          mutate(proportion = count / sum(count)) %>%
          ungroup()

        p <- ggplot(df_prop, aes(x = factor(Response, levels = EQ5D_LEVELS, labels = response_labels),
                                 y = proportion, fill = as.factor(Response))) +
          geom_bar(stat = "identity", show.legend = FALSE, alpha = profile_alpha) +
          scale_y_continuous(labels = scales::percent_format()) +
          scale_x_discrete(name = "", labels = scales::wrap_format(10)) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = labels$x, y = labels$y,
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill()

        if(input$profile_show_pct){
          p <- p + geom_text(aes(label = paste0(round(proportion * 100, 1), "%")),
                             vjust = -0.5,
                             color = input$profile_label_color,
                             position = position_dodge(width = 0.01))
        }
        if(input$dimension_choice == "All"){
          lab <- if(!is.null(input$language) && input$language == "Norwegian") {
            labeller(Dimension = eq5d_dim_nor)
          } else {
            labeller(Dimension = eq5d_dim_eng)
          }
          p <- p + facet_wrap(~ Dimension, labeller = custom_dim_labeller)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
      }
      if(input$profile_coord_flip){
        p <- p + coord_flip()
      }
    }

    if(plotType == "density"){
      df_long <- df_long %>% mutate(ResponseNum = as.numeric(as.character(Response)))
      if(compareGroups){
        p <- ggplot(df_long, aes(x = ResponseNum, fill = .data[[groupVar]], color = .data[[groupVar]])) +
          geom_density(alpha = profile_alpha) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = if(nzchar(labels$x)) labels$x else "Response (1-5)",
               y = if(nzchar(labels$y)) labels$y else "Density",
               fill = labels$fill, color = labels$fill,
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill() + pal_fun_colour() +
          scale_x_continuous(breaks = 1:5, labels = scales::wrap_format(10)(response_labels))
        if(input$dimension_choice == "All"){
          lab <- if(!is.null(input$language) && input$language == "Norwegian") {
            labeller(Dimension = eq5d_dim_nor)
          } else {
            labeller(Dimension = eq5d_dim_eng)
          }
          p <- p + facet_wrap(~ Dimension, labeller = custom_dim_labeller)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
      } else {
        p <- ggplot(df_long, aes(x = ResponseNum)) +
          geom_density(fill = "#A4A9AD", color = "#A4A9AD", alpha = profile_alpha) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = if(nzchar(labels$x)) labels$x else "Response (1-5)",
               y = if(nzchar(labels$y)) labels$y else "Density",
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill() +
          scale_x_continuous(breaks = 1:5, labels = scales::wrap_format(10)(response_labels))
        if(input$dimension_choice == "All"){
          lab <- if(!is.null(input$language) && input$language == "Norwegian") {
            labeller(Dimension = eq5d_dim_nor)
          } else {
            labeller(Dimension = eq5d_dim_eng)
          }
          p <- p + facet_wrap(~ Dimension, labeller = custom_dim_labeller)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
      }
      if(input$profile_coord_flip){
        p <- p + coord_flip()
      }
    }

    if(plotType == "stacked"){
      df_long <- df_long %>% mutate(Response = factor(Response, labels = response_labels))
      if(compareGroups){
        df_stack <- df_long %>%
          group_by(Dimension, Response, grp = .data[[groupVar]]) %>%
          summarise(count = n(), .groups = "drop") %>%
          group_by(Dimension, grp) %>%
          mutate(pct = count / sum(count) * 100,
                 cum_pct = cumsum(pct) - 0.5 * pct) %>%
          ungroup()
        p <- ggplot(df_stack, aes(x = grp, y = pct, fill = Response)) +
          geom_bar(stat = "identity", position = position_stack(reverse = TRUE),
                   color = "white", alpha = profile_alpha) +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = if(nzchar(labels$x)) labels$x else groupVar,
               y = if(nzchar(labels$y)) labels$y else "Percentage",
               fill = labels$fill,
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill() +
          guides(fill = guide_legend(reverse = TRUE))
        if(input$profile_show_pct){
          p <- p + geom_text(aes(y = cum_pct, label = paste0(round(pct,1), "%")),
                             color = input$profile_label_color, size = 4)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
        if(input$dimension_choice == "All"){
          lab <- if(!is.null(input$language) && input$language == "Norwegian") {
            labeller(Dimension = eq5d_dim_nor)
          } else {
            labeller(Dimension = eq5d_dim_eng)
          }
          p <- p + facet_wrap(~ Dimension, labeller = custom_dim_labeller)
        }
      } else {
        df_stack <- df_long %>%
          filter(!is.na(Response)) %>%
          count(Dimension, Response) %>%
          group_by(Dimension) %>%
          mutate(pct = prop.table(n) * 100) %>%
          ungroup() %>%
          group_by(Dimension) %>%
          arrange(Dimension, Response) %>%
          mutate(cum_pct = cumsum(pct) - 0.5 * pct) %>%
          ungroup()

        ## — relabel Dimension factor to full names based on language toggle —
        dim_labels <- if (input$language == "Norwegian") eq5d_dim_nor else eq5d_dim_eng
        df_stack <- df_stack %>%
          mutate(Dimension = factor(Dimension,
                                    levels = names(dim_labels),
                                    labels = dim_labels))
        p <- ggplot(df_stack, aes(x = pct, y = Dimension, fill = Response)) +
          geom_bar(stat = "identity", position = position_stack(reverse = TRUE),
                   color = "white", alpha = profile_alpha) +
          scale_x_continuous(labels = function(x) paste0(x, "%")) +
          labs(title = labels$title,
               subtitle = labels$subtitle,
               x = if(nzchar(labels$x)) labels$x else "",
               y = if(nzchar(labels$y)) labels$y else "",
               caption = labels$caption) +
          profile_theme +
          pal_fun_fill() +
          theme(legend.position = "bottom")
        if(input$profile_show_pct){
          p <- p + geom_text(aes(x = cum_pct, label = paste0(round(pct,1), "%")),
                             color = input$profile_label_color, size = 4)
        }
        if(!is.null(xlim_vals) || !is.null(ylim_vals)){
          p <- p + coord_cartesian(xlim = xlim_vals, ylim = ylim_vals)
        }
      }
      if(input$profile_coord_flip){
        p <- p + coord_flip()
      }
    }

    if (compareGroups && isTRUE(input$profile_group_n_caption)) {
      df_counts <- df_long %>%
        group_by(Dimension, grp = .data[[groupVar]]) %>%
        summarise(n = n(), .groups = "drop")

      caption_lines <- df_counts %>%
        group_by(Dimension) %>%
        summarise(
          line = paste0(
            first(Dimension), ": ",
            paste0(grp, ", n=", n, collapse = "; ")
          ),
          .groups = "drop"
        ) %>%
        pull(line)

      caption_text <- paste(caption_lines, collapse = "\n")
      p <- p + labs(caption = caption_text)
    }

    current_plot(p)
    print(p)
  })

  # ------------------------------
  # EQ VAS plots
  # ------------------------------
  output$vas_plot <- renderPlot({
    df <- current_data()
    req(df)
    groupVar <- input$vas_group
    compareGroups <- (groupVar != "None")
    plotType <- input$vas_plot_type
    labels_vas <- list(
      title = if(nzchar(input$vas_summary_title)) input$vas_summary_title else "EQ VAS Plot",
      subtitle = if(nzchar(input$vas_summary_subtitle)) input$vas_summary_subtitle else "",
      x = if(nzchar(input$vas_summary_xlabel)) input$vas_summary_xlabel else "EQ VAS",
      y = if(nzchar(input$vas_summary_ylabel)) input$vas_summary_ylabel else "Count",
      caption = if(nzchar(input$vas_summary_caption)) input$vas_summary_caption else NULL,
      fill = input$vas_summary_legend_title,
      base_size = input$vas_summary_base_size
    )
    vas_theme <- choose_theme(input$vas_theme, input$vas_summary_base_size)

    # if not faceting, append total-n to whatever subtitle we have
    if (input$vas_facet == "None") {
      total_n <- nrow(df)
      existing_sub <- labels_vas$subtitle
      labels_vas$subtitle <- if (nzchar(existing_sub)) {
        paste0(existing_sub, "\n", "n = ", total_n)
      } else {
        paste0("n = ", total_n)
      }
    }

    if(compareGroups){
      if(input$vas_color_mode == "qual"){
        pal_fun_fill <- get_palette_funcs(input$vas_qual_palette)$fill_fun
        pal_fun_colour <- get_palette_funcs(input$vas_qual_palette)$col_fun
      } else {
        pal_seq <- get_seq_palette_funcs(input$vas_seq_palette)
        pal_fun_fill <- pal_seq$fill_fun
        pal_fun_colour <- pal_seq$colour_fun
      }
    }

    vas_alpha <- input$vas_alpha
    vas_xlim_vals <- parse_limits(input$vas_xlim)
    vas_ylim_vals <- parse_limits(input$vas_ylim)

    xlab_orig <- labels_vas$x
    ylab_orig <- labels_vas$y
    if(input$vas_coord_flip_all){
      labels_vas$x <- ylab_orig
      labels_vas$y <- xlab_orig
    }

    if(plotType == "Histogram"){
      if(compareGroups){
        p <- ggplot(df, aes(x = EQ_VAS, fill = .data[[groupVar]])) +
          geom_histogram(position = "dodge", binwidth = input$vas_binwidth, color = "white", alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill) +
          vas_theme + pal_fun_fill() + pal_fun_colour()
      } else {
        p <- ggplot(df, aes(x = EQ_VAS)) +
          geom_histogram(binwidth = input$vas_binwidth, fill = input$vas_manual_fill, color = "white", alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y) +
          vas_theme
      }
      if(!is.null(vas_xlim_vals) || !is.null(vas_ylim_vals)){
        p <- p + coord_cartesian(xlim = vas_xlim_vals, ylim = vas_ylim_vals)
      }

    } else if(plotType == "Density"){
      if(compareGroups){
        p <- ggplot(df, aes(x = EQ_VAS, fill = .data[[groupVar]], color = .data[[groupVar]])) +
          geom_density(alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill, color = labels_vas$fill) +
          vas_theme + pal_fun_fill() + pal_fun_colour()
      } else {
        p <- ggplot(df, aes(x = EQ_VAS)) +
          geom_density(fill = input$vas_manual_fill, color = input$vas_manual_fill, alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y) +
          vas_theme
      }
      if(!is.null(vas_xlim_vals) || !is.null(vas_ylim_vals)){
        p <- p + coord_cartesian(xlim = vas_xlim_vals, ylim = vas_ylim_vals)
      }

    } else if(plotType == "Raincloud"){
      if(compareGroups){
        p <- ggplot(df, aes(x = factor(.data[[groupVar]]), y = EQ_VAS,
                            fill = .data[[groupVar]], color = .data[[groupVar]])) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill, color = labels_vas$fill) +
          vas_theme
        p <- p + pal_fun_fill() + pal_fun_colour()
      } else {
        p <- ggplot(df, aes(x = factor(1), y = EQ_VAS,
                            fill = I(input$vas_manual_fill), color = I("black"))) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill, color = labels_vas$fill) +
          vas_theme
      }
      pos_box <- list(position = ggpp::position_dodgenudge(x = 0.09, width = 0.12))
      point_args <- list(alpha = 0.08, size = 1.6, shape = 21)
      p <- p + geom_rain(alpha = vas_alpha,
                         point.args = point_args,
                         boxplot.args = list(color = "black", outlier.shape = NA, alpha = vas_alpha,
                                             width = if(compareGroups) 0.1 else 0.05),
                         boxplot.args.pos = pos_box)

    } else if(plotType == "Boxplot"){
      if(compareGroups){
        p <- ggplot(df, aes(x = factor(.data[[groupVar]]), y = EQ_VAS,
                            fill = .data[[groupVar]], color = .data[[groupVar]])) +
          geom_boxplot(width = 0.5, alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill, color = labels_vas$fill) +
          vas_theme + pal_fun_fill() + pal_fun_colour()
      } else {
        p <- ggplot(df, aes(x = factor(1), y = EQ_VAS)) +
          geom_boxplot(width = 0.5, alpha = vas_alpha, fill = input$vas_manual_fill, color = "black") +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y) +
          vas_theme
      }

    } else if(plotType == "Violin"){
      if(compareGroups){
        p <- ggplot(df, aes(x = factor(.data[[groupVar]]), y = EQ_VAS,
                            fill = .data[[groupVar]], color = .data[[groupVar]])) +
          geom_violin(alpha = vas_alpha) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y,
               fill = labels_vas$fill, color = labels_vas$fill) +
          vas_theme + pal_fun_fill() + pal_fun_colour()
      } else {
        p <- ggplot(df, aes(x = factor(1), y = EQ_VAS)) +
          geom_violin(alpha = vas_alpha, fill = input$vas_manual_fill, color = input$vas_manual_fill) +
          labs(title = labels_vas$title,
               subtitle = labels_vas$subtitle,
               x = labels_vas$x, y = labels_vas$y) +
          vas_theme
      }

    } else if(plotType == "Trend Line"){
      xvar <- input$vas_xvar
      if(!is.numeric(df[[xvar]])){
        df <- df %>% mutate(.xvar_numeric = as.numeric(factor(.data[[xvar]])))
        xvar_plot <- ".xvar_numeric"
        x_levels <- levels(factor(df[[xvar]]))
      } else {
        xvar_plot <- xvar
      }

      if(compareGroups){
        p <- ggplot(df, aes(x = .data[[xvar_plot]], y = EQ_VAS, color = .data[[groupVar]]))
      } else {
        p <- ggplot(df, aes(x = .data[[xvar_plot]], y = EQ_VAS))
      }

      trend_type <- input$vas_trend_type
      if(trend_type == "observed"){
        if(compareGroups){
          agg_data <- df %>% group_by(xvar_val = .data[[xvar_plot]], grp = .data[[groupVar]]) %>%
            summarise(mean_score = mean(EQ_VAS, na.rm = TRUE), .groups = "drop")
          p <- p + geom_line(data = agg_data,
                             aes(x = xvar_val, y = mean_score, group = grp, color = grp),
                             linewidth = input$vas_line_width, alpha = vas_alpha) +
            geom_point(data = agg_data,
                       aes(x = xvar_val, y = mean_score, group = grp, color = grp),
                       size = input$vas_point_size, alpha = vas_alpha) +
            vas_theme
          p <- p + pal_fun_fill() + pal_fun_colour()
        } else {
          agg_data <- df %>% group_by(xvar_val = .data[[xvar_plot]]) %>%
            summarise(mean_score = mean(EQ_VAS, na.rm = TRUE), .groups = "drop")
          p <- p + geom_line(data = agg_data,
                             aes(x = xvar_val, y = mean_score, group = 1),
                             linewidth = input$vas_line_width, alpha = vas_alpha,
                             color = input$vas_manual_fill) +
            geom_point(data = agg_data,
                       aes(x = xvar_val, y = mean_score, group = 1),
                       size = input$vas_point_size, alpha = vas_alpha,
                       color = input$vas_manual_fill) +
            vas_theme
        }
      } else {
        if(compareGroups){
          if(trend_type %in% c("lm", "loess")){
            if(input$vas_show_ci){
              p <- p + geom_smooth(
                aes(color = .data[[groupVar]], fill = .data[[groupVar]]),
                method = trend_type, se = TRUE,
                linewidth = input$vas_line_width,
                alpha = input$vas_ci_alpha,
                show.legend = TRUE
              ) + guides(fill = "none")
            }
            p <- p + geom_smooth(
              aes(color = .data[[groupVar]]),
              method = trend_type, se = FALSE,
              linewidth = input$vas_line_width,
              alpha = vas_alpha,
              show.legend = FALSE
            )
          } else {
            p <- p + geom_smooth(
              aes(color = .data[[groupVar]]),
              method = trend_type, se = input$vas_show_ci,
              linewidth = input$vas_line_width, alpha = vas_alpha
            )
          }
          if(input$vas_show_points)
            p <- p + geom_point(
              aes(x = .data[[xvar_plot]], y = EQ_VAS, color = .data[[groupVar]]),
              size = input$vas_point_size, alpha = vas_alpha
            )
          p <- p + vas_theme + pal_fun_fill() + pal_fun_colour()
        } else {
          if(trend_type %in% c("lm", "loess")){
            p <- p + geom_smooth(method = trend_type, se = input$vas_show_ci,
                                 linewidth = input$vas_line_width, alpha = input$vas_ci_alpha, color = input$vas_manual_fill)
          } else {
            p <- p + geom_smooth(method = trend_type, se = input$vas_show_ci,
                                 linewidth = input$vas_line_width, alpha = vas_alpha, color = input$vas_manual_fill)
          }
          if(input$vas_show_points)
            p <- p + geom_point(aes(x = .data[[xvar_plot]], y = EQ_VAS),
                                size = input$vas_point_size, alpha = vas_alpha, color = input$vas_manual_fill)
          p <- p + vas_theme
        }
      }
      if(!is.numeric(df[[xvar]]))
        p <- p + scale_x_continuous(breaks = seq_along(x_levels), labels = x_levels)
      p <- p + labs(title = labels_vas$title,
                    subtitle = labels_vas$subtitle,
                    x = if(nzchar(input$vas_summary_xlabel)) input$vas_summary_xlabel else xvar,
                    y = "EQ VAS",
                    color = if(compareGroups) input$vas_summary_legend_title else NULL)
    }

    if(input$vas_coord_flip_all)
      p <- p + coord_flip()
    if(!is.null(vas_xlim_vals) || !is.null(vas_ylim_vals))
      p <- p + coord_cartesian(xlim = vas_xlim_vals, ylim = vas_ylim_vals)
    # --- 1) build a labeller that adds “(n = …)” to each strip ---
    if (input$vas_facet != "None") {
      fv <- input$vas_facet
      facet_counts <- df %>%
        filter(!is.na(EQ_VAS)) %>%
        count(!!sym(fv))
      names(facet_counts) <- c("facet","n")
      facet_labeller <- as_labeller(function(x) {
        paste0(x, " (n = ",
               facet_counts$n[match(x, facet_counts$facet)],
               ")")
      })

      facet_scales <- switch(input$vas_facet_scales,
                             "fixed" = "fixed",
                             "free_y" = "free_y",
                             "free_x" = "free_x")
      nrow_val <- if(input$vas_facet_nrow > 0) input$vas_facet_nrow else NULL
      ncol_val <- if(input$vas_facet_ncol > 0) input$vas_facet_ncol else NULL

      p <- p +
        facet_wrap(as.formula(paste("~", fv)),
                   scales   = facet_scales,
                   labeller = facet_labeller,
                   nrow     = nrow_val,
                   ncol     = ncol_val)
    } else {
      # no faceting → fallback to default labeller
      facet_labeller <- label_value
    }


    use_vline <- (plotType %in% c("Histogram", "Density"))
    if(input$ref_show_overall || (compareGroups && input$ref_show_groups) || input$ref_custom){
      stat_choice <- input$ref_statistic
      stat_fun <- if(stat_choice == "mean") mean else median
      if(input$ref_show_overall){
        if(input$vas_facet != "None"){
          ref_overall <- df %>% group_by_at(input$vas_facet) %>% summarise(stat = stat_fun(EQ_VAS, na.rm = TRUE))
          if(use_vline){
            p <- p + geom_vline(data = ref_overall, aes(xintercept = stat), color = input$ref_overall_color,
                                linetype = input$ref_overall_linetype, linewidth = input$ref_overall_linewidth, show.legend = FALSE)
          } else {
            p <- p + geom_hline(data = ref_overall, aes(yintercept = stat), color = input$ref_overall_color,
                                linetype = input$ref_overall_linetype, linewidth = input$ref_overall_linewidth, show.legend = FALSE)
          }
        } else {
          overall_stat <- stat_fun(df$EQ_VAS, na.rm = TRUE)
          if(use_vline){
            p <- p + geom_vline(xintercept = overall_stat, color = input$ref_overall_color,
                                linetype = input$ref_overall_linetype, linewidth = input$ref_overall_linewidth, show.legend = FALSE)
          } else {
            p <- p + geom_hline(yintercept = overall_stat, color = input$ref_overall_color,
                                linetype = input$ref_overall_linetype, linewidth = input$ref_overall_linewidth, show.legend = FALSE)
          }
        }
      }

      if(compareGroups && input$ref_show_groups){
        if(input$vas_facet != "None"){
          ref_group <- df %>% group_by_at(c(groupVar, input$vas_facet)) %>% summarise(stat = stat_fun(EQ_VAS, na.rm = TRUE))
          if(use_vline){
            p <- p + geom_vline(data = ref_group, aes(xintercept = stat, color = .data[[groupVar]]),
                                linetype = input$ref_group_linetype, linewidth = input$ref_group_linewidth, show.legend = FALSE)
          } else {
            p <- p + geom_hline(data = ref_group, aes(yintercept = stat, color = .data[[groupVar]]),
                                linetype = input$ref_group_linetype, linewidth = input$ref_group_linewidth, show.legend = FALSE)
          }
        } else {
          ref_group <- df %>% group_by_at(groupVar) %>% summarise(stat = stat_fun(EQ_VAS, na.rm = TRUE))
          if(use_vline){
            p <- p + geom_vline(data = ref_group, aes(xintercept = stat, color = .data[[groupVar]]),
                                linetype = input$ref_group_linetype, linewidth = input$ref_group_linewidth, show.legend = FALSE)
          } else {
            p <- p + geom_hline(data = ref_group, aes(yintercept = stat, color = .data[[groupVar]]),
                                linetype = input$ref_group_linetype, linewidth = input$ref_group_linewidth, show.legend = FALSE)
          }
        }
      }

      if(input$ref_custom){
        custom_vals <- as.numeric(unlist(strsplit(input$ref_custom_values, split = ",")))
        custom_vals <- custom_vals[!is.na(custom_vals)]
        if(length(custom_vals) > 0){
          for(val in custom_vals){
            if(use_vline){
              p <- p + geom_vline(xintercept = val, color = input$ref_custom_color,
                                  linetype = input$ref_custom_linetype, linewidth = input$ref_custom_linewidth)
            } else {
              p <- p + geom_hline(yintercept = val, color = input$ref_custom_color,
                                  linetype = input$ref_custom_linetype, linewidth = input$ref_custom_linewidth)
            }
          }
        }
      }
    }

    if (compareGroups && isTRUE(input$vas_group_n_caption)) {
      # compute per‐facet, per‐group counts
      df_counts <- df %>%
        filter(!is.na(EQ_VAS)) %>%
        count(!!sym(input$vas_facet), grp = .data[[groupVar]])
      names(df_counts)[1] <- "facet"

      # build one caption line per facet
      caption_lines <- df_counts %>%
        distinct(facet, grp, n) %>%
        group_by(facet) %>%
        summarise(
          line = paste0(
            first(facet), ": ",
            paste0(grp, ", n=", n, collapse = "; ")
          ),
          .groups = "drop"
        ) %>%
        pull(line)

      caption_text <- paste(caption_lines, collapse = "\n")
      p <- p + labs(caption = caption_text)
    }

    p <- p + vas_theme
    current_plot(p)
    print(p)
  })


  observeEvent(input$download_btn, {
    showModal(modalDialog(
      title = "Download Figure",
      radioButtons("download_format", "Select File Format:",
                   choices = c("PDF", "PNG"), selected = "PDF"),
      conditionalPanel(
        condition = "input.download_format == 'PNG'",
        selectInput("download_dpi", "Select DPI:",
                    choices = c(300, 600, 900, 1200, 1500, 1800, 2100), selected = 1500)
      ),
      numericInput("download_width", "Width (inches):", value = 10, min = 1),
      numericInput("download_height", "Height (inches):", value = 6, min = 1),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_plot", "Download Figure"),
        helpText(tags$small("Set 'Width' and 'Height' to get the appropriate size of the figure.
                            As a default, for non-paneled figures, 'Width' = 10, and 'Height' = 6 may be appropriate.
                            For multipaneled figures, a sensible default may be 'Width' = 14 and 'Height' = 8 (depending on the number of panels/group comparisons)"))
      ),
      easyClose = TRUE
    ))
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      ext <- if (input$download_format == "PDF") "pdf" else "png"
      paste0("Figure.", ext)
    },
    content = function(file) {
      plot_obj <- current_plot()
      req(plot_obj)
      width  <- input$download_width
      height <- input$download_height
      if (input$download_format == "PDF") {
        pdf(file, width = width, height = height)
        print(plot_obj)
        dev.off()
      } else {
        png(file, width = width, height = height, units = "in", res = as.numeric(input$download_dpi))
        print(plot_obj)
        dev.off()
      }
    }
  )
}

# ------------------------------
# Run the App
# ------------------------------
shinyApp(ui = ui, server = server)
