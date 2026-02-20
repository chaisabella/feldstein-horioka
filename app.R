# app.R
# Feldstein–Horioka Shiny App (WEO-based)
# From scratch (clean + polished):
# - Filters: Sample (Balanced/Unbalanced), country groups, year range
# - Dropdown with checkboxes for countries (shinyWidgets::pickerInput)
# - Toggle: country averages (cross-section) vs country-year panel points
# - Interactive scatter with hover tooltips (plotly)
# - Coefficient card showing beta, SE, R^2, N (updates with filters)
# - Time-series panel for selected (or auto-picked) countries

# =========================
# Packages
# =========================
library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(DT)

# =========================
# Load data (project root OR data/)
# =========================
# You said you want "everything in the data file, no subfolders".
# If your CSV is in the project root, use:
#   fh <- read_csv("fh_panel_with_sample.csv", show_col_types = FALSE)
# If your CSV is in data/, use:
#   fh <- read_csv("data/fh_panel_with_sample.csv", show_col_types = FALSE)

fh <- read_csv("data/fh_panel_with_sample.csv", show_col_types = FALSE) %>%
  mutate(
    year = as.integer(year),
    Sample = as.character(Sample),
    country = as.character(country)
  )

# Countries with full 1980–2025 coverage (identified by Sample == "Balanced" rows)
balanced_country_set <- fh %>%
  filter(Sample == "Balanced") %>%
  distinct(country) %>%
  pull(country)

# =========================
# Country group definitions (custom)
# =========================
groups <- list(
  "All countries" = NULL,
  "OECD (F-H original)" = c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Greece","Iceland","Ireland","Italy","Japan","Luxembourg","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","United Kingdom","United States"),
  "OECD (current)" = c("Australia","Austria","Belgium","Canada","Chile","Colombia","Costa Rica","Czech Republic","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea, Republic of","Latvia","Lithuania","Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovak Republic","Slovenia","Spain","Sweden","Switzerland","Türkiye","United Kingdom","United States"),
  "G7" = c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States"),
  "ASEAN" = c("Indonesia", "Malaysia", "Philippines", "Thailand", "Vietnam", "Singapore"),
  "BRICS" = c("Brazil", "Russian Federation", "India", "China, People's Republic of", "South Africa")
)

# =========================
# UI
# =========================
ui <- fluidPage(
  titlePanel("Feldstein–Horioka Explorer (WEO: Saving & Investment, % GDP)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sample",
        label   = "Sample (dataset label)",
        choices = sort(unique(fh$Sample)),
        selected = if ("Unbalanced" %in% unique(fh$Sample)) "Unbalanced" else unique(fh$Sample)[1]
      ),
      
      
      selectInput(
        inputId = "group",
        label   = "Country group",
        choices = names(groups),
        selected = "All countries"
      ),
      
       
      # Dropdown with checkboxes (Tableau-like)
      pickerInput(
        inputId = "countries",
        label   = "Countries (dropdown w/ checkboxes)",
        choices = sort(unique(fh$country)),  # will be updated dynamically after filters
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,   # Select All / Deselect All
          `live-search` = TRUE,   # Search bar
          size = 10
        )
      ),
      
      sliderInput(
        inputId = "years",
        label   = "Year range",
        min     = min(fh$year, na.rm = TRUE),
        max     = max(fh$year, na.rm = TRUE),
        value   = c(1980, 2025),
        step    = 1,
        sep     = ""
      ),
      # pickerInput(
      #   inputId = "years",
      #   label   = "Years (select specific years)",
      #   choices = sort(unique(fh$year)),
      #   selected = 1980:2025,
      #   multiple = TRUE,
      #   options = list(
      #     `actions-box` = TRUE,
      #     `live-search` = FALSE,
      #     size = 10,
      #     `selected-text-format` = "count > 3",
      #     `count-selected-text` = "{0} years selected"
      #   )
      # ),
      
      checkboxInput("use_avg", "Use country averages over selected years (cross-section)", TRUE),
      checkboxInput("show_fit", "Show regression line (lm)", TRUE),
      
      hr(),
      helpText("Hover on points in the scatter to see country + values (Tableau-style tooltip).")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Scatter (FH)",
          tags$h4("Feldstein–Horioka Coefficient (updates with filters)"),
          uiOutput("fh_coef_box"),
          br(),
          plotlyOutput("fh_scatter", height = "520px"),
          hr(),
          tags$h4("Full Regression Output"),
          verbatimTextOutput("fh_lm_summary")
        ),

        tabPanel(
          "Data Preview",
          tableOutput("preview")
        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # -------------------------
  # Filter by sample + year + (optional) balanced-only + group
  # -------------------------
  data_filtered <- reactive({
    d <- fh %>%
      filter(Sample == input$sample) %>%
      filter(year >= input$years[1], year <= input$years[2])
    
    if (isTRUE(input$balanced_only_countries)) {
      d <- d %>% filter(country %in% balanced_country_set)
    }
    
    keep <- groups[[input$group]]
    if (!is.null(keep)) {
      d <- d %>% filter(country %in% keep)
    }
    
    d
  })
  
  # -------------------------
  # Dynamically update the country dropdown choices after filters
  # (keeps the list short and relevant)
  # -------------------------
  observeEvent(data_filtered(), {
    available <- sort(unique(data_filtered()$country))
    
    updatePickerInput(
      session = session,
      inputId = "countries",
      choices = available,
      selected = intersect(input$countries %||% character(0), available)
    )
  }, ignoreInit = TRUE)
  
  
  # Keep picker choices in sync with filtered data (don’t change selection unless needed)

  
  # When group changes, auto-select that group's countries (so you can see what's showing)
  observeEvent(input$group, {
    available <- sort(unique(data_filtered()$country))
    
    # desired selection = group's members, but only those that exist under current sample/years
    desired <- if (input$group == "All countries" || is.null(groups[[input$group]])) {
      character(0)  # keep empty -> your app interprets this as "no country filter = show all"
    } else {
      intersect(groups[[input$group]], available)
    }
    
    updatePickerInput(
      session = session,
      inputId = "countries",
      choices = available,
      selected = desired
    )
  }, ignoreInit = TRUE)
  # -------------------------
  # Optional country filter (based on picker selection)
  # If none selected, keep all countries in filtered data.
  # -------------------------
  data_final <- reactive({
    d <- data_filtered()
    
    if (!is.null(input$countries) && length(input$countries) > 0) {
      d <- d %>% filter(country %in% input$countries)
    }
    
    d
  })
  
  # -------------------------
  # Data used for scatter:
  # - Cross-section: country averages within selected years
  # - Panel: all country-year points
  # Adds tooltip text for plotly hover
  # -------------------------
  data_for_scatter <- reactive({
    d <- data_final()
    validate(need(nrow(d) > 0, "No data after filters. Try expanding year range or loosening filters."))
    
    if (isTRUE(input$use_avg)) {
      d %>%
        group_by(country) %>%
        summarise(
          saving_gdp = mean(saving_gdp, na.rm = TRUE),
          investment_gdp = mean(investment_gdp, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          tooltip = paste0(
            "<b>", country, "</b>",
            "<br>Saving (% GDP): ", sprintf("%.2f", saving_gdp),
            "<br>Investment (% GDP): ", sprintf("%.2f", investment_gdp),
            "<br><i>Avg ", input$years[1], "–", input$years[2], "</i>"
          )
        )
    } else {
      d %>%
        mutate(
          tooltip = paste0(
            "<b>", country, "</b>",
            "<br>Year: ", year,
            "<br>Saving (% GDP): ", sprintf("%.2f", saving_gdp),
            "<br>Investment (% GDP): ", sprintf("%.2f", investment_gdp)
          )
        )
    }
  })
  
  # -------------------------
  # Coefficient "card"
  # -------------------------
  output$fh_coef_box <- renderUI({
    d <- data_for_scatter()
    validate(need(nrow(d) >= 5, "Not enough observations for regression."))
    
    fit <- lm(investment_gdp ~ saving_gdp, data = d)
    s <- summary(fit)
    
    beta  <- coef(fit)[["saving_gdp"]]
    se    <- s$coefficients["saving_gdp", "Std. Error"]
    r2    <- s$r.squared
    n_obs <- nrow(d)
    
    tags$div(
      style = "padding:12px 14px; border:1px solid #ddd; border-radius:10px; background:#fafafa;",
      tags$h2(style = "margin:0;", sprintf("β = %.3f", beta)),
      tags$div(style = "margin-top:6px;",
               sprintf("Std. Error: %.3f | R²: %.3f | N: %d", se, r2, n_obs))
    )
  })
  
  # -------------------------
  # Interactive scatter with hover tooltips + regression line
  # IMPORTANT: map tooltip text ONLY in geom_point (not global aes)
  # so geom_smooth draws reliably under ggplotly
  # -------------------------
  output$fh_scatter <- renderPlotly({
    d <- data_for_scatter()
    validate(need(nrow(d) >= 5, "Not enough observations after filters. Try selecting more countries/years."))
    
    title_txt <- if (isTRUE(input$use_avg)) {
      paste0("FH Cross-Section (country averages, ", input$years[1], "–", input$years[2], ")")
    } else {
      paste0("FH Panel Scatter (country-year, ", input$years[1], "–", input$years[2], ")")
    }
    
    subtitle_txt <- paste0(
      "Sample: ", input$sample,
      if (isTRUE(input$balanced_only_countries)) " | Balanced countries only" else "",
      " | Group: ", input$group
    )
    
    p <- ggplot(d, aes(x = saving_gdp, y = investment_gdp)) +
      geom_point(aes(text = tooltip), alpha = 0.7) +
      labs(
        title = title_txt,
        subtitle = subtitle_txt,
        x = "Gross national saving (% GDP)",
        y = "Gross capital formation (% GDP)"
      )
    
    if (isTRUE(input$show_fit)) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  # -------------------------
  # Full regression output (text)
  # -------------------------
  output$fh_lm_summary <- renderPrint({
    d <- data_for_scatter()
    validate(need(nrow(d) >= 5, "Not enough observations for regression after filters."))
    
    summary(lm(investment_gdp ~ saving_gdp, data = d))
  })
  
  
  # -------------------------
  # Data preview
  # -------------------------
  output$preview <- renderTable({
    data_final() %>%
      arrange(country, year)
  })
}

shinyApp(ui, server)