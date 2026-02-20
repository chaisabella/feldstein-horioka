# app.R
# Feldstein–Horioka Shiny App (WEO-based)
# From scratch (clean + polished):
# - Filters: Sample (Balanced/Unbalanced), country groups, year range
# - Dropdown with checkboxes for countries (shinyWidgets::pickerInput)
# - Toggle: country averages (cross-section) vs country-year panel points
# - Interactive scatter with hover tooltips (plotly)
# - Coefficient card showing beta, SE, R^2, N (updates with filters)
# - Data preview as scrollable/searchable table (DT)

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

# helper (you were using %||% without defining it)
`%||%` <- function(x, y) if (is.null(x)) y else x

# =========================
# Load data (project root OR data/)
# =========================
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
  titlePanel("Feldstein–Horioka Puzzle Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sample",
        label   = "Sample",
        choices = sort(unique(fh$Sample)),
        selected = if ("Balanced" %in% unique(fh$Sample)) "Balanced" else unique(fh$Sample)[1]
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
        label   = "Select countries",
        choices = sort(unique(fh$country)),  # updated dynamically after filters
        selected = sort(unique(fh$country)),
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `live-search-normalize` = TRUE,
          size = 10,
          noneSelectedText = "All countries"
        )
      ),
      
      
      # If you want the year picker instead of slider, comment sliderInput above and uncomment below.
      # NOTE: The server logic supports BOTH.
      pickerInput(
        inputId = "years",
        label   = "Select years",
        choices = sort(unique(fh$year)),
        selected = 1980:2025,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = FALSE,
          size = 10,
          noneSelectedText = "Select years"
        )
      ),
      
      checkboxInput("use_avg", "Use country averages over selected years (cross-section)", TRUE),
      checkboxInput("show_fit", "Show regression line (lm)", TRUE),
      
      hr(),
      helpText("Hover your mouse on the scatterplot to values for individual data points")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Scatter (FH)",
          tags$h4("Feldstein–Horioka Coefficient"),
          uiOutput("fh_coef_box"),
          br(),
          plotlyOutput("fh_scatter", height = "520px"),
          hr(),
          tags$h4("Full Regression Output"),
          verbatimTextOutput("fh_lm_summary")
        ),
        
        tabPanel(
          "Data Preview",
          DTOutput("preview")
        ),
        
        tabPanel(
          "About",
          br(),
          h4("What is the Feldstein–Horioka Puzzle?"),
          p("Feldstein and Horioka (1980) documented a strong empirical relationship between national saving and domestic investment across OECD countries."),
          p("In a world of perfect capital mobility, domestic investment should not depend strongly on domestic saving, because capital can flow freely across borders."),
          p("However, empirically, investment is highly correlated with saving — suggesting limited international capital mobility. This empirical finding became known as the Feldstein–Horioka puzzle."),
          br(),
          h4("Model Estimated in This App"),
          p("The app estimates the cross-country regression:"),
          div(style = "font-size:18px; font-weight:bold; margin-bottom:10px;",
              HTML("Investment/GDP = α + β · Saving/GDP")
          ),
          p("A β close to 1 suggests low capital mobility. A β closer to 0 suggests high capital mobility."),
          br(),
          h4("Data Source"),
          p("Saving and investment data are from the IMF World Economic Outlook (WEO) database."),
          p("Variables: Gross national saving (% GDP) and Gross capital formation (% GDP)."),
          br(),
          h4("Reference"),
          p("Feldstein, Martin, and Charles Horioka (1980). \"Domestic Saving and International Capital Flows.\" Economic Journal 90(358): 314–329.")
        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # Make year filtering consistent across:
  # - slider (length 2): interpret as full continuous range
  # - picker (length > 2): interpret as explicit year set
  years_vec <- reactive({
    y <- input$years
    if (is.null(y)) return(integer(0))
    y <- as.integer(y)
    if (length(y) == 2) seq.int(min(y), max(y)) else sort(y)
  })
  
  years_range <- reactive({
    y <- years_vec()
    c(min(y), max(y))
  })
  
  # -------------------------
  # Filter by sample + years + group
  # -------------------------
  data_filtered <- reactive({
    d <- fh %>%
      filter(Sample == input$sample) %>%
      filter(year %in% years_vec())
    
    keep <- groups[[input$group]]
    if (!is.null(keep)) d <- d %>% filter(country %in% keep)
    
    d
  })
  
  # -------------------------
  # Update country dropdown choices after filters (preserve current selection)
  # -------------------------
  observeEvent(data_filtered(), {
    available <- sort(unique(data_filtered()$country))
    current <- input$countries %||% character(0)
    
    updatePickerInput(
      session = session,
      inputId = "countries",
      choices = available,
      selected = intersect(current, available)
    )
  }, ignoreInit = FALSE)
  
  # When group changes, auto-select that group's countries (so you can see what's showing)
  observeEvent(input$group, {
    available <- sort(unique(data_filtered()$country))
    
    desired <- if (input$group == "All countries" || is.null(groups[[input$group]])) {
      character(0)  # empty means "no filter" => show all available
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
  # Data used for scatter + tooltip text
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
            "<br><i>Avg ", years_range()[1], "–", years_range()[2], "</i>"
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
  # -------------------------
  output$fh_scatter <- renderPlotly({
    d <- data_for_scatter()
    validate(need(nrow(d) >= 5, "Not enough observations after filters. Try selecting more countries/years."))
    
    title_txt <- if (isTRUE(input$use_avg)) {
      paste0("FH Cross-Section (country averages, ", years_range()[1], "–", years_range()[2], ")")
    } else {
      paste0("FH Panel Scatter (country-year, ", years_range()[1], "–", years_range()[2], ")")
    }
    
    subtitle_txt <- paste0("Sample: ", input$sample, " | Group: ", input$group)
    
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
  # Data preview (scroll/search/sort)
  # -------------------------
  output$preview <- renderDT({
    data_final() %>% arrange(country, year)
  }, options = list(
    pageLength = 25,
    lengthMenu = c(10, 25, 50, 100, 250, 500),
    scrollX = TRUE
  ), rownames = FALSE)
}

shinyApp(ui, server)