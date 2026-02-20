---
title: "space mission project"
author: "PARSA MIRPOUR"
date: "`r format(Sys.Date(), '%D')`"
output:
  html_document:
    toc: yes            # table of contents
    toc_depth: 4        # toc will include headers <= ####
    toc_float: yes      # toc always on left of page
    code_folding: show  # allows hiding of code
runtime: shiny
---

```{r}
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
```


```{r}
# Space Missions Dashboard

missions <- read_csv("space_missions.csv", show_col_types = FALSE) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Price = suppressWarnings(as.numeric(Price))
  ) # mission data


# Required Functions
getMissionCountByCompany <- function(companyName) {
  missions %>%
    filter(Company == companyName) %>%
    nrow()
}

getSuccessRate <- function(companyName) {
  company_data <- missions %>% filter(Company == companyName)
  if (nrow(company_data) == 0) return(0.0)
  rate <- sum(company_data$MissionStatus == "Success", na.rm = TRUE) / nrow(company_data) * 100
  round(rate, 2)
}

getMissionsByDateRange <- function(startDate, endDate) {
  start <- as.Date(startDate)
  end   <- as.Date(endDate)
  missions %>%
    filter(Date >= start, Date <= end) %>%
    arrange(Date) %>%
    pull(Mission)
}

getTopCompaniesByMissionCount <- function(n) {
  missions %>%
    count(Company, name = "missionCount") %>%
    arrange(desc(missionCount), Company) %>%
    head(n) %>%
    { Map(c, .$Company, .$missionCount) } %>%
    lapply(function(x) list(x[[1]], as.integer(x[[2]])))
}

getMissionStatusCount <- function() {
  statuses <- c("Success", "Failure", "Partial Failure", "Prelaunch Failure")
  counts <- sapply(statuses, function(s) sum(missions$MissionStatus == s, na.rm = TRUE))
  as.list(counts)
}

getMissionsByYear <- function(year) {
  missions %>% filter(Year == year) %>% nrow()
}

getMostUsedRocket <- function() {
  missions %>%
    count(Rocket, name = "n") %>%
    arrange(desc(n), Rocket) %>%
    slice(1) %>%
    pull(Rocket)
}

getAverageMissionsPerYear <- function(startYear, endYear) {
  count <- missions %>%
    filter(Year >= startYear, Year <= endYear) %>%
    count(Year, name = "n")
  if (nrow(count) == 0) return(0.0)
  round(mean(count$n), 2)
}


# UI
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(
      # Logo: 3 overlapping circles
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg", width = "36", height = "26",
        viewBox = "0 0 36 26", style = "vertical-align:middle; margin-right:8px;",
        tags$circle(cx = "10", cy = "13", r = "10", fill = "none", stroke = "#4fc3f7", `stroke-width` = "2.2", `fill-opacity` = "0.18", style = "fill:#4fc3f7;fill-opacity:0.25"),
        tags$circle(cx = "18", cy = "13", r = "10", fill = "none", stroke = "#81d4fa", `stroke-width` = "2.2", style = "fill:#81d4fa;fill-opacity:0.25"),
        tags$circle(cx = "26", cy = "13", r = "10", fill = "none", stroke = "#b3e5fc", `stroke-width` = "2.2", style = "fill:#b3e5fc;fill-opacity:0.25")
      ),
      tags$span("Space Missions", style = "font-family: 'Orbitron', sans-serif; font-size: 16px; letter-spacing: 2px; color: #e0f7fa;")
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 260,
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&family=Share+Tech+Mono&display=swap"),
      tags$style(HTML("
        body, .content-wrapper, .main-sidebar, .sidebar { background-color: #0a0e1a !important; }
        .skin-black .main-header .logo { background-color: #0d1226 !important; border-bottom: 1px solid #1e3a5f; }
        .skin-black .main-header .navbar { background-color: #0d1226 !important; }
        .skin-black .main-sidebar { background-color: #0d1226 !important; }
        .skin-black .sidebar-menu > li.active > a,
        .skin-black .sidebar-menu > li:hover > a { background-color: #1a2a4a !important; border-left: 3px solid #4fc3f7 !important; }
        .sidebar-menu > li > a { color: #90caf9 !important; font-family: 'Share Tech Mono', monospace; font-size: 13px; letter-spacing: 1px; }
        .box { background-color: #0d1a2e !important; border: 1px solid #1e3a5f !important; border-radius: 6px; }
        .box-header { border-bottom: 1px solid #1e3a5f !important; }
        .box-title { color: #4fc3f7 !important; font-family: 'Orbitron', sans-serif !important; font-size: 13px !important; letter-spacing: 2px !important; }
        .small-box { border-radius: 8px !important; }
        .small-box h3 { font-family: 'Orbitron', sans-serif !important; }
        .small-box p { font-family: 'Share Tech Mono', monospace !important; font-size: 12px; }
        .bg-blue { background-color: #0d47a1 !important; }
        .bg-green { background-color: #1b5e20 !important; }
        .bg-yellow { background-color: #e65100 !important; }
        .bg-red { background-color: #b71c1c !important; }
        .content-wrapper { background-color: #0a0e1a !important; }
        .dataTables_wrapper, table.dataTable { color: #b0bec5 !important; font-family: 'Share Tech Mono', monospace; font-size: 12px; }
        table.dataTable thead th { background-color: #0d1a2e !important; color: #4fc3f7 !important; border-bottom: 1px solid #1e3a5f !important; }
        table.dataTable tbody tr { background-color: #0a0e1a !important; }
        table.dataTable tbody tr:hover { background-color: #1a2a4a !important; }
        .dataTables_filter input, .dataTables_length select { background-color: #0d1a2e !important; color: #90caf9 !important; border: 1px solid #1e3a5f !important; border-radius: 4px; }
        .form-control, .selectize-input { background-color: #0d1226 !important; color: #90caf9 !important; border: 1px solid #1e3a5f !important; }
        .selectize-dropdown { background-color: #0d1226 !important; color: #90caf9 !important; border: 1px solid #1e3a5f !important; }
        label { color: #90caf9 !important; font-family: 'Share Tech Mono', monospace; font-size: 12px; }
        .nav-tabs-custom { background: #0d1a2e !important; }
        .nav-tabs-custom > .nav-tabs > li.active > a { background-color: #0d1a2e !important; color: #4fc3f7 !important; border-top: 2px solid #4fc3f7 !important; font-family: 'Share Tech Mono', monospace; }
        .nav-tabs-custom > .nav-tabs > li > a { color: #546e7a !important; }
        ::-webkit-scrollbar { width: 6px; } ::-webkit-scrollbar-track { background: #0a0e1a; } ::-webkit-scrollbar-thumb { background: #1e3a5f; border-radius: 3px; }
        .shiny-input-container { margin-bottom: 12px; }
      "))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("rocket")),
      menuItem("Visualizations", tabName = "visuals", icon = icon("chart-bar")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table")),
      menuItem("Query Functions", tabName = "query", icon = icon("code"))
    ),
    tags$hr(style = "border-color: #1e3a5f; margin: 10px 15px;"),
    tags$div(style = "padding: 10px 15px;",
      tags$p(style = "color: #546e7a; font-family: 'Share Tech Mono'; font-size: 11px;", "FILTERS"),
      dateRangeInput("dateRange", "Date Range",
        start = min(missions$Date, na.rm = TRUE),
        end   = max(missions$Date, na.rm = TRUE),
        min   = min(missions$Date, na.rm = TRUE),
        max   = max(missions$Date, na.rm = TRUE)
      ),
      selectInput("companyFilter", "Company",
        choices  = c("All", sort(unique(missions$Company))),
        selected = "All"
      ),
      selectInput("statusFilter", "Mission Status",
        choices  = c("All", sort(unique(missions$MissionStatus))),
        selected = "All"
      ),
      selectInput("rocketStatusFilter", "Rocket Status",
        choices  = c("All", sort(unique(missions$RocketStatus))),
        selected = "All"
      )
    )
  ),

  dashboardBody(
    tabItems(

      # ── OVERVIEW TAB ──────────────────────────────────────────────
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("totalMissions", width = 3),
          valueBoxOutput("successRate",   width = 3),
          valueBoxOutput("totalCompanies", width = 3),
          valueBoxOutput("yearsActive",   width = 3)
        ),
        fluidRow(
          box(title = "MISSIONS PER YEAR", width = 8, plotlyOutput("yearlyTrendPlot", height = "320px")),
          box(title = "MISSION STATUS BREAKDOWN", width = 4, plotlyOutput("statusPiePlot", height = "320px"))
        ),
        fluidRow(
          box(title = "TOP 10 COMPANIES BY MISSION COUNT", width = 6, plotlyOutput("topCompaniesPlot", height = "300px")),
          box(title = "SUCCESS RATE — TOP 15 COMPANIES (≥20 MISSIONS)", width = 6, plotlyOutput("successRatePlot", height = "300px"))
        )
      ),

      # ── VISUALIZATIONS TAB ───────────────────────────────────────
      tabItem(tabName = "visuals",
        fluidRow(
          box(title = "LAUNCHES BY DECADE & COMPANY (TOP 8)", width = 12, plotlyOutput("decadePlot", height = "380px"))
        ),
        fluidRow(
          box(title = "ROCKET STATUS OVER TIME", width = 6, plotlyOutput("rocketStatusPlot", height = "320px")),
          box(title = "MISSION COST DISTRIBUTION (LOGGED)", width = 6, plotlyOutput("costPlot", height = "320px"))
        ),
        fluidRow(
          box(title = "LAUNCH FREQUENCY HEATMAP (YEAR × MONTH)", width = 12, plotlyOutput("heatmapPlot", height = "340px"))
        )
      ),

      # ── DATA EXPLORER TAB ────────────────────────────────────────
      tabItem(tabName = "data",
        box(title = "MISSION DATA TABLE", width = 12,
          DTOutput("missionTable")
        )
      ),

      # ── QUERY FUNCTIONS TAB ─────────────────────────────────────
      tabItem(tabName = "query",
        fluidRow(
          box(title = "getMissionCountByCompany()", width = 6,
            selectInput("qCompany1", "Company Name", choices = sort(unique(missions$Company))),
            verbatimTextOutput("qResult1")
          ),
          box(title = "getSuccessRate()", width = 6,
            selectInput("qCompany2", "Company Name", choices = sort(unique(missions$Company))),
            verbatimTextOutput("qResult2")
          )
        ),
        fluidRow(
          box(title = "getMissionsByDateRange()", width = 6,
            dateRangeInput("qDateRange", "Date Range",
              start = "1957-10-01", end = "1957-12-31",
              min = min(missions$Date, na.rm = TRUE),
              max = max(missions$Date, na.rm = TRUE)
            ),
            verbatimTextOutput("qResult3")
          ),
          box(title = "getTopCompaniesByMissionCount()", width = 6,
            numericInput("qN", "Top N", value = 5, min = 1, max = 50),
            verbatimTextOutput("qResult4")
          )
        ),
        fluidRow(
          box(title = "getMissionStatusCount()", width = 4,
            verbatimTextOutput("qResult5")
          ),
          box(title = "getMissionsByYear()", width = 4,
            numericInput("qYear", "Year", value = 2020, min = 1957, max = 2024),
            verbatimTextOutput("qResult6")
          ),
          box(title = "getMostUsedRocket()", width = 4,
            verbatimTextOutput("qResult7")
          )
        ),
        fluidRow(
          box(title = "getAverageMissionsPerYear()", width = 6,
            fluidRow(
              column(6, numericInput("qStartYear", "Start Year", value = 2010, min = 1957, max = 2024)),
              column(6, numericInput("qEndYear",   "End Year",   value = 2020, min = 1957, max = 2024))
            ),
            verbatimTextOutput("qResult8")
          )
        )
      )
    )
  )
)


# Server

server <- function(input, output, session) {

  # Filtered data
  filtered <- reactive({
    df <- missions
    df <- df %>% filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    if (input$companyFilter    != "All") df <- df %>% filter(Company == input$companyFilter)
    if (input$statusFilter     != "All") df <- df %>% filter(MissionStatus == input$statusFilter)
    if (input$rocketStatusFilter != "All") df <- df %>% filter(RocketStatus == input$rocketStatusFilter)
    df
  })

  # ── Value Boxes ──
  output$totalMissions <- renderValueBox({
    valueBox(nrow(filtered()), "TOTAL MISSIONS", icon = icon("rocket"), color = "blue")
  })
  output$successRate <- renderValueBox({
    df <- filtered()
    rate <- if (nrow(df) == 0) 0 else round(sum(df$MissionStatus == "Success") / nrow(df) * 100, 1)
    valueBox(paste0(rate, "%"), "SUCCESS RATE", icon = icon("check-circle"), color = "green")
  })
  output$totalCompanies <- renderValueBox({
    valueBox(n_distinct(filtered()$Company), "COMPANIES", icon = icon("building"), color = "yellow")
  })
  output$yearsActive <- renderValueBox({
    df <- filtered()
    yr <- if (nrow(df) == 0) 0 else max(df$Year, na.rm=TRUE) - min(df$Year, na.rm=TRUE) + 1
    valueBox(yr, "YEARS COVERED", icon = icon("calendar"), color = "red")
  })

  # ── Plots ──
  plotBg <- "#0d1a2e"
  plotPaper <- "#0a0e1a"
  gridCol <- "#1e3a5f"
  textCol <- "#90caf9"
  accentCol <- "#4fc3f7"

  applyTheme <- function(p) {
    p %>% layout(
      paper_bgcolor = plotPaper,
      plot_bgcolor  = plotBg,
      font = list(family = "Share Tech Mono", color = textCol, size = 11),
      xaxis = list(gridcolor = gridCol, zerolinecolor = gridCol, tickfont = list(color = textCol)),
      yaxis = list(gridcolor = gridCol, zerolinecolor = gridCol, tickfont = list(color = textCol)),
      legend = list(font = list(color = textCol), bgcolor = plotBg)
    )
  }

  output$yearlyTrendPlot <- renderPlotly({
    df <- filtered() %>% count(Year)
    plot_ly(df, x = ~Year, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = accentCol, width = 2),
            marker = list(color = accentCol, size = 5)) %>%
      applyTheme()
  })

  output$statusPiePlot <- renderPlotly({
    df <- filtered() %>% count(MissionStatus)
    cols <- c("#4fc3f7","#ef5350","#ffa726","#ab47bc")
    plot_ly(df, labels = ~MissionStatus, values = ~n, type = "pie",
            marker = list(colors = cols, line = list(color = plotBg, width = 2)),
            textfont = list(color = "#fff")) %>%
      applyTheme()
  })

  output$topCompaniesPlot <- renderPlotly({
    df <- filtered() %>% count(Company) %>% top_n(10, n) %>% arrange(n)
    plot_ly(df, x = ~n, y = ~reorder(Company, n), type = "bar", orientation = "h",
            marker = list(color = accentCol)) %>%
      applyTheme() %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Missions"))
  })

  output$successRatePlot <- renderPlotly({
    df <- filtered() %>%
      group_by(Company) %>%
      summarise(total = n(), success = sum(MissionStatus == "Success"), .groups = "drop") %>%
      filter(total >= 20) %>%
      mutate(rate = round(success / total * 100, 2)) %>%
      top_n(15, total) %>%
      arrange(rate)
    plot_ly(df, x = ~rate, y = ~reorder(Company, rate), type = "bar", orientation = "h",
            marker = list(color = ~rate, colorscale = list(c(0,"#ef5350"), c(0.5,"#ffa726"), c(1,"#66bb6a")))) %>%
      applyTheme() %>%
      layout(yaxis = list(title = ""), xaxis = list(title = "Success Rate (%)"))
  })

  output$decadePlot <- renderPlotly({
    top8 <- missions %>% count(Company) %>% top_n(8, n) %>% pull(Company)
    df <- filtered() %>%
      filter(Company %in% top8) %>%
      mutate(Decade = paste0(floor(Year / 10) * 10, "s")) %>%
      count(Decade, Company)
    plot_ly(df, x = ~Decade, y = ~n, color = ~Company, type = "bar") %>%
      applyTheme() %>%
      layout(barmode = "stack", xaxis = list(title = "Decade"), yaxis = list(title = "Missions"))
  })

  output$rocketStatusPlot <- renderPlotly({
    df <- filtered() %>% count(Year, RocketStatus)
    plot_ly(df, x = ~Year, y = ~n, color = ~RocketStatus, type = "scatter", mode = "lines") %>%
      applyTheme()
  })

  output$costPlot <- renderPlotly({
    df <- filtered() %>% filter(!is.na(Price), Price > 0)
    plot_ly(df, x = ~log10(Price), type = "histogram",
            marker = list(color = accentCol, line = list(color = plotBg, width = 0.5))) %>%
      applyTheme() %>%
      layout(xaxis = list(title = "log10(Price, USD millions)"), yaxis = list(title = "Count"))
  })

  output$heatmapPlot <- renderPlotly({
    df <- filtered() %>%
      mutate(Month = month(Date, label = TRUE)) %>%
      count(Year, Month) %>%
      tidyr::complete(Year, Month, fill = list(n = 0))
    plot_ly(df, x = ~Year, y = ~Month, z = ~n, type = "heatmap",
            colorscale = list(c(0,"#0d1a2e"), c(0.5,"#0d47a1"), c(1,"#4fc3f7"))) %>%
      applyTheme() %>%
      layout(xaxis = list(title = "Year"), yaxis = list(title = "Month"))
  })

  # ── Data Table ──
  output$missionTable <- renderDT({
    datatable(
      filtered() %>% select(Company, Mission, Date, Rocket, RocketStatus, MissionStatus, Price, Location),
      options = list(
        pageLength = 20, scrollX = TRUE,
        dom = "Bfrtip",
        columnDefs = list(list(targets = "_all", className = "dt-left"))
      ),
      filter = "top",
      rownames = FALSE
    )
  })

  # ── Query Function Outputs ──
  output$qResult1 <- renderPrint({ getMissionCountByCompany(input$qCompany1) })
  output$qResult2 <- renderPrint({ getSuccessRate(input$qCompany2) })
  output$qResult3 <- renderPrint({ getMissionsByDateRange(as.character(input$qDateRange[1]), as.character(input$qDateRange[2])) })
  output$qResult4 <- renderPrint({ getTopCompaniesByMissionCount(input$qN) })
  output$qResult5 <- renderPrint({ getMissionStatusCount() })
  output$qResult6 <- renderPrint({ getMissionsByYear(input$qYear) })
  output$qResult7 <- renderPrint({ getMostUsedRocket() })
  output$qResult8 <- renderPrint({ getAverageMissionsPerYear(input$qStartYear, input$qEndYear) })
}

shinyApp(ui, server)
```



