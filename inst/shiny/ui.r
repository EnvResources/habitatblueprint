shinyUI(navbarPage("Habitat Blueprint Browser", id = "navbar",
  # explanation of project and interface
  tabPanel("Description", value = "info",
    tabsetPanel("stuff",
      tabPanel("welcome",
        includeMarkdown("welcome.md")
      ),
      tabPanel("available data",
        includeMarkdown("data.md")
      ),
      tabPanel("habitat definitions",
        withMathJax(),
        includeCSS("table.css"),
        includeMarkdown("habitat.md")
      ),
      tabPanel("technical details",
        includeMarkdown("technical.md")
      )
    )
  ),
  # explore single transect
  tabPanel("Explore Transect", value = "transect",
    sidebarLayout(
      sidebarPanel(
        uiOutput("transect_date"),
        plotOutput("transect_flows"),
        plotOutput("transect_tides"),
        plotOutput("transect_wll")
      ),
      mainPanel(
        tags$style(type = "text/css", ".selectize-dropdown-content {max-height: 400px;}"),
        h3(selectInput("habitat_type", NULL, 
            choices = c(
              "Overall Habitat (freshwater-acclimated)" = "habitat.fwa", 
              "Overall Habitat (saltwater-acclimated)" = "habitat.swa",
              "Temperature Habitat" = "ta.qual", 
              "Salinity Habitat"  = "sa.qual", 
              "Dissolved Oxygen Habitat"  = "oa.qual",
              "Temperature (raw)" = "ta", "Salinity (raw)" = "sa",
              "Dissolved Oxygen (raw)" = "oa"), width = "50%")
        ),
        plotOutput("grid_plot"),
        fluidRow(
          column(7, 
            plotOutput("category_bar")
          ),
          column(5,
            plotOutput("depth_vol")
          )
        ),
        fluidRow(
          column(11,
            plotOutput("depth_cat")
          ),
          column(1)
        )
      )
    )
  ),
  # explore period (e.g. single closure
  tabPanel("Explore Period", value = "period",
    sidebarLayout(
      sidebarPanel(
        uiOutput("period"),
        selectInput("plot_type", "plot type", choices = c("stacked area",
          "stacked bar"), selected = "stacked bar"),
        plotOutput("period_flows"),
        plotOutput("period_tides"),
        plotOutput("period_wll")
      ),
      mainPanel(
        h3(selectInput("period_habitat_type", NULL, 
            choices = c(
              "Overall Habitat (freshwater-acclimated)" = "habitat.fwa", 
              "Overall Habitat (saltwater-acclimated)" = "habitat.swa",
              "Temperature Habitat" = "ta.qual", 
              "Salinity Habitat"  = "sa.qual", 
              "Dissolved Oxygen Habitat"  = "oa.qual"), width = "60%")),
        plotOutput("period_overall"),
        plotOutput("period_alldepth"),
        plotOutput("period_bydepth", height = "1200px")
      )
    )
  ),
  tabPanel("Perturb Transect", value = "perturb",
    sidebarLayout(
      sidebarPanel(
        uiOutput("perturb_date"),
        sliderInput("window_dist", label = "Perturbation window", min = 0, 
          max = 12200, value = c(0, 12200), step = 100, ticks = FALSE),
        sliderInput("window_elev", label = NULL, min = -16, 
          max = 3, value = c(-16, 3), step = 0.1, ticks = FALSE),
        fluidRow(
          column(3,
            helpText("Adjust by")
          ),
          column(3,
            numericInput("perturb_val",  label = NULL, width = "100%", 
              value = 0)
          ),
          column(3,
            actionButton("perturb_action", "Perturb", width = "100%")
          ),
          column(3,
            actionButton("perturb_reset", "Reset", width = "100%")
          )
        )
      ),
      mainPanel(
        h3(selectInput("perturb_var", NULL, choices = c("Temperature" = "ta", 
          "Salinity" = "sa", "Dissolved Oxygen" = "oa"), width = "50%")),
        plotOutput("perturb_plot")
      )  
    )
  ),
  tabPanel("Quit", value = "stop")
))
