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
        h3(selectInput("habitat_type", NULL, 
            choices = c("Overall Habitat" = "habitat", 
              "Temperature Habitat" = "ta.qual", 
              "Salinity Habitat"  = "sa.qual", 
              "Dissolved Oxygen Habitat"  = "oa.qual",
              "Temperature (raw)" = "ta", "Salinity (raw)" = "sa",
              "Dissolved Oxygen (raw)" = "oa"), width = "50%")
        ),
        plotOutput("grid_plot"),
        fluidRow(
          column(6, 
            plotOutput("category_bar")
          ),
          column(6,
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
            choices = c("Overall Habitat" = "habitat", 
              "Temperature Habitat" = "ta.qual", 
              "Salinity Habitat"  = "sa.qual", 
              "Dissolved Oxygen Habitat"  = "oa.qual"), width = "50%")),
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
        numericInput("perturb_val", "Adjust by", width = "50%", value = 0),
        actionButton("perturb_action", "Perturb", width = "25%"),
        actionButton("perturb_reset", "Reset", width = "25%")
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
