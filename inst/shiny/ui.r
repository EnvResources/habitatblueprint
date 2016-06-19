shinyUI(navbarPage("Habitat Blueprint Browser",
  # explanation of project and interface
  tabPanel("Description",
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
  tabPanel("Explore Transect",
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
              "Dissolved Oxygen Habitat"  = "oa.qual"), width = "50%")
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
  tabPanel("Explore Period",
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
        plotOutput("period_bydepth", height = "1200px")
      )
    )
  )
))
