## load libraries
library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Gun Violence in the United States"),
  tabsetPanel(
             tabPanel("Documentation",
              includeMarkdown("summary_text.Rmd")     
             ),
             tabPanel("Gun Violence by Year",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "years",
                            "Year",
                            choices = list(
                              "2014" = "2014",
                              "2015" = "2015",
                              "2016" = "2016",
                              "2017" = "2017"
                            )
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("All Incidents",
                                     plotlyOutput("scatterplot"),
                                     textOutput("scatter_obs"),
                                     includeMarkdown("national_summary.Rmd")
                            ),
                            tabPanel("State and Population Proportion",
                                     plotlyOutput("stateplot"),
                                     plotlyOutput("proportionplot"),
                                     includeMarkdown("population_relativity.Rmd")
                            )
                          )
                        )
                      )
             ),
             tabPanel("National Trends in Gun Violence",
                      plotlyOutput("linegraph"),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "region",
                            "Region",
                            choices = list(
                              "South" = "South",
                              "Northeast" = "Northeast",
                              "North Central" = "North Central",
                              "West" = "West"
                            )
                          ),includeMarkdown("political_piechart.Rmd")
                        ),
                        mainPanel(
                          plotlyOutput("piechart")
                        )
                  
                ),
                includeMarkdown("national_trends.Rmd")
             )
    )
)