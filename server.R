# load libraries
library(shiny)
library(plotly)
library(dplyr)
# source file with state_gun_incidents function
source("states_summary.R")


# server for shiny app
server <- function(input, output) {
  # read in gun, census, and political data
  gun_violence <- read.csv("./data/guns_data.csv", stringsAsFactors = FALSE)
  census <- read.csv("./data/census_data.csv", stringsAsFactors = FALSE)
  political <- read.csv("./data/political_data.csv", stringsAsFactors = FALSE)

  # data frame for states and regions
  st_reg <- data.frame(state = state.name, Region = state.region, stringsAsFactors = FALSE)

  # joins state region data frame and political leaning data frame
  # add column for color based on party
  # used in pie chart
  st_reg <- left_join(st_reg, political)
  regions_data <- st_reg %>%
    group_by(Region, party) %>%
    count(party) %>%
    mutate(color_map = ifelse(party == "Republican", "rgb(221, 94, 96)",
      ifelse(party == "Democratic", "rgb(114, 147, 203)",
        ifelse(party == "Neutral", "rgb(128, 133, 133)", NA_real_)
      )
    ))

  # filters gun data based on year selection by user
  gun_data_input <- reactive({
    # year from radio button
    year <- input$years
    year_lower_bound <- as.Date(paste0(year, "-01-01"), "%Y-%m-%d")
    year_upper_bound <- as.Date(paste0(year, "-12-31"), "%Y-%m-%d")
    gun_violence <- gun_violence %>%
      filter(date >= year_lower_bound & date <= year_upper_bound)
    output$scatter_obs <- renderText({
      paste0(
        "For the year ", year, " there were: " , nrow(gun_violence), 
        " incidents of gun violence and ", sum(gun_violence$n_killed), 
        " people killed in the United States."
      )
    })
    gun_violence
  })

  # filters census data based on year selection by user
  census_data_input <- reactive({
    # year from radio button
    year <- input$years
    census <- census %>%
      select(STNAME, paste0("YEAR_", year))
    census
  })

  # region selection by user for pie chart
  region_input <- reactive({
    region_name <- input$region
  })

  # renders a scatterplot of gun violence incidents by year
  output$scatterplot <- renderPlotly({
    gun_violence_df <- gun_data_input()
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      showland = TRUE,
      landcolor = toRGB("gray85"),
      subunitwidth = 1,
      countrywidth = 1,
      subunitcolor = toRGB("white"),
      countrycolor = toRGB("white")
    )
    # plot the scatterplot
    p <- plot_geo(gun_violence_df,
      locationmode = "USA-states", sizes = c(1, 250),
      marker = list(size = 4)
    ) %>%
      add_markers(
        text = ~paste(
          "Number killed: ", gun_violence_df$n_killed,
          "</br> Number injured: ", gun_violence_df$n_injured
        ),
        x = ~longitude, y = ~latitude,
        alpha = 0.3
      ) %>%
      layout(title = "\n Gun Violence Incidents in U.S.", geo = g)
  })

  # render choropleth map of the U.S. states' gun violence incidents
  output$stateplot <- renderPlotly({
    gun_violence_df <- gun_data_input()
    state_obs <- state_gun_incidents(gun_violence_df) %>%
      left_join(political)
    state_obs$hover <- with(state_obs, paste0(state, "\n", party))
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      lakecolor = toRGB("white")
    )
    # plot the choropleth map
    p <- plot_geo(state_obs, locationmode = "USA-states") %>%
      add_trace(
        z = ~Incidents, text = ~hover, locations = ~code, color = ~Incidents, colors = "Blues"
      ) %>%
      layout(geo = g, title = "\n Incidents by State")
  })

  # render choropleth map of U.S. states' gun violence incidents
  # as a proportion out of 100,000 persons
  output$proportionplot <- renderPlotly({
    gun_violence_df <- gun_data_input()
    census_df <- census_data_input()
    census_df <- census_df %>% filter(STNAME != "District of Columbia")
    state_obs <- state_gun_incidents(gun_violence_df)
    state_obs <- state_obs %>%
      mutate(Incidents = unlist((state_obs$Incidents / census_df[2]) * 100000)) %>%
      left_join(political)
    state_obs$hover <- with(state_obs, paste0(state, "\n", party))
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa"),
      lakecolor = toRGB("white")
    )
    # plot the choropleth map
    p <- plot_geo(state_obs, locationmode = "USA-states") %>%
      add_trace(
        z = ~Incidents, text = ~hover, locations = ~code, color = ~Incidents, colors = "Blues"
      ) %>%
      layout(geo = g, title = "Incidents by State Population per 100,000 Persons")
  })

  # render line graph of gun violence incidents by year and U.S. region
  output$linegraph <- renderPlotly({
    region_data <- full_join(gun_violence, st_reg, by = "state")
    region_data$date <- as.Date(region_data$date)
    region_data$Incidents <- 1

    gun_2017 <- filter(region_data, date >= as.Date("2017-01-01") & date <= as.Date("2017-12-31"))
    gun_2016 <- filter(region_data, date >= as.Date("2016-01-01") & date <= as.Date("2016-12-31"))
    gun_2015 <- filter(region_data, date >= as.Date("2015-01-01") & date <= as.Date("2015-12-31"))
    gun_2014 <- filter(region_data, date >= as.Date("2014-01-01") & date <= as.Date("2014-12-31"))

    totals_2017 <- aggregate(Incidents ~ Region, FUN = sum, data = gun_2017)
    totals_2016 <- aggregate(Incidents ~ Region, FUN = sum, data = gun_2016)
    totals_2015 <- aggregate(Incidents ~ Region, FUN = sum, data = gun_2015)
    totals_2014 <- aggregate(Incidents ~ Region, FUN = sum, data = gun_2014)

    years <- c(rep("2017", 4), rep("2016", 4), rep("2015", 4), rep("2014", 4))
    totals_overview <- bind_rows(totals_2017, totals_2016, totals_2015, totals_2014)
    totals_overview$years <- years
    # plot the line graph
    p <- plot_ly(
      data = totals_overview, x = ~years, y = ~Incidents,
      color = ~Region, type = "scatter", mode = "lines"
    ) %>%
      layout(
        title = "\n Gun Violence Trends by Region 2014 to 2017", xaxis = list(title = "Year"),
        yaxis = list(title = "Annual Incidences of Gun Violence")
      )
  })

  # render pie chart of political party leaning by selected region
  output$piechart <- renderPlotly({
    selected_region <- region_input()
    selected_region_df <- regions_data %>%
      filter(Region == selected_region)
    # plot the pie chart
    p <- plot_ly(selected_region_df,
      labels = ~party, values = ~n, type = "pie",
      marker = list(
        colors = ~color_map,
        line = list(color = "#FFFFFF", width = 1)
      )
    ) %>%
      layout(
        title = "Political Party Leaning by Region",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
}
