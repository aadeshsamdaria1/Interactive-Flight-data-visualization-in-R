# Load necessary libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(tidyr)
library(geosphere)
library(DT)
library(scales)
##########################################################################################################


# Load the datasets
airlines <- read.csv("airlines.csv")
airports <- read.csv("airports.csv")
flights_original <- read.csv("flights.csv")
flights <- flights_original

# Merge airlines and airports data to create a reference for origin and destination airports
airport_reference <- airports %>%
select(IATA_CODE, AIRPORT, CITY, STATE, COUNTRY, LATITUDE, LONGITUDE)

flights <- flights %>%
left_join(airport_reference, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
rename(
  ORIGIN_AIRPORT_NAME = AIRPORT,
  ORIGIN_CITY = CITY,
  ORIGIN_STATE = STATE,
  ORIGIN_COUNTRY = COUNTRY,
  ORIGIN_LATITUDE = LATITUDE,
  ORIGIN_LONGITUDE = LONGITUDE
) %>%
left_join(airport_reference, by = c("DESTINATION_AIRPORT" = "IATA_CODE")) %>%
rename(
  DESTINATION_AIRPORT_NAME = AIRPORT,
  DESTINATION_CITY = CITY,
  DESTINATION_STATE = STATE,
  DESTINATION_COUNTRY = COUNTRY,
  DESTINATION_LATITUDE = LATITUDE,
  DESTINATION_LONGITUDE = LONGITUDE
)
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Flight Performance Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graphs", tabName = "delayAnalysis", icon = icon("bar-chart")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Data Summary", tabName = "summary", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      # Graphs tab
      tabItem(
        tabName = "delayAnalysis",
        fluidRow(
          # Airlines average departure and arrival performance plot
          box(
            plotlyOutput("airlinesDelayPlot", height = 300),
            style = "box-shadow: 2px 2px 5px lightblue;",
            width = 12
          ),
          # Select airline filter
          box(
            selectInput("airlineFilter", "Select Airline", choices = unique(flights$AIRLINE)),
            width = 12,  # Set the box width to 12 to occupy the entire row
            style = "box-shadow: 2px 2px 5px lightblue;"
          ),
          # Monthly performance for a specific airline plot
          box(
            plotlyOutput("monthlyDelaysPlot", height = 300),
            style = "box-shadow: 2px 2px 5px lightblue;"
          ),
          # Specific airline delay cause pie chart
          box(
            plotlyOutput("delayCausesPlot", height = 300),
            style = "box-shadow: 2px 2px 5px lightblue;"
          ),
          # Top n airport select filter
          box(
            selectInput("nAirports", "Number of Airports to Display:",
                        choices = c("5", "10", "15", "All"),
                        selected = "10"
            ),
            width = 12,
            style = "box-shadow: 2px 2px 5px lightblue;"
          ),
          # top n airports plot for specific airline
          box(
            plotlyOutput("averageAirportDelayPlot", height = 300),
            style = "box-shadow: 2px 2px 5px lightblue;", width = 12
          ),
        )
      ),
      
      # Map tab
      tabItem(
        tabName = "map",
        # Select month filter
        box(
          selectInput("month", "Month:",
                      choices = list("All" = 99, 
                                     "Jan" = 1, "Feb" = 2, "Mar" = 3,
                                     "Apr" = 4, "May" = 5, "Jun" = 6,
                                     "Jul" = 7, "Aug" = 8, "Sep" = 9,
                                     "Oct" = 10, "Nov" = 11, "Dec" = 12
                      )),
          width = 12,
          style = "box-shadow: 2px 2px 5px lightblue;"
        ),
        fluidRow(
          valueBoxOutput("total_number", width = 6), valueBoxOutput("cancel_rate", width = 6)
        ),
        fluidRow(
          valueBoxOutput("delay_rate", width = 6), valueBoxOutput("departure_delay_rate", width = 6)
        ),
        # Select data to be displayed on map filter
        box(
          selectInput("radiochoice", "Select Data to Display:",
                       choices = list(
                         "Flights" = "Flights",
                         "Percentage Cancellations" = "PercentageCancellations",
                         "Percentage Arrival Delays" = "PercentageArrivalDelays",
                         "Percentage Departure Delays" = "PercentageDepartureDelays"
                       )),
          width = 12,
          style = "box-shadow: 2px 2px 5px lightblue;"
        ),
        # Map and download data
        fluidRow(
          box(
            plotlyOutput("map_state"),
            tabsetPanel(
              tabPanel("Data Download",
                downloadButton(
                "downloadData",
                paste("Download Map source data CSV")
              ))
            ),
            height = 600,
            width = 12,
            style = "box-shadow: 2px 2px 5px lightblue;"
          ) 
        )
      ),
      
      # Datasets tab
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "View Datasets and Data Structure",
            p("This section provides the structure of the flight delay data."),
            style = "box-shadow: 2px 2px 5px lightblue;",
            width = 12,
            tabsetPanel(
              tabPanel("Airlines Dataset", 
                       DTOutput("airlinesTable"),
                       downloadButton("downloadAirlines", "Download Airlines CSV"),
                       verbatimTextOutput("airlines_structure")),
              tabPanel("Airports Dataset",
                       DTOutput("airportsTable"),
                       downloadButton("downloadAirports", "Download Airports CSV"),
                       verbatimTextOutput("airports_structure")),
              tabPanel("Flights Dataset", 
                       DTOutput("flightsTable"),
                       downloadButton("downloadFlights", "Download Flights CSV"),
                       verbatimTextOutput("flights_structure"))
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
 flight_all <- flights_original %>% left_join(airports, by = c("ORIGIN_AIRPORT" = "IATA_CODE"))

filtered_flights <- reactive({
  if (input$month == 99) {
    flights  # No filtering if "All" is selected
  } else {
    filter(flights, MONTH == input$month)  
  }
})


# Render the value boxes based on the filtered data
output$total_number <- renderValueBox({
  total_number_value <- nrow(filtered_flights())
  valueBox(
    formatC(total_number_value, format = "d", big.mark = ','),
    subtitle = "Number of flights",
    color = 'olive',
    icon = icon("plane", lib = 'font-awesome')
  )
})

output$delay_rate <- renderValueBox({
  delayed_flights <- filtered_flights() %>% filter(ARRIVAL_DELAY > 0)
  delay_rate_value <- nrow(delayed_flights) / nrow(filtered_flights())
  valueBox(
    percent_format()(delay_rate_value),
    subtitle = "Arrival Delay Rate",
    color = 'yellow',
    icon = icon("plane-arrival", lib = 'font-awesome')
  )
})

output$cancel_rate <- renderValueBox({
  cancelled_flights <- filtered_flights() %>% filter(CANCELLED == 1)
  cancel_rate_value <- nrow(cancelled_flights) / nrow(filtered_flights())
  valueBox(
    percent_format()(cancel_rate_value),
    subtitle = "Cancel Rate",
    color = 'red',
    icon = icon("bell-slash", lib = 'font-awesome')
  )
})

output$departure_delay_rate <- renderValueBox({
  departure_delayed_flights <- filtered_flights() %>% filter(DEPARTURE_DELAY > 0)
  departure_delay_rate_value <- nrow(departure_delayed_flights) / nrow(filtered_flights())
  valueBox(
    percent_format()(departure_delay_rate_value),
    subtitle = "Departure Delay Rate",
    color = 'blue',
    icon = icon("plane-departure", lib = 'font-awesome')
  )
})


# Create a reactive expression for flight data based on user input
data_map <- reactive({
  column_select <- input$radiochoice
  column_title <- switch(
    column_select,
    "Flights" = "Number of Flights",
    "PercentageCancellations" = "Percentage of Cancellations",
    "PercentageArrivalDelays" = "Percentage of Arrival Delays",
    "PercentageDepartureDelays" = "Percentage of Departure Delays",
    "Flights"  # Default to "Flights" if the value is not recognized
  )
  flight_geo <- flight_all %>%
    filter(if (input$month == 99) TRUE else MONTH == input$month) %>%
    group_by(STATE) %>%
    summarise(
      Flights = n(),
      PercentageCancellations = paste0(round(sum(CANCELLED == 1, na.rm = TRUE) / n() * 100, 2)),
      PercentageArrivalDelays = paste0(round(sum(ARRIVAL_DELAY > 0, na.rm = TRUE) / n() * 100, 2)),
      PercentageDepartureDelays = paste0(round(sum(DEPARTURE_DELAY > 0, na.rm = TRUE) / n() * 100, 2))
    ) %>%
    select(STATE, column_select)
  
  
  names(flight_geo)[2] <- column_select
  
  return(flight_geo)
})

# Render the heat map using Plotly
output$map_state <- renderPlotly({
  column_select <- input$radiochoice
  flight_data <- data_map()
  
  # Define a title based on the selected column
  column_title <- switch(
    column_select,
    "Flights" = "Number of Flights",
    "PercentageCancellations" = "Percentage of Flights Canceled",
    "PercentageArrivalDelays" = "Percentage of Flights with Arrival Delays",
    "PercentageDepartureDelays" = "Percentage of Flights with Departure Delays",
    "Flights"  # Default to "Flights" if the value is not recognized
  )
  
  plot_ly(data = flight_data, type = "choropleth", locationmode = "USA-states") %>%
    add_trace(
      z = ~.data[[column_select]],
      colorscale = list(c(0, 1), c("lightblue", "darkblue")),
      locations = ~STATE,
      colorbar = list(title = NULL)
    ) %>%
    layout(
      geo = list(scope = "usa"),
      title = paste("State-Based", column_title),
      x = NULL,
      y = NULL
    ) %>%
    config(displayModeBar = TRUE)
})


# Create flight delay causes pie chart
output$delayCausesPlot <- renderPlotly({
  selected_airline <- input$airlineFilter
  filtered_flights <- flights %>%
    filter(AIRLINE == selected_airline)
  
  # Calculate the number of delays for each reason
  delay_data <- filtered_flights %>%
    summarise(
      AIR_SYSTEM_DELAY = sum(!is.na(AIR_SYSTEM_DELAY) & AIR_SYSTEM_DELAY > 0),
      SECURITY_DELAY = sum(!is.na(SECURITY_DELAY) & SECURITY_DELAY > 0),
      AIRLINE_DELAY = sum(!is.na(AIRLINE_DELAY) & AIRLINE_DELAY > 0),
      LATE_AIRCRAFT_DELAY = sum(!is.na(LATE_AIRCRAFT_DELAY) & LATE_AIRCRAFT_DELAY > 0),
      WEATHER_DELAY = sum(!is.na(WEATHER_DELAY) & WEATHER_DELAY > 0)
    ) %>%
    pivot_longer(cols = -1, names_to = "Delay_Type", values_to = "Num_Delays")
  # Create a Plotly pie chart
  plot_ly(delay_data, labels = ~Delay_Type, values = ~Num_Delays, type = 'pie',
          #      text = tooltip_text,
          marker = list(colors = RColorBrewer::brewer.pal(5, "Set1"))) %>%
    layout(title = paste("Flight Delay Causes for", selected_airline),
           x = NULL,
           y = NULL)
})
# Create monthly delays plot with automatic animation
output$monthlyDelaysPlot <- renderPlotly({
  selected_airline <- input$airlineFilter
  filtered_flights <- flights %>%
    filter(AIRLINE == selected_airline)
  
  # Calculate average departure delay by month
  monthly_delays <- filtered_flights %>%
    mutate(Month = factor(MONTH, levels = 1:12, labels = month.name)) %>%
    group_by(Month) %>%
    summarise(
      Avg_Departure_Delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      Avg_Arrival_Delay = mean(ARRIVAL_DELAY, na.rm = TRUE)
    )
  
  # Round the average departure delay and average arrival delay to 2 decimal places
  monthly_delays$Avg_Departure_Delay <- round(monthly_delays$Avg_Departure_Delay, 2)
  monthly_delays$Avg_Arrival_Delay <- round(monthly_delays$Avg_Arrival_Delay, 2)
  
  # Create a dual-line Plotly plot with automatic animation
  plot <- plot_ly() %>%
    add_lines(
      x = ~monthly_delays$Month,
      y = ~monthly_delays$Avg_Departure_Delay,
      name = "Departure Delay",
      line = list(color = 'blue'),
      hoverinfo = "y+name"
    ) %>%
    add_lines(
      x = ~monthly_delays$Month,
      y = ~monthly_delays$Avg_Arrival_Delay,
      name = "Arrival Delay",
      line = list(color = 'red'),
      hoverinfo = "y+name"
    ) %>%
    layout(
      title = paste("Monthly Performance for", selected_airline),
      xaxis = list(title = "Month"),
      yaxis = list(title = "Average Delay (minutes)"),
      showlegend = TRUE
    )
  
  plot
})

# Create airlines delay plot (average departure and arrival delay)
output$airlinesDelayPlot <- renderPlotly({
  flights
  
  # Calculate average departure and arrival delay by airline
  airline_delay <- flights %>%
    group_by(AIRLINE) %>%
    summarise(
      Avg_Departure_Delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
      Avg_Arrival_Delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
      Total_Delay = Avg_Departure_Delay + Avg_Arrival_Delay
    ) %>%
    mutate(
      Avg_Departure_Delay = Avg_Departure_Delay,
      Avg_Arrival_Delay = Avg_Arrival_Delay
    )
  
  # Order airlines by total delay in descending order
  airline_delay <- airline_delay %>%
    arrange(desc(Total_Delay))
  
  # Create a Plotly bar plot for departure and arrival delay
  plot <- plot_ly(
    data = airline_delay,
    x = ~reorder(AIRLINE, -Total_Delay),
    y = ~Avg_Departure_Delay,
    type = 'bar',
    marker = list(color = 'lightblue'),
    name = "Departure Delay"
  ) %>%
    add_trace(
      x = ~reorder(AIRLINE, -Total_Delay),
      y = ~Avg_Arrival_Delay,
      type = 'bar',
      marker = list(color = 'lightgreen'),
      name = "Arrival Delay"
    ) %>%
    layout(
      title = "Airlines Average Departure and Arrival Performance",
      xaxis = list(title = "Airline"),
      yaxis = list(title = "Average Delay (minutes)")
    )
  
  plot
})


# Create average departure and arrival delay by airline plot (top n airports)
output$averageAirportDelayPlot <- renderPlotly({
  selected_airline <- input$airlineFilter
  filtered_flights <- flights %>%
    filter(AIRLINE == selected_airline)
  
  # Calculate average departure and arrival delay by destination city
  departure_arrival_delay_by_airline <- filtered_flights %>%
    group_by(DESTINATION_CITY) %>%
    summarise(
      Avg_Departure_Delay = round(mean(DEPARTURE_DELAY, na.rm = TRUE), 2),  # Round to 2 decimal places
      Avg_Arrival_Delay = round(mean(ARRIVAL_DELAY, na.rm = TRUE), 2)  # Round to 2 decimal places
    )
  
  # Calculate the total delay (sum of departure and arrival delays) and round it
  departure_arrival_delay_by_airline <- departure_arrival_delay_by_airline %>%
    mutate(Total_Delay = round(Avg_Departure_Delay + Avg_Arrival_Delay, 2))  # Round to 2 decimal places
  
  # Select the top n cities based on total delay, where n is based on user input
  n_airports <- switch(input$nAirports,
                       "5" = 5,
                       "10" = 10,
                       "15" = 15,
                       "All" = nrow(departure_arrival_delay_by_airline)  # Show all airports
  )
  
  departure_arrival_delay_by_airline <- departure_arrival_delay_by_airline %>%
    arrange(desc(Total_Delay)) %>%
    top_n(n_airports)
  
  # Create a synchronized bar chart for departure and arrival delays
  plot_ly(data = departure_arrival_delay_by_airline) %>%
    add_bars(x = ~reorder(DESTINATION_CITY, -Total_Delay), y = ~Avg_Departure_Delay, name = "Departure Delay", marker = list(color = 'orange')) %>%
    add_bars(x = ~reorder(DESTINATION_CITY, -Total_Delay), y = ~Avg_Arrival_Delay, name = "Arrival Delay", marker = list(color = 'blue')) %>%
    add_bars(x = ~reorder(DESTINATION_CITY, -Total_Delay), y = ~Total_Delay, name = "Total Delay", marker = list(color = 'green')) %>%
    layout(
      title = paste("Top", n_airports, "On-Time performance by Airport", selected_airline),
      xaxis = list(title = "Destination City"),
      yaxis = list(title = "Average Delay (minutes)"),
      barmode = "group"  # Synchronize bars for departure and arrival delays
    )
})



# Render the Airlines dataset table
output$airlinesTable <- renderDT({
  datatable(airlines, 
            options = list(scrollX = TRUE, scrollY = TRUE, width = "100%", height = "100%"))
})

# Render the Airports dataset table
output$airportsTable <- renderDT({
  datatable(airports, 
            options = list(scrollX = TRUE, scrollY = TRUE, width = "100%", height = "100%"))
})

# Render the Flights dataset table
output$flightsTable <- renderDT({
  datatable(flights_original, 
            options = list(scrollX = TRUE, scrollY = TRUE, width = "100%", height = "100%"))
})

# Common download handler function
downloadDataset <- function(dataset, filename) {
  return(
    downloadHandler(
      filename = function() {
        filename  # Set the filename for the downloaded CSV
      },
      content = function(file) {
        write.csv(dataset, file)  # Write the specified dataset to the file
      }
    )
  )
}
# Map data to be downloaded
flight_data_download<- flight_all %>%
  group_by(STATE,MONTH) %>%
  summarise(
    Flights = n(),
    PercentageCancellations = paste0(round(sum(CANCELLED == 1, na.rm = TRUE) / n() * 100, 2)),
    PercentageArrivalDelays = paste0(round(sum(ARRIVAL_DELAY > 0, na.rm = TRUE) / n() * 100, 2)),
    PercentageDepartureDelays = paste0(round(sum(DEPARTURE_DELAY > 0, na.rm = TRUE) / n() * 100, 2))
  ) %>%
  select(STATE, MONTH, Flights, PercentageCancellations, PercentageArrivalDelays, PercentageDepartureDelays)


# Define download handlers for each dataset using the common function
output$downloadAirlines <- downloadDataset(airlines, "airlines.csv")
output$downloadAirports <- downloadDataset(airports, "airports.csv")
output$downloadFlights <- downloadDataset(flights_original, "flights.csv")
output$downloadData <- downloadDataset(flight_data_download, "map_source_data.csv")


# For Structure output
output$airlines_structure <- renderPrint({
  airlines %>% 
    str()
})
output$airports_structure <- renderPrint({
  airports %>% 
    str()
})
output$flights_structure <- renderPrint({
  flights_original %>% 
    str()
})

}

# Run the application
shinyApp(ui = ui, server = server)




