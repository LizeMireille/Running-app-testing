
# Import libraries ----------------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
library(lubridate)
library(geosphere)
library(shinythemes)

# Import & modify data -------------------------------------------------------------------------------------
  
  #import all csv files and save in one df 
  file_paths <- list.files('data/', pattern = '*.csv', full.names = TRUE)
  
  data <- map_df(file_paths, read_csv, .id = 'run') %>%
    mutate(run = dense_rank(run))
  
  
# Modify data to include distance and cumulative distance
  #distance between previous lat lng and current 
  data <- data %>%
    group_by(run) %>%
    mutate(
      # calculate distance and convert to km 
      distance = c(0, distHaversine(cbind(lng[-n()], lat[-n()]), cbind(lng[-1], lat[-1])) / 1000),
      cumulative_distance = cumsum(distance)
    ) %>%
    ungroup()
    
# Modify data to include the elevation change to show the change on the map
  data <- data %>%
    group_by(run) %>%
    mutate(
      # Calculate the change in elevation between each point
      elevation_change = c(0, diff(elevation))
    ) %>%
    ungroup()
    
  
# Define UI ----------------------------------------------------------------------------------------
  ui <- fluidPage(
    # Select a theme
    theme = shinytheme("sandstone"),
    # Add an image for vibes :) 
    div(class = "top-corner-logo", img(src = "featured.png", height = '150px')),
    titlePanel("Comprehensive Run Analysis"),
    
    # Tabs for different analyses
    tabsetPanel(
      # Single Run Analysis tab
      tabPanel(
        "Single Run Analysis",
        sidebarLayout(
          sidebarPanel(
            selectInput("selected_date", "Select a Date:", choices = unique(data$date)), 
            
            # Table for single run statistics
            h3("Single Run Performance Overview"),
            tableOutput("distance"),
            tableOutput("pace_and_speed"),
            tableOutput("time"),
            tableOutput("elevation")
          ),
          mainPanel(
            # Map output for the run route
            h3("Run Route"),
            leafletOutput("map"),
            actionButton("animate", "Show Path"),
            
            # Graph for average pace per km
            h3("Pace Profile per Kilometer"),
            plotlyOutput("pace_distance_graph")
          )
        )
      ),
      
      # Multiple Runs Analysis tab
      tabPanel(
        "Multiple Runs Analysis",
        sidebarLayout(
          sidebarPanel(
            dateRangeInput("date_range", "Select Date Range:", start = min(data$date), end = max(data$date)),
            
            # Table for multiple run statistics
            h3("Combined Run Performance Overview"),
            h4("Run Totals"),
            tableOutput("totals"),
            tableOutput("elevations"),
            h4("Run Averages"),
            tableOutput("averages")
          ),
          mainPanel(
            # Graph for run frequency over time
            h3("Temporal Distribution of Runs"),
            selectInput("time_grouping", "Time Interval Grouping:", choices = c("Month" = "month", "Week" = "week")),
            plotlyOutput("run_frequency"),
          )
        )
      )
    )
  )

  
  
  
#Define server logic -------------------------------------------------------------------------------------
  server <- function(input, output, session) {
    
# SINGLE RUN ANALYSIS
    
    # Filter data to the specific run for selected run
    filtered_data <- reactive({
      data %>%
        filter(date == input$selected_date) %>%
        arrange(time) 
    })
    
    # Calculate total time, average speed, and average pace for the selected run
    data <- data %>%
      group_by(run) %>%
      mutate(
        total_time = as.numeric(difftime(last(time), first(time), units = "hours")),
        average_speed = if_else(total_time > 0, (last(cumulative_distance) / total_time), 0) # Zeros to avoid division by 0
      ) %>%
      ungroup() %>%
      mutate(
        average_pace = if_else(average_speed > 0, 60 / average_speed, 0)  # Zeros to avoid division by 0
      )
    
    # Display the route on a Leaflet map 
    output$map <- renderLeaflet({
      data <- filtered_data()
      if (nrow(data) > 0) {
        
        # Create a function to add legend with elevation colours (relevant to animation)
        get_colour <- colorNumeric(
          palette = c("seagreen", "gold", "firebrick"), # green - downhill, yellow - flat, red -  uphill
          domain = range(data$elevation_change, na.rm = TRUE)
        )
        
        # Create route map with start/end markers & light route line (polyline) as base 
        leaflet(data) %>%
          
          # Specify map - high res aerial photo 
          addProviderTiles(providers$Esri.WorldImagery)%>%
          setView(lng = mean(data$lng), lat = mean(data$lat), zoom = 13.5) %>%
          addCircleMarkers(lng = first(data$lng), lat = first(data$lat), color = "green", radius = 6, label = "Start") %>%
          addCircleMarkers(lng = last(data$lng), lat = last(data$lat), color = "red", radius = 6, label = "End") %>%
          addPolylines(~lng, ~lat, group = "run_route", color = "darkgrey", weight = 7) %>%
          addLegend("bottomright", pal = get_colour, values = ~elevation_change,
                    title = "Elevation Change (m)", opacity = 1)
      }
    })
    
    # Show Path button - animate the path on the map 
    observeEvent(input$animate, {
      data <- filtered_data()
      
      # Clear possible previous animated route  
      if (nrow(data) > 1) {
        leafletProxy("map", session) %>%
          clearGroup("animated_route")  
        
        # Create a function to map the elevation change to a colour
        get_colour <- colorNumeric(
          palette = c("seagreen", "gold", "firebrick"), # green - downhill, yellow - flat, red -  uphill
          domain = range(data$elevation_change, na.rm = TRUE))
        
        for (i in 2:nrow(data)) {
          # Select the color for the elevation 
          leafletProxy("map", session) %>%
            addPolylines(lng = data$lng[(i-1):i], lat = data$lat[(i-1):i], color = get_colour(data$elevation_change[i]), weight = 5, group = "animated_route")
        }
      }
    })
    
    
    #Tables for static outputs 
    
    # Distance
    output$distance <- renderTable({
      data <- filtered_data()
      if (nrow(data) > 0) {
        total_distance <- last(data$cumulative_distance)
        # Put in df
        distance_data <- data.frame("Total Distance" = paste(round(total_distance, 2), "km"))
        return(distance_data)
      }
    })
    
    # Pace and Speed
    output$pace_and_speed <- renderTable({
      data <- filtered_data()
      if (nrow(data) > 0) {
        # Extract the first (and only) value for average speed and pace for the selected run
        average_speed <- data$average_speed[1]
        average_pace <- data$average_pace[1]
        
        pace_min <- floor(average_pace)
        pace_sec <- round((average_pace - pace_min) * 60)
        formatted_average_pace <- sprintf("%d:%02d", pace_min, pace_sec)
        
        # Put in df
        pace_speed_data <- data.frame(
          "Average Speed" = paste(round(average_speed, 2), "km/h"),
          "Average Pace" = paste(formatted_average_pace, "min/km")
        )
        return(pace_speed_data)
      }
    })
    
    # Time
    output$time <- renderTable({
      data <- filtered_data()
      if (nrow(data) > 0) {
        start_time <- format(first(data$time), "%H:%M:%S")
        end_time <- format(last(data$time), "%H:%M:%S")
        total_time <- difftime(last(data$time), first(data$time), units = "secs")
        
        # Put in df
        time_data <- data.frame(
          "Start Time" = start_time,
          "End Time" = end_time,
          "Total Time" = format(as.period(total_time), "%H:%M:%S")
        )
        return(time_data)
      }
    })
    
    # Elevation
    output$elevation <- renderTable({
      data <- filtered_data()
      if (nrow(data) > 0) {
        total_elevation_gain <- sum(data$elevation_change[data$elevation_change > 0])
        total_elevation_loss <- sum(data$elevation_change[data$elevation_change < 0])
        
        # Put in df
        elevation_data <- data.frame(
          "Total Elevation Gain" = paste(round(total_elevation_gain, 2), "m"),
          "Total Elevation Loss" = paste(round(abs(total_elevation_loss), 2), "m") # account for minus sign
          ) 
        return(elevation_data)
      }
    })
    

  
    
    # Create graph displaying the average pace for each km on the run
    output$pace_distance_graph <- renderPlotly({
      df <- filtered_data()
      if (nrow(df) > 0) {
        
        # Calculate the time difference and distance difference for each data point
        df <- df %>%
          mutate(
            time_diff = c(0, diff(as.numeric(time))),  
            dist_diff = c(0, distHaversine(cbind(lng[-n()], lat[-n()]), cbind(lng[-1], lat[-1]))) / 1000  
          ) %>%
          mutate(
            # Pace in min/km 
            pace = if_else(dist_diff > 0, time_diff / (dist_diff * 60), 0)  # Zeros to avoid division by 0
          )
        
        # Create 1 km segments 
        df <- df %>%
          # Group data into 1 km bins based on cumulative distance
          group_by(
            run,
            km_bin = cut(cumulative_distance, breaks=seq(0, max(cumulative_distance), by=1), include.lowest=TRUE, right=FALSE)  
          ) %>%
          summarize(
            # Distance covered at the start of each bin
            distance_covered = round(first(cumulative_distance), 2),  
            # Average pace for each bin
            average_pace = mean(pace, na.rm=TRUE),  
            .groups='drop'
          ) %>%
          # Convert to min:sec
          mutate(
            average_pace_min_sec = sprintf("%02d:%02d", floor(average_pace), round((average_pace - floor(average_pace)) * 60))  # Format average pace as minutes:seconds
          )
        
        # Plot the average pace for each segment; using average pace values to plot but displaying them in min:sec format
        pace_plot <- ggplot(df, aes(x=distance_covered, y=average_pace)) +
          geom_line(color='gold') + 
          geom_point(color='orange', size=2, aes(text=paste("Distance Covered: ", distance_covered, "km<br>Average Pace: ", average_pace_min_sec, "min/km"))) +  
          labs(
            x="Cumulative Distance (km)",
            y="Average Pace (min/km)"
          ) +
          scale_y_continuous(labels = function(x) sprintf("%02d:%02d", floor(x), round((x - floor(x)) * 60))) +  # Format y-axis  as min:sec
          theme_light() +
          scale_y_reverse()  # Reverse axis so that faster paces appear at top
        
        # Convert to Plotly 
        ggplotly(pace_plot, tooltip="text")
      }
    })
    

    
# MULTIPLE RUN ANALYSIS
    
    # Filter data to the specific timeframe for multiple run analysis
    multiple_filtered_data <- reactive({
      date_range <- input$date_range
      if (!is.null(date_range)) {
        data %>%
          filter(date >= date_range[1] & date <= date_range[2])
      } else {
        return(NULL) # Return nothing if a date range with no runs in it is selected
      }
    })
   
    
    #Tables for static outputs

    # Totals
    output$totals <- renderTable({
      data <- multiple_filtered_data()
      if (!is.null(data) && nrow(data) > 0) {
        # Calculate summaries for each run
        run_summaries <- data %>%
          group_by(run) %>%
          summarise(
            total_distance = last(cumulative_distance),
            total_time_secs = as.numeric(difftime(last(time), first(time), units = "secs")),
            .groups = 'drop'
          )
        
        # Convert total time from seconds to a period (H:M:S)
        total_time_period <- as.period(seconds_to_period(sum(run_summaries$total_time_secs)))
        
        # Put in df
        totals <- data.frame(
          "Number of Runs" = n_distinct(run_summaries$run),
          "Total Distance" = paste(round(sum(run_summaries$total_distance), 2), "km"),
          "Total Time" = format(total_time_period, "%H:%M:%S")
        )
        
        return(totals)
      } else {
        return(data.frame()) # Return an empty data frame if no runs are found within the selected date range
      }
    })
        
    
    
    # Elevations
    output$elevations <- renderTable({
      data <- multiple_filtered_data()
      if (!is.null(data) && nrow(data) > 0) {
        # Calculate total elevation gain and loss for each run
        run_summaries <- data %>%
          group_by(run) %>%
          summarise(
            total_elevation_gain = sum(elevation_change[elevation_change > 0]),
            total_elevation_loss = -sum(elevation_change[elevation_change < 0]),
            .groups = 'drop'
          )
        
        # Put in df
        elevations <- data.frame(
          "Total Elevation Gain" = paste(round(sum(run_summaries$total_elevation_gain), 2), "m"),
          "Total Elevation Loss" = paste(round(sum(run_summaries$total_elevation_loss), 2), "m")
        )
        
        return(elevations)
      } else {
        return(data.frame()) # Return an empty data frame if no runs are found within the selected date range
      }
    })
    
    
    
    # Avrages 
    output$averages <- renderTable({
      data <- multiple_filtered_data()
      if (!is.null(data) && nrow(data) > 0) {
        # Calculate average speed and pace for each run
        run_summaries <- data %>%
          group_by(run) %>%
          summarise(
            total_distance = last(cumulative_distance),
            total_time_hours = as.numeric(difftime(last(time), first(time), units = "hours")),
            .groups = 'drop'
          ) %>%
          mutate(
            average_speed = total_distance / total_time_hours,
            average_pace = (total_time_hours * 60) / total_distance  # Pace in minutes per km
          )
        
        # Calculate overall averages across all runs
        overall_average_speed <- mean(run_summaries$average_speed, na.rm = TRUE)
        overall_average_distance <- mean(run_summaries$total_distance, na.rm = TRUE)
        overall_average_pace <- mean(run_summaries$average_pace, na.rm = TRUE)
        
        # Convert average pace from minutes to "min:sec" format
        average_pace_min <- floor(overall_average_pace)
        average_pace_sec <- round((overall_average_pace - average_pace_min) * 60)
        formatted_average_pace <- sprintf("%d:%02d", average_pace_min, average_pace_sec)
        
        # Put in df
        averages <- data.frame(
          "Average Speed" = paste(round(overall_average_speed, 2), "km/h"),
          "Average Distance" = paste(round(overall_average_distance, 2), "km"),
          "Average Pace" = paste(formatted_average_pace, "min/km")
        )
        
        return(averages)
      } else {
        return(data.frame()) # Return an empty data frame if no runs are found within the selected date range
      }
    })
    
  
    # Display average number of runs over time 
    output$run_frequency <- renderPlotly({
      data <- multiple_filtered_data()
      if (!is.null(data) && nrow(data) > 0) {
        # Summarize the data for each run
        run_summaries <- data %>%
          group_by(run) %>%
          summarise(
            run_date = first(date), # Get date of each run
            total_distance = last(cumulative_distance),
            total_time_hours = as.numeric(difftime(last(time), first(time), units = "hours")),
            .groups = 'drop'
          ) %>%
          mutate(
            average_speed = total_distance / total_time_hours)
        
        # Group the run_summaries based on week or month (user selection)
        grouped_data <- run_summaries %>%
          group_by(group = floor_date(run_date, input$time_grouping)) %>%
          summarise(
            run_count = n(),# Get number of runs in each time group
            average_speed = mean(average_speed), # Average speed for each time group
            total_distance = sum(total_distance), # Total distance for each time group
            average_distance = total_distance / run_count,  # Average distance per time group
            .groups = 'drop'
          ) %>%
          arrange(group)
        
        
        # Create the run frequency 
        run_plot <- ggplot(grouped_data, aes(x = group, y = run_count, 
                                      text = paste("Count: ", run_count,
                                                   "<br>Average Speed: ", round(average_speed, 2), " km/h",
                                                   "<br>Total Distance: ", round(total_distance, 2), " km",
                                                   "<br>Average Distance: ", round(average_distance, 2), " km/run"))) +
          geom_bar(stat = "identity", fill = 'gold') +  
          labs(title = paste("Run Frequency by", input$time_grouping), x = input$time_grouping, y = "Number of Runs") +
          scale_x_date(date_breaks = if_else(input$time_grouping == "month", "1 month", "1 week"), date_labels = if_else(input$time_grouping == "month", "%b %Y", "%d %b %Y")) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
        
        # Convert to Plotly
        ggplotly(run_plot, tooltip = "text") 
      } else {
        return(NULL)  # Return nothing if no runs are found within the selected date range
      }
    })
  }
# Run the app
shinyApp(ui = ui, server = server) 