library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("PartA", tabName = "PartA"),
    menuItem("Sandhya One", tabName = "sandhya_one"),
    menuItem("Sandhya Two", tabName = "sandhya_two"),
    menuItem("Sandhya Map", tabName = "sandhya_map"),
    menuItem("Sandhya Three", tabName = "sandhya_color")
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "PartA",
              fluidRow(
                box(plotOutput("plot_one")),
                
                box(
                  title = "Controls",
                  selectInput(
                    inputId = "n_breaks",
                    label = "Number of bins in histogram (approximate):",
                    choices = c(10, 20, 35, 50),
                    selected = 20
                  ),
                  
                  checkboxInput(
                    inputId = "individual_obs",
                    label = strong("Show individual observations"),
                    value = FALSE
                  ),
                  
                  checkboxInput(
                    inputId = "density",
                    label = strong("Show density estimate"),
                    value = FALSE
                  ),
                  
                  conditionalPanel(
                    condition = "input.density == true",
                    sliderInput(
                      inputId = "bw_adjust",
                      label = "Bandwidth adjustment:",
                      min = 0.2,
                      max = 2,
                      value = 1,
                      step = 0.2
                    )
                  )
                )
              )),
      
      tabItem(tabName = "sandhya_one",
              fluidRow(
                box(plotOutput("plot_two")),
                
                box(
                  title = "Controls",

                  checkboxInput(
                    inputId = "playoffs",
                    label = strong("Made it to Playoffs"),
                    value = FALSE
                  ), 
                  
                  checkboxInput(
                    inputId = "superbowl",
                    label = strong("Made it to Superbowl"),
                    value = FALSE
                  ),
                  
                  selectInput(
                    inputId = "n_breaks2",
                    label = "Number of bins in histogram (approximate):",
                    choices = c(10, 20, 35, 50),
                    selected = 20
                  )
                  
                  #,
                  
                   # checkboxInput(
                   #   inputId = "colored",
                   #   label = strong("Show by Wins and Losses"),
                   #   value = FALSE
                   # )
                )
              )), 
      
      tabItem(tabName = "sandhya_two",
              fluidRow(
                box(plotOutput("plot_three")),
                
                box(
                  title = "Controls",
                  
                  checkboxInput(
                    inputId = "playoffs2",
                    label = strong("Made it to Playoffs"),
                    value = FALSE
                  ), 
                  
                  checkboxInput(
                    inputId = "superbowl2",
                    label = strong("Made it to Superbowl"),
                    value = FALSE
                  ),
                  
                  selectInput(
                    inputId = "n_breaks3",
                    label = "Number of bins in histogram (approximate):",
                    choices = c(10, 20, 35, 50),
                    selected = 20
                  )
                  
                  #,
                  
                  # checkboxInput(
                  #   inputId = "colored",
                  #   label = strong("Show by Wins and Losses"),
                  #   value = FALSE
                  # )
                  
                )
              )),
      
      tabItem(tabName = "sandhya_map",
              fluidRow(
                box(width=12,leafletOutput(outputId = "plot_map", height = "600px"))
                  
                  #,
                  
                  # checkboxInput(
                  #   inputId = "colored",
                  #   label = strong("Show by Wins and Losses"),
                  #   value = FALSE
                  # )
                  
              )),
      
      tabItem(tabName = "sandhya_color",
              fluidRow(
                box(plotOutput("plot_color")),
                
                box(
                  title = "Controls",
                  
                  selectInput(
                    inputId = "fill_by_color",
                    label = "Fill by:",
                    choices = c("Nothing","Made it to Playoffs", "Won Superbowl"),
                    selected = "Nothing"
                  ), 
                  
                  sliderInput(
                    inputId = "bw_adjust_col",
                    label = "Bandwidth adjustment:",
                    min = 0.2,
                    max = 2,
                    value = 1,
                    step = 0.2
                  )

                )
              ))
      
      
    ))
)

server <- function(input, output) {
  
  attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
  standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
  games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')
  joined <- dplyr::left_join(attendance, standings, by = c("year", "team_name"))
  #joined <- dplyr::left_join(attendance, standings, games, by = c("year", "team_name", "team"))
                             
  #histogram distribution of attendance
  #can select by wins or by losses or by nothing
  #made playoffs or made made it to superbowl
  
  output$plot_one <- renderPlot({
    plt <- ggplot(faithful, aes(x = eruptions)) +
      geom_histogram(aes(y = ..density..), bins = input$n_breaks, fill = "white", color = "black") +
      labs(title = "Geyser eruption duration",
           x = "Duration (minutes)",
           y = "Density")
    
    if (input$individual_obs) {
      plt <- plt + geom_rug()
    }
    
    if (input$density) {
      plt <- plt + geom_density(adjust = input$bw_adjust, color = "blue")
    }
    
    return(plt)
  })
  
  output$plot_two<- renderPlot({
    data <- joined
    if(input$playoffs){
      data <- filter(data, playoffs == "Playoffs")
    }
    
    if(input$superbowl){
      data <- filter(data, sb_winner == "Won Superbowl")
    }
    
    plt <- ggplot(data, aes(x = weekly_attendance)) + 
      geom_histogram(bins = input$n_breaks2) + xlab("Weekly Attendance") +
      ylab("Count")
    
    return(plt)
  })
  
  output$plot_three <- renderPlot({
    data <- joined
    full_data <- dplyr::select(joined, weekly_attendance, playoffs, sb_winner)
    full_data <- mutate(full_data, playoff_data = 
                          ifelse(playoffs == "Playoffs", weekly_attendance, NA ))
    
    full_data <- mutate(full_data, sb_data = 
                          ifelse(sb_winner == "Won Superbowl", weekly_attendance, NA ))
    
    plt <- ggplot(full_data) +  
      geom_histogram(aes(x = weekly_attendance), bins = input$n_breaks3)
    
    if(input$playoffs2){
      # data <- filter(full_data, playoffs == "Playoffs")
      # plt2 <- ggplot(data, aes(x = weekly_attendance)) +  
      #   geom_histogram(bins = input$n_breaks3)
      # plt <- plt + plt2
      plt <- plt + geom_histogram(aes(x = playoff_data), bins = input$n_breaks3, color = "blue")
      
    }
    
    if(input$superbowl2){
      # data <- filter(full_data, sb_winner == "Won Superbowl")
      # plt3 <- ggplot(data, aes(x = weekly_attendance)) +  
      #   geom_histogram(bins = input$n_breaks3)
      # plt <- plt + plt3
      plt <- plt + geom_histogram(aes(x = sb_data), bins = input$n_breaks3, color = "green")
    }
    
    return(plt)
  })
  
  output$plot_color <- renderPlot({
    attendance_subset <- dplyr::select(attendance, team_name, year, weekly_attendance)
    attendance_subset <- attendance_subset %>%
      group_by(team_name, year) %>%
      summarize(weekly_attendance = mean(weekly_attendance, na.rm = TRUE))
    # attendance_subset has team, year, weekly attendance
    
    standings_subset <- dplyr::select(standings, team_name, year, playoffs, sb_winner)

    combined_subset <- attendance_subset %>% dplyr::left_join(standings_subset, 
                                            by = c("team_name" = "team_name",
                                                   "year" = "year"))
    
    if(input$fill_by_color == "Nothing"){
      plt <- ggplot(combined_subset) + 
        geom_density(aes(x = weekly_attendance), fill = "light blue", adjust = input$bw_adjust_col)
    } else if(input$fill_by_color == "Won Superbowl"){
      plt <- ggplot(combined_subset) + 
        geom_density(aes(x = weekly_attendance, fill = sb_winner), alpha = 0.5, adjust = input$bw_adjust_col) +
        labs(fill = "Superbowl Outcome")
    } else{
      plt <- ggplot(combined_subset) + 
        geom_density(aes(x = weekly_attendance, fill = playoffs), alpha = 0.5, adjust = input$bw_adjust_col) +
        labs(fill = "Playoffs Outcome")
    }
    
    plt <- plt + xlab("Average Weekly Attendance") + ylab("Density")
    
    return(plt)
  })
  
  output$plot_map <- renderLeaflet({
    library(tigris)
    
    states <- states(cb=T)
    
    sub_data <- select(attendance, team, weekly_attendance)
    sub_data_avg <- sub_data %>%
      group_by(team) %>%
      summarize(avg_attendance = mean(weekly_attendance, na.rm = TRUE))
    
    states_csv <- read_csv("https://raw.githubusercontent.com/sandhyabala31/315-Final_Project/main/states2.csv")
    
    sub_data_joined <- sub_data_avg %>% dplyr::left_join(states_csv, by = c("team" = "Input"))

    sub_data_joined <- select(sub_data_joined, avg_attendance, State)
    
    #AVERAGE OR SUM? - SUMMED
    sub_data_joined2 <- sub_data_joined %>%
      group_by(State) %>%
      summarize(new_col = sum(avg_attendance, na.rm = TRUE))
    
    states_merged_sb <- geo_join(states, sub_data_joined2, "NAME", "State")
    
    # Getting rid of rows with NA values
    states_merged_sb <- subset(states_merged_sb, !is.na(new_col))
    
    #Rounding it to whole number
    states_merged_sb <- mutate(states_merged_sb, new_col = round(new_col, 0))
    
    # Creating a color palette based on the number range in the total column
    pal <- colorNumeric("Reds", domain=states_merged_sb$new_col)
    
    # Setting up the pop up text
    popup_sb <- paste0(states_merged_sb$NAME, "'s total: ", as.character(states_merged_sb$new_col))
    
    # Mapping it with the new tiles CartoDB.Positron
    plt <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(-98.483330, 38.712046, zoom = 4) %>% 
      addPolygons(data = states_merged_sb , 
                  fillColor = ~pal(states_merged_sb$new_col), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb) %>%
      addLegend(pal = pal, 
                values = states_merged_sb$new_col, 
                position = "bottomright", 
                title = "Average Weekly Attendance")
    
    return(plt)
    
    # library(geojsonio)
    # world_vis_string <- paste0("https://raw.githubusercontent.com/johan/",
    #                            "world.geo.json/master/countries.geo.json")
    # world = geojson_read(world_vis_string, what = "sp")
    # 
    # 
    # m_leaflet <- world %>% leaflet() %>% setView(0, 20, 2) %>%
    #   addProviderTiles("MapBox", 
    #                    options = providerTileOptions(
    #                      id = "mapbox.light",
    #                      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    #   addPolygons(fillColor = ~pal(world_info$lifeExp),
    #               weight = 1,
    #               opacity = 1,
    #               color = "white",
    #               dashArray = "3",
    #               fillOpacity = 0.7,
    #               highlight = highlightOptions(
    #                 weight = 3,
    #                 color = "#666",
    #                 dashArray = "",
    #                 fillOpacity = 0.7,
    #                 bringToFront = TRUE),
    #               label = labels_world,
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "16px",
    #                 direction = "auto")) %>%
    #   addLegend(pal = pal, 
    #             values = ~world_info$lifeExp,
    #             title = "Life Expectancy, 2007 (Qrt)")
    # 
    # return(m_leaflet)
  })
  
}

shinyApp(ui, server)