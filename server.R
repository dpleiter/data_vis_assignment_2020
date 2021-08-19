library(shiny)
library(readr)
library(dplyr)
library(magrittr)
library(leaflet)
library(plotly)
library(DT)
library(tidyr)

stations <- read_csv("in/stations_good.csv")
locations <- read_csv("in/locations_good.csv")

stations$year <- factor(stations$year)

data_type_options <- c("Whole Day Entry" = "entries_24hr",
                       "Whole Day Exit" = "exits_24hr",
                       "AM Peak Entry" = "morning_peak_entry",
                       "AM Peak Exit" = "morning_peak_exit",
                       "Day Off-peak Entry" = "day_offpeak_entry",
                       "Day Off-peak Exit" = "day_offpeak_exit",
                       "PM Peak Entry" = "afternoon_peak_entry",
                       "PM Peak Exit" = "afternoon_peak_exit",
                       "Overnight Off-peak Entry" = "overnight_entry",
                       "Overnight Off-peak Exit" = "overnight_exit"
)

shinyServer(function(input, output, session) {
    show_commentary_scenario1 <- reactiveVal(FALSE)
    show_commentary_scenario2 <- reactiveVal(FALSE)
    
    lock_commentary <- reactiveVal(FALSE)
    
    output$showScenario1 <- reactive({
        show_commentary_scenario1()
    })
    
    output$showScenario2 <- reactive({
        show_commentary_scenario2()
    })
    
    output$showScenario3 <- reactive({
        show_commentary_scenario3()
    })
    
    outputOptions(output, "showScenario1", suspendWhenHidden = FALSE)
    outputOptions(output, "showScenario2", suspendWhenHidden = FALSE)
    
    observeEvent(input$button_scenario_1, {
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "station_select_1",
                          selected = "Parramatta Station")
        
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "station_select_2",
                          selected = "Town Hall Station")
        
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "data_type",
                          selected = "afternoon_peak_exit")
        
        show_commentary_scenario1(TRUE)
        show_commentary_scenario2(FALSE)
    })
    
    observeEvent(input$button_scenario_2, {
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "station_select_1",
                          selected = "Bondi Junction Station")
        
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "station_select_2",
                          selected = "Kings Cross Station")
        
        lock_commentary(TRUE)
        updateSelectInput(session, 
                          "data_type",
                          selected = "entries_24hr")
        
        show_commentary_scenario1(FALSE)
        show_commentary_scenario2(TRUE)
    })
    
    observeEvent(input$map_marker_click, {
        click<-input$map_marker_click
        if(is.null(click))
            return()
        
        if (input$station_select_1 == "") {
            updateSelectInput(session, "station_select_1", selected = click$id[1])
        } else {
            updateSelectInput(session, "station_select_2", selected = click$id[1])
        }
        
        show_commentary_scenario1(FALSE)
        show_commentary_scenario2(FALSE)
    })
    
    observeEvent(c(input$station_select_1, input$station_select_2, input$data_type), {
        if (lock_commentary()) {
            lock_commentary(FALSE)
        } else {
            show_commentary_scenario1(FALSE)
            show_commentary_scenario2(FALSE)
        }
    })
    
    dataset_station_1 <- reactive({
        if (input$station_select_1 != "") {
            stations %>% filter(station == input$station_select_1,
                                period == input$data_type)
        } else {
            stations %>% filter(period == input$data_type)
        }
    })
    
    dataset_station_2 <- reactive({
        if (input$station_select_2 != "") {
            stations %>% filter(station == input$station_select_2,
                                period == input$data_type)
        } else {
            stations %>% filter(period == input$data_type)
        }
    })
    
    locations_with_growth <- reactive({
        stations_2016 <- stations %>% filter(period == input$data_type, year == 2016) %>% select(station, patrons)
        stations_2019 <- stations %>% filter(period == input$data_type, year == 2019) %>% select(station, patrons)
        
        locations_new <- locations %>% left_join(stations_2016, by = "station")
        locations_new <- locations_new %>% left_join(stations_2019, by = "station")
        
        locations_new <- locations_new %>% mutate(differ = patrons.y - patrons.x)
    })
    
    locations_grow <- reactive({
        locations_with_growth() %>% filter(is.na(differ) | differ > 0)
    })
    
    locations_shrink <- reactive({
        locations_with_growth() %>% filter(!is.na(differ) & differ < 0)      
    })
    
    dataset_stations_combined <- reactive({
        locations_shrink()
        
        if (input$station_select_1 != "") {
            if (input$station_select_2 != "") {
                df <- bind_rows(dataset_station_1(), dataset_station_2())
            } else {
                df <- dataset_station_1()
            }
        } else {
            if (input$station_select_2 != "") {
                df <- dataset_station_2()
            } else {
                df <- dataset_station_1()
            }
        }
    })
    
    datatable_reactive <- reactive({
        DT::datatable(dataset_stations_combined()) %>%
            formatPercentage("annual_growth", 1)
    })
    
    output$map <- renderLeaflet({
        leaflet() %>% addProviderTiles(providers$CartoDB.Voyager) %>%
            setView(
                lng = 151.099696,
                lat = -33.848299,
                zoom = 11
            ) %>%
            addCircleMarkers(lng = locations_grow()$lng,
                       lat = locations_grow()$lat,
                       weight = 2,
                       fillOpacity = 0.8,
                       radius = 2 * locations_grow()$bin,
                       popup = locations_grow()$station,
                       layerId = locations_grow()$station) %>%
            addCircleMarkers(lng = locations_shrink()$lng,
                             lat = locations_shrink()$lat,
                             weight = 2,
                             fillOpacity = 0.8,
                             radius = 2 * locations_shrink()$bin,
                             popup = locations_shrink()$station,
                             layerId = locations_shrink()$station,
                             fillColor = rgb(1,0,0,1))
    })
    
    output$table_1 <- DT::renderDataTable({
        datatable_reactive()
    })

    plot_1 <- reactive({
        df <- dataset_stations_combined()
        
        if (input$station_select_1 == "" & input$station_select_2 == "") {
            df <- df %>% group_by(year) %>% summarise(patrons = sum(patrons))
        }
        
        if (input$station_select_1 != "" & input$station_select_2 != "") {
            df <- df %>% select(-annual_growth) %>% spread(station, patrons)

            plot_ly(
                data = df,
                x = ~year,
                y = ~get(input$station_select_1),
                type = 'bar',
                name = input$station_select_1,
                text = ~get(input$station_select_1),
                textposition = 'auto',
                hovertemplate = "Year: %{x} <br>Count: %{y:.0f}"
            ) %>%
                add_trace(
                    y = ~get(input$station_select_2),
                    name = input$station_select_2,
                    text = ~get(input$station_select_2),
                    textposition = 'auto'
                ) %>%
                layout(
                    barmode = 'group',
                    title = paste0("Comparison of ", names(which(data_type_options == input$data_type))," (2016-2019)"),
                    xaxis = list(
                        title = "Year"
                    ),
                    yaxis = list(
                        title = "Daily Count of Patrons"
                    ),
                    legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.2
                    )
                )
        } else {
            if (input$station_select_1 != "") {
                chart_title <- paste0("Count of ", names(which(data_type_options == input$data_type)), " at ", input$station_select_1, " (2016-2019)")
            } else if (input$station_select_2 != "") {
                chart_title <- paste0("Count of ", names(which(data_type_options == input$data_type)), " at ", input$station_select_2, " (2016-2019)")              
            } else {
                chart_title <- paste0("Count of ", names(which(data_type_options == input$data_type)), " at all Stations")
            }
            
            plot_ly(
                data = df,
                x = ~year,
                y = ~patrons,
                type = 'bar',
                hovertemplate = "Year: %{x} <br>Count: %{y:.0f}",
                text = ~patrons,
                textposition = 'auto'
            ) %>%
                layout(
                    title = chart_title,
                    yaxis = list(
                        title = "Daily Count of Patrons"
                    ),
                    legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.2
                    )
                )
        }
    })
    
    plot_2 <- reactive({
        df <- dataset_stations_combined()
        df$annual_growth <- df$annual_growth * 100
        
        if (input$station_select_1 == "" & input$station_select_2 == "") {
            df <- df %>% group_by(year) %>% summarise(patrons = sum(patrons))
            
            df$lag <- df$patrons %>% lag(1)
            df$annual_growth <- NA
            df$annual_growth <- round(df$patrons / df$lag - 1, 3) * 100
        }
        
        if (input$station_select_1 != "" & input$station_select_2 != "") {
            df <- df %>% select(-patrons) %>% spread(station, annual_growth)
            
            plot_ly(
                data = df,
                x = ~year,
                y = ~get(input$station_select_1),
                type = 'scatter',
                mode = 'lines',
                name = input$station_select_1,
                hovertemplate = "Year: %{x} <br>Change: %{y}%"
            ) %>%
                add_trace(
                    y = ~get(input$station_select_2),
                    name = input$station_select_2
                ) %>%
                layout(
                    title = paste0("Patronage Change in ", names(which(data_type_options == input$data_type)), " by Station (2016-2019)"),
                    yaxis = list(
                        title = "Percentage Change since previous year",
                        range = c(
                            min(df$annual_growth, 0, na.rm = TRUE),
                            max(df$annual_growth, na.rm = TRUE)
                        )
                    ),
                    legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.2
                    )
                )
        } else {
            if (input$station_select_1 != "") {
                chart_title <- paste0("Percentage Change in ", names(which(data_type_options == input$data_type)), " at ", input$station_select_1, "(2016-2019)")
            } else if (input$station_select_2 != "") {
                chart_title <- paste0("Percentage Change in ", names(which(data_type_options == input$data_type)), " at ", input$station_select_2, "(2016-2019)")              
            } else {
                chart_title <- paste0("Overall Change in ", names(which(data_type_options == input$data_type)), " (2016-2019)")
            }
            
            ymin <- min(df$annual_growth, 0, na.rm = TRUE)
            
            plot_ly(
                data = df,
                x = ~year,
                y = ~annual_growth,
                type = 'scatter',
                mode = 'lines',
                name = input$station_select_1,
                hovertemplate = "Year: %{x} <br>Change: %{y}%"
            ) %>%
                layout(
                    title = chart_title,
                    yaxis = list(
                        title = "Percentage Change since previous year",
                        range = c(
                            min(df$annual_growth, 0, na.rm = TRUE),
                            max(df$annual_growth, na.rm = TRUE)
                        )
                    ),
                    legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.2
                    )
                )
        }
    })
    
    output$chart_1 <- renderPlotly(plot_1())
    output$chart_2 <- renderPlotly(plot_2())
})
