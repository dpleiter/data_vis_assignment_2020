library(shiny)
library(leaflet)
library(plotly)
library(readr)

locations <- read_csv("in/locations_good.csv")

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

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        title = "Sydney Trains Patronage Dashboard",
        fluidRow(
            column(12,
                   h1("Sydney Trains Patronage Dashboard"),
                   hr())
        ),
        fluidRow(
            column(2,
                   HTML("<h4><b>How to use this dashboard</b></h4>"),
                   HTML("<p>First select which data you would like to view using the first dropdown below. To compare different stations either select
                        using the two dropdowns or by clicking a circle on the map.</p>
                        
                        <p>Alternatively, click on one of the scenario buttons to get started.</p>
                        
                        <p>The size of the circles on the map are scaled to size based on the rank of the total daily patronage over the period 2016-2019.</p>
                        
                        <p>Blue circles are stations that have increased patronage for the selected data type from 2016-2019, and red have decreased over the same time period.</p>
                        
                        <p>Happy exploring!</p>"),
                   hr(),
                   selectInput("data_type",
                               "Choose which data you wish to view",
                               data_type_options),
                   hr(),
                   selectInput("station_select_1",
                               "Choose Station 1",
                               c("Choose one" = "", locations$station)),
                   selectInput("station_select_2",
                               "Choose Station 2",
                               c("Choose one" = "", locations$station)),
                   hr(),
                   HTML("<h4><b>Scenarios</b></h4>"),
                   actionButton(
                       inputId = "button_scenario_1",
                       label = "Scenario 1"
                   ),
                   actionButton(
                       inputId = "button_scenario_2",
                       label = "Scenario 2"
                   ),
                   conditionalPanel(condition = "output.showScenario1",
                                    hr(),
                                    HTML("<h4><b>Commentary</b></h4>"),
                                    HTML("<p>Parramatta, located in the heart of Sydney's sprawling CBD, has a substantial growth curve when compared to
                                         Town Hall station in the centre of Sydney's CBD. Growth in the west has been around 10% higher for the last 3 years</p>")
                                    ),
                   conditionalPanel(condition = "output.showScenario2",
                                    hr(),
                                    HTML("<h4><b>Commentary</b></h4>"),
                                    HTML("<p>Comparing two well known inner city areas, Kings Cross and Bondi Junction, it's easy to see that growth appears to have
                                         stagnated. Maybe people are getting sick of the hustle and bustle and are spending more time away from the big smoke.</p>")
                   ),
                   hr(),
                   HTML("<h4><b>Data Source</b></h4>
                        <p>Opendata.transport.nsw.gov.au. 2020. Train Station Entries And Exits Data. [online] Available at: <https://opendata.transport.nsw.gov.au/dataset/train-station-entries-and-exits-data> [Accessed 17 June 2020].</p>")
                   ),
            column(10,
                   fluidRow(
                       column(6,
                              leafletOutput("map",
                                            height = "500px")),
                       column(6,
                              plotlyOutput("chart_1"))
                   ),
                   fluidRow(
                       column(6,
                              DT::dataTableOutput("table_1")),
                       column(6,
                              plotlyOutput("chart_2"))
                   ))
        ),
        tags$footer("Dashboard created by Dylan Pleiter (s3252987) for a RMIT University Data Visualisation project. Semester 1, 2020")
    )
)
