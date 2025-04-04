library(shiny)
library(sf)
library(readxl)
library(plotly)
library(tidyverse)
library(DT)
library(dplyr)
library(gridlayout)
library(bslib)

# Sample data loading function
load_data <- function() {
     # Load the data from files
     
     #from here: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
     data <- read_sf("US_Counties/cb_2018_us_county_20m.shp")
     
     #from here: https://www.bea.gov/data/income-saving/personal-income-county-metro-and-other-areas
     income <- read_excel("US_Counties/CountyIncomes.xlsx")
     
     #from here: https://www.zillow.com/research/data/
     house <- read_csv("US_Counties/CountyHouse.csv")
     
     # Data cleaning
     house$NAME <- gsub(" County", "", house$NAME)
     house$NAME <- gsub("Saint", "St.", house$NAME)
     house$NAME <- gsub(" Parish", "", house$NAME)
     
     data$STATEFP <- as.numeric(data$STATEFP)
     data <- left_join(data, income, by = c("STATEFP", "NAME"))
     data <- left_join(data, house, by = c("STATEFP", "NAME"))
     
     data <- data %>% filter(as.numeric(STATEFP) != 72) # Remove Puerto Rico
     data <- data %>% filter(as.numeric(STATEFP) != 15) # Remove Hawaii
     data <- data %>% filter(as.numeric(STATEFP) != 2)  # Remove Alaska
     
     return(data)
}

ui <- grid_page(
     layout = c(
          "header      header      header      header     ",
          "HilightCard HilightCard HilightCard radarArea  ",
          "HilightCard HilightCard HilightCard radarArea  ",
          "sidebar     sidebar     sidebar     download   ",
          "DataTable   DataTable   mapArea     mapArea    "
     ),
     row_sizes = c(
          "90px",
          "515px",
          "500px",
          "280px",
          "780px"
     ),
     col_sizes = c(
          "250px",
          "0.98fr",
          "0.77fr",
          "0.5fr"
     ),
     gap_size = "2rem",
     
     ## Header Card --------------
     grid_card_text(
          area = "header",
          content = "Income and Housing Cost by County in the United States",
          alignment = "start",
          is_title = FALSE,
     ),
     
     ## Download Card -----------
     grid_card(
          area = "download",
          card_header("Download File"),
          card_body(
               div(
                    style = "display: flex; justify-content: center; align-items: center; height: 80%;",
                    downloadButton(
                         outputId = "report",
                         label = "Generate Report"
                    )
               )
          )
     ),
     
     ## Sidebar Card -------------
     grid_card(
          area = "sidebar",
          card_header("Parameters"
          ),
          card_body(
               inputPanel(
               selectInput( #Make drop down not go into card making it hard to see.
                    inputId = "myState",
                    label = "Choose State",
                    choices = NULL 
               ),
               tags$p("      "),
               sliderInput("income", "Income", min = 25000, max = 450000, value = c(50000, 80000), step = 10000),
               tags$p("      "),
               sliderInput("housing", "Housing Cost", min = 50000, max = 2750000, value = c(250000, 500000), step = 25000),
               tags$p("      "),
               tags$p("-------------------------------------- All counties that do not fit within the parameters will appear black.", style = "font-size: 17px;")
               )
           ),
          card_footer(
               actionButton(
                    inputId = "update",
                    label = "Update"
               )
          )
     ),
     
     ## Tab Card for Value Boxes ---------------
     grid_card(
          area = "HilightCard",
          full_screen = TRUE,
          card_header("County Statistics"),
          card_body(
               tabsetPanel(
                    id = "HighlightsTab",
                    nav_panel(
                         title = "By Income",
                         grid_container(
                              layout = c(
                                   "valueA1 valueA2"
                              ),
                              row_sizes = c(
                                   "900px"
                              ),
                              col_sizes = c(
                                   "1fr",
                                   "1fr"
                              ),
                              gap_size = "10px",
                              grid_card(
                                   area = "valueA1",
                                   card_body(
                                        plotOutput(outputId = "plot2")
                                   )
                              ),
                              grid_card(
                                   area = "valueA2",
                                   card_body(
                                        plotOutput(outputId = "plot3")
                                   )
                              )
                          )
                    ),
                    nav_panel(
                         title = "By Housing",
                         grid_container(
                              layout = c(
                                   "valueA1 valueA2"
                              ),
                              row_sizes = c(
                                   "900px"
                              ),
                              col_sizes = c(
                                   "1fr",
                                   "1fr"
                              ),
                              gap_size = "10px",
                              grid_card(
                                   area = "valueA1",
                                   card_body(
                                        plotOutput(outputId = "plot4")
                                   )
                              ),
                              grid_card(
                                   area = "valueA2",
                                   card_body(
                                        plotOutput(outputId = "plot5")
                                   )
                              )
                         )
                    ),
                    nav_panel(
                         title = "By Ratio",
                         grid_container(
                              layout = c(
                                   "valueA1 valueA2"
                              ),
                              row_sizes = c(
                                   "900px"
                              ),
                              col_sizes = c(
                                   "1fr",
                                   "1fr"
                              ),
                              gap_size = "10px",
                              grid_card(
                                   area = "valueA1",
                                   card_body(
                                        plotOutput(outputId = "plot6")
                                   )
                              ),
                              grid_card(
                                   area = "valueA2",
                                   card_body(
                                        plotOutput(outputId = "plot7")
                                   )
                              )
                         )
                    )
               )
          )
     ),
     
     
     ## Data Table Panel --------------
     grid_card(
          area = "DataTable",
          card_header("County Level Data"),
          card_body(
               DTOutput(outputId = "table1")
          )
     ),
     
     ## mapArea Panel --------------
     grid_card(
          area = "mapArea",
          card_header("Income vs. Housing"),
          card_body(
               plotlyOutput(outputId = "plot1")
          )
     ),
     
     ## radarArea Panel ------------
     grid_card(
          area = "radarArea",
          card_header("State Averages"),
          card_body(
               plotlyOutput(outputId = "radar1")
          )
     )
)

server <- function(input, output, session) #Need to fix default values not showing up until update is pressed.
     {
     # Load the data
     data <- load_data()
     
     # Calculate national averages
     national_avg <- data %>%
          summarise(avg_income = mean(INCOME), avg_housing_cost = mean(COST))
     
     # Populate the state dropdown with unique values
     observe({
          updateSelectInput(session, "myState", choices = c("All", sort(unique(data$STATE))))
     })
     
     # Render the data table
     output$table1 <- renderDT({
          input$update  # Depend on the update button
          
          isolate({
               data_df <- as.data.frame(data)
               
               if (input$myState != "All") {
                    data_df <- data_df %>% filter(STATE == input$myState)
               }
               
               # Filtering based on input ranges
               data_df <- data_df %>%
                    filter(INCOME >= input$income[1] & INCOME <= input$income[2]) %>%
                    filter(COST >= input$housing[1] & COST <= input$housing[2])
               
               # Renaming and selecting columns
               data_df <- as.data.frame(data_df)
                                        data_df <- data_df %>%
                                        rename(HOUSING_COST = COST) %>%
                                        rename(COUNTY_NAME = NAME) %>%
                                             drop_na() %>%
                                        select(COUNTY_NAME, STATE, INCOME, HOUSING_COST)
               
               # Render the datatable
               datatable(data_df, options = list(pageLength = 10, autoWidth = FALSE))
          })
     })
     
     # Radar plot function
     output$radar1 <- renderPlotly({ #add rank for each state
          isolate({
               radar_df <- data %>%
                    group_by(STATE) %>%
                    summarise(avg_income = mean(INCOME), avg_housing_cost = mean(COST))
               
               plot_ly(
                    type = 'scatterpolar',
                    fill = 'toself'
               ) %>%
                    add_trace(
                         r = c(radar_df$avg_income, radar_df$avg_income[1]),
                         theta = c(radar_df$STATE, radar_df$STATE[1]),
                         name = "Avg Income"
                    ) %>%
                    add_trace(
                         r = c(radar_df$avg_housing_cost, radar_df$avg_housing_cost[1]),
                         theta = c(radar_df$STATE, radar_df$STATE[1]),
                         name = "Avg Housing Cost"
                    ) %>%
                    layout(
                         polar = list(
                              radialaxis = list(
                                   visible = T,
                                   range = c(0, max(c(radar_df$avg_income, radar_df$avg_housing_cost)))
                              )
                         ),
                         showlegend = T
                    )
          })
     })
     
     # Render the plot
     output$plot1 <- renderPlotly({
          input$update  # Depend on the update button
                    isolate({
                         plot_data <- data
                         
                         if (input$myState != "All") {
                              plot_data <- plot_data %>% filter(STATE == input$myState)
                         }

                         custom_colors <- c("grey", "blue")
                         breaks <- c(min(input$income), max(input$income))
                         labels <- c(min(input$income), max(input$income))
                         threshold <- max(input$income)
                         
                         plot_param <- ggplot(data = plot_data) +
                              geom_sf(aes(fill = ifelse(INCOME >= input$income[1] & INCOME <= input$income[2] &
                                                             COST >= input$housing[1] & COST <= input$housing[2], INCOME, NA),
                                          text = paste("County: ", NAME, "<br>",
                                                       "Income: $", INCOME, "<br>",
                                                       "Housing Cost: $", COST))) +
                              scale_fill_gradientn(name = "Yearly Personal Income ($)",
                                                   colors = custom_colors,
                                                   limits = c(min(input$income), threshold),
                                                   breaks = breaks,
                                                   labels = labels,
                                                   na.value = "#000000") +
                              theme_minimal() +
                              theme(plot.title = element_text(size = 18L, hjust = 0.5),
                                    plot.subtitle = element_text(size = 11L, hjust = 0.5),
                                    panel.grid = element_blank(),
                                    axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank())
                         
                         ggplotly(plot_param, tooltip = "text") %>%
                              layout(showlegend = FALSE)
                    })
     })
     
     output$plot3 <- renderPlot({
          input$update  # Depend on the update button
          isolate({
               plot_data <- data
               
               custom_colors <- c("grey", "blue")
               breaks <- c(min(input$income), max(input$income))
               labels <- c(min(input$income), max(input$income))
               threshold <- max(input$income)
               
               plot_param2 <- ggplot(data = plot_data) +
                    geom_sf(aes(fill = ifelse(INCOME >= input$income[1] & INCOME <= input$income[2], INCOME, NA))) +
                    scale_fill_gradientn(name = "Yearly Personal Income ($)",
                                         colors = custom_colors,
                                         limits = c(min(input$income), threshold),
                                         breaks = breaks,
                                         labels = labels,
                                         na.value = "#000000") +
                    labs(title = paste("Income between", min(input$income), "and", max(input$income)),
                         subtitle = "On Average by County") +
                    theme_minimal() +
                    theme(plot.title = element_text(size = 18L, hjust = 0.5),
                          plot.subtitle = element_text(size = 11L, hjust = 0.5),
                          panel.grid = element_blank(),
                          axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank())
               
               print(plot_param2)
          })
     })
          output$plot2 <- renderPlot({
               isolate({
                    plot_data <- data

                    custom_colors <- c("grey", "blue")
                              breaks <- c(0, 100000)
                              labels <- c("< 30000", "> 100000")
                              threshold <- 100000

                              income_plot <- ggplot(data = data) +
                                   geom_sf(aes(fill = INCOME)) +
                                   scale_fill_gradientn(name = "Yearly Personal Income ($)",
                                                        colors = custom_colors,
                                                        limits = c(0, threshold),
                                                        breaks = breaks,
                                                        labels = labels,
                                                        na.value = "blue") +

                                   labs(title = "Income is Fairly Consistent",
                                        subtitle = "By County in the United States") +

                                   theme_minimal() +
                                   theme(plot.title = element_text(size = 18L, hjust = 0.5),
                                         plot.subtitle = element_text(size = 11L, hjust = 0.5),
                                         panel.grid = element_blank(),
                                         axis.line = element_blank(),
                                         axis.text = element_blank(),
                                         axis.ticks = element_blank())

                    print(income_plot)
               })
          })

          output$plot4 <- renderPlot({
               isolate({
                    plot_data <- data

                    custom_colors <- c("grey", "blue")
                              breaks <- c(0, 700000)
                              labels <- c("< 100000", "> 700000")
                              threshold <- 700000

                              house_plot <- ggplot(data = data) +
                                   geom_sf(aes(fill = COST)) +
                                   scale_fill_gradientn(name = "Average House Cost ($)",
                                                        colors = custom_colors,
                                                        limits = c(0, threshold),
                                                        breaks = breaks,
                                                        labels = labels,
                                                        na.value = "blue") +

                                   labs(title = "Western US is More Expensive for Housing",
                                        subtitle = "On Average by County") +

                                   theme_minimal() +
                                   theme(plot.title = element_text(size = 18L, hjust = 0.5),
                                         plot.subtitle = element_text(size = 11L, hjust = 0.5),
                                         panel.grid = element_blank(),
                                         axis.line = element_blank(),
                                         axis.text = element_blank(),
                                         axis.ticks = element_blank())

                    print(house_plot)
               })
          })
          
          output$plot5 <- renderPlot({
               input$update  # Depend on the update button
               isolate({
                    plot_data <- data
                    
                    custom_colors <- c("grey", "blue")
                    breaks <- c(min(input$housing), max(input$housing))
                    labels <- c(min(input$housing), max(input$housing))
                    threshold <- max(input$housing)
                    
                    plot_param3 <- ggplot(data = plot_data) +
                         geom_sf(aes(fill = ifelse(COST >= input$housing[1] & COST <= input$housing[2], COST, NA))) +
                         scale_fill_gradientn(name = "Average House Cost ($)", 
                                              colors = custom_colors,
                                              limits = c(min(input$housing) , threshold),
                                              breaks = breaks,
                                              labels = labels,
                                              na.value = "black") +
                         labs(title = paste("Housing Prices between", min(input$housing), "and", max(input$housing)),
                              subtitle = "On Average by County") +
                         theme_minimal() +
                         theme(plot.title = element_text(size = 18L, hjust = 0.5),
                               plot.subtitle = element_text(size = 11L, hjust = 0.5),
                               panel.grid = element_blank(),
                               axis.line = element_blank(),
                               axis.text = element_blank(),
                               axis.ticks = element_blank())
                    
                    print(plot_param3)
               })
          })

          output$plot6 <- renderPlot({
               isolate({
                    plot_data <- data

                    custom_colors <- c("grey", "blue")
                              breaks <- c(0, 10)
                              labels <- c("< 1", "> 10")
                              threshold <- 10

                              ratio_plot <- ggplot(data = data) +
                                   geom_sf(aes(fill = COST/INCOME)) +
                                   scale_fill_gradientn(name = "Price : Income Ratio",
                                                        colors = custom_colors,
                                                        limits = c(0, threshold),
                                                        breaks = breaks,
                                                        labels = labels,
                                                        na.value = "blue") +

                                   labs(title = "The Midwest is the Best Place to Buy a House",
                                        subtitle = "Based on Percent of Income Spent") +

                                   theme_minimal() +
                                   theme(plot.title = element_text(size = 18L, hjust = 0.5),
                                         plot.subtitle = element_text(size = 11L, hjust = 0.5),
                                         panel.grid = element_blank(),
                                         axis.line = element_blank(),
                                         axis.text = element_blank(),
                                         axis.ticks = element_blank())

                    print(ratio_plot)
               })
          })
          
          output$plot7 <- renderPlot({
               input$update  # Depend on the update button
               isolate({
                    plot_data <- data

                    min_ratio = min(input$housing)/min(input$income)
                    max_ratio = max(input$housing)/max(input$income)
                    min_ratio <- round(min_ratio, 2)
                    max_ratio <- round(max_ratio, 2)

                    custom_colors <- c("grey", "blue")
                    breaks <- c(min_ratio, max_ratio)
                    labels <- c(min_ratio, max_ratio)
                    threshold <- max_ratio

                    plot_param4 <- ggplot(data = plot_data) +
                         geom_sf(aes(fill = COST/INCOME)) +
                         scale_fill_gradientn(name = "Price : Income Ratio",
                                              colors = custom_colors,
                                              limits = c(min_ratio, threshold),
                                              breaks = breaks,
                                              labels = labels,
                                              na.value = "black") +
                         labs(title = paste("Housing/Income Ratio between", min_ratio, "and", max_ratio),
                              subtitle = "Based on Average Housing Cost and Income") +
                         theme_minimal() +
                         theme(plot.title = element_text(size = 18L, hjust = 0.5),
                               plot.subtitle = element_text(size = 11L, hjust = 0.5),
                               panel.grid = element_blank(),
                               axis.line = element_blank(),
                               axis.text = element_blank(),
                               axis.ticks = element_blank())

                    print(plot_param4)
               })
          })
          
          output$report <- downloadHandler(
               filename = function(){
                    paste0(input$myState,"_",Sys.Date(),".html")
               },
               content = function(file) {
                    tempReport <- file.path(tempdir(), "STAT407Assignment3.Rmd")
                    file.copy("STAT407Assignment3.Rmd", tempReport, overwrite = TRUE)
                    params <- list(income = input$income, housing = input$housing, state = input$myState, data = load_data())
                    rmarkdown::render(tempReport,
                                      output_file = file,
                                      params = params,
                                      envir = new.env(parent = globalenv())
                    )
               }
          )
}

shinyApp(ui, server)
