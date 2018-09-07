# Title: Homework 1
# Author: Dominic Contreras
# Course: R Shiny for Operations Management
# Date: September 7, 2018

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

cars.load <- mtcars  # read in data
cars.load <- tibble::rownames_to_column(cars.load)  # convert row names to columns
colnames(cars.load)[1] <- "model"  # rename column containig car names
cars.load$disp <- NULL  # delete extra data
cars.load$drat <- NULL
cars.load$qsec <- NULL
cars.load$vs <- NULL
cars.load$am <- NULL
cars.load$gear <- NULL
cars.load$carb <- NULL
cars.load$wt <- cars.load$wt*1000  # convert weight to thousands

cars.load$model <- as.factor(cars.load$model)  # turn model into a factor
carnames <- as.character(unique(cars.load$model))  # create vector with car names

pdf(NULL)

# Define UI for application that draws a histogram
ui <- navbarPage("Cars NavBar", 
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("modelSelect",  # input slot used to access the value
                                          "Model:",  # choice interface label
                                          choices = levels(cars.load$model),  # inputs user selects
                                          multiple = TRUE,  # allows user to select multiple inputs
                                          selectize = TRUE,  # hybrid text box and select box 
                                          selected = carnames[1:32]),  # default inputs selected, here all
                              sliderInput("hpSelect",  # input slot used to access the value
                                          "Horsepower:",  # choice interface label
                                          min = min(cars.load$hp, na.rm = T),  # min value that can be selected
                                          max = max(cars.load$hp, na.rm = T),  # max value that can be selected
                                          value = c(min(cars.load$hp, na.rm = T),  # default range selected
                                                    max(cars.load$hp, na.rm = T)),
                                          step = 1)  # interval between selectable values
                            ),
                            # Output plot
                            mainPanel(  # output elements included in main panel
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 # Data Table
                 tabPanel("Table",  # output elements included in table panel
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

# Define server logic
server <- function(input, output) {
  carsInput <- reactive({  # function that creates reactive expression, or one that will change over time
    cars <- cars.load %>%  # load cars data to reactive expression
      filter(hp >= input$hpSelect[1] & hp <= input$hpSelect[2])  # filters cars data by slider inputs
    
    if (length(input$modelSelect) > 0 ) {  # protects against null input values
      cars <- subset(cars, model %in% input$modelSelect)
    }
    
    return(cars)
  })
  mwInput <- reactive({
    carsInput() %>%
      melt(id = "model")
  })
  
  output$plot <- renderPlotly({
    dat <- carsInput()  # loads data based on input parameters (model + slider range)
    ggplotly(  # function that creates interactive visualizations for each data point
      ggplot(data = dat, aes(x = reorder(model, hp), y = hp, fill = hp,  # sets ggplot aesthetics
                             text = paste0("<b>", model, ":</b> ",  # information parameters for pop-up boxes
                                           "<br>MPG: ", mpg,
                                           "<br>Weight (lbs): ", format(wt, big.mark=","),
                                           "<br>Cylinders: ", cyl,
                                           "<br>Horsepower: ", hp))) + 
        geom_bar(stat = "Identity") +  # creates bar chart
        scale_fill_gradient(low = "red", high = "green") +  # color scale for bar fill
        labs(x = "Car Model",  # axis and main title labels
             y = "Horsepower",
             title = "Horsepower by Car Model") +
        theme(plot.title = element_text(family = 'Helvetica',  # title formatting
                                        color = '#181414', 
                                        face = 'bold', 
                                        size = 18, 
                                        hjust = 0)) +
        theme(axis.title.x = element_text(family = 'Helvetica',  # x-axis formatting
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(axis.title.y = element_text(family = 'Helvetica',  # y-axis formatting
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(legend.position = "none") +  # deletes legend
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +  # tilts x-axis labels
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({  # renders data table
    cars <- carsInput()
    
    subset(cars, select = c(model, mpg, cyl, hp, wt))  # select features to include in data table
  })
  observe({
    print(reactiveValuesToList(input))
  })
}

