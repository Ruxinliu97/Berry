library(shiny)
library(tidyverse)

# Load in the data
strawberry <- read.csv("Strawberry.csv")
unfood <- read.csv("Unfood.csv")
yield <- strawberry %>% filter(harvest == "YIELD") 
production <- read.csv("Production.csv") 
production$Value <- as.numeric(gsub(",","",production$Value))

ui <- fluidPage(

    # Add web page title
    titlePanel(h1("Chemicals In Strawberry Farms", 
                  style = {'background-color:hsl(90, 85%,90%);color:green; 
                    border:4px double black'})),
        mainPanel(
          tabsetPanel(
            tabPanel("Distribution of chemical usage in different states", 
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("state",
                                     "State:",
                                     choices = unique(strawberry$State)),
                         hr(),
                         helpText("Chemical-related Data collected by USDA 
                 in strawberry farms")),
                     plotOutput("strawberryPlot"))
          
        ),
        tabPanel("Yield",
                 fluidRow(
                   column(4,
                          selectInput("yield",
                                      "Summary of yield based on Year in CWT/ACRE:",
                                      c("Select",
                                        unique(yield$Year)))
                   )
                 ),
                 DT::dataTableOutput("yield_table"),
                 fluidRow(
                   column(4,
                           selectInput("yield2",
                             "Summary of yield based on State in CWT/ACRE:",
                             c("Select",
                               unique(yield$State)))
          )
        ),
        DT::dataTableOutput("yield_table2")),
        
        tabPanel("Production",
                 fluidRow(
                   column(4,
                          selectInput("prod",
                                      "Summary of production based on Year in $:",
                                      c("Select",
                                        unique(production$Year)))
                   )
                 ),
                 DT::dataTableOutput("prod_table"),
                 
                 fluidRow(
                   column(4,
                          selectInput("prod2",
                                      "Summary of production based on State in $:",
                                      c("Select",
                                        unique(production$State)))
                   )
                 ),
                 DT::dataTableOutput("prod_table2")
        ),
        tabPanel("View Data",
                 fluidRow(
                   column(4,
                          selectInput("yr",
                                      "Year:",
                                      c("Select",
                                        unique(unfood$Year)))
                   ),
                   column(4,
                          selectInput("st",
                                      "State:",
                                      c("Select",
                                        unique(unfood$State)))
                   )
                 ),
                 DT::dataTableOutput("data")) 
                 
    
        
        
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$strawberryPlot <- renderPlot({
  filtered <-
    strawberry %>%
    filter(State == input$state,
    )
    ggplot(filtered, aes(x = Year)) +
    geom_bar(aes(fill = chemical))

}) 
  output$yield_table <- DT::renderDataTable(DT::datatable({
    yield_max <- yield %>% 
      filter(Value != "(NA)") %>% 
      group_by(Year) %>% 
      summarize(max_yield = max(as.numeric(Value)), 
                min_yield = min(as.numeric(Value)),
                mean_yield = mean(as.numeric(Value)),
                median_yield = median(as.numeric(Value)))
    if (input$yield != "Select") {
      yield_max[yield_max$Year == input$yield, ]
      
    }
    
  }))
  output$yield_table2 <- DT::renderDataTable(DT::datatable({
    yield_max2 <- yield %>% 
      filter(Value != "(NA)") %>% 
      group_by(State) %>% 
      summarize(max_yield = max(as.numeric(Value)), 
                min_yield = min(as.numeric(Value)),
                mean_yield = mean(as.numeric(Value)),
                sum_yield = sum(as.numeric(Value)))
    if (input$yield2 != "Select") {
      yield_max2[yield_max2$State == input$yield2, ]
      
    }
    
  }))
  
  output$prod_table <- DT::renderDataTable(DT::datatable({
    prod_max <- production %>% 
      group_by(Year) %>% 
      summarize(max_production = max(Value, na.rm = TRUE), 
                min_production = min(Value, na.rm = TRUE),
                mean_production = mean(Value, na.rm = TRUE),
                sum_production = sum(Value, na.rm = TRUE))
    if (input$prod != "Select") {
      prod_max[prod_max$Year == input$prod, ]
      
    }
    
  }))
  
  output$prod_table2 <- DT::renderDataTable(DT::datatable({
    prod_max2 <- production %>% 
      group_by(State) %>% 
      summarize(max_production = max(Value, na.rm = TRUE), 
                min_production = min(Value, na.rm = TRUE),
                mean_production = mean(Value, na.rm = TRUE),
                sum_production = sum(Value, na.rm = TRUE))
    if (input$prod2 != "Select") {
      prod_max2[prod_max2$State == input$prod2, ]
      
    }
    
  }))
  output$data <- DT::renderDataTable(DT::datatable({
    viewdata <- unfood
    if (input$yr != "Select") {
      viewdata <- viewdata[viewdata$Year == input$yr, ]
    }
    if (input$st != "Select") {
      viewdata <- viewdata[viewdata$State == input$st,]
      
    }
    viewdata
  }))
} 

# Run the application 
shinyApp(ui = ui, server = server)






