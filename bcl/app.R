library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  theme = shinytheme("paper"), #Choose Shiny Theme
  titlePanel("BC Liquor Store Beverages",
             windowTitle = "BC Liquor App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", min = 0, max = 100, value = c(5, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
      checkboxInput("sortPrice", label = "Sort by Price", value = FALSE)
    ),
    mainPanel(
      plotOutput("coolplot"),  #Output plot
      br(), br(), #Line breaks
      downloadButton(outputId = "downloadTable", label = "Download Table"), #Download button
      br(), br(), #Line breaks
      DT::dataTableOutput("results") #Output table
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      ) 
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) + #Histogram functionality
      geom_histogram(fill = "red", color = "red") +
      theme_bw() +
      labs(title = "Beverage Prices", x = "Alcohol Content", y = "Count")
  })
  
  output$results <- DT::renderDataTable({ #Render interactive table
    if (input$sortPrice == TRUE) {
      filtered() %>%
        arrange(Price)
    } else { 
      filtered()
    }
  })
  
  output$downloadTable <- downloadHandler( #Download Button Functionality
    filename = "bc_liquor_results.csv",
    content = function(file) {
      write.csv(filtered(), file) #Remember parentheses when calling a reactive variable like filtered
    }
  )
}

#Run Application
shinyApp(ui = ui, server = server)
