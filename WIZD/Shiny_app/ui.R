### TOP TEAMS IN GAME

library(shiny)

ui <- shinyServer(
  
  
  pageWithSidebar(
    
    headerPanel("ZESPOLY Z LIGI WLOSKIEJ SA NAJLEPSZE W GRZE"),
    
    sidebarPanel(
      
      sliderInput("Number", "Wybierz dla ilu zespolow pokazac dane:",
                  
                  min = 1, max = 30, value = 10, step = 1)
      
    ),
    
    mainPanel(
      
      plotOutput("myPlot")
      
    )
    
  )
  
)
