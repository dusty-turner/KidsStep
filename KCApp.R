#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
   
   # Application title
   dashboardHeader(title="Running Cadence and Speed",
                   titleWidth = 300),
   
   # Sidebar with a slider input for number of bins 
   dashboardSidebar(
     width=300,
      sidebarMenu(
         menuItem("Information",tabName="information",icon= icon("dashboard"), startExpanded=TRUE,
                  selectInput("sex",label="Gender",choices = c("Male"="M","Female"="F")),
                  sliderInput("age", label = "Age (years)", min=1, max=30, value=10),
                  sliderInput("weight", label = "Weight (pounds)", min=25, max=300, value=40),
                  sliderInput("waist", label = "Waist Circumference (cms)", min=10, max=100, value=40),
                  selectInput("race",label="Race",choices = c("White"="White","Other"="Other")),
                  sliderInput("tanita", label = "Tanita Body Fat %", min=0, max=100, value=20)),
                  actionButton("submit", "Submit Information")
          )
      ),
      
      # Show a plot of the generated distribution
     dashboardBody(
       tabItems(
         tabItem(tabName="information")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$submit,{
    preddf=data.frame(Sex=input$sex,Age_years=input$age,WaistCMAvg=input$waist,Racelimited=input$race,Tanita.Avg_percentbodyfat=input$tanita)
  })
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

