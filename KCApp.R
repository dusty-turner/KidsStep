library(shiny)
library(shinydashboard)
library(tidyverse)

kidsraw = read_csv("CADENCE-Kids data.csv")

kids = kidsraw %>%
  filter(complete.cases(.)) %>% ## filter out 2 rows of missing data (only 1 subject) - not worth imputing IMO
  mutate(Stage = as.factor(Stage), Sex = as.factor(Sex), Race = as.factor(Race),Agecat = as.factor(Agecat),Obese_status = as.factor(Obese_status)) %>%
  group_by(id) %>%
  top_n(1, TreadmillSpeed_MPH) %>%
  ungroup() %>%
  select(Sex, Age_years, Race, HeightCMAvg, WeightKGAvg, WaistCMAvg, Cadence_stepsmin, leglengthCM, Tanita.Avg_percentbodyfat, BMI_rawscore, Obese_status) ## removed because of high correlation with BMI and two linearly dependent variables

kids$Race = ifelse(as.character(kids$Race)=="Black or African American","Other",as.character(kids$Race))
mod = lm(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Race+Tanita.Avg_percentbodyfat, data = kids)
pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Race = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction")



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
                  sliderInput("age", label = "Age (years)", min=min(kids$Age_years), max=max(kids$Age_years), value=10),
                  sliderInput("weight", label = "Weight (pounds)", min=25, max=300, value=40),
                  sliderInput("waist", label = "Waist Circumference (cms)", min=10, max=100, value=40),
                  selectInput("race",label="Race",choices = c("White"="White","Other"="Other")),
                  sliderInput("tanita", label = "Tanita Body Fat %", min=0, max=100, value=20)),
                  actionButton("submit", "Submit Information")
          )
      ),
      
      # Show a plot of the generated distribution
     dashboardBody(
         tabItem(tabName="information",
                 tabPanel("All Selections", 
                          # textOutput("modresultout"), 
                          plotOutput("ggplotout")))
      )
  
   # box(
   #   title = "Histogram", status = "primary", solidHeader = TRUE,
   #   collapsible = FALSE,
   #   plotOutput("ggplotout")
   # ),
   # 
   # box(
   #   title = "Inputs", status = "warning", solidHeader = TRUE,
   #   "Box content here", br(), "More box content",
   #   plotOutput("ggplotout", height = 250)
   #    )
   #   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  # observeEvent(input$submit,{
  #   preddf=data.frame(Sex=input$sex,Age_years=input$age,WaistCMAvg=input$waist,Racelimited=input$race,Tanita.Avg_percentbodyfat=input$tanita)
  # })
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  
  modresult <- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=input$sex, Age_years = input$age, WaistCMAvg = input$waist,Race = input$race, Tanita.Avg_percentbodyfat = input$tanita), interval = "prediction")
    guesses = data.frame(
      Sex = input$sex,
      Age_years = input$age,
      Race = input$race,
      Tanita.Avg_percentbodyfat = input$tanita,
      Waist = input$waist,
      preds = pred[1],
      predmin = pred[2],
      predmax = pred[3],
      # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction")
      # Confidence = c(input$game1confidence),
      stringsAsFactors = FALSE)
    return(guesses)
    
  })
  
  
  output$modresultout = renderText(unlist(modresult()))
  # output$modresultout = renderText(unlist(data))
  
  data = data.frame(x=c(1:10),y=(2:11))
  
  output$ggplotout = renderPlot(
    ggplot(aes(x=Age_years,y=preds), data = modresult()) +
      geom_point() +
      geom_errorbar(aes(ymin = predmin, ymax = predmax)) +
      ylim(50,300) + xlim(min(kids$Age_years),(max(kids$Age_years))) +
      xlab("Age") + ylab("Jogging Transition Prediction") + 
      ggtitle("Jogging Transition as Age Changes")
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

