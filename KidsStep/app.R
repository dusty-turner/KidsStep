library(shiny) 
library(tidyverse)
library(shinyAce)
library(ggplot2)  
library(googlesheets)
library(DT)
library(shinydashboard)

# install.packages("xml2")

kidsraw = read_csv("CADENCE-Kids data.csv")

kids = kidsraw %>%
  filter(complete.cases(.)) %>% ## filter out 2 rows of missing data (only 1 subject) - not worth imputing IMO
  mutate(Stage = as.factor(Stage), Sex = as.factor(Sex), Race = as.factor(Race),Agecat = as.factor(Agecat),Obese_status = as.factor(Obese_status)) %>%
  group_by(id) %>%
  top_n(1, TreadmillSpeed_MPH) %>%
  ungroup() %>%
  select(Sex, Age_years, Race, HeightCMAvg, WeightKGAvg, WaistCMAvg, Cadence_stepsmin, leglengthCM, Tanita.Avg_percentbodyfat, BMI_rawscore, Obese_status) ## removed because of high correlation with BMI and two linearly dependent variables

kids$Racelimited = ifelse(as.character(kids$Race)=="Other","Black or African American",as.character(kids$Race))
mod = lm(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat, data = kids)
pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction")


# http://fontawesome.io/icons/
# Define UI for slider demo app ----
ui <- dashboardPage(skin = "yellow",
  
  # App title ----
  dashboardHeader(title = "Kids Cadence"),
  
  # Sidebar layout with input and output definitions ----
    # Sidebar to demonstrate various slider options ----
    dashboardSidebar(
      sidebarMenu(
        # menuItem("Instructions", tabName = "instructions", icon = icon("newspaper-o")),
        menuItem("Information", tabName = "information", icon = icon("dashboard"), startExpanded = TRUE, 
                 radioButtons("gender", label = "Gender", choices = c("M", "F")),
                 numericInput("age", label = "Age", value = 10, min = 4, max = 20)),
                 numericInput("waist", labe = "Waist", value = 30, min = 5, max = 50),
                 radioButtons("race", label = "Race", choices = c("White", "Non-White")),
                 numericInput("bfpercent", label = "Body Fat Percent", value = 5, min = 12, max = 60))
      ),

  # Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat
      
  dashboardBody(
      # tabItems(
      #   ),
        tabItem(tabName = "picks",
                tabPanel("All Selections", textOutput("modresultout"), plotOutput("ggplotout")))
                  # tabPanel("All Selections", tableOutput("fromgoogle"))
      
    )
  )



server <- function(input, output) {
  
  # Reactive expression to create data frame of all input values ----
  modresult <- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=input$gender, Age_years = input$age, WaistCMAvg = input$waist,Racelimited = input$race, Tanita.Avg_percentbodyfat = input$bfpercent), interval = "prediction")
    guesses = data.frame(
      Sex = input$gender,
      Age_years = input$age,
      Racelimited = input$race,
      Tanita.Avg_percentbodyfat = input$bfpercent,
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
      ylim(50,300)
  )

}

shinyApp(ui, server)