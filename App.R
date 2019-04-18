library(shiny)
library(shinydashboard)
library(tidyverse)
library(AGD)

kids = read_csv("Predict_Running_Transition_Final.csv") 

logdata = kids %>%
  filter(Transitioned_FullStage==1) %>%
  mutate_if(is.character,as.factor) %>%
  select(Age,HeightCMAvg,WeightKGAvg,Walk_Cadence,Run_Cadence,Sex,BMIz) %>%
  gather(WalkOrRun,Cadence,c(Walk_Cadence,Run_Cadence)) %>%
  mutate(WalkOrRun=case_when(
    WalkOrRun=="Run_Cadence" ~ 1,
    WalkOrRun=="Walk_Cadence" ~ 0)
  ) %>%
  mutate(WalkOrRun=as.factor(WalkOrRun))

logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+BMIz+Cadence, data=logdata,family=binomial(link="logit"))

pymin=min(kids$Run_Cadence)-100
pymax=max(kids$Run_Cadence)+100

ui <- dashboardPage(skin = "blue",
                    
                    # Application title
                    dashboardHeader(title="Walk to Run Cadence",
                                    titleWidth = 300),
                    
                    # Sidebar with a slider input for number of bins 
                    dashboardSidebar(
                      width=300,
                      
                      sidebarMenu(
                        
                        menuItem("Information",tabName="information",icon= icon("dashboard"), startExpanded=TRUE,
                                 sliderInput("age", label = "Age (years)", min=min(kids$Age), max=max(kids$Age), value=10, step=.5),
                                 sliderInput("weight", label = "Weight (kgs)", min(kids$WeightKGAvg), max=max(kids$WeightKGAvg), value=40),
                                 radioButtons("gender", label = "Gender", choiceNames = list("Male","Female"), choiceValues = list("M","F"), inline = TRUE),
                                 #sliderInput("BMIz", label = "BMIz", min=min(kids$BMIz), max=max(kids$BMIz), value=0),
                                 sliderInput("height", label = "Height (cms)", min=min(kids$HeightCMAvg), max=max(kids$HeightCMAvg), value=mean(kids$HeightCMAvg)))
                        #valueBoxOutput("progressBox")
                        
                      ),
                      valueBoxOutput("progressBoxBMI", width=12),       
                      valueBoxOutput("progressBoxBMIz", width=12),
                      valueBoxOutput("progressBoxlb", width=12),
                      # valueBoxOutput("progressBoxWalk", width=12),
                      valueBoxOutput("progressBox", width=12),
                      valueBoxOutput("progressBoxub", width=12)
                      
                    ),
                    
                    # Show a plot of the generated distribution
                    dashboardBody(
                      
                      headerPanel("Current Estimate in Red"),
                      # tags$h5("First Run Prediction in Orange"),
                      # tags$h5("Last Walk Prediction in Blue"),
                      
                      # fluidRow(
                        box(width = 12,
                          title = "Walking or Running",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          # "Box content here", br(), "More box content",
                          plotOutput("ggplotout")
                          # textOutput("modrunresultout")
                      )
                    )
)

# Define server logic 
server <- function(input, output) {

output$progressBoxBMI <- renderValueBox({
  valueBox(
    round(modrunresult()[6,8],3), "BMI",icon = icon("thumbs-up", lib = "glyphicon"),
    color = ifelse(pnorm(modrunresult()[6,2])>.95,"red",ifelse(pnorm(modrunresult()[6,2])>.85,"yellow","green"))
  )
})


output$progressBox <- renderValueBox({
  valueBox(
    paste(round(modrunresult()[6,5]),"Steps"), "Expected First Run Cadence",icon = icon("thumbs-up", lib = "glyphicon"),
    color = "orange"
  )
})

# output$progressBoxWalk <- renderValueBox({
#   valueBox(
#     paste(round(modwalkresult()[6,5]),"Steps"), "Expected Last Walk Cadence",icon = icon("thumbs-up", lib = "glyphicon"),
#     color = "blue"
#   )
# })



modrunresult <- reactive({
  BMItemp=input$weight/(input$height/100)^2
  BMIzcalc=y2z(BMItemp,input$age,sex=input$gender,ref=cdc.bmi)
  nextdata =
    expand.grid(Age = input$age,
                HeightCMAvg = input$height,
                WeightKGAvg = input$weight,
                BMIz = BMIzcalc,
                Cadence = seq(min(logdata$Cadence),max(logdata$Cadence),.1)) %>%
    as_tibble() 
  nextdata = nextdata %>%
    mutate(predictions = predict.glm(logmodel, newdata = nextdata , type = "response"))
  # 
  # cut =
  #   nextdata %>%
  #   mutate(close = abs(.5-predictions)) %>% 
  #   arrange(close) %>%
  #   filter(row_number()==1)
  # cut = cut$Cadence

  return(nextdata)
})

# output$modrunresultout = renderText(unlist(cutresult()))
# output$modrunresultout = renderText(unlist(modrunresult()))


cutresult <- reactive({
  cut =
    modrunresult() %>%
    mutate(close = abs(.5-predictions)) %>%
    arrange(close) %>%
    filter(row_number()==1)
  cut = cut$Cadence
  return(cut)
})

output$ggplotout = renderPlot(
  
  modrunresult() %>%
    ggplot(aes(x=Cadence,y=predictions)) +
    geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = predictions), fill = "orange", alpha = .5) +
    geom_ribbon(aes(ymin = predictions, ymax = 1), fill = "blue", alpha = .5) +
    geom_segment(aes(x= cutresult(), y = 0, xend = cutresult(), yend = .5), color = "red", size = 2) +
    geom_segment(aes(x= cutresult(), y = .5, xend = min(modrunresult()$Cadence ), yend = .5), color = "red", size = 1) +
    geom_label(aes(x=min(modrunresult()$Cadence+10),y=.9, label = "Walking")) +
    geom_label(aes(x=max(modrunresult()$Cadence-10),y=.9, label = "Running")) + 
    geom_label(aes(x=cutresult(),y=0, label = cutresult())) +
    labs(x = "Cadence", y = "Probability", title = "Transition Probability as Cadence Increases")
  )

}

# Run the application
shinyApp(ui = ui, server = server)
