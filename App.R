library(shiny)
library(shinydashboard)
library(tidyverse)
library(AGD)

# options(scipen=999)
kidsraw = read_csv("Predict_Running_Transition_Final.csv")

kids = kidsraw %>%
  filter(Transitioned_FullStage==1) %>%
  mutate_if(is.character,as.factor)



mod = lm(Run_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data = kids)

pymin=min(kids$Run_Cadence)-100
pymax=max(kids$Run_Cadence)+100

ui <- dashboardPage(skin = "blue",
                    
                    # Application title
                    dashboardHeader(title="Running Cadence and Speed",
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
                      valueBoxOutput("progressBox", width=12),
                      valueBoxOutput("progressBoxub", width=12)
                      
                    ),
                    
                    # Show a plot of the generated distribution
                    dashboardBody(
                      
                      headerPanel("Current Estimate in Red"),
                      
                      fluidRow(
                        box(
                          title = "Age Changes",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          # "Box content here", br(), "More box content",
                          plotOutput("ggplotout")
                          
                        ),
                        
                        box(
                          title = "Weight Changes",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          # "Box content here", br(), "More box content",
                          plotOutput("ggplotout2")
                        ),
                        
                        box(
                          title = "Gender Changes",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          # "Box content here", br(), "More box content",
                          # tableOutput("table1"),
                          plotOutput("ggplotout3")
                        ),
                        
                        box(
                          title = "Height Changes",
                          status = "primary",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          # "Box content here", br(), "More box content",
                          # tableOutput("progressBoxub1"),
                          plotOutput("ggplotout4")
                        )
                      )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$progressBoxBMI <- renderValueBox({
    valueBox(
      round(modresult()[6,8],3), "BMI",icon = icon("thumbs-up", lib = "glyphicon"),
      color = ifelse(pnorm(modresult()[6,2])>.95,"red",ifelse(pnorm(modresult()[6,2])>.85,"yellow","green"))
    )
  })
  
  output$progressBoxBMIz <- renderValueBox({
    valueBox(
      round(modresult()[6,2],3), "BMIz",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$progressBoxlb <- renderValueBox({
    valueBox(
      paste(round(modresult()[6,6]),"Steps"), "Lower Bound",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "orange"
    )
  })
  output$progressBox <- renderValueBox({
    valueBox(
      paste(round(modresult()[6,5]),"Steps"), "Expected Run Cadence",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "orange"
    )
  })
  output$progressBoxub <- renderValueBox({
    valueBox(
      paste(round(modresult()[6,7]),"Steps"), "Upper Bound",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "orange"
    )
  })
  # output$progressBoxub1 <- renderTable({
  #     modresult4()
  #     # paste(round(modresult()[6,6]),"Steps"), "Upper Bound",icon = icon("thumbs-up", lib = "glyphicon"),
  # })
  
  
  modresult <- reactive({
    BMItemp=input$weight/(input$height/100)^2
    BMIzcalc=y2z(BMItemp,input$age,sex=input$gender,ref=cdc.bmi)
    pred = predict.lm(mod, newdata = data.frame(Age = c((input$age-5):(input$age+5)), WeightKGAvg = rep(input$weight,11), BMIz =rep(BMIzcalc,11), HeightCMAvg = rep(input$height,11)), interval = "prediction", level=.95)
    guesses = data.frame(
      Age = c((input$age-5):(input$age+5)),
      BMIz = rep(BMIzcalc,11),
      Weight = rep(input$weight,11),
      Height = rep(input$height,11),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      BMI=BMItemp,
      
      stringsAsFactors = FALSE)
    return(guesses)
  })
  
  modresult2 <- reactive({
    BMItemp=input$weight/(input$height/100)^2
    BMIzcalc=y2z(BMItemp,input$age,sex=input$gender,ref=cdc.bmi)
    pred = predict.lm(mod, newdata = data.frame(Age = rep(input$age,11), WeightKGAvg =  c((input$weight-5):(input$weight+5)), BMIz = rep(BMIzcalc,11), HeightCMAvg = rep(input$height,11)), interval = "prediction", level=.95)
    guesses = data.frame(
      Age = rep(input$age,11),
      BMIz = rep(BMIzcalc,11),
      Weight = c((input$weight-5):(input$weight+5)),
      Height = rep(input$height,11),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],      
      BMI=BMItemp,
      stringsAsFactors = FALSE)
    return(guesses)
  })
  
  modresult3 <- reactive({
    BMItemp=input$weight/(input$height/100)^2
    BMIzcalc=y2z(BMItemp,input$age,sex=input$gender,ref=cdc.bmi)
    pred = predict.lm(mod, newdata = data.frame(Age = rep(input$age,11), WeightKGAvg = rep(input$weight,11) , BMIz = c((BMIzcalc-5):(BMIzcalc+5)), HeightCMAvg = rep(input$height,11)), interval = "prediction", level=.95)
    guesses = data.frame(
      Age = rep(input$age,11),
      BMIz = c((BMIzcalc-5):(BMIzcalc+5)),
      Weight = rep(input$weight,11),
      Height = rep(input$height,11),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      BMI=BMItemp,
      stringsAsFactors = FALSE)
    return(guesses)
  })
  
  modresult4 <- reactive({
    BMItemp=input$weight/(input$height/100)^2
    BMIzcalc=y2z(BMItemp,input$age,sex=input$gender,ref=cdc.bmi)
    pred = predict.lm(mod, newdata = data.frame(Age = rep(input$age,11), WeightKGAvg = rep(input$weight,11) , BMIz = rep(BMIzcalc,11), HeightCMAvg = c((input$height-5):(input$height+5))), interval = "prediction", level=.95)
    guesses = data.frame(
      Age = rep(input$age,11),
      BMIz = rep(BMIzcalc,11),
      Weight = rep(input$weight,11),
      Height = c((input$height-5):(input$height+5)),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      BMI=BMItemp,
      stringsAsFactors = FALSE)
    return(guesses)
  })
  
  modresult5 <- reactive({
    BMItemp=input$weight/(input$height/100)^2
    BMIzcalcM=y2z(BMItemp,input$age,sex="M",ref=cdc.bmi)
    BMIzcalcF=y2z(BMItemp,input$age,sex="F",ref=cdc.bmi)
    pred = predict.lm(mod, newdata = data.frame(Age = rep(input$age,2), WeightKGAvg = rep(input$weight,2) , BMIz = c(BMIzcalcM,BMIzcalcF), HeightCMAvg = rep(input$height,2)), interval = "prediction", level=.95)
    guesses = data.frame(
      Age = rep(input$age,2),
      BMIz = c(BMIzcalcM,BMIzcalcF),
      Weight = rep(input$weight,2),
      Height = rep(input$height,2),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      gender = c("M","F"),
      BMI=BMItemp,
      stringsAsFactors = FALSE)
    return(guesses)
  })
  
  
  # output$modresultout = renderText(unlist(modresult()))
  # output$modresultout2 = renderTable(unlist(modresult2()))
  # 
  # data = data.frame(x=c(1:10),y=(2:11))
  
  
  output$ggplotout = renderPlot(
    ggplot(aes(x=Age,y=preds), data = modresult()) +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_point() +
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$age,y=preds[which(Age==input$age)]), color="red")+
      geom_errorbar(aes(x=input$age,ymin = predmin[which(Age==input$age)], ymax = predmax[which(Age==input$age)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$age-5),(input$age+5)) +
      xlab("Age") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Age Changes")
  )
  
  output$ggplotout2 = renderPlot(
    ggplot(aes(x=Weight,y=preds), data = modresult2()) +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_point() +
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$weight,y=preds[which(Weight==input$weight)]), color="red")+
      geom_errorbar(aes(x=input$weight,ymin = predmin[which(Weight==input$weight)], ymax = predmax[which(Weight==input$weight)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$weight-5),(input$weight+5)) +
      xlab("Weight") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Weight Changes")
  )

  
  output$ggplotout3 = renderPlot(
    ggplot(aes(x=c("Male","Female"),y=preds), data = modresult5()) +
      # geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_point() +
      # geom_line(aes(y=predmin)) +
      # geom_line(aes(y=predmax)) +
      #geom_point(aes(x=input$gender,y=preds[which(BMIz==input$BMIz)]), color="red")+
      geom_errorbar(aes(x=c("Male","Female"),ymin = predmin[which(gender==c("M","F"))], ymax = predmax[which(gender==c("M","F"))]),color="dark red") +
      # geom_errorbar(aes(x=input$gender,ymin = predmin[which(gender=="M")], ymax = predmax[which(gender=="M")]),color="dark red") +
      ylim(pymin,pymax)  +
      xlab("Gender") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Gender Changes")
  )
  
  # output$table1 = renderTable(modresult5())

  output$ggplotout4 = renderPlot(
    ggplot(aes(x=Height,y=preds), data = modresult4()) +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_point() +
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$height,y=preds[which(Height==input$height)]), color="red")+
      geom_errorbar(aes(x=input$height,ymin = predmin[which(Height==input$height)], ymax = predmax[which(Height==input$height)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$height-5),(input$height+5)) +
      xlab("Height") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Height Changes")
  )
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

