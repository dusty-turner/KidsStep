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
pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Race = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
#plotylim=ylim(50,350),xlim()
pymin=50
pymax=350

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
                  sliderInput("age", label = "Age (years)", min=min(kids$Age_years), max=max(kids$Age_years), value=10, step=1),
                  sliderInput("weight", label = "Weight (pounds)", min=25, max=300, value=40),
                  sliderInput("waist", label = "Waist Circumference (cms)", min=30, max=200, value=40),
                  sliderInput("tanita", label = "Tanita Body Fat %", min=0, max=60, value=20),
                  selectInput("sex",label="Gender",choices = c("Male"="M","Female"="F")),
                  selectInput("race",label="Race",choices = c("White"="White","Other"="Other")))
                  #valueBoxOutput("progressBox")

          ),
       valueBoxOutput("progressBox", width=12)
      ),
      
      # Show a plot of the generated distribution
   dashboardBody(
     
    
       # tabBox(
       #   title = "Input Change Effects  (Current Estimate in Red)",
       #   # The id lets us use input$tabset1 on the server to find the current tab
       #   tabPanel("Age", plotOutput("ggplotout2")),
       #   tabPanel("Weight", plotOutput("ggplotout5")),
       #   tabPanel("Waist", plotOutput("ggplotout3")),
       #   tabPanel("Body Fat", plotOutput("ggplotout4")),
       #   tabPanel("Race", plotOutput("ggplotout6")),
       #   tabPanel("Gender", plotOutput("ggplotout")),
       #   width=12
       # )
      headerPanel("Current Estimate in Red"),

     fluidRow(
     box(
       title = "Age Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       # "Box content here", br(), "More box content",
       plotOutput("ggplotout2")
     ),

     box(
       title = "Weight Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       # "Box content here", br(), "More box content",
       plotOutput("ggplotout5")
     ),

     box(
       title = "Waist Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       # "Box content here", br(), "More box content",
       plotOutput("ggplotout3")
     ),

     box(
       title = "Body Fat % Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       # "Box content here", br(), "More box content",
       plotOutput("ggplotout4")
     ),

     box(
       title = "Gender Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       plotOutput("ggplotout")
     ),

     box(
       title = "Race Changes",
       status = "primary",
       solidHeader = TRUE,
       collapsible = TRUE,
       plotOutput("ggplotout6")
     )

   )
   )
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
  
  # modresult <- reactive({
  #   pred = predict.lm(mod, newdata = data.frame(Sex=input$sex, Age_years = input$age, WaistCMAvg = input$waist,Race = input$race, Tanita.Avg_percentbodyfat = input$tanita), interval = "prediction", level=.9)
  #   guesses = data.frame(
  #     Sex = input$sex,
  #     Age_years = input$age,
  #     Race = input$race,
  #     Tanita.Avg_percentbodyfat = input$tanita,
  #     Waist = input$waist,
  #     preds = pred[1],
  #     predmin = pred[2],
  #     predmax = pred[3],
  #     # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
  #     # Confidence = c(input$game1confidence),
  #     stringsAsFactors = FALSE)
  #   return(guesses)
  #   
  # })
  
  # output$tabset1Selected <- renderText({
  #   input$tabset1
  # })
  
  output$progressBox <- renderValueBox({
    valueBox(
      #paste0(modresult6()[6,6]), "Progress", icon = icon("list"),
      paste(round(modresult2()[6,6]),"Steps"), "Expected Run Cadence",icon = icon("thumbs-up", lib = "glyphicon"),
      color = "orange"
    )
  })
  

  modresult <- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=c("M","F"), Age_years = rep(input$age,2), WaistCMAvg = rep(input$waist,2),Race = rep(input$race,2), Tanita.Avg_percentbodyfat = rep(input$tanita,2)), interval = "prediction", level=.9)
    guesses = data.frame(
      # Sex = input$sex,
      # Age_years = input$age,
      # Race = input$race,
      # Tanita.Avg_percentbodyfat = input$tanita,
      # Waist = input$waist,
      Sex = c("M","F"),
      Age_years = rep(input$age,2),
      Race = rep(input$race,2),
      Tanita.Avg_percentbodyfat = rep(input$tanita,2),
      Waist = rep(input$waist,2),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      stringsAsFactors = FALSE)
    return(guesses)
  })


  modresult2 <- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=rep(input$sex,11), Age_years = c((input$age-5):(input$age+5)), WaistCMAvg = rep(input$waist,11),Race = rep(input$race,11), Tanita.Avg_percentbodyfat = rep(input$tanita,11)), interval = "prediction", level=.9)
    guesses = data.frame(
      # Sex = input$sex,
      # Age_years = input$age,
      # Race = input$race,
      # Tanita.Avg_percentbodyfat = input$tanita,
      # Waist = input$waist,
      Sex = rep(input$sex,11),
      Age_years = c((input$age-5):(input$age+5)),
      Race = rep(input$race,11),
      Tanita.Avg_percentbodyfat = rep(input$tanita,11),
      Waist = rep(input$waist,11),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
      # Confidence = c(input$game1confidence),
      stringsAsFactors = FALSE)
    return(guesses)
    
  })
  
  modresult3<- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=rep(input$sex,11), Age_years = rep(input$age,11), WaistCMAvg = c((input$waist-5):(input$waist+5)),Race = rep(input$race,11), Tanita.Avg_percentbodyfat = rep(input$tanita,11)), interval = "prediction", level=.9)
    guesses = data.frame(
      # Sex = input$sex,
      # Age_years = input$age,
      # Race = input$race,
      # Tanita.Avg_percentbodyfat = input$tanita,
      # Waist = input$waist,
      Sex = rep(input$sex,11),
      Age_years = rep(input$age,11),
      Race = rep(input$race,11),
      Tanita.Avg_percentbodyfat = rep(input$tanita,11),
      Waist = c((input$waist-5):(input$waist+5)),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
      # Confidence = c(input$game1confidence),
      stringsAsFactors = FALSE)
    return(guesses)
    
  })

  modresult4<- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=rep(input$sex,11), Age_years = rep(input$age,11), WaistCMAvg = rep(input$waist,11) ,Race = rep(input$race,11), Tanita.Avg_percentbodyfat = c((input$tanita-5):(input$tanita+5))), interval = "prediction", level=.9)
    guesses = data.frame(
      # Sex = input$sex,
      # Age_years = input$age,
      # Race = input$race,
      # Tanita.Avg_percentbodyfat = input$tanita,
      # Waist = input$waist,
      Sex = rep(input$sex,11),
      Age_years = rep(input$age,11),
      Race = rep(input$race,11),
      Tanita.Avg_percentbodyfat = c((input$tanita-5):(input$tanita+5)),
      Waist = rep(input$waist,11),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
      # Confidence = c(input$game1confidence),
      stringsAsFactors = FALSE)
    return(guesses)
    
  })

    modresult5<- reactive({
    pred = predict.lm(mod, newdata = data.frame(Sex=rep(input$sex,11), Age_years = rep(input$age,11), WaistCMAvg = rep(input$waist,11) ,Race = rep(input$race,11), Tanita.Avg_percentbodyfat = rep(input$tanita,11)), interval = "prediction", level=.9)
    guesses = data.frame(
      # Sex = input$sex,
      # Age_years = input$age,
      # Race = input$race,
      # Tanita.Avg_percentbodyfat = input$tanita,
      # Waist = input$waist,
      Sex = rep(input$sex,11),
      Age_years = rep(input$age,11),
      Race = rep(input$race,11),
      Tanita.Avg_percentbodyfat = rep(input$tanita,11),
      Waist = rep(input$waist,11),
      Weight= c((input$weight-5):(input$weight+5)),
      preds = pred[,1],
      predmin = pred[,2],
      predmax = pred[,3],
      # pred = predict.lm(mod, newdata = data.frame(Sex="M", Age_years = 15, WaistCMAvg = 75,Racelimited = "White", Tanita.Avg_percentbodyfat = 20), interval = "prediction", level=.9)
      # Confidence = c(input$game1confidence),
      stringsAsFactors = FALSE)
    return(guesses)
    
  })

    modresult6 <- reactive({
      pred = predict.lm(mod, newdata = data.frame(Sex=rep(input$sex,2), Age_years = rep(input$age,2), WaistCMAvg = rep(input$waist,2),Race = c("White","Other"), Tanita.Avg_percentbodyfat = rep(input$tanita,2)), interval = "prediction", level=.9)
      guesses = data.frame(
        # Sex = input$sex,
        # Age_years = input$age,
        # Race = input$race,
        # Tanita.Avg_percentbodyfat = input$tanita,
        # Waist = input$waist,
        Sex = rep(input$sex,2),
        Age_years = rep(input$age,2),
        Race = c("White","Other"),
        Tanita.Avg_percentbodyfat = rep(input$tanita,2),
        Waist = rep(input$waist,2),
        preds = pred[,1],
        predmin = pred[,2],
        predmax = pred[,3],
        stringsAsFactors = FALSE)
      return(guesses)
    })
  
  
  output$modresultout = renderText(unlist(modresult()))
  output$modresultout2 = renderTable(unlist(modresult2()))
  
  data = data.frame(x=c(1:10),y=(2:11))
  
  # output$ggplotout = renderPlot(
  #   ggplot(aes(x=Age_years,y=preds), data = modresult()) +
  #     geom_point() +
  #     geom_errorbar(aes(ymin = predmin, ymax = predmax)) +
  #     ylim(50,300) + xlim(min(kids$Age_years),(max(kids$Age_years))) +
  #     xlab("Age") + ylab("Jogging Transition Prediction") + 
  #     ggtitle("Jogging Transition as Age Changes")
  # )
  
  output$ggplotout = renderPlot(
    ggplot(aes(x=Sex,y=preds), data = modresult()) +
      geom_point() +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$sex,y=preds[which(Sex==input$sex)]), color="red")+
      geom_errorbar(aes(ymin = predmin, ymax = predmax)) +
      geom_errorbar(aes(x=input$sex,ymin = predmin[which(Sex==input$sex)], ymax = predmax[which(Sex==input$sex)]), color="dark red") +
      ylim(pymin,pymax) + xlim("M","F") +
      xlab("Gender") + ylab("Jogging Transition Prediction") + 
      ggtitle("Jogging Transition as Gender Changes")
  )
  
  output$ggplotout2 = renderPlot(
    ggplot(aes(x=Age_years,y=preds), data = modresult2()) +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_point() +
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      #geom_point(aes(x=input$age,y=pred[input$age], color="red"))+
      geom_point(aes(x=input$age,y=preds[which(Age_years==input$age)]), color="red")+
      geom_errorbar(aes(x=input$age,ymin = predmin[which(Age_years==input$age)], ymax = predmax[which(Age_years==input$age)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$age-5),(input$age+5)) +
      xlab("Age") + ylab("Jogging Transition Prediction") + 
      ggtitle("Jogging Transition as Age Changes")
  )
  

  output$ggplotout3 = renderPlot(
    ggplot(aes(x=Waist,y=preds), data = modresult3()) +
      geom_point() +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$waist,y=preds[which(Waist==input$waist)]), color="red")+
      geom_errorbar(aes(x=input$waist,ymin = predmin[which(Waist==input$waist)], ymax = predmax[which(Waist==input$waist)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$waist-5),(input$waist+5)) +
      xlab("Waist Circumference (cm)") + ylab("Jogging Transition Prediction") + 
      ggtitle("Jogging Transition as Waist Circumference Changes")
  )

  output$ggplotout4 = renderPlot(
    ggplot(aes(x=Tanita.Avg_percentbodyfat,y=preds), data = modresult4()) +
      geom_point() +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$tanita,y=preds[which(Tanita.Avg_percentbodyfat==input$tanita)]), color="red")+
      geom_errorbar(aes(x=input$tanita,ymin = predmin[which(Tanita.Avg_percentbodyfat==input$tanita)], ymax = predmax[which(Tanita.Avg_percentbodyfat==input$tanita)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$tanita-5),(input$tanita+5)) +
      xlab("Body Fat %") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Body Fat Changes")
  )
  
  output$ggplotout5 = renderPlot(
    ggplot(aes(x=Weight,y=preds), data = modresult5()) +
      geom_point() +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$weight,y=preds[which(Weight==input$weight)]), color="red")+
      geom_errorbar(aes(x=input$weight,ymin = predmin[which(Weight==input$weight)], ymax = predmax[which(Weight==input$weight)]),color="dark red") +
      ylim(pymin,pymax) + xlim((input$weight-5),(input$weight+5)) +
      xlab("Weight (lbs)") + ylab("Jogging Transition Prediction") +
      ggtitle("Jogging Transition as Weight Changes")
  )

  output$ggplotout6 = renderPlot(
    ggplot(aes(x=Race,y=preds), data = modresult6()) +
      geom_point() +
      geom_ribbon(aes(ymin=predmin,ymax=predmax),alpha=.2)+
      geom_line(aes(y=predmin)) +
      geom_line(aes(y=predmax)) +
      geom_point(aes(x=input$race,y=preds[which(Race==input$race)]), color="red")+
      geom_errorbar(aes(ymin = predmin, ymax = predmax)) +
      geom_errorbar(aes(x=input$race, ymin = predmin[which(Race==input$race)], ymax = predmax[which(Race==input$race)]), color="dark red") +
      ylim(pymin,pymax) + xlim("White","Other") +
      xlab("Race") + ylab("Jogging Transition Prediction") + 
      ggtitle("Jogging Transition as Race Changes")
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

