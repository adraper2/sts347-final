# Written by Chris Laskay, Christian Wagner, Graham Parker, and Aidan Draper
# Final Project for STS 347

library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  theme = shinytheme("yeti"),
  
   # Application title
  img(src='elon-logo.png', width = 200, height =65, align= "left"),
  titlePanel("Playing with the Central Limit Theorem"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        # need to add functionality
        selectInput(inputId = "dataset",
                    label = h4("Choose a distribution"),
                    choices = c("Normal", "Skewed")),
        conditionalPanel( condition = "input.dataset =='Normal'",
                          numericInput("mean",
                                       label = h5("Population mean (0-100)"),
                                       min=0,
                                       max=100,
                                       value= 0),
                          numericInput("sd",
                                       label = h5("Population standard deviation (1-100)"),
                                       min= 1,
                                       max=100,
                                       value= 1)),
        conditionalPanel( condition = "input.dataset =='Skewed'",
                          radioButtons("skew", label = h5("Select your skew"),
                                       choices = list("Right" = 1, "Left" = 2), 
                                       selected = 1),
                          sliderInput("severe",
                                      label = div(style='width:250px;',
                                              div(h5('Choose the severity')), 
                                              div(style='float:left;', 'Less'), 
                                              div(style='float:right;', 'More')),
                                      min = 1,
                                      max = 3,
                                      value = 1)
        ),
        br(),
        h4("Choose how to sample"),
        
         # html tag styling in CSS for slider input
        tags$style(HTML(".irs-single {background: #73000a}")),
        tags$style(HTML(".irs-bar {background: white}")),
        tags$style(HTML(".irs-bar {border: 2px solid #73000a}")),
        tags$style(HTML(".irs-bar-edge {background: white}")),
        tags$style(HTML(".irs-bar-edge {border: 2px solid #73000a}")),
        tags$style(HTML("::selection {background: #b59a57}")),
        
        sliderInput("size",
                     h5("Sample size to draw from population"),
                     min = 1,
                     max = 200,
                     value = 30),
        sliderInput("samples",
                     h5("Number of times to repeat sampling"),
                     min = 1,
                     max = 2000,
                     value = 1000)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Population",
                             plotOutput("popPlot"),
                             h4("This is the distribution of the population that our samples will be drawing from.\n"),
                             br(),
                             em("Click on the results tab to view the distribution of this population's sample means.")
                             ),
                    tabPanel("Results", 
                             plotOutput("distPlot"),
                             h4(textOutput("mean")),
                             em("Did you notice anything about this distribution?"),
                             actionButton("answer", label = textOutput("ansLabel")),
                             textOutput("clt")
                             )
                    )
      )
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
   vals <- reactiveValues()
   
   output$popPlot <- renderPlot({
     
     # assign the df1 and df2 values
     vals$mydf1 <- 40
     if (input$skew == 1){
       if (input$severe == 1){
         vals$mydf2 <- 60
       } else if (input$severe == 2){
         vals$mydf2 <- 30
       } else {
         vals$mydf2 <- 10
       }
     } else{
       if (input$severe == 1){
         vals$mydf2 <- 8
       } else if (input$severe == 2){
         vals$mydf2 <- 4
       } else {
         vals$mydf2 <- 2
       }
     }
     
     mydata <- NULL
     if (input$dataset=="Normal"){
       mydata <- rnorm(n=10000, mean=input$mean, sd=input$sd)
       
     } else if (input$dataset=="Skewed" && input$skew==1){
       mydata <- rf(n=10000, df1 = vals$mydf1, df2= vals$mydf2)
     } else if (input$dataset=="Skewed" && input$skew==2){
       mydata <- rbeta(n=10000, shape1 = vals$mydf1, shape2= vals$mydf2)
     }
     
     vals$mydata <- mydata
     
     ggplot(as.data.frame(mydata)) + 
       geom_histogram(aes(x=mydata,y=..density..), bins= 30, position="identity", fill='#b59a57', color="white") + 
       theme_classic() + 
       theme(plot.title = element_text(color="#b59a57", face="bold", size=20, hjust=0)) +
       labs(title="Population Distribution", x ="", y = "Density")
   })
   
   output$distPlot <- renderPlot({
     
     # assign the df1 and df2 values
     vals$mydf1 <- 40
     if (input$skew == 1){
       if (input$severe == 1){
         vals$mydf2 <- 60
       } else if (input$severe == 2){
         vals$mydf2 <- 30
       } else {
         vals$mydf2 <- 10
       }
     } else{
       if (input$severe == 1){
         vals$mydf2 <- 8
       } else if (input$severe == 2){
         vals$mydf2 <- 4
       } else {
         vals$mydf2 <- 2
       }
     }
     
     xmeans <- NULL
     if (input$dataset=="Normal"){
       for (i in 1:input$samples){
         xmeans[i] <- mean(rnorm(n=input$size, mean=input$mean, sd=input$sd))
       }
     } else if (input$dataset=="Skewed" && input$skew == 1){
       for (i in 1:input$samples){
         xmeans[i] <- mean(rf(n=input$size, df1 = vals$mydf1, df2= vals$mydf2))
       }
     } else if (input$dataset=="Skewed" && input$skew == 2){
       for (i in 1:input$samples){
         xmeans[i] <- mean(rbeta(n=input$size, shape1 = vals$mydf1, shape2= vals$mydf2))
       }
     }
      # generate data from ui.R
     
      vals$xmeans <- xmeans
      
      ggplot(as.data.frame(xmeans)) + 
        geom_histogram(aes(x=xmeans,y=..density..), bins=20, fill='#73000a', color="white", position="identity") +
        geom_density(aes(x=xmeans,y=..density..), color='#b59a57', size = 1.5) + theme_classic() +
        theme(plot.title = element_text(color="#73000a", face="bold", size=20, hjust=0)) +
        labs(title="Distribution of Sample Means", x ="Sample Means", y = "Density")
      
   })
   
   output$mean <- renderText({
     
     # generate data from ui.R
     ourmean <- mean(vals$xmeans)
     paste("The mean of all of our sample means was:", signif(ourmean,3))
     
   })
   
   output$ansLabel <- renderText({
     if(input$answer[1] %% 2 != 0){
       paste("Hide")
     } else{
       paste("Answer")
     }
   })
   
   output$clt <- renderText({
     if(input$answer[1] %% 2 != 0){
       paste("According to the Central Limit Theorem, the distribution of our sample means should always be normally distributed (bell-shaped) regardless of the population's distribution so long as the repeated sample size is 30 or greater.")
     } else{
       paste("")
     }
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

