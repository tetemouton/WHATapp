

dat <- read.csv("C:/Users/samm/Desktop/GitHub_Desk/CATGIRL/Practice/TmpDat.csv", header=TRUE)

datsum <- apply(dat[, -1], 2, sum)

datrel <- dat
datrel[, -1] <- dat[, -1]/datsum

CCM_cols <- setNames(c("olivedrab","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4","grey48","dodgerblue2","yellow3","black","pink","green","blue","dodgerblue1","seagreen1","darkgoldenrod4","azure","bisque1","tomato","wheat1","turquoise1","turquoise3","deeppink4","coral3","coral4"),
                     c("AS","AU","CN","CA","CK","EU","FM","FJ","PF","GU","ID","JP","KI","KR","MH","NR","NC","NZ","NU","MP","PW","PG","PH","WS","SB","TW","TK","TO","TV","US","VU","WF"))


# pl <- ggplot(dat, aes(x=factor(1), y=Equal, fill=as.factor(CCM))) + geom_bar(stat="identity", width=1) + coord_polar(theta="y")
# print(pl)


ui <- fluidPage(
  
  # App title ----
  #titlePanel("CATGIRL - a catch/effort allocation tool"),
  titlePanel(title=div(img(height=150, width=100, src="cat.jpg"), "CATGIRL - a catch/effort allocation visualisation tool")),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Scenario:", # Put the choice between PS and LL here
                  c("Equal split" = "Equal",
                    "Best effort" = "Eff1")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("Scen1", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen1 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider1", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen2", "Avg highest 2 effort HS 14-16", TRUE),
      
      conditionalPanel(condition="input.Scen2 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider2", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen3", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen3 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider3", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen4", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen4 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider4", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen5", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen5 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider5", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen6", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen6 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider6", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen7", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen7 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider7", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen8", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen8 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider8", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen9", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen9 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider9", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen10", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen10 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider10", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE))
      
      # checkboxGroupInput("ScenChoice", "Choose scenarios",
      #               choices=names(dat)[-1],
      #               selected=names(dat)[-1], inline=TRUE),
      # 
      # conditionalPanel(condition="input.ScenChoice.indexOf('Eff1') != -1",#condition = "input.ScenChoice.includes('Eff1')",
      #                  sliderInput("sliderOne", names(dat)[1+1],  min=0, max=10, value=1, width="150px", ticks=TRUE)),
      # 
      # conditionalPanel(condition=paste0("input.ScenChoice.indexOf('", names(dat)[2+1], "') != -1"),
      #                  sliderInput("sliderTwo", names(dat)[2+1],  min=0, max=10, value=1, width="150px", ticks=TRUE))
      
      
      
      # for(i in 3:4){
      # conditionalPanel(condition=paste0("input.ScenChoice.indexOf('", names(dat)[i+1], "') != -1"),#condition = "input.ScenChoice.includes('Eff1')",
      #                  sliderInput("sliderTwo", names(dat)[i+1],  min=0, max=10, value=1))
      # }
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Formatted text for caption ----
      #h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      #plotOutput("IndPlot")
      plotlyOutput("IndPlot")
      
    )
  )
)


library(ggplot2)
library(tidyverse)
theme_set(theme_bw())

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  # formulaText <- reactive({
  #   paste("mpg ~", ifelse(input$sliderOne > 0, input$sliderOne, 0))
  # })
  # 
  # # Return the formula text for printing as a caption ----
  # output$caption <- renderText({
  #   formulaText()
  # })

  
  wgtVec <- reactive({
    paste(c(ifelse(input$Scen1 == TRUE, input$slider1, 0),
      ifelse(input$Scen2 == TRUE, input$slider2, 0),
      ifelse(input$Scen3 == TRUE, input$slider3, 0),
      ifelse(input$Scen4 == TRUE, input$slider4, 0),
      ifelse(input$Scen5 == TRUE, input$slider5, 0),
      ifelse(input$Scen6 == TRUE, input$slider6, 0),
      ifelse(input$Scen7 == TRUE, input$slider7, 0),
      ifelse(input$Scen8 == TRUE, input$slider8, 0),
      ifelse(input$Scen9 == TRUE, input$slider9, 0),
      ifelse(input$Scen10 == TRUE, input$slider10, 0)))
  })
  
  

  
  aa <- reactive({
    
    sumfnc <- function(x) sum(datrel[x, -1] * as.numeric(wgtVec()))/sum(as.numeric(wgtVec()))
    
    
    aaa <- sapply(1:32, sumfnc)
  })
  
  
  output$caption <- renderText({
    paste(aa())
  })
  
  
  datpl <- reactive({
    test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2)
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  # output$IndPlot <- renderPlot({
  #   ggplot(datpl(), aes(x=factor(1), y=Res, fill=as.factor(CCM))) + geom_bar(stat="identity", width=1) +
  #          coord_polar(theta="y") + geom_text(aes(x=factor(1), y=pos, label=Resrnd)) + 
  #          scale_fill_manual(name="CCM", values=CCM_cols)
  #   
  #   
  output$IndPlot <- renderPlotly({
    plot_ly(datpl(), labels = ~CCM, values = ~Resrnd, type = 'pie', textposition = 'outside', textinfo = 'label+percent') %>%
      layout(title = "CCM - Allocations",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
}



shinyApp(ui, server)










