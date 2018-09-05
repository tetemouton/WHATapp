

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
    
    # Sidebar panel for inputs ----
    fluidRow(
      column(2,
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Gear type:", # Put the choice between PS and LL here
                  c("Purse seine" = "Purse",
                    "Longline" = "LongL"), width="150px"),
      
      checkboxInput("Scen1", "Best effort HS 06-16", TRUE),
      
      conditionalPanel(condition="input.Scen1 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider1", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen2", "Avg highest 2 effort HS 14-16", FALSE),
      
      conditionalPanel(condition="input.Scen2 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider2", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen4", "Avg highest 5 effort HS 02-16", FALSE),
      
      conditionalPanel(condition="input.Scen4 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider4", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen6", "Current EEZ allocation Att Tab 1", FALSE),
      
      conditionalPanel(condition="input.Scen6 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider6", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen8", "EEZ size in CA", FALSE),
      
      conditionalPanel(condition="input.Scen8 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider8", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE))),
      
      column(2,
      
             # Input: Selector for variable to plot against mpg ----
             selectInput("PHinc", "Include Philippines?", # Put the choice between PS and LL here
                         c("Include PH" = "InPH",
                           "Exclude PH" = "OutPH"), width="150px"),       
             
             numericInput("num1", h4("Allocation TAE"), value=10000, width="150px"),
             
      # Input: Checkbox for whether outliers should be included ----
      
      checkboxInput("Scen3", "Avg highest 3 effort HS 07-16", FALSE),
      
      conditionalPanel(condition="input.Scen3 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider3", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
     
      
      checkboxInput("Scen5", "Current HS allocation Att Tab 1", FALSE),
      
      conditionalPanel(condition="input.Scen5 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider5", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      
      
      checkboxInput("Scen7", "Biomass SKJ in zones", FALSE),
      
      conditionalPanel(condition="input.Scen7 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider7", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      
      
      checkboxInput("Scen9", "Economic dependency", FALSE),
      
      conditionalPanel(condition="input.Scen9 == true",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("slider9", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      checkboxInput("Scen10", "Equal split", FALSE),
      
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
    column(4, plotlyOutput("IndPlot")),
    column(2, offset=1, dataTableOutput("AllocTab")))
    # For a more simple table:
    # column(1, offset=1, dataTableOutput("AllocTab")))
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
    paste(input$num1)
  })
  
  
  datpl <- reactive({
    test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1)
    
    if(input$PHinc == "OutPH"){
      test <- filter(test, CCM != "PH")
    } else {
      test <- test
    }
    
  })
  
  dattab <- reactive({
    test <- data.frame(CCM=datrel$CCM, Percent=round(aa()*100, 0), Allocate=round(aa()*input$num1,0))
    
    if(input$PHinc == "OutPH"){
      test <- filter(test, CCM != "PH")
    } else {
      test <- test
    }
    
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
    plot_ly(datpl(), labels=~CCM, values=~Resrnd, type="pie",
            textposition="inside", textinfo="label+value", width=700, height=700) %>%
      layout(title = "CCM - Allocations",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$AllocTab <- renderDataTable(dattab(), options=list(pageLength=15))
  # To show the default no. output$AllocTab <- renderDataTable(dattab())
  
  # For a more simple table:
  # output$AllocTab <- renderTable(dattab())#, options=list(pageLength=10))
  
}



shinyApp(ui, server)










