
mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))

dat <- read.csv("C:/HighSeas_Allocation/Shiny/Practice/TmpDat.csv", header=TRUE)

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
      checkboxInput("IncPH", "Include PH in allocations", TRUE),
      
      checkboxGroupInput("ScenChoice", "Choose scenarios",
                    choices=names(dat)[-1],
                    selected=names(dat)[-1], inline=TRUE),
      
      conditionalPanel(condition="input.ScenChoice.indexOf('Eff1') != -1",#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("sliderOne", names(dat)[1+1],  min=0, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition=paste0("input.ScenChoice.indexOf('", names(dat)[2+1], "') != -1"),#condition = "input.ScenChoice.includes('Eff1')",
                       sliderInput("sliderTwo", names(dat)[2+1],  min=0, max=10, value=1, width="150px", ticks=TRUE))
      
      
      
      # for(i in 3:4){
      # conditionalPanel(condition=paste0("input.ScenChoice.indexOf('", names(dat)[i+1], "') != -1"),#condition = "input.ScenChoice.includes('Eff1')",
      #                  sliderInput("sliderTwo", names(dat)[i+1],  min=0, max=10, value=1))
      # }
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
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
  formulaText <- reactive({
    paste("mpg ~", ifelse(input$sliderOne > 0, input$sliderOne, 0))
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  datpl <- reactive({
    test <- dat %>% mutate(Res=dat[, input$variable])
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    ggplot(datpl(), aes(x=factor(1), y=Res, fill=as.factor(CCM))) + geom_bar(stat="identity", width=1) + coord_polar(theta="y")
  })
  
}



shinyApp(ui, server)










