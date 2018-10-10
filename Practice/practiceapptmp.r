




ui <- fluidPage(
  
  # App title ----
  #titlePanel("CATGIRL - a catch/effort allocation tool"),
  titlePanel(title=div(img(height=150, width=100, src="cat.jpg"), "CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining")),
  #headerPanel( title=div(img(height=150, width=100, src="cat.jpg"),
  #                       h3("CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining", align="center", style="bold")
  #)),
  # Sidebar layout with input and output definitions ----
    
    # Sidebar panel for inputs ----
    fluidRow(
      column(2,
      
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Gear type:", # Put the choice between PS and LL here
                  c("Purse seine" = "Purse",
                    "Longline" = "LongL"), width="150px"),
      
      radioButtons("figtype", label = h3("Figure scale:"),
                   choices = list("By CCM" = 1, "By category" = 2), 
                   selected = 1),
      
      # selectInput("figtype", "Figure scale:", # Put the choice between PS and LL here
      #             c("By CCM" = "Coutry",
      #               "By category" = "Regnl"), width="150px"),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",#condition = "input.ScenChoice.includes('Eff1')",
                       checkboxInput("Scen1", "Best effort HS 06-16", TRUE)),
      
      conditionalPanel(condition="input.Scen1 == true & input.variable == 'Purse'",
                       sliderInput("slider1", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",#condition = "input.ScenChoice.includes('Eff1')",
                       checkboxInput("Scen11", "Best effort HS 06-16", TRUE)),
      
      conditionalPanel(condition="input.Scen11 == true & input.variable == 'LongL'",
                       sliderInput("slider11", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen2", "Avg highest 2 effort HS 14-16", FALSE)),
      
      conditionalPanel(condition="input.Scen2 == true & input.variable == 'Purse'",
                       sliderInput("slider2", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen12", "Avg highest 2 effort HS 14-16", FALSE)),
      
      conditionalPanel(condition="input.Scen12 == true & input.variable == 'LongL'",
                       sliderInput("slider12", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen4", "Avg highest 5 effort HS 02-16", FALSE)),
      
      conditionalPanel(condition="input.Scen4 == true & input.variable == 'Purse'",
                       sliderInput("slider4", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen14", "Avg highest 5 effort HS 02-16", FALSE)),
      
      conditionalPanel(condition="input.Scen14 == true & input.variable == 'LongL'",
                       sliderInput("slider14", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen6", "Current EEZ allocation Att Tab 1", FALSE)),
      
      conditionalPanel(condition="input.Scen6 == true & input.variable == 'Purse'",
                       sliderInput("slider6", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen16", "Biomass BET in zones", FALSE)),
      
      conditionalPanel(condition="input.Scen16 == true & input.variable == 'LongL'",
                       sliderInput("slider16", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen8", "EEZ size in CA", FALSE)),
      
      conditionalPanel(condition="input.Scen8 == true & input.variable == 'Purse'",
                       sliderInput("slider8", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen18", "Economic dependency", FALSE)),
      
      conditionalPanel(condition="input.Scen18 == true & input.variable == 'LongL'",
                       sliderInput("slider18", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE))),
      
      #__________________
      
      
      column(2,
      
             numericInput("num1", h4("Allocation TAE/TAC"), value=10000, width="180px"),
             
             conditionalPanel(condition="input.variable == 'Purse'",
                              selectInput("PHinc", "Include Philippines?", 
                                          c("Include PH" = "InPH",
                                            "Exclude PH" = "OutPH"), width="150px")),
      #__________________
             
      # Input: Checkbox for whether outliers should be included ----
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen3", "Avg highest 3 effort HS 07-16", FALSE)),
      
      conditionalPanel(condition="input.Scen3 == true & input.variable == 'Purse'",
                       sliderInput("slider3", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen13", "Avg highest 3 effort HS 07-16", FALSE)),
      
      conditionalPanel(condition="input.Scen13 == true & input.variable == 'LongL'",
                       sliderInput("slider13", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
     
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen5", "Current HS allocation Att Tab 1", FALSE)),
      
      conditionalPanel(condition="input.Scen5 == true & input.variable == 'Purse'",
                       sliderInput("slider5", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen15", "Current HS allocation Att Tab 1", FALSE)),
      
      conditionalPanel(condition="input.Scen15 == true & input.variable == 'LongL'",
                       sliderInput("slider15", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen7", "Biomass SKJ in zones", FALSE)),
      
      conditionalPanel(condition="input.Scen7 == true & input.variable == 'Purse'",
                       sliderInput("slider7", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen17", "EEZ size in CA", FALSE)),
      
      conditionalPanel(condition="input.Scen17 == true & input.variable == 'LongL'",
                       sliderInput("slider17", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen9", "Economic dependency", FALSE)),
      
      conditionalPanel(condition="input.Scen9 == true & input.variable == 'Purse'",
                       sliderInput("slider9", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      conditionalPanel(condition="input.variable == 'LongL'",
                       checkboxInput("Scen19", "Equal split", FALSE)),
      
      conditionalPanel(condition="input.Scen19 == true & input.variable == 'LongL'",
                       sliderInput("slider19", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
      
      #__________________
      
      conditionalPanel(condition="input.variable == 'Purse'",
                       checkboxInput("Scen10", "Equal split", FALSE)),
      
      conditionalPanel(condition="input.Scen10 == true & input.variable == 'Purse'",
                       sliderInput("slider10", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE))
      
      #__________________
      
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




# Define server logic to plot various variables against mpg ----
server <- function(input, output) {


  
  wgtVec <- reactive({
    paste(c(ifelse(input$Scen1 == TRUE & input$variable == 'Purse', input$slider1, 0),
            ifelse(input$Scen2 == TRUE & input$variable == 'Purse', input$slider2, 0),
            ifelse(input$Scen3 == TRUE & input$variable == 'Purse', input$slider3, 0),
            ifelse(input$Scen4 == TRUE & input$variable == 'Purse', input$slider4, 0),
            ifelse(input$Scen5 == TRUE & input$variable == 'Purse', input$slider5, 0),
            ifelse(input$Scen6 == TRUE & input$variable == 'Purse', input$slider6, 0),
            ifelse(input$Scen7 == TRUE & input$variable == 'Purse', input$slider7, 0),
            ifelse(input$Scen8 == TRUE & input$variable == 'Purse', input$slider8, 0),
            ifelse(input$Scen9 == TRUE & input$variable == 'Purse', input$slider9, 0),
            ifelse(input$Scen10 == TRUE & input$variable == 'Purse', input$slider10, 0),
            ifelse(input$Scen11 == TRUE & input$variable == 'LongL', input$slider11, 0),
            ifelse(input$Scen12 == TRUE & input$variable == 'LongL', input$slider12, 0),
            ifelse(input$Scen13 == TRUE & input$variable == 'LongL', input$slider13, 0),
            ifelse(input$Scen14 == TRUE & input$variable == 'LongL', input$slider14, 0),
            ifelse(input$Scen15 == TRUE & input$variable == 'LongL', input$slider15, 0),
            ifelse(input$Scen16 == TRUE & input$variable == 'LongL', input$slider16, 0),
            ifelse(input$Scen17 == TRUE & input$variable == 'LongL', input$slider17, 0),
            ifelse(input$Scen18 == TRUE & input$variable == 'LongL', input$slider18, 0),
            ifelse(input$Scen19 == TRUE & input$variable == 'LongL', input$slider19, 0)))
  })
  
  
  aa <- reactive({
    
    sumfnc <- function(x) sum(datrel[x, -c(1,2)] * as.numeric(wgtVec()))/sum(as.numeric(wgtVec()))
    
    
    aaa <- sapply(1:dim(datrel)[1], sumfnc)
  })
  
  
  output$caption <- renderText({
    paste(input$num1)
  })
  
  
  datpl <- reactive({
 #   test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1)
    
    
    if(input$figtype == 1){
    if(input$variable == "Purse"){
      if(input$PHinc == "OutPH"){
        test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1) %>% filter(CCM != "PH")
      } else {
        test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1)
      }
    } else {
      test <- data.frame(CCM=datrel$CCM, Res=aa()) %>% mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1)
    }
    } else {
      
      if(input$variable == "Purse"){
        if(input$PHinc == "OutPH"){
          test <- data.frame(CCM=datrel$CCM, Regn=datrel$Regnl, Res=aa()) %>% filter(CCM != "PH") %>% group_by(Regn) %>% summarise(Res=sum(Res)) %>%
                             mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1) %>% rename(CCM=Regn)
        } else {
          test <- data.frame(CCM=datrel$CCM, Regn=datrel$Regnl, Res=aa()) %>% group_by(Regn) %>% summarise(Res=sum(Res)) %>%
                             mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1) %>% rename(CCM=Regn)
        }
      } else {
        test <- data.frame(CCM=datrel$CCM, Regn=datrel$Regnl, Res=aa()) %>% group_by(Regn) %>% summarise(Res=sum(Res)) %>%
                          mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1) %>% rename(CCM=Regn)
      }
      
    }
    
    
    
    # 
    # if(input$figtype == 1){
    #   test <- test
    # } else{
    #   test <- data.frame(CCM=datrel$CCM, Regn=datrel$Regnl, Res=aa()) %>% group_by(Regn) %>% summarise(Res=sum(Res)) %>%
    #                      mutate(Resrnd=round(Res*100,0), pos=cumsum(Res)-Res/2, Allocate=Res*input$num1) %>% rename(CCM=Regn)
    # }
    
  })
  
  dattab <- reactive({
    test <- data.frame(CCM=datrel$CCM, Percent=round(aa()*100, 0), Allocate=round(aa()*input$num1,0)) %>% arrange(desc(Allocate))
    
    if(input$variable == 'Purse'){
      if(input$PHinc == "OutPH"){
        test <- filter(test, CCM != "PH")
      } else {
        test <- test
      }
    } else {
      test <- test
    }
    
    if(input$figtype == 1){
      test <- test
    } else{
      test <- data.frame(CCM=datrel$CCM, Regn=datrel$Regnl, Res=aa()) %>% group_by(Regn) %>% summarise(Res=sum(Res)) %>%
                         mutate(Percent=round(Res*100, 0), Allocate=round(Res*input$num1,0)) %>% rename(CCM=Regn) %>%
                         select(-Res) %>% arrange(desc(Allocate))
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
  
  shinyServer(function(input, output, session){
    session$onSessionEnded(function() {
      stopApp()
    })
  })
  
}



shinyApp(ui, server)










