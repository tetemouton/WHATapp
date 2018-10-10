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