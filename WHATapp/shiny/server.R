server <- function(input, output) {
  
  wgtVec <- reactive({
    paste(c(#ifelse(input$Scen2 == TRUE & input$variable == 'Purse', input$slider2, 0),
      #ifelse(input$Scen3 == TRUE & input$variable == 'Purse', input$slider3, 0),
      #ifelse(input$Scen4 == TRUE & input$variable == 'Purse', input$slider4, 0),
      ifelse(input$Scen5 == TRUE & input$variable == 'Purse', input$slider5, 0),
      ifelse(input$Scen6 == TRUE & input$variable == 'Purse', input$slider6, 0),
      ifelse(input$Scen7 == TRUE & input$variable == 'Purse', input$slider7, 0),
      ifelse(input$Scen8 == TRUE & input$variable == 'Purse', input$slider8, 0),
      ifelse(input$Scen9 == TRUE & input$variable == 'Purse', input$slider9, 0),
      ifelse(input$Scen24 == TRUE & input$variable == 'Purse', input$slider24, 0),
      ifelse(input$Scen26 == TRUE & input$variable == 'Purse', input$slider26, 0),
      ifelse(input$Scen10 == TRUE & input$variable == 'Purse', input$slider10, 0),
      #ifelse(input$Scen12 == TRUE & input$variable == 'LongL', input$slider12, 0),
      #ifelse(input$Scen13 == TRUE & input$variable == 'LongL', input$slider13, 0),
      #ifelse(input$Scen14 == TRUE & input$variable == 'LongL', input$slider14, 0),
      ifelse(input$Scen15 == TRUE & input$variable == 'LongL', input$slider15, 0),
      ifelse(input$Scen16 == TRUE & input$variable == 'LongL', input$slider16, 0),
      ifelse(input$Scen17 == TRUE & input$variable == 'LongL', input$slider17, 0),
      ifelse(input$Scen18 == TRUE & input$variable == 'LongL', input$slider18, 0),
      ifelse(input$Scen23 == TRUE & input$variable == 'LongL', input$slider23, 0),
      ifelse(input$Scen25 == TRUE & input$variable == 'LongL', input$slider25, 0),
      ifelse(input$Scen19 == TRUE & input$variable == 'LongL', input$slider19, 0),
      ifelse(input$Scen1 == TRUE & input$variable == 'Purse', input$slider1, 0),
      ifelse(input$Scen11 == TRUE & input$variable == 'LongL', input$slider11, 0)))
  })
  
  
  output$numest <- renderUI({
    numericInput("numbest", h5("No. Years"), min=1, 
                 max=input$sliderrng[2] - (input$sliderrng[1] - 1),
                 value=1, width="70px")
  })
  
  output$numest1 <- renderUI({
    numericInput("numbest1", h5("No. Years"), min=1, 
                 max=input$sliderrng1[2] - (input$sliderrng1[1] - 1),
                 value=1, width="70px")
  })
  
  
  histfish <- reactive({
    
    
    if(input$variable == "Purse"){
      
      ind1 <- input$sliderrng[1] - (minyr - 2) # -2 on all these as it indexes the data frame where first column is CCMs
      ind2 <- input$sliderrng[2] - (minyr - 2)
      Nyrs <- input$sliderrng[2] - (input$sliderrng[1] - 1)
      
      effrng <- effrt[, c(1,ind1:ind2)]
      
      tmpdat <- effrng
      
      ntm <- min(input$numbest, Nyrs)
      
      tmpmean <- sapply(1:dim(tmpdat)[1], function(x) mean(rev(sort(t(tmpdat[x, -1])))[1:ntm]))
      tmpmean <- tmpmean/sum(tmpmean)
      
      tmpmean <- cbind(tmpmean, rep(1/dim(tmpdat)[1], dim(tmpdat)[1]))
      
    } else {
      ind1 <- input$sliderrng1[1] - (minyr - 2) # -2 on all these as it indexes the data frame where first column is CCMs
      ind2 <- input$sliderrng1[2] - (minyr - 2)
      Nyrs <- input$sliderrng1[2] - (input$sliderrng1[1] - 1)
      
      catrng <- catch[, c(1,ind1:ind2)]
      
      tmpdat <- catrng
      
      ntm <- min(input$numbest1, Nyrs)
      
      tmpmean <- sapply(1:dim(tmpdat)[1], function(x) mean(rev(sort(t(tmpdat[x, -1])))[1:ntm]))
      tmpmean <- tmpmean/sum(tmpmean)
      
      tmpmean <- cbind(rep(1/dim(tmpdat)[1], dim(tmpdat)[1]), tmpmean)
      
    }
    
  })
  
  
  aa <- reactive({
    
    sumfnc <- function(x) sum(cbind(datrel[, -c(1,2)], histfish())[x,] * as.numeric(wgtVec()))/sum(as.numeric(wgtVec()))
    
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
    plot_ly(datpl(), labels=~CCM, values=~Resrnd, type="pie", marker=list(colors=CCM_cols), sort=FALSE,
            textposition="inside", textinfo="label+value", width=700, height=700) %>% config(displayModeBar = F) %>%
      layout(title = "CCM - Allocations (Percentages)", plot_bgcolor='transparent', paper_bgcolor='transparent',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
  output$AllocTab <- renderDataTable(dattab(), options=list(pageLength=15, searching=FALSE, dom="ltp")) #, lengthChange = FALSE))
  # To show the default no. output$AllocTab <- renderDataTable(dattab())
  
  # For a more simple table:
  # output$AllocTab <- renderTable(dattab())#, options=list(pageLength=10))
  
  shinyServer(function(input, output, session){
    session$onSessionEnded(function() {
      stopApp()
    })
  })
  
}