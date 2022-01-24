#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(plotly)
library(ggplot2)
library(tidyverse)
library(shinyWidgets)

#setwd("C:/GitHub/WHATapp/WHATapp_web")

theme_set(theme_bw())

CCM_cols <- c("dodgerblue2","firebrick3","darkorchid","darkred","lightskyblue","yellow1","orange1","honeydew3","darkgoldenrod3","deeppink3","dodgerblue4","seagreen3","purple4","grey48","yellow3","black","pink","green","blue","olivedrab","dodgerblue1","seagreen1","darkgoldenrod4","azure","bisque1","tomato","wheat1","turquoise1","turquoise3","deeppink4","coral3","coral4")

dat <- read.csv("./Data/Data_Matrix.csv", header=TRUE)

datrel <- dat
datrel[, -c(1,2)] <- as.data.frame(prop.table(as.matrix(datrel[, -c(1,2)]), 2))

datalt <- datrel
datalt[23,3] <- 0   # Allow removal of PH HS allocation from CMM
datalt[, -c(1,2)] <- as.data.frame(prop.table(as.matrix(datalt[, -c(1,2)]), 2)) 

effrt <- read.csv("./Data/Annual_PS_Effort.csv", header=TRUE)
effrthsp <- read.csv("./Data/Annual_PS_Effort-I1.csv", header=TRUE)

catch <- read.csv("./Data/Annual_LL_Catch.csv", header=TRUE)

minyr <- 2003
maxyr <- 2018


#____________________________________________________________________________________________________________
# User interface

ui <- fluidPage(
    
    tags$head(
        tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    
    #titlePanel("CATGIRL - a catch/effort allocation tool"),
    titlePanel(title=div(img(height=120, width=1500, src="HeaderBar1.png")), windowTitle="Mmmmeeeeeoooowwwww"),
    #titlePanel(title=div(img(height=150, width=100, src="cat.jpg"), "CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining"), windowTitle="Mmmmeeeeeoooowwwww"),
    #headerPanel( title=div(img(height=150, width=100, src="cat.jpg"),
    #                       h3("CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining", align="center", style="bold")
    #)),
    
    #setBackgroundColor("blue"),
    
    setBackgroundColor(
        color = c("white", "white"),
        gradient = "linear",
        direction = c("bottom","left")
    ),
    
    fluidRow(
        column(2, #style = "background-color:#4d3a7d;",
               
               #br(),
               #hr(),
               br(),
               h3("Choose your scenario"),
               h5("Tick the scenario/s to include and"),
               h5("shift slider to modfify the weightings"),
               br(),
               br(),
               
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen5", "Current HS allocation Att Tab 1", FALSE)),
               
               conditionalPanel(condition="input.Scen5 == true & input.variable == 'Purse'",
                                sliderInput("slider5", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen15", "Current bigeye limits Att Tab 3", FALSE)),
               
               conditionalPanel(condition="input.Scen15 == true & input.variable == 'LongL'",
                                sliderInput("slider15", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen6", "Current EEZ allocation Att Tab 1", FALSE)),
               
               conditionalPanel(condition="input.Scen6 == true & input.variable == 'Purse'",
                                sliderInput("slider6", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen18", "Economic dependency", FALSE)),
               
               conditionalPanel(condition="input.Scen18 == true & input.variable == 'LongL'",
                                sliderInput("slider18", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen9", "Economic dependency", FALSE)),
               
               conditionalPanel(condition="input.Scen9 == true & input.variable == 'Purse'",
                                sliderInput("slider9", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen23", "Development status", FALSE)),
               
               conditionalPanel(condition="input.Scen23 == true & input.variable == 'LongL'",
                                sliderInput("slider23", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen24", "Development status", FALSE)),
               
               conditionalPanel(condition="input.Scen24 == true & input.variable == 'Purse'",
                                sliderInput("slider24", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen17", "EEZ size in CA", FALSE)),
               
               conditionalPanel(condition="input.Scen17 == true & input.variable == 'LongL'",
                                sliderInput("slider17", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________  
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen8", "EEZ size in CA", FALSE)),
               
               conditionalPanel(condition="input.Scen8 == true & input.variable == 'Purse'",
                                sliderInput("slider8", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen16", "Biomass BET in zones", FALSE)),
               
               conditionalPanel(condition="input.Scen16 == true & input.variable == 'LongL'",
                                sliderInput("slider16", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________          
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen7", "Biomass SKJ in zones", FALSE)),
               
               conditionalPanel(condition="input.Scen7 == true & input.variable == 'Purse'",
                                sliderInput("slider7", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen25", "Adjacency", FALSE)),
               
               conditionalPanel(condition="input.Scen25 == true & input.variable == 'LongL'",
                                sliderInput("slider25", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen26", "Adjacency", FALSE)),
               
               conditionalPanel(condition="input.Scen26 == true & input.variable == 'Purse'",
                                sliderInput("slider26", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",
                                checkboxInput("Scen19", "Equal split", FALSE)),
               
               conditionalPanel(condition="input.Scen19 == true & input.variable == 'LongL'",
                                sliderInput("slider19", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                checkboxInput("Scen10", "Equal split", FALSE)),
               
               conditionalPanel(condition="input.Scen10 == true & input.variable == 'Purse'",
                                sliderInput("slider10", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               #__________________
               
               
               conditionalPanel(condition="input.variable == 'Purse'",#condition = "input.ScenChoice.includes('Eff1')",
                                checkboxInput("Scen1", "Historical effort", TRUE)),
               
               conditionalPanel(condition="input.Scen1 == true & input.variable == 'Purse'",
                                sliderInput("sliderrng", NULL,  min=minyr, max=maxyr, value=c(minyr,maxyr), width="200px", ticks=TRUE, sep="")),
               
               conditionalPanel(condition="input.Scen1 == true & input.variable == 'Purse'",
                                uiOutput("numest")),
               
               conditionalPanel(condition="input.Scen1 == true & input.variable == 'Purse'",
                                sliderInput("slider1", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
               
               
               conditionalPanel(condition="input.variable == 'LongL'",#condition = "input.ScenChoice.includes('Eff1')",
                                checkboxInput("Scen11", "Historical catch", TRUE)),
               
               conditionalPanel(condition="input.Scen11 == true & input.variable == 'LongL'",
                                sliderInput("sliderrng1", NULL,  min=minyr, max=maxyr, value=c(minyr,maxyr), width="200px", ticks=TRUE, sep="")),
               
               conditionalPanel(condition="input.Scen11 == true & input.variable == 'LongL'",
                                uiOutput("numest1")),
               
               conditionalPanel(condition="input.Scen11 == true & input.variable == 'LongL'",
                                sliderInput("slider11", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE))
               
               
        ),
        
        
        column(2, #style = "background-color:#4d3a2d;",
               
               #br(),
               #hr(),
               br(),
               h3("Set your options"),
               h5("Choose your gear, set the total allocation,"),
               h5(" remove PH if desired (PS only),"),
               h5("select country or region level plot"),
               br(),
               #br(),
               
               selectInput("variable", h4("Gear type:"), # Put the choice between PS and LL here
                           c("Purse seine" = "Purse",
                             "Longline" = "LongL"), width="150px"),
               
               
               numericInput("num1", h4("Allocation TAE/TAC"), value=10000, width="180px"),
               
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                selectInput("PHinc", h4("Include Philippines?"), 
                                            c("Include PH" = "InPH",
                                              "Exclude PH" = "OutPH"), width="180px")),
               
               conditionalPanel(condition="input.variable == 'Purse'",
                                selectInput("HSPinc", h4("Include HSP1 etc.?"), 
                                            c("Keep pockets" = "InHSP",
                                              "Exclude pockets" = "OutHSP"), width="180px")),
               
               
               radioButtons("figtype", label = h4("Figure scale:"),
                            choices = list("By CCM" = 1, "By category" = 2), 
                            selected = 1)
               
        ),
        
        
        column(4, br(), br(), plotlyOutput("IndPlot")),
        column(2, offset=1, dataTableOutput("AllocTab")))
    # For a more simple table:
    # column(1, offset=1, dataTableOutput("AllocTab")))
)


#____________________________________________________________________________________________________________
# The server

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
    
    
    HSPdat <- reactive({
        
        if(input$HSPinc == "InHSP"){
            
            hspdat <- effrt
            
        } else {
            
            hspdat <- effrthsp
            
        }
    })
    
    
    histfish <- reactive({
        
        if(input$variable == "Purse"){
            
            ind1 <- input$sliderrng[1] - (minyr - 2) # -2 on all these as it indexes the data frame where first column is CCMs
            ind2 <- input$sliderrng[2] - (minyr - 2)
            Nyrs <- input$sliderrng[2] - (input$sliderrng[1] - 1)
            
            effrng <- HSPdat()[, c(1,ind1:ind2)]
            
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
        
        if(input$variable == 'Purse'){
            if(input$HSPinc == "InHSP"){
                
                datrel <- datrel
                
            } else {
                
                datrel <- datalt
                
            }
        } else {
            datrel <- datrel
        }
        
        sumfnc <- function(x) sum(cbind(datrel[, -c(1,2)], histfish())[x,] * as.numeric(wgtVec()))/sum(as.numeric(wgtVec()))
        
        aaa <- sapply(1:dim(datrel)[1], sumfnc)
    })
    
    
    output$caption <- renderText({
        paste(input$num1)
    })
    
    
    datpl <- reactive({
        
        if(input$variable == 'Purse'){
            if(input$HSPinc == "InHSP"){
                
                datrel <- datrel
                
            } else {
                
                datrel <- datalt
                
            }
        } else {
            datrel <- datrel
        }
        
        
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
        
        if(input$variable == 'Purse'){
            if(input$HSPinc == "InHSP"){
                
                datrel <- datrel
                
            } else {
                
                datrel <- datalt
                
            }
        } else {
            datrel <- datrel
        }
        
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
    
    
    output$IndPlot <- renderPlotly({
        
        plot_ly(datpl(), labels=~CCM, values=~Resrnd, type="pie", marker=list(colors=CCM_cols), sort=FALSE,
                textposition="inside", textinfo="label+value", width=700, height=700) %>% config(displayModeBar = F) %>%
            layout(title = "CCM - Allocations (Percentages)", plot_bgcolor='transparent', paper_bgcolor='transparent',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        
    })
    
    output$AllocTab <- renderDataTable(dattab(), options=list(pageLength=15, searching=FALSE, dom="ltp"))
    
}


shinyApp(ui = ui, server = server)
