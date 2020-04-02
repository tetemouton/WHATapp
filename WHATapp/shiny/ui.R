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