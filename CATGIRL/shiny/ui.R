ui <- fluidPage(
  
  
  #titlePanel("CATGIRL - a catch/effort allocation tool"),
  titlePanel(title=div(img(height=150, width=100, src="cat.jpg"), "CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining"), windowTitle="Mmmmeeeeeoooowwwww"),
  #headerPanel( title=div(img(height=150, width=100, src="cat.jpg"),
  #                       h3("CATGIRL - Catch Allocation Tool Generalised to Include Regional Longlining", align="center", style="bold")
  #)),
  
  
 
  fluidRow(
    column(2,
           
          
           
           selectInput("variable", h4("Gear type:"), # Put the choice between PS and LL here
                       c("Purse seine" = "Purse",
                         "Longline" = "LongL"), width="150px"),
           
           radioButtons("figtype", label = h4("Figure scale:"),
                        choices = list("By CCM" = 1, "By category" = 2), 
                        selected = 1),
           
           br(),
           br(),
           
           # selectInput("figtype", "Figure scale:",
           #             c("By CCM" = "Coutry",
           #               "By category" = "Regnl"), width="150px"),
           
           #__________________
           
           conditionalPanel(condition="input.variable == 'Purse'",#condition = "input.ScenChoice.includes('Eff1')",
                            checkboxInput("Scen1", "Best effort HS 06-16", TRUE)),
           
           conditionalPanel(condition="input.Scen1 == true & input.variable == 'Purse'",
                            sliderInput("slider1", NULL,  min=1, max=10, value=1, width="150px", ticks=TRUE)),
           
           conditionalPanel(condition="input.variable == 'LongL'",
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
                            selectInput("PHinc", h4("Include Philippines?"), 
                                        c("Include PH" = "InPH",
                                          "Exclude PH" = "OutPH"), width="180px")),
           
           
           br(),
           br(),
           
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
           

    ),
    

    column(4, plotlyOutput("IndPlot")),
    column(2, offset=1, dataTableOutput("AllocTab")))
  # For a more simple table:
  # column(1, offset=1, dataTableOutput("AllocTab")))
)