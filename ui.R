library(shiny)
library(shinyFiles)
library(rhandsontable)
library(lubridate)
library(aligne2)
library(nloptr)
library(plotly)
library(shinythemes)


ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
  titlePanel("Potential Future Exposure (PFE) Calculator"),
  sidebarPanel(
    
    # alternative spreadsheet input (inactive) ####
    # helpText("Spreadsheet input is not preferred due to fixed formatting. Use Save Inputs button
    #          to show and enter positions."),
    # checkboxInput("spreadsheet", "spreadsheet input", FALSE),
    # conditionalPanel(
    #   condition = "input.spreadsheet == true",
    #   helpText("Select if you want to upload instead of enter the positions. As the formatting
    #            needs to be exact, using the Position Inputs tab is highly recommended."),
    #   checkboxInput("lin", "linear", FALSE),
    #   checkboxInput("vanilla", "vanilla options", FALSE),
    #   checkboxInput("spread", "spread options", FALSE),
    #   conditionalPanel(
    #     condition = "input.lin == true",
    #     fileInput("file1_linear", "Choose CSV file that contains the linear positions",
    #               accept = c(
    #                 "test/csv",
    #                 "test/comma-separated-values,text/plain",
    #                 ".csv")
    #     )
    #   ),
    #   conditionalPanel(
    #     condition = "input.vanilla == true",
    #     fileInput("file2_vanilla", "Choose CSV file that contains the vanilla option positions",
    #               accept = c(
    #                 "test/csv",
    #                 "test/comma-separated-values,text/plain",
    #                 ".csv")
    #     )
    #   ),
    #   conditionalPanel(
    #     condition = "input.spread == true",
    #     fileInput("file3_spread", "Choose CSV file that contains the spread option positions",
    #               accept = c(
    #                 "test/csv",
    #                 "test/comma-separated-values,text/plain",
    #                 ".csv")
    #     )
    #   )
    #   ),
    
    # radioButtons("intervalType", "Choose the record time interval",
    #              c("fifteen minutes" = "fifteen",
    #                "one hour" = "onehour")
    #              ),
    
    # curve simulation inputs #####
    # A.1 historical forward curve dates ####
    helpText("Select the range of curve dates that the correlation matrix will be based on.
             The last date in the range will be used as simulation starting point. Make sure
             it doesn't fall on a weekend or holiday."),
    dateRangeInput("histCurveDates", "Choose historical curve date window",
                   start = '2017-04-01',
                   end = as.character(Sys.Date())),
    
    # A.2 PFE position horizon ####
    helpText("Select the farthest out month for the positions under the PFE.
             If the last month is beyond 2021, roll the positions into 2021 as
             prices beyond 2021 are not currently validated."),
    dateInput("lastmon", "Choose the last month (end) for the positions",
              value = "2018-12-31", startview = "year"),
    
    # A.3 curve list to select from ####
    helpText("Select the power curve underlying the power positions. Gas curve will
             default to NYMEX NG."),
    selectInput("curvebyregion", "Choose power region.",
                c("ERCOT", "PJM"), selected = "ERCOT"),
    checkboxGroupInput("curvelist", "Select specific curves of the region", choices = c("")),
    
    # A.4 number of forward simulations ####
    sliderInput("numsims", "Select number of simulations",
                min = 400, max = 4000, step = 400, value = 1000),
    
    # B. save input set and clear position tables
    actionButton("saveInputs", "Initialize inputs."),
    
    # C. start forward simulations and FPE calcs with entered positions
    actionButton("goButton", "Compute PFE!"),
    
    # D. save as csv file select output
    shinySaveButton("save", "Save table output", "save file as ...", 
                    filetype = list(csv = "csv")),
    
    helpText("To clear positions and start a new calc, refresh web browser.")
    ),
  
  mainPanel(    
    tabsetPanel(
      tabPanel("Position Inputs", # multiple tabPanels create issue when some are inactive
               tags$h4("Enter Linear Positions"),

               fluidRow(
                 column(4,"enter linear power peak positions",
                        rHandsontableOutput("hot_pwr_lin_on")),
                 column(4,"enter linear power off-peak positions",
                        rHandsontableOutput("hot_pwr_lin_off")),
                 column(4,"enter linear gas positions",
                        rHandsontableOutput("hot_gas_lin"))
               ),
               hr(),
               
               tags$h4("Enter Vanilla Option Positions"),
                fluidRow(
                  column(4, "enter vanilla peak power option info",
                         rHandsontableOutput("hot_pwr_vanilla_on")),
                  column(4, "enter vanilla off-peak power option info",
                         rHandsontableOutput("hot_pwr_vanilla_off")),
                  column(4, "enter vanilla gas option info",
                         rHandsontableOutput("hot_gas_vanilla"))
                ),
               hr(),
               
               tags$h4("Enter Spread Option Positions"),
                fluidRow(
                  column(6, "enter spread peak power option info",
                         rHandsontableOutput("hot_pwr_spread_on")),
                  column(6, "enter spread off-peak power option info",
                         rHandsontableOutput("hot_pwr_spread_off"))
                )
      ),
      # tabPanel("sample plot",
      #          plotOutput("simPlot")
      #          ),
      
      # tabPanel("test",
      #          textOutput("test")
      #          ),
      
      tabPanel("PFE Plot and Data Table",
               plotlyOutput("pfePlot"),
               dataTableOutput("pfeTbl")),
      tabPanel("Curve Data (Aligne)",
               tags$h4("Gas Forward Prices and Volatilities"),
               tableOutput("gasCurveTbl"),
               hr(),
               tags$h4("Power Forward Prices and Volatilities"),
               tableOutput("pwrCurveTbl")
               )
      )
      
    )
  )
  
)