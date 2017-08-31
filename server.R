library(shiny)
library(shinyFiles)
library(rhandsontable)
library(lubridate)
library(aligne2)
library(nloptr)
library(plotly)
library(shinythemes)


source("helpers.R")

options(shiny.maxRequestSize = 60*1024^2)

server <- shinyServer(function(input, output, session) {
  
  rowNum <- reactive({
    x <- interval(promptmonth(input$histCurveDates[2]), input$lastmon)%/%months(1) + 1
  })
  
  values = reactiveValues()
  
  # linear positions ####
  # on peak linear power position
  
  data_plpk = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_lin_on)) {
      DF_PLPK = hot_to_r(input$hot_pwr_lin_on)
    } else {
      if (is.null(values[["DF_PLPK"]]))
        DF_PLPK = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("PEAK", rowNum()), MWh = rep(0, rowNum()), stringsAsFactors = F)
      else
        DF_PLPK = values[["DF_PLPK"]]
    }
    
    
    values[["DF_PLPK"]] = DF_PLPK
    DF_PLPK
  })
  
  output$hot_pwr_lin_on <- renderRHandsontable({
    DF_PLPK = data_plpk()
    if (!is.null(DF_PLPK))
      rhandsontable(DF_PLPK, useTypes = TRUE, stretchH = "all")
  })
  
  # off peak linear power position

  data_plfp = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_lin_off)) {
      DF_PLFP = hot_to_r(input$hot_pwr_lin_off)
    } else {
      if (is.null(values[["DF_PLFP"]]))
        DF_PLFP = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("OFF PEAK", rowNum()), MWh = rep(0, rowNum()), stringsAsFactors = F)
      else
        DF_PLFP = values[["DF_PLFP"]]
    }
    
    
    values[["DF_PLFP"]] = DF_PLFP
    DF_PLFP
  })
  
  output$hot_pwr_lin_off <- renderRHandsontable({
    DF_PLFP = data_plfp()
    if (!is.null(DF_PLFP))
      rhandsontable(DF_PLFP, useTypes = TRUE, stretchH = "all")
  })
  
  # gas linear position

  data_gl = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_gas_lin)) {
      DF_GL = hot_to_r(input$hot_gas_lin)
    } else {
      if (is.null(values[["DF_GL"]]))
        DF_GL = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                           Underlying = rep("RTC", rowNum()), MMBtu = rep(0, rowNum()), stringsAsFactors = F)
      else
        DF_GL = values[["DF_GL"]]
    }
    
    
    values[["DF_GL"]] = DF_GL
    DF_GL
  })
  
  output$hot_gas_lin <- renderRHandsontable({
    DF_GL = data_gl()
    if (!is.null(DF_GL))
      rhandsontable(DF_GL, useTypes = TRUE, stretchH = "all")
  })
  
  # vanilla positions ####
  # power peak vanilla position
  
  values = reactiveValues() # why is this line necessary? reactivelog?
  
  data_pvpk = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_vanilla_on)) {
      DF_PVPK = hot_to_r(input$hot_pwr_vanilla_on)
    } else {
      if (is.null(values[["DF_PVPK"]]))
        DF_PVPK = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("PEAK", rowNum()), MWh = rep(0, rowNum()), 
                             Strike = rep(0, rowNum()), Type = rep("CALL", rowNum()), stringsAsFactors = F)
      else
        DF_PVPK = values[["DF_PVPK"]]
    }
    
    
    values[["DF_PVPK"]] = DF_PVPK
    DF_PVPK
  })
  
  output$hot_pwr_vanilla_on <- renderRHandsontable({
    DF_PVPK = data_pvpk()
    if (!is.null(DF_PVPK))
      rhandsontable(DF_PVPK, useTypes = TRUE, stretchH = "all")
  })
  
  # power off-peak vanilla position
  
  data_pvfp = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_vanilla_off)) {
      DF_PVFP = hot_to_r(input$hot_pwr_vanilla_off)
    } else {
      if (is.null(values[["DF_PVFP"]]))
        DF_PVFP = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("OFF PEAK", rowNum()), MWh = rep(0, rowNum()), 
                             Strike = rep(0, rowNum()), Type = rep("CALL", rowNum()), stringsAsFactors = F)
      else
        DF_PVFP = values[["DF_PVFP"]]
    }
    
    
    values[["DF_PVFP"]] = DF_PVFP
    DF_PVFP
  })
  
  output$hot_pwr_vanilla_off <- renderRHandsontable({
    DF_PVFP = data_pvfp()
    if (!is.null(DF_PVFP))
      rhandsontable(DF_PVFP, useTypes = TRUE, stretchH = "all")
  })
  
  # gas vanilla position
  
  data_gv = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_gas_vanilla)) {
      DF_GV = hot_to_r(input$hot_gas_vanilla)
    } else {
      if (is.null(values[["DF_GV"]]))
        DF_GV = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("RTC", rowNum()), MMBtu = rep(0, rowNum()), 
                             Strike = rep(0, rowNum()), Type = rep("CALL", rowNum()), stringsAsFactors = F)
      else
        DF_GV = values[["DF_GV"]]
    }
    
    
    values[["DF_GV"]] = DF_GV
    DF_GV
  })
  
  output$hot_gas_vanilla <- renderRHandsontable({
    DF_GV = data_gv()
    if (!is.null(DF_GV))
      rhandsontable(DF_GV, useTypes = TRUE, stretchH = "all")
  })
  
  # spread positions #####
  # power peak spread position
  
  values = reactiveValues()
  
  data_pspk = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_spread_on)) {
      DF_PSPK = hot_to_r(input$hot_pwr_spread_on)
    } else {
      if (is.null(values[["DF_PSPK"]]))
        DF_PSPK = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("PEAK", rowNum()), MWh = rep(0, rowNum()), 
                             Type = rep("CALL", rowNum()), 
                             HR = rep(10, rowNum()), VOM = rep(0, rowNum()), stringsAsFactors = F)
      else
        DF_PSPK = values[["DF_PSPK"]]
    }
    
    
    values[["DF_PSPK"]] = DF_PSPK
    DF_PSPK
  })
  
  output$hot_pwr_spread_on <- renderRHandsontable({
    DF_PSPK = data_pspk()
    if (!is.null(DF_PSPK))
      rhandsontable(DF_PSPK, useTypes = TRUE, stretchH = "all")
  })
  
  # power off-peak spread position
  
  data_psfp = reactive({
    if(input$saveInputs == 0)
      return()
    if (!is.null(input$hot_pwr_spread_off)) {
      DF_PSFP = hot_to_r(input$hot_pwr_spread_off)
    } else {
      if (is.null(values[["DF_PSFP"]]))
        DF_PSFP = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
                             Underlying = rep("OFF PEAK", rowNum()), MWh = rep(0, rowNum()), 
                             Type = rep("CALL", rowNum()), 
                             HR = rep(10, rowNum()), VOM = rep(0,rowNum()), stringsAsFactors = F)
      else
        DF_PSFP = values[["DF_PSFP"]]
    }
    
    
    values[["DF_PSFP"]] = DF_PSFP
    DF_PSFP
  })
  
  output$hot_pwr_spread_off <- renderRHandsontable({
    DF_PSFP = data_psfp()
    if (!is.null(DF_PSFP))
      rhandsontable(DF_PSFP, useTypes = TRUE, stretchH = "all")
  })
  
  # gas spread position
  
  # data_gs = reactive({
  #   if(input$saveInputs == 0)
  #     return()
  #   if (!is.null(input$hot_gas_spread)) {
  #     DF_GS = hot_to_r(input$hot_gas_spread)
  #   } else {
  #     if (is.null(values[["DF_GS"]]))
  #       DF_GS = data.frame(Delmo = seq(from = promptmonth(input$histCurveDates[2]), by = "month", length.out = rowNum()),
  #                          Underlying = rep("RTC", rowNum()), MMBtu = rep(0, rowNum()), 
  #                          Strike = rep(0, rowNum()), Type = rep("CALL", rowNum()), stringsAsFactors = F)
  #     else
  #       DF_GS = values[["DF_GS"]]
  #   }
  #   
  #   
  #   values[["DF_GS"]] = DF_GS
  #   DF_GS
  # })
  # 
  # output$hot_gas_spread <- renderRHandsontable({
  #   DF_GS = data_gs()
  #   if (!is.null(DF_GS))
  #     rhandsontable(DF_GS, useTypes = TRUE, stretchH = "all")
  # })
  # 
  
  # test code for simulation piece
  
  # simOut <- reactive({
  #   if(input$goButton == 0)
  #     return()
  #   isolate(
  #     sampleSimPaths <- PFESimWrapper(nsims = input$numsims, curve.date.begin = input$histCurveDates[1], 
  #                                     curve.date.end = input$histCurveDates[2],
  #                                     end_date = input$lastmon, 
  #                                     marketcomponentstr = paste(input$curvebyregion, input$curvelist, sep = "-"))
  #   )
  # })
  # 
  # output$simPlot <- renderPlot({
  #   if(input$goButton == 0)
  #     return()
  #   isolate(
  #     matplot(t(simOut()), type = "l")
  #   )
  # 
  #   })
  
  # mapping positions
  
  posOut <- reactive({
    if(input$goButton == 0){
      return()
    } else {
      
      # linear
      ForwardVolume <- c(
        values[["DF_GL"]]$MMBtu,
        values[["DF_PLPK"]]$MWh,
        values[["DF_PLFP"]]$MWh
      )
      
      
      # vanilla
      VanillaVolume <- c(
        values[["DF_GV"]]$MMBtu,
        values[["DF_PVPK"]]$MWh,
        values[["DF_PVFP"]]$MWh
      )
      
      VanillaStrike <- c(
        values[["DF_GV"]]$Strike,
        values[["DF_PVPK"]]$Strike,
        values[["DF_PVFP"]]$Strike
      )
      
      VanillaType <- c(
        values[["DF_GV"]]$Type,
        values[["DF_PVPK"]]$Type,
        values[["DF_PVFP"]]$Type
      )
      
      
      # spread
      
      SpreadVolumeOn <- values[["DF_PSPK"]]$MWh
      SpreadVolumeOff <- values[["DF_PSFP"]]$MWh
      
      SpreadStrikeOn <- values[["DF_PSPK"]]$Strike
      SpreadStrikeOff <- values[["DF_PSFP"]]$Strike
      
      SpreadTypeOn <- values[["DF_PSPK"]]$Type
      SpreadTypeOff <- values[["DF_PSFP"]]$Type
      
      SpreadVOMOn <- values[["DF_PSPK"]]$VOM
      SpreadVOMOff <- values[["DF_PSFP"]]$VOM
      
      SpreadHROn <- values[["DF_PSPK"]]$HR
      SpreadHROff <- values[["DF_PSFP"]]$HR
      
      return(list(
        linvolume = ForwardVolume,
        vanillavolume = VanillaVolume,
        spreadvolumeon = SpreadVolumeOn,
        spreadvolumeoff = SpreadVolumeOff,
        vanillastrike = VanillaStrike,
        vanillatype = VanillaType,
        spreadvomon = SpreadVOMOn,
        spreadtypeon = SpreadTypeOn,
        spreadhron = SpreadHROn,
        spreadvomoff = SpreadVOMOff,
        spreadtypeoff = SpreadTypeOff,
        spreadhroff = SpreadHROff))
    }
    
  })
  
  
  # test (comment out in prod) #####
  # output$test <- renderPrint({
  #   if(input$goButton == 0)
  #     return()
  #   isolate(
  #     print(
  #       list(nsims = input$numsims,
  #           curve.date.begin = input$histCurveDates[1],
  #           curve.date.end = input$histCurveDates[2],
  #           end_date <- input$lastmon,
  #           marketcomponentstr <- paste(input$curvebyregion, input$curvelist, sep = "-"),
  #           Forward.Volume = posOut()$linvolume,
  #           Vanilla.Volume = posOut()$vanillavolume,
  #           Vanilla.Strike = posOut()$vanillastrike,
  #           Vanilla.Type = posOut()$vanillatype,
  #           Spread.VOM.On = posOut()$spreadvomon,
  #           Spread.VOM.Off = posOut()$spreadvomoff,
  #           Spread.Volume.On = posOut()$spreadvolumeon,
  #           Spread.Volume.Off = posOut()$spreadvolumeoff,
  #           Spread.HR.On = posOut()$spreadhron,
  #           Spread.HR.Off = posOut()$spreadhroff,
  #           Spread.Type.On = posOut()$spreadtypeon,
  #           Spread.Type.Off = posOut()$spreadtypeoff)
  #     )
  #   )
  # })
  
  pfeOut <- reactive({
    if(input$goButton == 0)
      return()
    PFESimWrapper(    
      nsims = input$numsims,
      curve.date.begin = input$histCurveDates[1],
      curve.date.end = input$histCurveDates[2],
      end_date <- input$lastmon,
      marketcomponentstr <- paste(input$curvebyregion, input$curvelist, sep = "-"),
      Forward.Volume = posOut()$linvolume,
      Vanilla.Volume = posOut()$vanillavolume,
      Vanilla.Strike = posOut()$vanillastrike,
      Vanilla.Type = posOut()$vanillatype,
      Spread.VOM.On = posOut()$spreadvomon,
      Spread.VOM.Off = posOut()$spreadvomoff,
      Spread.Volume.On = posOut()$spreadvolumeon,
      Spread.Volume.Off = posOut()$spreadvolumeoff,
      Spread.HR.On = posOut()$spreadhron,
      Spread.HR.Off = posOut()$spreadhroff,
      Spread.Type.On = posOut()$spreadtypeon,
      Spread.Type.Off = posOut()$spreadtypeoff)
    
  })
  
  output$pfeTbl <- renderDataTable({
  
    if(input$goButton == 0)
      return()
    
    tableData <- pfeOut()$PFEoutput
    tableData <- tableData[-dim(tableData)[1], ]
    tableData[, -1] <- round(tableData[, -1], 0)
    return(tableData)
    
  })
  
  output$pfePlot <- renderPlotly({
    if(input$goButton == 0)
      return()
    plotData <- pfeOut()$PFEoutput[-dim(pfeOut()$PFEoutput)[1],]
    plotData[, c('Potential_Collateral', 'Expected_Collateral')] <- -plotData[, c('Potential_Collateral', 'Expected_Collateral')]
    p <- plot_ly(
      plotData, x = ~Month, y = ~ PFE, name = "PFE", type = 'scatter', mode = 'lines+markers', color = I('red')
    ) %>% add_trace(y = ~ Potential_Collateral, name = "Potential Collateral", mode = 'lines+markers', color = I('red')) %>% 
      add_trace(y = ~ Expected_Exposure, name = "Expected Exposure", mode = 'lines', color = I('blue')) %>% 
      add_trace(y = ~ Expected_Collateral, name = "Expected Collateral", mode = 'lines', color = I('blue'))
  })
  
  output$pwrCurveTbl <- renderTable({
    if(input$goButton == 0)
      return()
    pfeOut()$pwroutput
  })
  
  output$gasCurveTbl <- renderTable({
    if(input$goButton == 0)
      return()
    pfeOut()$gasoutput
  })
  
  observe({
    volumes <- c("UserFolder" = "C:/")
    shinyFileSave(input, "save", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$save)
    if(nrow(fileinfo) > 0){
      write.csv(pfeOut()$PFEoutput, as.character(fileinfo$datapath), row.names = FALSE)
    }
  })
  
  observe({
    curvebyregion.curvelist <- switch(input$curvebyregion,
                                      ERCOT  = c("ZONE H"),
                                      PJM = c("WESTRT"))
    updateCheckboxGroupInput(session, "curvelist", choices = curvebyregion.curvelist, 
                             selected = curvebyregion.curvelist)
    
  })
  
  
})