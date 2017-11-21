# rm(list = ls())



# PFE forward price simulation wrapper function ####
PFESimWrapper <- function(nsims = 100, curve.date.begin = NA, curve.date.end = NA,
                          end_date = NA, marketcomponentstr = NA, 
                          Forward.Volume = NA, Vanilla.Volume = NA,
                          Vanilla.Strike = NA, Vanilla.Type = NA, 
                          Spread.Strike.On = NA, Spread.Strike.Off = NA, Spread.VOM.On = NA,
                          Spread.VOM.Off = NA, Spread.Volume.On = NA, Spread.Volume.Off = NA,
                          Spread.HR.On = NA, Spread.HR.Off = NA, Spread.Type.On = NA,
                          Spread.Type.Off = NA){
  
  # input specific to the application (comment out in prod) ####
  # Forward.Volume = rep(100, 48)
  # Vanilla.Volume = rep(0, 48)
  # Vanilla.Strike = rep(0, 48)
  # Vanilla.Type = rep("CALL", 48)
  # Spread.Strike.On = rep(0, 16)
  # Spread.Strike.Off = rep(0, 16)
  # Spread.VOM.On = rep(0, 16)
  # Spread.VOM.Off = rep(0, 16)
  # Spread.Volume.On = rep(0, 16)
  # Spread.Volume.Off = rep(0, 16)
  # Spread.HR.On = rep(10, 16)
  # Spread.HR.Off = rep(10, 16)
  # Spread.Type.On = rep("CALL", 16)
  # Spread.Type.Off = rep("CALL", 16)

  # nsims <- 100
  # curve.date.begin <- '2017-06-01'
  # curve.date.end <- '2017-08-29'
  # end_date <- '2018-12-31'
  # marketcomponentstr <- 'ERCOT-ZONE H'
  
  
  start_date <- promptmonth(curve.date.end) # this currently has to be the prompt month implied by curve.date.end
  Beta <- 1
  Gamma <- 0.05
  conf <- 0.95
  
  tte_init <- as.numeric(as.Date(start_date) - as.Date(curve.date.end))/365
  tte_increm <- expandDates(start_date = start_date, end_date = end_date)$time.days[['numTotDays']]/365
  
  tte <- c(tte_init, tte_init + cumsum(tte_increm))
  
  market <- unlist(strsplit(marketcomponentstr, '-'))[c(TRUE, FALSE)]
  component <- unlist(strsplit(marketcomponentstr, '-'))[c(FALSE, TRUE)]
  numofmonth <- as.integer(round((as.yearmon(end_date) - as.yearmon(start_date))*12 + 1))
  # first forward month
  first.month <- start_date
  # forward delivery months included
  month.used <- seq(as.Date(first.month), by = "month", length.out = numofmonth)
  end_date <- timeLastDayInMonth(end_date)
  
  
  # set seed
  set.seed(123)
  
  # pull forward and terminal vol info ####
  pwr.curves <- futures.pwr.query(f_curve_date = curve.date.begin, t_curve_date = curve.date.end,
                                  start_month = first.month, end_month = month.used[numofmonth],
                                  market = market, component = component, allsegs = TRUE)
  
  pwr.curves <- melt(pwr.curves, id.vars = c('Date', 'Market', 'Component', 'Delmo'),
                     variable.name = 'Segment', value.name = 'Price') %>% filter(Segment != 'rtcPrice')
  # browser()
  ## fudge section start ####
  pwr.curves <- read_csv('P:/Risk/Risk - Risk Management/aa_Yang/Development/Temp R Input/miso_lagn_forPFE.csv')
  pwr.curves <- pwr.curves %>% mutate(Date = as.Date(as.character(Date), "%Y%m%d"), 
                                      Delmo = as.Date(as.character(Delmo), "%Y%m%d")) 
  ## fudge section end ####
  ng.curve <- futures.fuel.query(f_curvedate = curve.date.begin, t_curvedate = curve.date.end,
                                 start_date = first.month, end_date = month.used[numofmonth],
                                 market = 'NYMEX', comp = 'NG') %>% rename(rtcPrice = Price)
  ng.curve <- melt(ng.curve, id.vars = c('Date', 'Market', 'Component', 'Delmo'),
                   variable.name = 'Segment', value.name = 'Price')
  
  curves.comb <- rbind.data.frame(pwr.curves, ng.curve) %>%
    mutate(Component = trimws(Component), Market = NULL)
  
  # make delmo as subscript of distinct commodities
  # currently only one power and one gas; the following insures power prices and vols line up
  curves.comb.pivot <- dcast(curves.comb, Date~Component + Segment + Delmo, value.var = 'Price')
  curves.comb.pivot <- cbind(select(curves.comb.pivot, Date, starts_with("NG")),
                             select(curves.comb.pivot, -Date, -starts_with("NG")))
  curves.comb.pivot <- curves.comb.pivot[complete.cases(curves.comb.pivot),]
  curves.comb.ret <- log(curves.comb.pivot[-1,-1]/curves.comb.pivot[-dim(curves.comb.pivot)[1],-1])
  rownames(curves.comb.ret) <- curves.comb.pivot$Date[-1]
  curves.comb.cor <- cor(curves.comb.ret)
  
  price.fwd <- curves.comb.pivot[which(curves.comb.pivot$Date == curve.date.end), -1]
  
  vol.fwd <- monthlyVol(curve_date = curve.date.end,
                        start_date = first.month, end_date = month.used[numofmonth],
                        market = c(market, 'NYMEX'), component =c(component, 'NG')) %>%
    filter(DELMO %in% month.used) %>%
    mutate(SEGMENT = ifelse(MARKET %in% c('NYMEX '), 'rtc', 'pk'))
  vol.fwd.app <- vol.fwd %>% filter(!MARKET %in% c('NYMEX ')) %>%
    mutate(VOLATILITY = VOLATILITY * 0.8, SEGMENT = 'op')
  vol.fwd <- rbind(vol.fwd, vol.fwd.app)
  # reorder so it's natgas then power alphabetical (off before on)
  vol.fwd <- vol.fwd %>% mutate(SEGMENT = factor(SEGMENT, levels = c('rtc', 'op', 'pk')))
  vol.fwd <- vol.fwd[order(vol.fwd$SEGMENT), ]
  vol.fwd.out <- vol.fwd # saving for vol output
  
  # calculate instantanenous vol #####
  vol.fwd.inst <- vol.fwd.out
  
  # repeating some inputs for the optimization
  TTE <- tte
  INCREM <- c(tte_init, tte_increm)
  
  # calibrate Alpha based on objective
  objFunc <- function(x, n, TV2EXP, Beta, Gamma, tte, increm){
    s <- 0
    s <- s + (x * exp(-Beta*(tte[n]-tte[1])) + Gamma)^2*increm[1]
    for (iter in 2:n){
      s <- s + (x * exp(-Beta*(tte[n]-tte[iter])) + Gamma)^2*increm[iter]
    }
    (TV2EXP[n] - s)^2
  }
  
  # browser()
  
  ## fill missing vols in the outer years ####
  # vol.fwd.inst.append <- read_csv('P:/Risk/Risk - Risk Management/aa_Yang/Development/Temp R Input/fill_vol_forPFE.csv')
  # vol.fwd.inst.append <- vol.fwd.inst.append %>% mutate(CURVEDATE = as.Date(CURVEDATE, '%m/%d/%Y'),
  #                                                       DELMO = as.Date(DELMO, '%m/%d/%Y'))
  # vol.fwd.inst <- rbind.data.frame(vol.fwd.inst, vol.fwd.inst.append)
  # vol.fwd.inst <- vol.fwd.inst %>% mutate(SEGMENT = factor(SEGMENT, levels = c('rtc', 'op', 'pk')))
  # vol.fwd.inst <- vol.fwd.inst[order(vol.fwd.inst$SEGMENT), ]
  ## end of fill missing vols in the outer years ####
  
  ## replace power vols with calculated but not-yet-in-aligne numbers ####
  vol.fwd.inst <- read_csv('P:/Risk/Risk - Risk Management/aa_Yang/Development/Temp R Input/fill_vol_forPFE_all.csv')
  vol.fwd.inst <- vol.fwd.inst %>% mutate(CURVEDATE = as.Date(CURVEDATE, '%m/%d/%Y'),
                                          DELMO = as.Date(DELMO, '%m/%d/%Y'))
  vol.fwd.inst <- vol.fwd.inst %>% mutate(SEGMENT = factor(SEGMENT, levels = c('rtc', 'op', 'pk')))
  vol.fwd.inst <- vol.fwd.inst[order(vol.fwd.inst$SEGMENT), ]
  # browser()
  
  vol.fwd.inst$TTE <- TTE[-length(TTE)] # cycling
  vol.fwd.inst$INCREM <-  INCREM[-length(INCREM)] #cycling
  vol.fwd.inst <- vol.fwd.inst %>%  mutate(TV2EXP = VOLATILITY * VOLATILITY * TTE,
                                           n = interval(ymd(first.month), ymd(DELMO)) %/% months(1))
  numofcontracts <- as.integer(round(dim(vol.fwd.inst)[1]))
  numofcommodity <- as.integer(round(numofcontracts/numofmonth))
  Alpha <- vector(mode = 'numeric', length = numofcommodity * numofmonth)
  
  for (commodityitem in 0: (numofcommodity - 1)){
    TV2EXP <- vol.fwd.inst$TV2EXP[(commodityitem*numofmonth+1):
                                    (commodityitem*numofmonth+numofmonth)]
    for (monthitem in 2:numofmonth){
      Alpha[commodityitem*numofmonth+monthitem] <- nloptr(0.5, eval_f = objFunc, lb = 0, 
                                               opts = list("algorithm"="NLOPT_LN_COBYLA", "xtol_rel"=1.0e-6), 
                                               n = monthitem, # expire at ith month
                                               TV2EXP = TV2EXP, Beta = Beta, Gamma = Gamma, 
                                               tte = TTE, increm = INCREM)$solution
      
    }
  }
  
  vol.fwd.inst <- cbind.data.frame(vol.fwd.inst, Alpha)
  
  
  # fill in instantaneous volatility and (recalc'ed) average volatility matrices
  VolIns <- matrix(data = 0, nrow = dim(curves.comb.cor)[1], ncol = numofmonth)
  avgVol <- matrix(data = 0, nrow = dim(curves.comb.cor)[1], ncol = numofmonth)
  
  # browser()
  
  for (commodityitem in 0:(numofcommodity-1)){
    VolIns[(1+commodityitem*numofmonth),1] <- vol.fwd.inst$VOLATILITY[1+commodityitem*numofmonth]
    
    for (monthitem in 2:numofmonth){
      for (j in 1:monthitem){
        VolIns[commodityitem*numofmonth+monthitem,j] <- Alpha[commodityitem*numofmonth+monthitem]*exp(-Beta*(TTE[monthitem]-TTE[j])) + Gamma
      }
    }
  }
  for (commodityitem in 0:(numofcommodity-1)){
    for (monthitem in 2:numofmonth){
      TimeInterval <- TTE - TTE[monthitem-1]
      temp <- (VolIns[(monthitem+commodityitem*numofmonth):((commodityitem+1)*numofmonth), monthitem:numofmonth]^2) %*% INCREM[monthitem:numofmonth]
      avgVol[(commodityitem*numofmonth+monthitem):((commodityitem+1)*numofmonth), (monthitem-1)] <- sqrt(temp/TimeInterval[monthitem:numofmonth])
    }
  }
  
  # simulate forwards using instantaneous vol ####
  pathForward <- array(data = NA, dim = c(nsims, numofmonth, numofcontracts))
  pathVanilla <- array(data = NA, dim = c(nsims, numofmonth, numofcontracts))
  pathSpreadOn <- array(data = NA, dim = c(nsims, numofmonth, numofcontracts))
  pathSpreadOff <- array(data = NA, dim = c(nsims, numofmonth, numofcontracts))
  
  
  rndOrig <- matrix(rnorm(numofcontracts*nsims/2), nrow = numofcontracts) 
  rndAnti <- - rndOrig
  if(min(eigen(curves.comb.cor)$values) < 0){
    curves.comb.cor <- matrix(Matrix::nearPD(curves.comb.cor, corr = TRUE)$mat@x,
                              nrow = numofcontracts)
  }
  # browser()
  dw <- t(cbind(rndOrig, rndAnti))  %*% chol(curves.comb.cor)
  for (i in 1:numofcontracts){
    pathForward[,1,i] <- unlist(price.fwd, use.names = FALSE)[i] * exp(-0.5* VolIns[i,1]^2 *INCREM[1] + sqrt(INCREM[1])*dw[,i]*VolIns[i,1] )
  }
  
  for (k in 2:numofmonth){
    rndOrig <- matrix(rnorm(numofcontracts*nsims/2), nrow = numofcontracts)
    rndAnti <- - rndOrig
    dw <- t(cbind(rndOrig, rndAnti))  %*% chol(curves.comb.cor) # for each month out, dw is generated anew?
    for (i in 1:numofcontracts){
      pathForward[,k,i] <- pathForward[,k-1,i] * exp(-0.5* VolIns[i,k]^2 *INCREM[k] + sqrt(INCREM[k])*dw[,i]*VolIns[i,k])
    }
  }
  
  Option_0.Vanilla <- rep(0, numofcontracts)
  Option_0.Spread.offpeak <- rep(0, numofcontracts)
  Option_0.Spread.peak <- rep(0, numofcontracts)
  
  # vanilla option pricing ####
  # Price the options at time 0
  Time <- matrix(data = TTE[-length(TTE)], nrow = numofcontracts, ncol = 1)
  for (i in 1:numofcontracts){
    Option_0.Vanilla[i] <- BlackScholes(unlist(price.fwd, use.names = FALSE)[i], Vanilla.Strike[i], 
                                        0, Time[i], vol.fwd.inst$VOLATILITY[i], Vanilla.Type[i])
  }
  
  # The value of options that expire later follows the closed-form formula
  for (commodityitem in 0:(numofcommodity-1)){        
    for (k in 1:(numofmonth-1)){        
      for (i in (k+1):numofmonth){      
        TimeRemain <- TTE[i] - TTE[k]
        pos <- commodityitem*numofmonth + i              
        sigma <- avgVol[pos,k]   
        
        pathVanilla[,k,pos] <- BlackScholes(pathForward[,k,pos], Vanilla.Strike[pos], 0, 
                                                   TimeRemain, sigma, Vanilla.Type[pos])
      }  
      
      i <- k  # i is the expiration month
      pos <- commodityitem*numofmonth + i
      pathVanilla[,i,pos] <- Payoff_Vanilla(pathForward[,i,pos], Vanilla.Strike[pos], Vanilla.Type[pos]) 
    }
  }
  
  # Spread Option Pricing ####
  
  Spread.Corr <- matrix(data = 0, nrow = numofmonth, ncol = 2)
  curves.comb.cor.cross <- curves.comb.cor[(numofmonth+1):dim(curves.comb.cor)[1], 1:numofmonth]
  
  for (j in 1:(numofcommodity - 1)){
    for (i in 1:numofmonth){
      Spread.Corr[i, 1] <- curves.comb.cor.cross[i, i]
      Spread.Corr[i, 2] <- curves.comb.cor.cross[i + numofmonth, i]
    }
  }
  
  for (i in 1:numofmonth){

    s2 <- unlist(price.fwd, use.names = FALSE)[i]
    s1.peak <- unlist(price.fwd, use.names = FALSE)[i + 2*numofmonth]
    s1.offpeak <- unlist(price.fwd, use.names = FALSE)[i + numofmonth]
    
    sig2 <- vol.fwd.inst$VOLATILITY[i]
    sig1.peak <- vol.fwd.inst$VOLATILITY[i + 2*numofmonth]
    sig1.offpeak <- vol.fwd.inst$VOLATILITY[i + numofmonth]

    
    Option_0.Spread.offpeak[2*numofmonth + i] <- improvedPearson(s1.offpeak, s2, sig1.offpeak, sig2, Spread.Corr[i,2], Spread.VOM.Off[i],
                                                    Spread.HR.Off[i], TTE[i], Spread.Type.Off[i], 0)
    
    Option_0.Spread.peak[2*numofmonth + i] <- improvedPearson(s1.peak, s2, sig1.peak, sig2, Spread.Corr[i,1], Spread.VOM.On[i],
                                                  Spread.HR.On[i], TTE[i], Spread.Type.On[i], 0)
    
  }
  
  for (k in 1:(numofmonth-1)){   
    for (i in (k+1):numofmonth){  
      TimeRemain <- TTE[i] - TTE[k]
       
      sig2 <- avgVol[i,k]
      sig1.peak <- avgVol[i + 2*numofmonth,k]
      sig1.offpeak <- avgVol[i + numofmonth,k]
      
      
      s2 <- pathForward[,k,i]
      s1.peak <- pathForward[,k,i + 2*numofmonth]
      s1.offpeak <- pathForward[,k,i + numofmonth]
      
      pathSpreadOff[,k,2*numofmonth + i ] <- improvedPearson(s1.offpeak,s2,sig1.offpeak,sig2,Spread.Corr[i,2],Spread.VOM.Off[i],
                                                           Spread.HR.Off[i],TimeRemain,Spread.Type.Off[i], 0)
      
      pathSpreadOn[,k,2*numofmonth + i ] <- improvedPearson(s1.peak,s2,sig1.peak,sig2,Spread.Corr[i,1],Spread.VOM.On[i],
                                                        Spread.HR.On[i],TimeRemain,Spread.Type.On[i],0)
    }
    
    i <- k  # i is the expiration month
    pathSpreadOff[,i,2*numofmonth + i ] <- Payoff_Spread(pathForward[,i,i], pathForward[,i, 2*numofmonth + i ],
                                                       Spread.VOM.Off[i], Spread.HR.Off[i],Spread.Type.Off[i] )
    pathSpreadOn[,k,2*numofmonth + i ] <- Payoff_Spread(pathForward[,i,(i + numofmonth)], pathForward[,i, i],
                                                    Spread.VOM.On[i], Spread.HR.On[i],Spread.Type.On[i] )
  }
  
  
  ## temporary, to extract by path values ####
  # newpathSpreadOn <- matrix(data = NA, nrow = 1000, ncol = 90)
  # newpathSpreadOff <- matrix(data = NA, nrow = 1000, ncol = 90)
  # for (i in 1:1000){
  #   temp <- pathSpreadOn[i, , ]
  #   newpathSpreadOn[i, ] <- diag(temp[, colSums(is.na(temp)) < nrow(temp)])
  # } 
  # 
  # browser()
  # 
  # for (i in 1:1000){
  #   temp <- pathSpreadOff[i, , ]
  #   temp <- temp[, colSums(is.na(temp)) < nrow(temp)]
  #   for (j in 1:89){
  #     newpathSpreadOff[i, j] <- temp[j,  j + 1]
  #   }
  #   
  # } 
  # 
  # write.csv(newpathSpreadOn, file = 'P:/Risk/Risk - Risk Management/aa_Yang/Development/Temp R Output/spreadon.csv',
  #           row.names = FALSE)
  # write.csv(newpathSpreadOff, file = 'P:/Risk/Risk - Risk Management/aa_Yang/Development/Temp R Output/spreadoff.csv',
  #           row.names = FALSE)

  # PFE Calculation ####
  
  MtmPath <- matrix(data = 0, nrow = nsims, ncol = numofmonth)
  # RealizedPath <- matrix(data = 0, nrow = nsims, ncol = numofmonth)              
  PFE.Exposure.Mtm <- matrix(data = NA, nrow = numofmonth, ncol = 1)             
  PFE.Collateral.Mtm <- matrix(data = NA, nrow = numofmonth, ncol = 1) 
  ExpValue.Exposure <- matrix(data = NA, nrow = numofmonth, ncol = 1)             
  ExpValue.Collateral <- matrix(data = NA, nrow = numofmonth, ncol = 1) 
  
  for (i in 1:(numofmonth-1)){
    for (m in 1:numofcommodity){
      
      # add Forwards and Vanilla options
      for (k in (i+1):numofmonth){  
        pos <- (m-1)*numofmonth + k
        MtmPath[,i] <- MtmPath[,i] + Forward.Volume[pos]*(pathForward[,i,pos] - unlist(price.fwd, use.names = FALSE)[pos]) + 
          Vanilla.Volume[pos]*(pathVanilla[,i,pos] - Option_0.Vanilla[pos])
      }
    }
      
      # add spread options
      for (k in (i+1):numofmonth){
        MtmPath[,i] <- MtmPath[,i] + Spread.Volume.Off[k]*(pathSpreadOff[,i,2*numofmonth+k] - Option_0.Spread.offpeak[2*numofmonth+k]) + 
          Spread.Volume.On[k]*(pathSpreadOn[,i,2*numofmonth+k] - Option_0.Spread.peak[2*numofmonth+k])
      }
  }
  
  tempMtm <- matrix(data = NA, nrow = nsims, ncol = 1)
  tempReal <- matrix(data = NA, nrow = nsims, ncol = 1)
  
  for (i in 1:numofmonth){
    tempMtm <- MtmPath[,i]
    tempMtm <- sort(tempMtm)
    
    PFE.Exposure.Mtm[i] <- max(tempMtm[as.integer(nsims*conf)], 0)
    PFE.Collateral.Mtm[i] <- max(-tempMtm[as.integer(nsims*(1-conf))],0)
    
    ExpValue.Exposure[i] <- mean(tempMtm[tempMtm > 0])
    ExpValue.Collateral[i] <- mean(-tempMtm[tempMtm < 0])

    
  }
  
  # output dataframe ####
  PFEoutput <- data.frame( "Month" = seq.Date(from = start_date, to = as.Date(end_date), by = 'month'), 
                           "PFE" = PFE.Exposure.Mtm, "Potential_Collateral" = PFE.Collateral.Mtm,
                           "Expected_Exposure" = ExpValue.Exposure, "Expected_Collateral" = ExpValue.Collateral)
  
  vol.fwd.out$COMPONENT <- trimws(vol.fwd.out$COMPONENT)
  vol.fwd.out.pwr <- dcast(filter(vol.fwd.out, COMPONENT != 'NG'), DELMO+COMPONENT~SEGMENT, value.var = 'VOLATILITY')
  vol.fwd.out.pwr <- rename(vol.fwd.out.pwr, PEAK_POWER_VOL = pk, OFFPEAK_POWER_VOL = op)
  vol.fwd.out.gas <- dcast(filter(vol.fwd.out, COMPONENT == 'NG'), DELMO+COMPONENT~SEGMENT, value.var = 'VOLATILITY')
  vol.fwd.out.gas <- rename(vol.fwd.out.gas, GAS_VOL = rtc)
  curves.comb.out.pwr <- dcast(filter(curves.comb, Component != 'NG' & Date == curve.date.end), 
                               Delmo+Component~Segment, value.var = 'Price')
  curves.comb.out.gas <- dcast(filter(curves.comb, Component == 'NG' & Date == curve.date.end),
                               Delmo+Component~Segment, value.var = 'Price')
  curves.comb.out.pwr <- rename(curves.comb.out.pwr, PEAK_POWER_PRICE = pkPrice, OFFPEAK_POWER_PRICE = opPrice)
  curves.comb.out.gas <- rename(curves.comb.out.gas, GAS_PRICE = rtcPrice)
  
  pwroutput <- left_join(curves.comb.out.pwr, vol.fwd.out.pwr, by = c('Delmo' = 'DELMO', 'Component' = 'COMPONENT'))
  gasoutput <- left_join(curves.comb.out.gas, vol.fwd.out.gas, by = c('Delmo' = 'DELMO', 'Component' = 'COMPONENT'))
  pwroutput$Delmo <- as.character(pwroutput$Delmo) # to avoid renderTable interpreting dates as numbers
  pwroutput <- select(pwroutput, Delmo, PEAK_POWER_PRICE,PEAK_POWER_VOL,
                      OFFPEAK_POWER_PRICE, OFFPEAK_POWER_VOL) # re-arrange column order
  gasoutput$Delmo <- as.character(gasoutput$Delmo)
  
  return(list(PFEoutput = PFEoutput, pwroutput = pwroutput, gasoutput = gasoutput))
  
}


# promptmonth helper function ####
promptmonth <- function(date){
  promptmonth<- as.Date(date)
  day(promptmonth) <- 1
  if(month(date)<12){
    month(promptmonth) <- month(date)+1
  } else{
    month(promptmonth)<-1
    year(promptmonth) <- year(date) + 1
  }
  return(promptmonth)
}

# Vanilla Option function ----------------------------------------------------------

BlackScholes <- function(S, X, rf, T, sigma, type) {    #type: 1-call  2-put
  
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  callPrice <- S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2)
  putPrice <- X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1)
  
  if(type == 'CALL'){
    callPrice
  }
  else
    putPrice
}


Payoff_Vanilla <- function(S, X, type){
  if(type == 'CALL'){ 
    pmax(S-X,0) #call option
  } else{
    pmax(X-S,0) #put option
  }
}




# Spread Option fuction -----------------------------------------------------------

# Kirk approximation

Payoff_Spread <- function(S1,S2,VOM,hr,type){  # inputs are vectors of simulated forward prices with same paremeters value
  if(type == 'CALL'){ 
    pmax(S1 - S2*hr - VOM,0) #call option
  } else{
    pmax(-(S1 - S2*hr - VOM),0) #put option
  }
}



Kirk <- function(s1,s2,sig1,sig2,corr,VOM,hr,t,type,r){ # s1-s2*hr-VOM > K
  s2 <- s2*hr
  f = s1/(s2+VOM)
  
  sigma <- sqrt((sig1^2+(sig2*s2/(s2 + VOM))^2-2*corr*sig1*sig2*s2/(s2 + VOM)))
  
  d1 <- (log(f)+(sigma^2/2)*t)/sigma/sqrt(t)
  d2 <- d1-sigma*sqrt(t)
  n1 <- pnorm(d1)
  n2 <- pnorm(d2)
  callPrice <- (s2 + VOM)*exp(-r*t)*(f*n1-n2)
  putPrice <- callPrice - exp(-r*t)*(s1 - s2 - VOM) 
  if(type == 'CALL'){
    callPrice
  } else{
    putPricce
  }
  
}



# improved Pearson
improvedPearson <- function(s1,s2,sig1,sig2,corr,VOM,hr,t,type,r){
  K <- VOM
  s2 <- s2*hr
  #  sig2 <- sig2*hr
  u1 <- log(s1)+(-sig1^2/2)*t
  u2 <- log(s2)+(-sig2^2/2)*t
  v1 <- sig1*sqrt(t)
  v2 <- sig2*sqrt(t)  
  
  #  R <- exp(sig2*y0)*s2
  # When y0 = 0, a simplified form could be used 
  R <- exp(u2)
  C3 <- 1/(v1*sqrt(1-corr^2))*(u1 - log(R+K))
  D3 <- 1/(v1*sqrt(1-corr^2))*(corr*v1 - v2*R/(R+K))
  e <- -1/(2*v1*sqrt(1-corr^2))*(v2^2*R*K/(R+K)^2)
  
  C1 <- C3 + D3*corr*v1 + e*(corr*v1)^2 + sqrt(1-corr^2)*v1
  D1 <- D3 + 2*corr*v1*e
  C2 <- C3 + D3*v2 + e*v2^2
  D2 <- D3 + 2*v2*e
  
  I1 <- J0(C1,D1) + J1(C1,D1)*e + 0.5*J2(C1,D1)*e^2
  I2 <- J0(C2,D2) + J1(C2,D2)*e + 0.5*J2(C2,D2)*e^2
  I3 <- J0(C3,D3) + J1(C3,D3)*e + 0.5*J2(C3,D3)*e^2
  
  callPrice <- exp(v1^2/2 + u1 - r*t)*I1 - exp(v2^2/2 + u2 - r*t)*I2 - K*exp(-r*t)*I3 
  putPrice <- callPrice - exp(-r*t)*(s1 - s2 - VOM)
  #  callPrice
  if(type == 'CALL'){
    callPrice
  } else{
    putPricce
  }
}


J0 <- function(u,v){
  pnorm(u/sqrt(1+v^2))
}

J1 <- function(u,v){
  (1+(1+u^2)*v^2)/((1+v^2)^2.5)*dnorm(u/sqrt(1+v^2))
}

J2 <- function(u,v){
  ((6 - 6*u^2)*v^2 + (21 - 2*u^2 - u^4)*v^4 + 4*(3 + u^2)*v^6 - 3)/((1+v^2)^5.5)*u*dnorm(u/sqrt(1+v^2))
}



# Option Delta Functions  -------------------------------------------------------------------
Delta.Vanilla <- function(S, X, rf, T, sigma, type){
  d1 <- (log(S/X)+(rf+sigma^2/2)*T)/(sigma*sqrt(T))
  callDelta <- pnorm(d1)
  putDelta <- pnorm(d1) - 1
  
  if(type == 'CALL'){
    callDelta
  }
  else
    putDelta
}

Delta.Spread <- function(s1,s2,sig1,sig2,corr,VOM,hr,t,type,r){
  K <- VOM
  s2 <- s2*hr
  callDelta <- rep(0,2)
  putDelta <- rep(0,2)
  
  u1 <- log(s1)+(-sig1^2/2)*t
  u2 <- log(s2)+(-sig2^2/2)*t
  v1 <- sig1*sqrt(t)
  v2 <- sig2*sqrt(t)  
  
  #  R <- exp(sig2*y0)*s2
  # When y0 = 0, a simplified form could be used 
  R <- exp(u2)
  C3 <- 1/(v1*sqrt(1-corr^2))*(u1 - log(R+K))
  D3 <- 1/(v1*sqrt(1-corr^2))*(corr*v1 - v2*R/(R+K))
  e <- -1/(2*v1*sqrt(1-corr^2))*(v2^2*R*K/(R+K)^2)
  
  C1 <- C3 + D3*corr*v1 + e*(corr*v1)^2 + sqrt(1-corr^2)*v1
  D1 <- D3 + 2*corr*v1*e
  C2 <- C3 + D3*v2 + e*v2^2
  D2 <- D3 + 2*v2*e
  
  I1 <- J0(C1,D1) + J1(C1,D1)*e + 0.5*J2(C1,D1)*e^2
  I2 <- J0(C2,D2) + J1(C2,D2)*e + 0.5*J2(C2,D2)*e^2
  I3 <- J0(C3,D3) + J1(C3,D3)*e + 0.5*J2(C3,D3)*e^2
  
  callDelta[1] <- I1
  callDelta[2] <- -I2
  
  putDelta[1] <- I1 - exp(-r*t)
  putDelta[2] <- -I2 + exp(-r*t)
  
  #  callPrice
  if(type == 'CALL'){
    callDelta
  } else{
    putDelta
  }
}
