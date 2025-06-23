## Run analysis, write model results

## Before: data.Rdata, conf.Rdata
## After: model.Rdata
#clear environment check data folder has data.Rdata
library(icesTAF)
library(stockassessment)
library(multiStockassessment)

#mkdir("model") # might be a missing package ??
#dir.create("model", recursive = TRUE)

source("utilities.R")

# 1 Fit multistock SAM
#taf.boot(software = FALSE)
load('data/data.Rdata')
load('config/conf.Rdata')

## Fix size of idxCor - Edit from Christoffer 
# 8 are true fleets, and 2 are auxiliary 
for(i in 1:3){
  ## Fix size of idxCor to 10 rows
  dat$Stock_data[[i]]$idxCor <- do.call(rbind,c(list(dat$Stock_data[[i]]$idxCor),replicate(10-nrow(dat$Stock_data[[i]]$idxCor),NA,FALSE)))
}
dat$Shared_data$idxCor <- do.call(rbind,c(list(dat$Shared_data$idxCor),replicate(10-nrow(dat$Shared_data$idxCor),NA,FALSE)))

## Patch the configuration for the new version - still need 2025
dc <- defcon(dat$Stock_data$Northwest)
for(i in 1:3){
  conf[[i]]$fixVarToWeight <- rep(conf[[i]]$fixVarToWeight, length.out = length(dc$fixVarToWeight))
  for(nm in setdiff(names(dc),names(conf[[i]])))
    conf[[i]][[nm]] <- dc[[nm]]
}
## End of patch
system.time(fit <- runFit(conf, run = TRUE))
# AIC(fit) AIC: -2519.345
save(fit, file="model/model.Rdata")
load("model/model.Rdata")
#plot(fit)

## 2 Residuals
RES <- residuals(fit)
save(RES, file="model/residuals.Rdata")

## 3 Retrospective runs - longer to run
system.time(RETRO <- retro(fit, 5, 1, initializePars = TRUE, initializeRandomEffects = TRUE))
stockassessment::mohn(RETRO, addTotal = TRUE) # values for slides on retros, report mohn's Rho for SSB, Rec includes 2025 Q1
stockassessment::mohn(RETRO, addTotal=TRUE, lag = 1) # values for slides on retros just for F #lag of 1, because we don't have fishing mortality for 2025
save(RETRO, file="model/retro.Rdata")

# stop here-----------------------------------

## 4 Reference points - not running this year, but will need to copy over last years objects so don't need to copy over values 
#mkdir("model/refpts")
# Extract sd of ln(SSB)
#v1 <- summary(attr(fit,"m_sdrep"))
#v2 <- v1[grepl(".+_logssb",rownames(v1)),]
#sigmas <- lapply(split(v2[,2],rownames(v2)),tail,n=1)
#save(sigmas, file="model/refpts/sigmas.Rdata")

#source.taf("model_northwesternRPs.R")
#source.taf("model_southernRPs.R")
#source.taf("model_vikingRPs.R")

## 5 Forecasts ------------------------------------------------------------------------------------------
load("model/model.Rdata") # removed _updated
#load("model/forecast.Rdata") # reloading to add new scenario to it 
load("model/NW_1997_type2_refPoints.Rdata") # copied over from last years repository into model
load("model/S_1997_type2_5yr_refPoints.Rdata")
load("model/V_1997_typeM_refPoints.Rdata")
NW <- NW_1997_type2_refPoints  
SO <- S_1997_type2_5yr_refPoints
VI <- V_1997_typeM_refPoints

cores <- 1
DF <- FALSE        # deterministicF 
UNLC <- !DF        # useNonLinearityCorrection 
UFH <- TRUE        # useFHessian
PNF <- FALSE       # processNoiseF
FFD <- FALSE       # fixedFDeviation
NOSIM <- 1000      # Needs changing to 1000 - changed to 100 to run without precautionary considerations 

## Average biological input over last three years
aveYears <- lapply(fit,function(x) tail(head(x$data$years,-1),3))
## Use recruitment from 1998 onwards instead of log-random walk
#recYears <- lapply(fit,function(x) 1998:max(x$data$years)) # 2015 change lapply, single line per substock 
recYears <- lapply(fit,function(x) 2023:max(x$data$years)) # 2015 change lapply, single line per substock # test for working group request

AdviceYear <- 2026 

## Reference points
Fmsy <- c("Northwest" = as.numeric(round(NW['Fmsy'],3)), "South" = as.numeric(round(SO['Fmsy'],3)), "Viking" = as.numeric(round(VI['Fmsy'],3)))
Flower <- c("Northwest" = as.numeric(round(NW['Fmsylower'],3)), "South" = as.numeric(round(SO['Fmsylower'],3)), "Viking" = as.numeric(round(VI['Fmsylower'],3)))
Fupper <- c("Northwest" = as.numeric(round(NW['Fmsyupper'],3)), "South" = as.numeric(round(SO['Fmsyupper'],3)), "Viking" = as.numeric(round(VI['Fmsyupper'],3)))
MSYBtrigger <- c("Northwest" = as.numeric(round(NW['MSYbtrigger'])), "South" = as.numeric(round(SO['MSYbtrigger'])), "Viking" = as.numeric(round(VI['MSYbtrigger'])))
Blim <- c("Northwest" = as.numeric(round(NW['Blim'])), "South" = as.numeric(round(SO['Blim'])), "Viking" = as.numeric(round(VI['Blim'])))
Fpa <- c("Northwest" = as.numeric(round(NW['Fpa'],3)), "South" = as.numeric(round(SO['Fpa'],3)), "Viking" = as.numeric(round(VI['Fpa'],3)))
Flim <- c("Northwest" = as.numeric(round(NW['Flim'],3)), "South" = as.numeric(round(SO['Flim'],3)), "Viking" = as.numeric(round(VI['Flim'],3)))

#--------------------------------------------------------------------------------------------------------------------------
# Re-code NS cod forecasts according to new back-calculation to achieve SSB & F targets 
#--------------------------------------------------------------------------------------------------------------------------
# List to store results - make sure year.base is updated for 2024 for each scenario below, and years are updated in forecast names 

FC <- list()

# F Constraints  -------------------------------------------------------------------------------------------------------

## 0) Set seed
set.seed(12345)
## 1) Forecast with arbitrary F
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep("F=0.1",2))
# F0 dummy forecast 
F0 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)

## 2) Calculate backcorrection (F target in second cstr [y==3], assuming first will not be changed)
NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1)
sim <- lapply(F0,function(x) lapply(x,function(y) y$sim))
## Assuming one fleet with F for all ages
logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,-(1:NN[s])][,fit[[s]]$conf$keyLogFsta[1,]+1]))
getFbar <- function(eta, s,y){
  fr <- fit[[s]]$conf$fbarRange - fit[[s]]$conf$minAge + 1
  rowMeans(exp(logF[[s]][[y]][,fr[1]:fr[2]] + eta))    
}

# -----------------------------------------------------------------------------------------------------------------------
# #1 F = FMSY
#------------------------------------------------------------------------------------------------------------------------
Target <- c(Northwest=0.196, South=0.231, Viking=0.187) # given the target FMSY

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
    cstr[[i]] <- c("F=1*", # F=1* F status quo 
                   rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 

FC[[length(FC)+1]] <- list(name = "F = F~MSY~",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #2 F = FmsyLower
#------------------------------------------------------------------------------------------------------------------------

Target <- c(Northwest=0.121, South=0.144, Viking=0.102) # given the target FMSYlower

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
FC[[length(FC)+1]] <- list(name = "F = F~MSY lower~",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))


# -----------------------------------------------------------------------------------------------------------------------
# #3 F = FmsyUpper
#------------------------------------------------------------------------------------------------------------------------

Target <- c(Northwest=0.335, South=0.366, Viking=0.297) # given the target FMSYupper

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
FC[[length(FC)+1]] <- list(name = "F = F~MSY Upper~",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #4 F = 0  
#------------------------------------------------------------------------------------------------------------------------
# No back correction here, we use the original forecast here as a back correction is not needed, this forecast hits zero 

##4 F = 0
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*",rep("F=0.000001",2))
set.seed(12345)
FC[[length(FC)+1]] <- list(name = "F = 0",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

# # Don't do that target back-correction here:
# # we set a lower 0.000001 before and it hit zero for all three stocks 
# 
# Target <- c(Northwest=0.0001, South=0.0001, Viking=0.0001) # given the target F=0 , resulted in NA for the south??
# 
# #### Find eta for all three stocks
# bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))
# 
# ## 4) Set seed again
# set.seed(12345)
# ## 5) Forecast again
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*", # F=1* F status quo 
#                  rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
# FC[[length(FC)+1]] <- list(name = "F = 0",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))


# -----------------------------------------------------------------------------------------------------------------------
# #5 F = Fpa
#------------------------------------------------------------------------------------------------------------------------

Target <- c(Northwest=0.543, South=0.464, Viking=0.307) # given the target F=Fpa  

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
FC[[length(FC)+1]] <- list(name = "F = Fpa",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))


# -----------------------------------------------------------------------------------------------------------------------
# #6 F = F2025
#------------------------------------------------------------------------------------------------------------------------

Target <- c(Northwest=0.42, South=0.59, Viking=0.34) # given the target F=F2025 - Intermediate assumptions??   

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
FC[[length(FC)+1]] <- list(name = "F = F2025",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# # HCRs - they are because we have two stocks below Btrigger, what our advice will based on
#------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------
# 7 ICES Advice Rule FMSY - MSY approach: F~MSY~ x SSB (2026) / MSY B~trigger
#------------------------------------------------------------------------------------------------------------------------
SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # fval would be our target here - check 

Target <- c(Northwest=fval[1], South=fval[2], Viking=fval[3]) # NW:0.1766751  S:0.1105951  V:0.1519341 

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 

FC[[length(FC)+1]] <- list(name = "MSY approach: F~MSY~ x SSB (2026) / MSY B~trigger~",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

#------------------------------------------------------------------------------------------------------------------------
# 8 ICES Advice Rule FLower - F = F~MSY lower~ x SSB (2026) / MSY B~trigger~
#------------------------------------------------------------------------------------------------------------------------
fval <- pmin(Flower*SSB/MSYBtrigger, Flower) 

Target <- c(Northwest=fval[1], South=fval[2], Viking=fval[3]) # NW:0.10906984 S:0.06894242 V:0.08287314 

bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 

FC[[length(FC)+1]] <- list(name = "F = F~MSY lower~ x SSB (2026) / MSY B~trigger~",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

save(FC, file="model/forecast.Rdata")

#-------------------------------------------------------------------------------------------------------------------------
# SSB constraints --------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------

## 0) Set seed
set.seed(12345) # 12345
## 1) Forecast with arbitrary F
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep("F=0.1",2))
#Dummy forecast 
F0 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)

## 2) Calculate backcorrection (SSB target in second cstr [y==3], assuming first will not be changed)
NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1) #+ group
sim <- lapply(F0,function(x) lapply(x,function(y) y$sim)) # 
logN <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,1:NN[s]])) #lists for substocks and year 
logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,-(1:NN[s])]))
M <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_natMor)))
PropMat <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_propMat)))
SW <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_stockMeanWeight)))
getNextSSB <- function(eta, s,y){
  R <- logN[[s]][[y+1]][,1] #recruitment in year+1
  logPredN0 <-  logN[[s]][[y]] - exp(logF[[s]][[y]]) - M[[s]][[y]] # predicts log numbers without fishing mortality deviations
  logPredN0 <- cbind(R,logPredN0[,1:(ncol(logPredN0)-2)],log(rowSums(exp(logPredN0[,-(1:(ncol(logPredN0)-2))]))))
  eps <- logN[[s]][[y+1]]-logPredN0
  logPredN <- logN[[s]][[y]] - exp(logF[[s]][[y]]+eta) - M[[s]][[y]]
  logPredN <- cbind(R,logPredN[,1:(ncol(logPredN)-2)],log(rowSums(exp(logPredN[,-(1:(ncol(logPredN)-2))])))) + eps
  rowSums(exp(logPredN) * PropMat[[s]][[y+1]] * SW[[s]][[y+1]])
}

# -----------------------------------------------------------------------------------------------------------------------
# #9 SSB = Blim
#------------------------------------------------------------------------------------------------------------------------

Target <- c(Northwest=29378, South=14002, Viking=9619) # Blim

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getNextSSB(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2))
FC[[length(FC)+1]] <- list(name = "SSB = Blim",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

# -----------------------------------------------------------------------------------------------------------------------
# #10 SSB = MSY Btrigger
#------------------------------------------------------------------------------------------------------------------------

#south is so low, 2 years is not long enough to rebuild - get rid of these scenario for the south or footnote 

Target <- c(Northwest=40823, South=19851, Viking=13732) # MSY Btrigger - Not hitting target for the south as expected 

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getNextSSB(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2))
FC[[length(FC)+1]] <- list(name = "SSB = MSY Btrigger",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))


# -----------------------------------------------------------------------------------------------------------------------
# #11 SSB(2027) = SSB(2026)
#------------------------------------------------------------------------------------------------------------------------
# double check intermediate assumptions here with new forecasts, the values should be the same, should be the same median with 1000 runs 

Target <- c(Northwest=35837, South=9259, Viking=11096) # SSB(2027) = SSB(2026) # values taken from intermediate assumption

#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getNextSSB(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2))
FC[[length(FC)+1]] <- list(name = "SSB(2027) = SSB(2026)",
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))

#save(FC, file="model/forecast.Rdata")

#-------------------------------------------------------------------------------------------------------------------------------------
# 12 Common F reduction - With Precautionary Considerations 
# Specifying the SSB=blim scenario as an SSB initially to find southern value, then an F constraint to find the multiplier for NW & V
#-------------------------------------------------------------------------------------------------------------------------------------

#manual check: Fmult = f thats gives us SSB=Blim for the south / intermediate year SSb south, if this is the value 0.42 (NW) * Fmult should give us the F in this forecast for NW and same again for the south 
#manual checks need to be done for 1000 simulations checks 

Target <- c(Northwest=29378, South=14002, Viking=9619) # Blim

#### Find eta for all three stocks - only use the value for the south 
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getNextSSB(ee[s],s,3)))-Target)^2))

cstr <- vector("list",3) #setting the constraint for the south 
for(i in 2)
  cstr[[i]] <- c("F=1*", 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # the F we need 

fval<-0.1*exp(bc_eta$par[2])# we need this F for the south
Fmult <- fval / attr(FC[[1]]$result[[2]], "shorttab")[1,2] # This gives us the values of F reduction needed for the other two 

# need to apply to intermediate year F's for other stocks
Target <- c(Northwest=attr(FC[[1]]$result[[1]], "shorttab")[1,2]*Fmult, South=fval, Viking=attr(FC[[1]]$result[[3]], "shorttab")[1,2]*Fmult) # given the target F=F2025 - Intermediate assumptions??  
#~0.02299083 0.03385715 0.01881588 

bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
#cstr <- vector("list",3)
for(i in c(1,3)) # using Christoffer's optimizer only for the NW & V as we've done above already, don't double up on correction 
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2)) # optimisation to find the multiplier 
FC[[length(FC)+1]] <- list(name = "F = 0.04F~2025~", # Common F reduction 
                           constraints = cstr,
                           result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                                                  deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                                                  nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))


save(FC, file="model/forecast.Rdata")

#FC[[12]]<-NULL # getting rid of extra test simulations, cleaning up final forecast database 

#--------------------------------------------------------------------------------------------------------------------------
# 13 Without Precautionary Considerations  
# F multiplier that equates to the cumulative substock advice 
#-------------------------------------------------------------------------------------------------------------------------

##########################################################################################
## SSB target (AdviceYear+1) per stock
##########################################################################################

Target <- c(Northwest=29378, South=14002, Viking=9619)

## 0) Set seed
set.seed(12345)
## 1) Forecast with arbitrary F
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep("F=0.1",2))
F0 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)


## 2) Calculate backcorrection (SSB target in second cstr [y==3], assuming first will not be changed)
NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1)
sim <- lapply(F0,function(x) lapply(x,function(y) y$sim))
logN <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,1:NN[s]]))
logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,-(1:NN[s])]))
M <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_natMor)))
PropMat <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_propMat)))
SW <- lapply(F0,function(x) lapply(x,function(y) t(y$bio_stockMeanWeight)))
getNextSSB <- function(eta, s,y){
  R <- logN[[s]][[y+1]][,1]
  logPredN0 <-  logN[[s]][[y]] - exp(logF[[s]][[y]]) - M[[s]][[y]]
  logPredN0 <- cbind(R,logPredN0[,1:(ncol(logPredN0)-2)],log(rowSums(exp(logPredN0[,-(1:(ncol(logPredN0)-2))]))))
  eps <- logN[[s]][[y+1]]-logPredN0
  logPredN <- logN[[s]][[y]] - exp(logF[[s]][[y]]+eta) - M[[s]][[y]]
  logPredN <- cbind(R,logPredN[,1:(ncol(logPredN)-2)],log(rowSums(exp(logPredN[,-(1:(ncol(logPredN)-2))])))) + eps
  rowSums(exp(logPredN) * PropMat[[s]][[y+1]] * SW[[s]][[y+1]])
}
#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getNextSSB(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2))
F1 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)


## Compare to direct SSB target
set.seed(12345)
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("SSB=%f",Target[i]),2))
F2 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)


saveRDS(list(F0=F0,eta=bc_eta,F1=F1,F2=F2),"BCF_SSB.rds")

##########################################################################################
## F target (AdviceYear) per stock
##########################################################################################


Target <- c(Northwest=0.196, South=0.231, Viking=0.187)

## 0) Set seed
set.seed(12345)
## 1) Forecast with arbitrary F
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep("F=0.1",2))
F0 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)


## 2) Calculate backcorrection (F target in second cstr [y==3], assuming first will not be changed)
NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1)
sim <- lapply(F0,function(x) lapply(x,function(y) y$sim))
## Assuming one fleet with F for all ages
logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,-(1:NN[s])][,fit[[s]]$conf$keyLogFsta[1,]+1]))
getFbar <- function(eta, s,y){
  fr <- fit[[s]]$conf$fbarRange - fit[[s]]$conf$minAge + 1
  rowMeans(exp(logF[[s]][[y]][,fr[1]:fr[2]] + eta))    
}
#### Find eta for all three stocks
bc_eta <- nlminb(numeric(3), function(ee) sum((sapply(1:3,function(s)median(getFbar(ee[s],s,3)))-Target)^2))

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",0.1*exp(bc_eta$par[i])),2))
F1 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)



set.seed(12345)
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",Target[i]),2))
F2 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)



saveRDS(list(F0=F0,eta=bc_eta,F1=F1,F2=F2),"BCF_F.rds")


##########################################################################################
## Catch target (AdviceYear) combined
##########################################################################################

## Last F
lastF <- tail(fbartable(fit),1)[seq(1,len=length(fit),by=length(fit))]


Target <- c(Total=12679) #12719

## 0) Set seed
set.seed(12345)
## 1) Forecast with "arbitrary" F (i.e. last F)
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",lastF[i]),2))
F0 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)#, returnObj=2)


## 2) Calculate backcorrection (SSB target in second cstr [y==3], assuming first will not be changed)
getEta <- function(FC0){
  NN <- sapply(fit,function(s) s$conf$maxAge-s$conf$minAge+1)
  sim <- lapply(FC0,function(x) lapply(x,function(y) y$sim))
  logN <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,1:NN[s]]))
  logF <- lapply(seq_along(sim), function(s) lapply(sim[[s]], function(y) y[,-(1:NN[s])]))
  M <- lapply(FC0,function(x) lapply(x,function(y) t(y$bio_natMor)))
  PropMat <- lapply(FC0,function(x) lapply(x,function(y) t(y$bio_propMat)))
  SW <- lapply(FC0,function(x) lapply(x,function(y) t(y$bio_stockMeanWeight)))
  CW <- lapply(FC0,function(x) lapply(x,function(y) t(y$bio_catchMeanWeight)))
  getCatch <- function(eta, s,y){
    ## Predicted catch
    ## pred = logN(a,y) + log(mort.fleetCumulativeIncidence(a,y,f-1));
    ## NOTE: Values needed to calculate cumulative incidence is not currently available from forecast
    ## Quick approximation, catch equation
    Fx <- exp(logF[[s]][[y]])
    Zx <- Fx + M[[s]][[y]]
    predCx <- rowSums(Fx / Zx * (1 - exp(-Zx)) * exp(logN[[s]][[y]]) * CW[[s]][[y]])
    seasonMultiplier <- median(FC0[[s]][[y]]$catch) / median(predCx)
    F <- exp(logF[[s]][[y]] + eta)
    Z <- F + M[[s]][[y]]
    predC <- rowSums(F / Z * (1 - exp(-Z)) * exp(logN[[s]][[y]]) * CW[[s]][[y]])
    median(predC)*seasonMultiplier
  }
  #### Find eta for all three stocks
  nlminb(numeric(1), function(ee) (sum((sapply(1:3,function(s)(getCatch(ee,s,3)))))-Target)^2)    
}
bc_eta <- getEta(F0)

## 4) Set seed again
set.seed(12345)
## 5) Forecast again
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",lastF[i]*exp(bc_eta$par)),2))
F1 <- modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                    deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                    nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE)

local({x0 <- sapply(F0,function(x)attr(x,"tab")[as.character(AdviceYear),"catch:median"]);c(x0,Total=sum(x0))})

local({x0 <- sapply(F1,function(x)attr(x,"tab")[as.character(AdviceYear),"catch:median"]);c(x0,Total=sum(x0))})

## Try again to get closer (repeat these lines until satisfied)
lastF <- sapply(lapply(F1,fbartable),function(x)x[as.character(AdviceYear),"Estimate"])
bc_eta2 <- getEta(F1)
set.seed(12345)
cstr <- vector("list",3)
for(i in 1:3)
  cstr[[i]] <- c("F=1*", # F=1* F status quo 
                 rep(sprintf("F=%f",lastF[i]*exp(bc_eta2$par)),2))
FC[[length(FC)+1]] <- list(name = "WPC", # Without precautionary considerations 
                            constraints = cstr,
                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
                            deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
                            nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
local({x0 <- sapply(F1,function(x)attr(x,"tab")[as.character(AdviceYear),"catch:median"]);c(x0,Total=sum(x0))})

save(FC, file="model/forecast.Rdata")

#FC[[length(FC)+1]] <- list(name = "WPC", constraints = cstr, result =F1)
#FC[[13]]<-NULL

# #-------------------------------------------------------------------------------------------------------------------------
# # Forecasts Version 1 - No Back Correction, Note:[[FC7]] below is now FC[[11]] above 
# #-------------------------------------------------------------------------------------------------------------------------
# 
# ##1 F = FMSY
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*", # F=1* F status quo 
#                  rep(sprintf("F=%f",Fmsy[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~MSY~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##2 F = FmsyLower
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("F=%f",Flower[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~MSY lower~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##3 F = FmsyUpper
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("F=%f",Fupper[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~MSY upper~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##4 F = 0
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",rep("F=0.000001",2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = 0",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##5 F = Fpa
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",rep(sprintf("F=%f",Fpa[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~pa~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##6 F = Flim - Don't include 
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",rep(sprintf("F=%f",Flim[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~lim~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# 
# 
# ##7 SSB = Blim
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("SSB=%f",Blim[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = sprintf("SSB (%d) = B~lim~",AdviceYear+1),
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##8 SSB = MSY Btrigger
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("SSB=%f",MSYBtrigger[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = sprintf("SSB (%d) = MSY B~trigger~",AdviceYear+1),
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##9 F = F2025
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",rep("F=1*",2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = F~2025~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# ##10 SSB(2027) = SSB(2026)
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# cstr <- vector("list",3)
# for(i in 1:3)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("SSB=%f",SSB[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "SSB(2027)=SSB(2026)",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# #save(FC, file="model/forecast_Dummy_Rec_2904.Rdata")
# # can load the above if don't have time can reload 
# 
# # -------------------------------------------------------------------------------------------------------------------
# # 11 HCRs (if applicable) - they are because we have two stocks below Btrigger, what our advice will based on 
# #---------------------------------------------------------------------------------------------------------------------
# 
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# 
# if(any(SSB<MSYBtrigger)){
#   
#   ## ICES Advice rule, FMSY
#   fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy)
#   cstr <- vector("list",3)
#   for(i in 1:3)
#     cstr[[i]] <- c("F=1*",
#                    rep(sprintf("F=%f",fval[i]),2))
#   set.seed(12345)
#   FC[[length(FC)+1]] <- list(name = "MSY approach: F~MSY~ x SSB (2026) / MSY B~trigger~",
#                              constraints = cstr,
#                              result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                     deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                     nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
#   
#   ## 12 ICES Advice rule, Flower
#   fval <- pmin(Flower*SSB/MSYBtrigger, Flower) # will give F target, don't need a number here, then optimise then rest 
#   cstr <- vector("list",3)
#   for(i in 1:3)
#     cstr[[i]] <- c("F=1*",
#                    rep(sprintf("F=%f",fval[i]),2))
#   set.seed(12345)
#   FC[[length(FC)+1]] <- list(name = "F = F~MSY lower~ x SSB (2026) / MSY B~trigger~",
#                              constraints = cstr,
#                              result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                     deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                     nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# }
# 
# #save(FC, file="model/forecast.Rdata")
# 
# ###################################### - 
# ## 13 Common F reduction
# # so this sets the F reduction for the NW and Viking according to the southern F that would achieve blim etc etc check notes 
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy)
# Fmult <- fval[[2]] / attr(FC[[1]]$result[[2]], "shorttab")[1,2]
# cstr <- vector("list",3)
# for(i in c(1,3))
#   cstr[[i]] <- c("F=1*",
#                  rep(paste0(sprintf("F=%f",Fmult),'*'),2))
# for(i in 2)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("F=%f",fval[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = 0.182F~2025~", # make sure these values are correct future runs (2026?)
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# save(FC, file="model/forecast.Rdata")
# 
# 
# #-------------------------------------------------------------------------------------------------------------------------
# # Additional scenario 2025 WG Meeting - find scenario for the south, plus we need to run a F reduction for the other two
# # DO NOT RUN BLINDLY NEXT YEAR - QUICK VERSION BELOW FOR SPEED 
# #-------------------------------------------------------------------------------------------------------------------------
# 
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3])) # shorttab 3,3 is the beginning of the advice year 
# if(any(SSB<MSYBtrigger)){
#   
#   ## ICES Advice rule, FMSY
#   fval <- pmin(Fmsy*SSB/MSYBtrigger, Fmsy) # want this for NW, V - South 
#   cstr <- vector("list",3)
#   for(i in c(1,3)) # selecting NW & V 
#     cstr[[i]] <- c("F=1*",
#                    rep(sprintf("F=%f",fval[i]),2)) # 2 = south 
#   cstr[[2]]<-c("F=1*",
#                rep(sprintf("F=%f",attr(FC[[7]]$result[[2]],"shorttab")[1,3]),2)) # 2 = south
#   set.seed(12345)
#   FC[[length(FC)+1]] <- list(name = "MSY approach: F~MSY~ x SSB (2025) / MSY B~trigger~",
#                              constraints = cstr,
#                              result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                     deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                     nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))}
# save(FC, file="model/forecast.Rdata")
# 
# #-------------------------------------------------------------------------------------------------------------------------
# ## Common F reduction
# # so this sets the F reduction for the NW and Viking according to the southern F that would achieve blim etc etc check notes 
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# fval <- attr(FC[[7]]$result[[2]],"shorttab")[1,3]
# Fmult <- fval / attr(FC[[1]]$result[[2]], "shorttab")[1,2] # picked up for only the southern intermediate year F, so can 
# cstr <- vector("list",3)
# for(i in c(1,3))
#   cstr[[i]] <- c("F=1*",
#                  rep(paste0(sprintf("F=%f",Fmult),'*'),2))
# for(i in 2)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("F=%f",fval),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = 0.08F~2025~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# save(FC, file="model/forecast.Rdata")
# 
# #--------------------------------------------------------------------------------------------------------------------------
# ## Common F reduction #2 - this is the one we used in previous forecast run 
# # Specifying the SSB=blim scenario as an SSB (rather than F, as above) constraint.See notes WGNSSK 2025_Workflow /30.  
# SSB <- unlist(lapply(FC[[1]]$result,function(x)attr(x,"shorttab")[3,3]))
# fval <- attr(FC[[7]]$result[[2]],"shorttab")[1,3]
# Fmult <- fval / attr(FC[[1]]$result[[2]], "shorttab")[1,2] # picked up for only the southern intermediate year F, so can 
# cstr <- vector("list",3)
# for(i in c(1,3))
#   cstr[[i]] <- c("F=1*",
#                  rep(paste0(sprintf("F=%f",Fmult),'*'),2))
# for(i in 2)
#   cstr[[i]] <- c("F=1*",
#                  rep(sprintf("SSB=%f",Blim[i]),2))
# set.seed(12345)
# FC[[length(FC)+1]] <- list(name = "F = 0.08F~2025~",
#                            constraints = cstr,
#                            result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                   deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                   nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# 
# save(FC, file="model/forecast_test.Rdata")
# 
# #------------------------------------------------------------------------------------------------------------------------
# # Without precautionary considerations - see running forecasts notes
# #------------------------------------------------------------------------------------------------------------------------
# 
# FCR2 <- list()
# 
# for (mult in seq(0.3, 0.5, by=0.1)){
#   
#   cstr <- vector("list",3)
#   for(i in 1:3)
#     cstr[[i]] <- c("F=1*",
#                    rep(paste0("F=",mult,"*"),2))# Specify F multiplier rather than F value (via *)
#   set.seed(12345)
#   FCR2[[length(FCR2)+1]] <- list(name = paste("Mult=",mult),
#                                  constraints = cstr,
#                                  result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
#                                                         deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
#                                                         nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# }
# 
# save(FC, FCR2, file="model/forecast.Rdata") 
# 
# # Adapted from the report_tables script will pull out the catch values by substock for each multiplier tested in the loop
# for(i in 1:length(FCR2))print(sapply(FCR2[[i]]$result,function(x)attr(x,"shorttab")[4,3]))
# 
# #-----------------------------------------------------------------------------------------------------------------------
# # Forecasts in F increments # -- Change the year.base = 2024 ------ 
# ## Takes 1.5 days to run - We need this for the checklist 
# #-----------------------------------------------------------------------------------------------------------------------
# 
# # FCR2 <- list()
# #  
# #  for (fval in c(0.000001, seq(0.01,round(max(Fupper), digits=2),0.01))){
# #    
# #    cstr <- vector("list",3)
# #    for(i in 1:3)
# #      cstr[[i]] <- c("F=1*",
# #                     rep(sprintf("F=%f",fval),2))
# #    set.seed(12345)
# #    FCR2[[length(FCR2)+1]] <- list(name = "Frange",
# #                                   constraints = cstr,
# #                                   result = modelforecast(fit, cstr, ave.years=aveYears, rec.years=recYears,year.base=2024, useModelLastN=TRUE, resampleFirst = TRUE, splitLD = TRUE,
# #                                                          deterministicF = DF, useNonLinearityCorrection= UNLC, processNoiseF = PNF, fixedFDeviation = FFD,
# #                                                          nosim=NOSIM,useFHessian=UFH, ncores = cores, progress = TRUE))
# #  }
# #  
# # save(FC, FCR2, file="model/forecast.Rdata") #FCR1






