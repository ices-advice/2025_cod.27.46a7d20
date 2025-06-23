## Prepare plots for report

## Before: Assessment input files, data.Rdata, model.Rdata, residuals.Rdata, retro.Rdata, forecast.Rdata
## After: Report plots

library(icesTAF)
library(ggplot2)
library(reshape2)
library(stockassessment)
library(multiStockassessment)
#install.packages('caMisc', repos = c('https://calbertsen.r-universe.dev', 'https://cloud.r-project.org'))
library(caMisc)

source("utilities.r")
source("utilities_multiStockPlots.R")

#mkdir("report/plots")
dir.create("report/plots", recursive = TRUE)

## 1 Assessment
load('model/model.Rdata')
#load('model/model_updated_2024.Rdata')
load('model/residuals.Rdata')
load('model/retro.Rdata')
load('model/forecast.Rdata')

# SSB
png("report/plots/Fig 4.9a SSB.png", width=952, height=609)
makePlot(fit, "ssb", cols=c("blue","green","red","black"))
dev.off()

# Fbar
png("report/plots/Fig 4.9b Fbar.png", width=952, height=609)
makePlot(fit, "fbar", cols=c("blue","green","red","black"))
dev.off()

# Recruitment
png("report/plots/Fig 4.9c Rec.png", width=952, height=609)
makePlot(fit, "rec", cols=c("blue","green","red","black"))
dev.off()

# Catch
png("report/plots/Fig 4.9d Catch.png", width=952, height=609)
makePlot(fit, "catch", cols=c("blue","green","red","black"))
v <- observedCatch(fit)
points(as.numeric(names(v)),v, col = "black",pch="x",cex=1.5)
dev.off()

# TSB
png("report/plots/Fig 4.9e TSB.png", width=952, height=609)
makePlot(fit, "tsb", cols=c("blue","green","red","black"))
dev.off()

# Difference between observed and predicted catches
# Not a plot but the best place to extract this information
cdif <- data.frame(Observed=v,
                   Estimated=catchtable(fit, addTotal=TRUE,returnList = TRUE)$Total[-43,1]) #43 needs to be incremented by 1 each year 
cdif$diff <- cdif$Observed - cdif$Estimated
cdif$perc <- (cdif$Observed / cdif$Estimated -1) * 100
#write.csv(cdif, "report/tables/cdiff.csv", row.names=FALSE)

# F-at-age
fay <- faytable(fit,returnList = TRUE)
png("report/plots/Fig 4.10a Faa.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(fay[[1]])-2), fay[[1]][-nrow(fay[[1]]),], type="l", lty="solid", col=1:ncol(fay[[1]]), lwd=2, xlab="Year", ylab="F-at-age", main="Northwest", ylim=c(0,1.5))
matplot(1983:(1983+nrow(fay[[2]])-2), fay[[2]][-nrow(fay[[2]]),], type="l", lty="solid", col=1:ncol(fay[[2]]), lwd=2, xlab="Year", ylab="F-at-age", main="South", ylim=c(0,1.5))
matplot(1983:(1983+nrow(fay[[3]])-2), fay[[3]][-nrow(fay[[3]]),], type="l", lty="solid", col=1:ncol(fay[[3]]), lwd=2, xlab="Year", ylab="F-at-age", main="Viking", ylim=c(0,1.5))
legend('topright', col=1:ncol(fay[[3]]), legend=c(colnames(fay[[3]])[1:6],"Age 7+"), bty='n', lwd=2)
dev.off()

# Selectivity in F - For extra years they want horizontal line 
png("report/plots/Fig 4.10b Fsel.png", width=952, height=609)
par(mfrow=c(1,3))
for (s in 1:3){
  # From the forecasts
  sel <- do.call(rbind, lapply(FC[[13]]$result[[s]], function(x) apply(exp(x$sim[,8:14]), 2, median)))
  sel <- t(apply(sel, 1, function(x) x/max(x)))
  rownames(sel) <- rownames(attr(FC[[1]]$result[[s]],"tab"))

  # And from the assessment
  fay[[s]] <- fay[[s]][-c((nrow(fay[[s]])-1),nrow(fay[[s]])),]
  fay[[s]] <- rbind(fay[[s]], sel)

  fmat <- fay[[s]]
  barplot(t(fmat/rowSums(fmat)), border = NA, space = c(0),
          xlab = "Year", main = "Selectivity in F")
  text(1, cumsum(t(fmat/rowSums(fmat))[, 1]) - 0.5 * t(fmat/rowSums(fmat))[, 1], label = as.character(1:ncol(fmat)),
       adj = c(0, 0.2))
  abline(v=nrow(fmat)-3, lty=2)
}
dev.off()

# Number-at-age
nay <- ntable(fit,returnList = TRUE)
png("report/plots/Fig 4.11 Naa.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(nay[[1]])-1), log(nay[[1]]), type="l", lty="solid", col=1:ncol(nay[[1]]), lwd=2, xlab="Year", ylab="log N-at-age", main="Northwest", ylim=c(2,14))
matplot(1983:(1983+nrow(nay[[2]])-1), log(nay[[2]]), type="l", lty="solid", col=1:ncol(nay[[2]]), lwd=2, xlab="Year", ylab="log N-at-age", main="South", ylim=c(2,14))
matplot(1983:(1983+nrow(nay[[3]])-1), log(nay[[3]]), type="l", lty="solid", col=1:ncol(nay[[3]]), lwd=2, xlab="Year", ylab="log N-at-age", main="Viking", ylim=c(2,14))
legend('topright', col=1:ncol(nay[[3]]), legend=c(colnames(nay[[3]])[1:6],"Age 7+"), bty='n', lwd=2)
dev.off()

# Catch-at-age
rp <- attr(fit, "m_rep")
cay <- list()
for (i in 1:length(fit)){
  cay[[i]] <- t(apply(exp(rp$logCatchByFleetAge[[i]][,,]),1:2,sum))
}
png("report/plots/Fig 4.12 Caa.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(cay[[1]])-2), log(cay[[1]][-nrow(cay[[1]]),]), type="l", lty="solid", col=1:ncol(cay[[1]]), lwd=2, xlab="Year", ylab="log Catch-at-age", main="Northwest", ylim=c(1,12))
matplot(1983:(1983+nrow(cay[[2]])-2), log(cay[[2]][-nrow(cay[[1]]),]), type="l", lty="solid", col=1:ncol(cay[[2]]), lwd=2, xlab="Year", ylab="log Catch-at-age", main="South", ylim=c(1,12))
matplot(1983:(1983+nrow(cay[[3]])-2), log(cay[[3]][-nrow(cay[[1]]),]), type="l", lty="solid", col=1:ncol(cay[[3]]), lwd=2, xlab="Year", ylab="log Catch-at-age", main="Viking", ylim=c(1,12))
legend('topright', col=1:ncol(cay[[3]]), legend=c(paste("Age",1:6),"Age 7+"), bty='n', lwd=2)
dev.off()

# Stock weights
sway <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logSW))
sway_lo <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logSW-2*attr(fit,"m_plsd")$logSW))
sway_up <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logSW+2*attr(fit,"m_plsd")$logSW))
png("report/plots/Fig 4.13 SW_Additional_280425.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(sway[[1]])-1), sway[[1]], type="l", lty="solid", col=1:ncol(sway[[1]]), lwd=2, xlab="Year", ylab="Stock weights", main="Northwest", ylim=c(0,16))
matplot(1983:(1983+nrow(fit[[1]]$data$stockMeanWeight)-1), fit[[1]]$data$stockMeanWeight, col=1:ncol(sway[[1]]), add=TRUE)
matplot(1983:(1983+nrow(sway_lo[[1]])-1), sway_lo[[1]], type="l", lty="dotted", col=1:ncol(sway[[1]]), add=TRUE)
matplot(1983:(1983+nrow(sway_up[[1]])-1), sway_up[[1]], type="l", lty="dotted", col=1:ncol(sway[[1]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(sway[[2]])-1), sway[[2]], type="l", lty="solid", col=1:ncol(sway[[2]]), lwd=2, xlab="Year", ylab="Stock weights", main="South", ylim=c(0,16))
matplot(1983:(1983+nrow(fit[[2]]$data$stockMeanWeight)-1), fit[[2]]$data$stockMeanWeight, col=1:ncol(sway[[2]]), add=TRUE)
matplot(1983:(1983+nrow(sway_lo[[2]])-1), sway_lo[[2]], type="l", lty="dotted", col=1:ncol(sway[[2]]), add=TRUE)
matplot(1983:(1983+nrow(sway_up[[2]])-1), sway_up[[2]], type="l", lty="dotted", col=1:ncol(sway[[2]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(sway[[3]])-1), sway[[3]], type="l", lty="solid", col=1:ncol(sway[[3]]), lwd=2, xlab="Year", ylab="Stock weights", main="Viking", ylim=c(0,16))
matplot(1983:(1983+nrow(fit[[3]]$data$stockMeanWeight)-1), fit[[3]]$data$stockMeanWeight, col=1:ncol(sway[[3]]), add=TRUE)
matplot(1983:(1983+nrow(sway_lo[[3]])-1), sway_lo[[3]], type="l", lty="dotted", col=1:ncol(sway[[3]]), add=TRUE)
matplot(1983:(1983+nrow(sway_up[[3]])-1), sway_up[[3]], type="l", lty="dotted", col=1:ncol(sway[[3]]), add=TRUE)
abline(v=2025)
legend('topright', col=1:ncol(sway[[3]]), legend=paste("Age",c(1:6,"7+")), bty='n', lwd=2)
dev.off()

## Maturity
mo <- multiStockassessment:::splitParameter(plogis(attr(fit,"m_pl")$logitMO))
mo_lo <- multiStockassessment:::splitParameter(plogis(attr(fit,"m_pl")$logitMO-2*attr(fit,"m_plsd")$logitMO))
mo_up <- multiStockassessment:::splitParameter(plogis(attr(fit,"m_pl")$logitMO+2*attr(fit,"m_plsd")$logitMO))
png("report/plots/Fig 4.14 MO_AdditionaL_280425.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(mo[[1]])-1), mo[[1]], type="l", lty="solid", col=1:ncol(mo[[1]]), lwd=2, xlab="Year", ylab="Maturity", main="Northwest", ylim=c(0,1.2))
matplot(1983:(1983+nrow(fit[[1]]$data$propMat)-1), fit[[1]]$data$propMat, col=1:ncol(mo[[1]]), add=TRUE)
matplot(1983:(1983+nrow(mo_lo[[1]])-1), mo_lo[[1]], type="l", lty="dotted", col=1:ncol(mo[[1]]), add=TRUE)
matplot(1983:(1983+nrow(mo_up[[1]])-1), mo_up[[1]], type="l", lty="dotted", col=1:ncol(mo[[1]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(mo[[2]])-1), mo[[2]], type="l", lty="solid", col=1:ncol(mo[[2]]), lwd=2, xlab="Year", ylab="Maturity", main="South", ylim=c(0,1.2))
matplot(1983:(1983+nrow(fit[[2]]$data$propMat)-1), fit[[2]]$data$propMat, col=1:ncol(mo[[2]]), add=TRUE)
matplot(1983:(1983+nrow(mo_lo[[2]])-1), mo_lo[[2]], type="l", lty="dotted", col=1:ncol(mo[[2]]), add=TRUE)
matplot(1983:(1983+nrow(mo_up[[2]])-1), mo_up[[2]], type="l", lty="dotted", col=1:ncol(mo[[2]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(mo[[3]])-1), mo[[3]], type="l", lty="solid", col=1:ncol(mo[[3]]), lwd=2, xlab="Year", ylab="Maturity", main="Viking", ylim=c(0,1.2))
matplot(1983:(1983+nrow(fit[[3]]$data$propMat)-1), fit[[3]]$data$propMat, col=1:ncol(mo[[3]]), add=TRUE)
matplot(1983:(1983+nrow(mo_lo[[3]])-1), mo_lo[[3]], type="l", lty="dotted", col=1:ncol(mo[[3]]), add=TRUE)
matplot(1983:(1983+nrow(mo_up[[3]])-1), mo_up[[3]], type="l", lty="dotted", col=1:ncol(mo[[3]]), add=TRUE)
abline(v=2025)
legend('topright', col=1:ncol(mo[[3]]), legend=paste("Age",c(1:6,"7+")), bty='n', lwd=2)
dev.off()

# Natural mortality
nm <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logNM))
nm_lo <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logNM-2*attr(fit,"m_plsd")$logNM))
nm_up <- multiStockassessment:::splitParameter(exp(attr(fit,"m_pl")$logNM+2*attr(fit,"m_plsd")$logNM))
png("report/plots/Fig 4.15 NM_Additional_2804.png", width=952, height=609)
par(mfrow=c(1,3))
matplot(1983:(1983+nrow(nm[[1]])-1), nm[[1]], type="l", lty="solid", col=1:ncol(nm[[1]]), lwd=2, xlab="Year", ylab="Natural mortality", main="Northwest", ylim=c(0,1.6))
matplot(1983:(1983+nrow(fit[[1]]$data$natMor)-1), fit[[1]]$data$natMor[,1:4], col=1:ncol(nm[[1]]), add=TRUE)
matplot(1983:(1983+nrow(nm_lo[[1]])-1), nm_lo[[1]], type="l", lty="dotted", col=1:ncol(nm[[1]]), add=TRUE)
matplot(1983:(1983+nrow(nm_up[[1]])-1), nm_up[[1]], type="l", lty="dotted", col=1:ncol(nm[[1]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(nm[[2]])-1), nm[[2]], type="l", lty="solid", col=1:ncol(nm[[2]]), lwd=2, xlab="Year", ylab="Natural mortality", main="South", ylim=c(0,1.6))
matplot(1983:(1983+nrow(fit[[2]]$data$natMor)-1), fit[[2]]$data$natMor[,1:4], col=1:ncol(nm[[2]]), add=TRUE)
matplot(1983:(1983+nrow(nm_lo[[2]])-1), nm_lo[[2]], type="l", lty="dotted", col=1:ncol(nm[[2]]), add=TRUE)
matplot(1983:(1983+nrow(nm_up[[2]])-1), nm_up[[2]], type="l", lty="dotted", col=1:ncol(nm[[2]]), add=TRUE)
abline(v=2025)
matplot(1983:(1983+nrow(nm[[3]])-1), nm[[3]], type="l", lty="solid", col=1:ncol(nm[[3]]), lwd=2, xlab="Year", ylab="Natural mortality", main="Viking", ylim=c(0,1.6))
matplot(1983:(1983+nrow(fit[[3]]$data$natMor)-1), fit[[3]]$data$natMor[,1:4], col=1:ncol(nm[[3]]), add=TRUE)
matplot(1983:(1983+nrow(nm_lo[[3]])-1), nm_lo[[3]], type="l", lty="dotted", col=1:ncol(nm[[3]]), add=TRUE)
matplot(1983:(1983+nrow(nm_up[[3]])-1), nm_up[[3]], type="l", lty="dotted", col=1:ncol(nm[[3]]), add=TRUE)
abline(v=2025)
legend('topright', col=1:4, legend=paste("Age",c(1:4)), bty='n', lwd=2)
dev.off()

# Residuals
png("report/plots/Fig 4.16 RES.png", width=1228, height=787)
plot(RES)
dev.off()

# Retros
makeRetroPlot(RETRO, "ssb",1:4, cols = c("blue","green","red","black"))
png("report/plots/Fig 4.17a SSBretro.png", width=721,height=725)
par(mfrow=c(3,1))
makeRetroPlot(RETRO, "ssb",1, cols = c("blue"))
makeRetroPlot(RETRO, "ssb",2, cols = c("green"))
makeRetroPlot(RETRO, "ssb",3, cols = c("red"))
dev.off()

makeRetroPlot(RETRO, "fbar",1:4, cols = c("blue","green","red","black"))
png("report/plots/Fig 4.17b Fretro.png", width=721,height=725)
par(mfrow=c(3,1))
makeRetroPlot(RETRO, "fbar",1, cols = c("blue"))
makeRetroPlot(RETRO, "fbar",2, cols = c("green"))
makeRetroPlot(RETRO, "fbar",3, cols = c("red"))
dev.off()

makeRetroPlot(RETRO, "rec",1:4, cols = c("blue","green","red","black"))
png("report/plots/Fig 4.17c Rretro.png", width=721,height=725)
par(mfrow=c(3,1))
makeRetroPlot(RETRO, "rec",1, cols = c("blue"))
makeRetroPlot(RETRO, "rec",2, cols = c("green"))
makeRetroPlot(RETRO, "rec",3, cols = c("red"))
dev.off()

makeRetroPlot(RETRO, "tsb",1:4, cols = c("blue","green","red","black"))
png("report/plots/Fig 4.17d TSBretro.png", width=721,height=725)
par(mfrow=c(3,1))
makeRetroPlot(RETRO, "tsb",1, cols = c("blue"))
makeRetroPlot(RETRO, "tsb",2, cols = c("green"))
makeRetroPlot(RETRO, "tsb",3, cols = c("red"))
dev.off()

#------------------------------------------------------------------------------------------------------------------
# Assessment comparisons
# This needs to be done as part of the first assessment presentation
WG <- fit # this year 2025 
load("model/model_updated_2024.Rdata") # last years object (model_updated.Rdata inside last years assessment and model folder renamed to be 2024)
WG24 <- fit 
# double check ranges and years plotting correctly ------

# Northwest
ssb <- data.frame(Year=as.numeric(row.names(ssbtable(WG,returnList=TRUE)[[1]])),
                  WGNSSK2024=c(ssbtable(WG24,returnList=TRUE)[[1]][,1],NA),
                  WGNSSK2025=ssbtable(WG,returnList=TRUE)[[1]][,1])
ssb <- melt(ssb, id = "Year")
ssb$plot <- "SSB"

fbar <- data.frame(Year=as.numeric(row.names(fbartable(WG,returnList=TRUE)[[1]])),
                  WGNSSK2024=c(fbartable(WG24,returnList=TRUE)[[1]][,1],NA),
                  WGNSSK2025=fbartable(WG,returnList=TRUE)[[1]][,1])
rownames(fbar) <- fbar$Year
fbar["2024","WGNSSK2024"] <- fbar["2025","WGNSSK2025"] <- NA #<- fbar["2023","key20"] <- fbar["2023","key20fix"] <- NA 
fbar <- melt(fbar, id = "Year")
fbar$plot <- "Fbar"

rec <- data.frame(Year=as.numeric(row.names(rectable(WG,returnList=TRUE)[[1]])),
                  WGNSSK2024=c(rectable(WG24,returnList=TRUE)[[1]][,1],NA),
                  WGNSSK2025=rectable(WG,returnList=TRUE)[[1]][,1])
rec <- melt(rec, id = "Year")
rec$plot <- "Recruitment"
plot <- rbind(ssb,fbar,rec)
plot$plot=factor(plot$plot, levels=c("SSB","Fbar","Recruitment"))
ggplot(plot, aes(x=Year,y=value)) + geom_line(aes(col=variable),lwd=1) +
  facet_grid(plot~., scales="free_y") +
  theme_bw() +
  ylab("")
ggsave("report/plots/Fig 4.18a NWcomparison.png")

# South-----------------------------------------------------------------------------------------------------------------
ssb <- data.frame(Year=as.numeric(row.names(ssbtable(WG,returnList=TRUE)[[2]])),
                  WGNSSK2024=c(ssbtable(WG24,returnList=TRUE)[[2]][,1],NA),
                  WGNSSK2025=ssbtable(WG,returnList=TRUE)[[2]][,1])
ssb <- melt(ssb, id = "Year")
ssb$plot <- "SSB"
fbar <- data.frame(Year=as.numeric(row.names(fbartable(WG,returnList=TRUE)[[2]])),
                   WGNSSK2024=c(fbartable(WG24,returnList=TRUE)[[2]][,1],NA),
                   WGNSSK2025=fbartable(WG,returnList=TRUE)[[2]][,1])
rownames(fbar) <- fbar$Year
#fbar["2023","WGNSSK2023"] <- fbar["2023","WGNSSK2024"] <- NA # <- fbar["2023","key20"] <- fbar["2023","key20fix"] <- NA
fbar["2024","WGNSSK2024"] <- fbar["2025","WGNSSK2025"] <- NA # <- fbar["2023","key20"] <- fbar["2023","key20fix"] <- NA
fbar <- melt(fbar, id = "Year")
fbar$plot <- "Fbar"
rec <- data.frame(Year=as.numeric(row.names(rectable(WG,returnList=TRUE)[[2]])),
                  WGNSSK2024=c(rectable(WG24,returnList=TRUE)[[2]][,1],NA),
                  WGNSSK2025=rectable(WG,returnList=TRUE)[[2]][,1])
rec <- melt(rec, id = "Year")
rec$plot <- "Recruitment"
plot <- rbind(ssb,fbar,rec)
plot$plot=factor(plot$plot, levels=c("SSB","Fbar","Recruitment"))
ggplot(plot, aes(x=Year,y=value)) + geom_line(aes(col=variable),lwd=1) +
  facet_grid(plot~., scales="free_y") +
  theme_bw() +
  ylab("")
ggsave("report/plots/Fig 4.18b SouthComparison.png")

# Viking-------------------------------------------------------------------------------------------------------------------
ssb <- data.frame(Year=as.numeric(row.names(ssbtable(WG,returnList=TRUE)[[3]])),
                  WGNSSK2024=c(ssbtable(WG24,returnList=TRUE)[[3]][,1],NA),
                  WGNSSK2025=ssbtable(WG,returnList=TRUE)[[3]][,1])
ssb <- melt(ssb, id = "Year")
ssb$plot <- "SSB"

fbar <- data.frame(Year=as.numeric(row.names(fbartable(WG,returnList=TRUE)[[3]])),
                   WGNSSK2024=c(fbartable(WG24,returnList=TRUE)[[3]][,1],NA),
                   WGNSSK2025=fbartable(WG,returnList=TRUE)[[3]][,1])
rownames(fbar) <- fbar$Year
fbar["2024","WGNSSK2024"] <- fbar["2025","WGNSSK2025"] <- NA # <- fbar["2023","key20"] <- fbar["2023","key20fix"] <- NA 
fbar <- melt(fbar, id = "Year")
fbar$plot <- "Fbar"

rec <- data.frame(Year=as.numeric(row.names(rectable(WG,returnList=TRUE)[[3]])),
                  WGNSSK2024=c(rectable(WG24,returnList=TRUE)[[3]][,1],NA),
                  WGNSSK2025=rectable(WG,returnList=TRUE)[[3]][,1]) 
rec <- melt(rec, id = "Year")
rec$plot <- "Recruitment"
plot <- rbind(ssb,fbar,rec)
plot$plot=factor(plot$plot, levels=c("SSB","Fbar","Recruitment"))
ggplot(plot, aes(x=Year,y=value)) + geom_line(aes(col=variable),lwd=1) +
  facet_grid(plot~., scales="free_y") +
  theme_bw() +
  ylab("")
ggsave("report/plots/Fig 4.18c Viking_Comparison.png")

#--------------------------------------------------------------------------------------------------------------
## 2 Forecasts
load("model/model.Rdata")
load("model/forecast.Rdata")
load("model/NW_1997_type2_refPoints.Rdata")
load("model/S_1997_type2_5yr_refPoints.Rdata")
load("model/V_1997_typeM_refPoints.Rdata")
NW <- NW_1997_type2_refPoints
SO <- S_1997_type2_5yr_refPoints
VI <- V_1997_typeM_refPoints

MSYBtrigger <- c("Northwest" = as.numeric(round(NW['MSYbtrigger'])), "South" = as.numeric(round(SO['MSYbtrigger'])), "Viking" = as.numeric(round(VI['MSYbtrigger'])))
Blim <- c("Northwest" = as.numeric(round(NW['Blim'])), "South" = as.numeric(round(SO['Blim'])), "Viking" = as.numeric(round(VI['Blim'])))
Fmsy <- c("Northwest" = as.numeric(round(NW['Fmsy'],3)), "South" = as.numeric(round(SO['Fmsy'],3)), "Viking" = as.numeric(round(VI['Fmsy'],3)))

# add in f=0 [[3]], or blim 
png("report/plots/Fig 4.19a FCssb.png", width=955,height=615)
makeFCPanel(fit, list(FC[[12]]), "ssb") # , FC[[15]]
dev.off()

png("report/plots/Fig 4.19b FCfbar.png", width=955,height=615)
makeFCPanel(fit, list(FC[[12]]), "fbar")#, FC[[15]]
dev.off()

png("report/plots/Fig 4.19c FCrec.png", width=955,height=615)
makeFCPanel(fit, list(FC[[12]]), "rec") #, FC[[15]]
dev.off()

png("report/plots/Fig 4.19d FCcatch.png", width=955,height=615) # 15 is the more precautionary run 
makeFCPanel(fit, list(FC[[12]]), "catch") # catch advice , FC[[15]]
dev.off()

## 3 Additional ---------------------------------------------------------------------------------------------

## Landings weight proportions 
# Q1
rp <- attr(fit,"m_rep")
shD <- attr(fit,"m_data")$sharedObs

auxL <- split(as.data.frame(shD$aux),shD$aux[,2])
auxDL <- split(as.data.frame(shD$auxData),shD$aux[,2])
predL <- split(as.data.frame(rp$predPerStock),shD$aux[,2])

e <- new.env()
load(file.path("boot/data/WGNSSK2025_SubstockProportions.RData"),e)# Make sure that this data is definitely ONLY for Q1, and not the FY

PredNew <- exp(predL[[10]])
ObsNew <- xtabs(prop ~ year + areaPopulation, e$substockProps)

yy2 <- as.numeric(rownames(ObsNew))

png("report/plots/LPfitQ1_2504.png", width=762,height=571)
matplot(yy2,ObsNew,col=c('blue','green','red'),type="p",pch=16,cex=2)
matplot(yy2,PredNew[seq_len(nrow(PredNew))%%2==1,],type="l",add=TRUE, col=c('blue','green','red'),lty=1,lwd=3)
legend("top",c("Northwest","South","Viking"),col=c('blue',"green","red"),lwd=3,bty="n",ncol=3)
dev.off()

## Whole year--------------------------------------------------------
maxAge <- 7
maxYear <- Inf
minYear <- 1983
ln_WS <- read.ices("boot/data/lf_WS.dat") * read.ices("boot/data/cn_WS.dat")
ln_NS <- read.ices("boot/data/lf_NS.dat")
ln <- cutSum(ln_NS) + cutSum(ln_WS)
lw_WS <- read.ices("boot/data/lw_WS.dat")
lw_NS <- read.ices("boot/data/lw_NS.dat")
lw <- (na2zero(cutMean(lw_NS,ln_NS) * cutSum(ln_NS)) + na2zero(cutMean(lw_NS,ln_NS) * cutSum(ln_WS))) / ln
cn_WS <- read.ices("boot/data/cn_WS.dat")
cn_NS <- read.ices("boot/data/cn_nS.dat")
cn <- cutSum(cn_NS) + cutSum(cn_WS)

lf <- ln / cn

ly <- data.frame(Northwestern=rowSums(cay[[1]][-nrow(cay[[1]]),] * lf * lw),
                 Southern=rowSums(cay[[2]][-nrow(cay[[2]]),] * lf * lw),
                 Viking=rowSums(cay[[3]][-nrow(cay[[3]]),] * lf * lw))

e2 <- new.env()
load(file.path("boot/data/FY_SubstockProportions_WGNSSK2025.RData"),e2) # need to get this data from Ale 

PredFY <- proportions(as.matrix(ly), margin=1)[-(1:12),]
ObsFY <- xtabs(prop ~ year + areaPopulation, e2$substockProps)

png("report/plots/LPfit_2604.png", width=762,height=571)
matplot(yy2,ObsFY,col=c('blue','green','red'),type="p",pch=16,cex=2, xlab="Year", ylab="Proportion")
matplot(yy2,PredFY,type="l",add=TRUE, col=c('blue','green','red'),lty=1,lwd=3)
legend("top",c("Northwest","South","Viking"),col=c('blue',"green","red"),lwd=3,bty="n",ncol=3)
dev.off()

# Output seasonal landings ---------------------------------------------------------------------------------

rp <- attr(fit,"m_rep")
pl <- attr(fit,"m_pl")

getLandings <- function(stock,season){
  Reduce("+",lapply(which(fit[[1]]$data$fleetTypes %in% c(0,7)), getLandings_SSF, stock=stock,season=season))
}

## Q1
names(fit)
NWQ1 <- getLandings(1,1)
SOQ1 <- getLandings(2,1)
VIQ1 <- getLandings(3,1)

## Q2-4
NWQ234 <- Reduce("+",lapply(2:4,function(s)getLandings(1,s)))
SOQ234 <- Reduce("+",lapply(2:4,function(s)getLandings(2,s)))
VIQ234 <- Reduce("+",lapply(2:4,function(s)getLandings(3,s)))

# Plots
Q1 <- data.frame(cbind(NWQ1,SOQ1,VIQ1), Year=names(NWQ1), Quarter="1")
colnames(Q1)[1:3] <- substr(colnames(Q1)[1:3], 1, 2)
Q1 <- melt(Q1, id.vars = c("Year","Quarter"))
Q234 <- data.frame(cbind(NWQ234,SOQ234,VIQ234), Year=names(NWQ234), Quarter="234")
colnames(Q234)[1:3] <- substr(colnames(Q234)[1:3], 1, 2)
Q234 <- melt(Q234, id.vars = c("Year","Quarter"))
plot <- rbind(Q1, Q234)

ggplot(plot[plot$Year>=1995,], aes(x = Year, y = value, fill = Quarter)) +
  geom_bar(stat = "identity", position = 'stack') +
  facet_grid(.~variable) +
  scale_y_continuous("Landings",
                     #labels = scales::percent,
                     expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("report/plots/Seasonal_landings.png", width = 30, height=15, units='cm')

ggplot(plot[plot$Year>=1995,], aes(x = Year, y = value, fill = Quarter)) +
  geom_bar(stat = "identity", position = 'fill') +
  facet_grid(.~variable) +
  scale_y_continuous("Landings",
                     #labels = scales::percent,
                     expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave("report/plots/Seasonal_proportions.png", width = 30, height=15, units='cm')

#-------------------------------------------------------------------------------
# Natural Mortality plot for raw values taken from SMS key run
library(ggplot2)
library(tidyr)
library(dplyr)

nm0 <- as.data.frame(nm0)
nm0$year <- as.numeric(rownames(nm0))

nm0_long <- nm0 %>%
  pivot_longer(cols = -year, names_to = "age", values_to = "value")

nm0_complete <- expand.grid(year = 1983:2025, age = unique(nm0_long$age)) %>%
  left_join(nm0_long, by = c("year", "age"))

# Plot
ggplot(nm0_long, aes(x = year, y = value, color = age)) +
  geom_line(size = 1, na.rm = FALSE) +
  labs(x = "Year", y = "Natural Mortality", color = "Age") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  scale_y_continuous(breaks = seq(0.2, 1.4, by = 0.2))

ggsave("report/plots/NM_Raw.png", width = 30, height=15, units='cm')





