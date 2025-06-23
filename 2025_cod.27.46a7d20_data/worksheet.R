library(icesTAF)

# taf.skeleton()

draft.data(file=TRUE, data.scripts=NULL, originator="WGNSSK")

draft.software(c("icesTAF","DATRAS","plyr","dplyr","mapplots","ggplot2","xlsx","surveyIndex",
                 "XLConnect","reshape2","tidyr","stockassessment","lme4","DHARMa",
                 "xtable","patchwork","glmmTMB","icesAdvice","FLCore","MASS","boot","grid"), file=TRUE)

taf.boot()
taf.boot(software=TRUE)
