if(!exists("PrepareRawData", mode="function")) source("functions.R")

# Set parameters----------------------
dataPath = '~/Desktop/URAP/sd_orientation_github/data/'


#prepare data of experiment 2 old-------------------
dataName = "exp2_20170208"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp2"]
trial[, exp:="exp2"]
sbjAll = sbj
trialAll = trial

#prepare data of experiment 2 new-------------------
dataName = "exp2_20170222"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp2"]
trial[, exp:="exp2"]
sbjAll = rbind(sbjAll, sbj)
trialAll = rbind(trialAll, trial)


#prepare data of experiment 3-------------------------
dataName = "exp3_20170208"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp3"]
trial[, exp:="exp3"]
trial[, countdown:=NA]
trial[, miss:=NA]

sbjAll = rbind(sbjAll, sbj)
trialAll = rbind(trialAll, trial)


#prepare data of experiment 4-------------------------
dataName = "exp4_20170208"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp4"]
trial[, exp:="exp4"]
trial[, countdown:=NA]
trial[, miss:=NA]
trial[, direction:=NA]
trial[, responseGivenTime:=NA]

sbjAll = rbind(sbjAll, sbj)
trialAll = rbind(trialAll, trial)

#change data type---------
sbjAll[, exp:=factor(exp)]
trialAll[, exp:=factor(exp)]
