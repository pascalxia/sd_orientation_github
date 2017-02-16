if(!exists("PrepareRawData", mode="function")) source("functions.R")

# Set parameters----------------------
dataPath = 'data/'


#prepare data of experiment 2-------------------
dataName = "exp2_20170208"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp2"]
trial[, exp:="exp2"]
sbjAll = sbj
trialAll = trial


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
