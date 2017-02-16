if(!exists("PrepareRawData", mode="function")) source("functions.R")

# Set parameters----------------------
dataPath = 'data/'
dateThresh = as.POSIXct("2000-12-06 00:00:00")
progressThresh = 90


#prepare data of experiment 2-------------------
dataName = "exp2_20170208"

qIdTable = data.table(qId = c('QID18', 'QID8', 'QID7', 'QID3', 'QID4', 'QID5', 'QID25', 
                              'QID19', 'QID11', 'QID12', 'QID13', 'QID14', 'QID15', 'QID29'),
                      variable = rep(c('miss', 'countdown', 'direction', 'stimulus', 'response', 'stimulusOverTime', 'responseGivenTime'),2),
                      run = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2))

PrepareRawData(inputPathToFolder = dataPath, outputPathToFolder = dataPath,
                 qIdTable = qIdTable, dataName = dataName, lowerThresh = dateThresh,
                 progressThresh = progressThresh)


#prepare data of experiment 3-------------------------
dataName = "exp3_20170208"

qIdTable = data.table(qId = c('QID22', 'QID3', 'QID4', 'QID5', 'QID24', 
                              'QID23', 'QID9', 'QID10', 'QID11', 'QID25'),
                      variable = rep(c('direction', 'stimulus', 'response', 'stimulusOverTime', 'responseGivenTime'),2),
                      run = c(1,1,1,1,1,2,2,2,2,2))

PrepareRawData(inputPathToFolder = dataPath, outputPathToFolder = dataPath,
               qIdTable = qIdTable, dataName = dataName, lowerThresh = dateThresh,
               progressThresh = progressThresh)


#prepare data of experiment 4-------------------------
dataName = "exp4_20170208"

qIdTable = data.table(qId = c('QID3', 'QID4', 'QID5', 'QID9', 'QID10', 'QID11'),
                      variable = rep(c('stimulus', 'response', 'stimulusOverTime'),2),
                      run = c(1,1,1,2,2,2))

PrepareRawData(inputPathToFolder = dataPath, outputPathToFolder = dataPath,
               qIdTable = qIdTable, dataName = dataName, lowerThresh = dateThresh,
               progressThresh = progressThresh)

