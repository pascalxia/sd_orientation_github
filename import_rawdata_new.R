library(data.table)

# Requires path to folder containing csv file, path to folder to store output file, 
# name of the csv file (without extension), qID table, lower and upper startdate 
# thresholds (defaulted to null if not passed in).

readEditAndWrite = function (inputPathToFolder, outputPathToFolder, dataName, 
                             qIdTable, lowerThresh = NULL, upperThresh = NULL, 
                             progressThresh = NULL) {
  rawData = fread(paste(inputPathToFolder, paste0(dataName, ".csv"), sep = ''))
  rawData = rawData[2:nrow(rawData),]
  
  columnNames = unlist(rawData[1,])
  columnNames = gsub("\\{\"\"ImportId\"\":\"\"", "", columnNames)
  columnNames = gsub("\"\"\\}", "", columnNames)
  setnames(rawData, 1:ncol(rawData), columnNames)
  
  rawData = rawData[2:nrow(rawData),]
  rawData[, startDate:=as.POSIXct(startDate)]
  rawData[, endDate:=as.POSIXct(endDate)]
  rawData[, progress:=as.integer(progress)]
  
  if (!is.null(lowerThresh)) {
    rawData = rawData[startDate > lowerThresh, ]
  }
  if (!is.null(upperThresh)) {
    rawData = rawData[startDate < upperThresh, ]
  }
  if(!is.null(progressThresh)){
    rawData = rawData[progress > progressThresh,]
  }
  
  subjectData = rawData[, c(1:9, ncol(rawData)), with = FALSE]
  setnames(subjectData, 9, 'sbjId')
  setnames(subjectData, 10, 'random')
  
  subjectData[, sbjId:=as.factor(sbjId)]
  subjectData[, duration:=as.integer(duration)]
  setkey(subjectData, sbjId)
  
  
  trial = rawData[, c(9, 20:ncol(rawData) - 3), with = FALSE]
  setnames(trial, 1, 'sbjId')
  trial = melt(trial, 'sbjId', variable.name = 'qId')
  trial[, order:=strtoi(substr(qId, 1, regexpr('_', qId)-1))]
  trial[, value:=as.numeric(value)]
  for(i in 1:nrow(qIdTable)){
    trial[grep(qIdTable$qId[i], qId), 
          c('variable', 'run'):=.(qIdTable$variable[i], qIdTable$run[i])]
  }
  trial = dcast(trial[!is.na(variable),], sbjId+run+order~variable, value.var = 'value')
  
  trial[, sbjId:=as.factor(sbjId)]
  trial[, run:=as.factor(run)]
  trial[, direction:=as.factor(direction)]
  
  setkey(trial, sbjId, run, order)
  
  save(trial, subjectData, 
       file = paste(outputPathToFolder, paste0(dataName, '.RData'), sep = ''))
  
}

# Set parameters.
dataPath = 'data/'
dataName = "exp2_20170208"
dateThresh = as.POSIXct("2000-12-06 00:00:00")
progressThresh = 90

qIdTable = data.table(qId = c('QID18', 'QID8', 'QID7', 'QID3', 'QID4', 'QID5', 'QID25', 
                              'QID19', 'QID11', 'QID12', 'QID13', 'QID14', 'QID15', 'QID29'),
                      variable = rep(c('miss', 'countdown', 'direction', 'stimulus', 'response', 'stimulusOverTime', 'responseGivenTime'),2),
                      run = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2))

# Invoke Function.
readEditAndWrite(inputPathToFolder = dataPath, outputPathToFolder = dataPath,
                qIdTable = qIdTable, dataName = dataName, lowerThresh = dateThresh,
                progressThresh = progressThresh)
