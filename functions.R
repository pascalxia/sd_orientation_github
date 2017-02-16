library(data.table)

# Requires path to folder containing csv file, path to folder to store output file, 
# name of the csv file (without extension), qID table, lower and upper startdate 
# thresholds (defaulted to null if not passed in).

PrepareRawData = function (inputPathToFolder, outputPathToFolder, dataName, 
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
  
  sbj = rawData[, c(1:9, ncol(rawData)), with = FALSE]
  setnames(sbj, 9, 'sbjId')
  setnames(sbj, 10, 'random')
  
  sbj[, sbjId:=as.factor(sbjId)]
  sbj[, duration:=as.integer(duration)]
  setkey(sbj, sbjId)
  
  
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
  if(!is.null(trial$direction))
    trial[, direction:=as.factor(direction)]
  
  setkey(trial, sbjId, run, order)
  
  save(trial, sbj, 
       file = paste(outputPathToFolder, paste0(dataName, '.RData'), sep = ''))
  
}

OrientDiff = function(a, b){
  diff = a-b
  diff = diff %% 180
  inds = which(diff>90)
  diff[inds] = diff[inds] - 180
  return(diff)
}

GetOrientation = function(angle){
  orientation = angle %% 180
  inds = which(orientation>90)
  orientation[inds] = orientation[inds] - 180
  return(orientation)
}