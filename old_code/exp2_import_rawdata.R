library(data.table)

#set parameters----------
dataPath = 'data/'
dataName = "20170205"
from = as.POSIXct("2016-10-26 00:00:00")
to = as.POSIXct("2016-11-30 00:00:00")
outputName = "random_location"

ImportRawdata = function(dataPath, dataName, from, to, outputName){
  #import raw data----------------
  fileName = paste0(dataName, ".csv")
  rawData = fread(paste0(dataPath, fileName))
  #delete one row
  rawData = rawData[2:nrow(rawData),]
  #replace string
  colNames = unlist(rawData[1,])
  colNames = gsub("\\{\"\"ImportId\"\":\"\"", "", colNames)
  colNames = gsub("\"\"\\}", "", colNames)
  #set column names
  setnames(rawData, 1:ncol(rawData), colNames)
  #delete one row
  rawData = rawData[2:nrow(rawData),]
  
  rawData[, startDate:=as.POSIXct(startDate)]
  rawData[, endDate:=as.POSIXct(endDate)]
  
  #leave data within the date range
  if(!is.null(from))
    rawData = rawData[startDate>from,]
  if(!is.null(to))
    rawData = rawData[startDate<to,]
  
  
  #sbj table----------
  sbj = rawData[, c(1:9, 624), with = FALSE]
  setnames(sbj, 9, 'sbjId')
  setnames(sbj, 10, 'random')
  
  sbj[, sbjId:=as.factor(sbjId)]
  sbj[, progress:=as.integer(progress)]
  sbj[, duration:=as.integer(duration)]
  
  setkey(sbj, sbjId)
  
  
  #trial table---------
  trial = rawData[, c(9, 20:620), with = FALSE]
  setnames(trial, 1, 'sbjId')
  trial = melt(trial, 'sbjId', variable.name = 'qId')
  trial[, order:=strtoi(substr(qId, 1, regexpr('_', qId)-1))]
  trial[, value:=as.numeric(value)]
  #get variable names
  qIdTable = data.table(qId = c('QID18', 'QID8', 'QID7', 'QID3', 'QID4', 'QID5', 'QID25', 
                                'QID19', 'QID11', 'QID12', 'QID13', 'QID14', 'QID15', 'QID29'),
                        variable = rep(c('miss', 'countdown', 'direction', 'stimulus', 'response', 'stimulusOverTime', 'responseGivenTime'),2),
                        run = c(1,1,1,1,1,1,1,2,2,2,2,2,2,2))
  for(i in 1:nrow(qIdTable)){
    trial[grep(qIdTable$qId[i], qId), 
          c('variable', 'run'):=.(qIdTable$variable[i], qIdTable$run[i])]
  }
  trial = dcast(trial[!is.na(variable),], sbjId+run+order~variable, value.var = 'value')
  
  trial[, sbjId:=as.factor(sbjId)]
  trial[, run:=as.factor(run)]
  trial[, direction:=as.factor(direction)]
  
  setkey(trial, sbjId, run, order)
  
  save(trial, sbj, 
       file = paste0(dataPath, paste0(outputName, '.RData')))
}


#import data-------------
ImportRawdata(dataPath, dataName, from, to, outputName)


