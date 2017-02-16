AngularDiff = function(a, b){
  diff = a-b
  diff = diff %% 360
  inds = which(diff>180)
  diff[inds] = diff[inds] - 360
  return(diff)
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

MovingAverage = function(x, y, window, step, circle = FALSE){
  dataTable = data.table(x, y)
  setkey(dataTable, x)
  xmin = min(x, na.rm = TRUE)
  xmax = max(x, na.rm = TRUE)
  xRange = xmax - xmin
  if(circle){
    headData = dataTable[x<=xmin+0.5*window,]
    tailData = dataTable[x>=xmax-0.5*window,]
    headData[, x:=x + xRange]
    headData[, y:=y + 180]
    tailData[, x:=x - xRange]
    tailData[, y:=y - 180]
    dataTable = rbind(dataTable, headData, tailData)
    return(MovingAverage(dataTable$x, dataTable$y, window, step, FALSE))
  }
  else if(circle==FALSE){
    n = floor((xRange - window)/step) + 1
    ma = numeric(n)
    for(i in 1:n){
      start = xmin+(i-1)*step
      ma[i] = dataTable[x>=start & x<=start+window, mean(y, na.rm = TRUE)]
    }
    maTable = data.table(x = seq(xmin+0.5*window, xmax-0.5*window, step),
                         ma)
    return(maTable)
  }
  else{
    print('circle should be a logical value')
  }
}


kbToRa = function(k, b){
  root = acos((sqrt(4*k^2+1)-1)/(2*k))
  amplitude = b*exp(k*cos(root))*k*sin(root)
  return(list(root = root, amplitude = amplitude))
}

raToKb = function(root, amplitude){
  k = -cos(root)/(cos(root)^2-1)
  b = amplitude/exp(k*cos(root))/k/sin(root)
  return(list(k=k, b=b))
}

dvm = function(x, k, b){
  return(-b*k*exp(k*cos(x))*sin(x))
}

dvmLoss = function(param, x, y){
  k = param[1]
  b = param[2]
  yHat = dvm(x, k, b)
  loss = sum((yHat - y)^2, na.rm = TRUE)
  return(loss)
}


OrientBias = function(orient, param, stretch){
  k0 = param[1]
  b0 = param[2]
  k90 = param[3]
  b90 = param[4]
  biasHat0 = dvm(stretch*orient, k0, b0)
  biasHat90 = dvm(stretch*(orient-90), k90, b90)
  biasHat = biasHat0 + biasHat90
  return(biasHat)
}

OrientCorrectLoss = function(param, orient, bias, stretch, norm="L2"){
  biasHat = OrientBias(orient, param, stretch)
  if(norm=="L2"){
    loss = sum((biasHat - bias)^2, na.rm = TRUE)
  } else if(norm=="L1"){
    loss = sum(abs(biasHat - bias), na.rm = TRUE)
  } else {
    print("Unknown norm type")
  }
  return(loss)
}

#subject analysis-------------
CalCor = function(perm=NULL, data){
  if(!is.null(perm)){
    permResponse = data$response[perm]
  }
  else{
    permResponse = data$response
  }
  permError = OrientDiff(permResponse, data$stimulus)
  permResponse = data$stimulus + permError
  correlation = cor(data$stimulus, 
                    permResponse, 
                    use='na.or.complete')
  return(correlation)
}

CalMeanError = function(perm=NULL, data){
  if(!is.null(perm)){
    permResponse = data$response[perm]
  }
  else{
    permResponse = data$response
  }
  permError = OrientDiff(permResponse, data$stimulus)
  return(mean(abs(permError), na.rm = TRUE))
}

CalStickyFBDiff = function(perm=NULL, data){
  if(!is.null(perm)){
    permForward = data$forward[perm]
  }
  else{
    permForward = data$forward
  }
  permDiff = mean(data$sticky[permForward]) - mean(data$sticky[!permForward])
  return(permDiff)
}

#permutation test-----------
CalP = function(testStat, simus, tail = 'two'){
  meanSimu = mean(simus)
  if(tail=='two'){
    p = (sum(abs(simus-meanSimu)>=abs(testStat-meanSimu))+1)/(nPerm+1)
  } 
  else if(tail=='one'){
    if(testStat-meanSimu>0){
      p = (sum(simus-meanSimu>=testStat-meanSimu)+1)/(nPerm+1)
    } else{
      p = (sum(simus-meanSimu<=testStat-meanSimu)+1)/(nPerm+1)
    }
  }
  else if(tail=='upper'){
    p = (sum(simus-meanSimu>=testStat-meanSimu)+1)/(nPerm+1)
  }
  else if(tail=='lower'){
    p = (sum(simus-meanSimu<=testStat-meanSimu)+1)/(nPerm+1)
  }
  return(p)
}


PermutationTest = function(data, nPerm, Cal, 
                           strata = NULL, tail = 'two',
                           returnSimus = FALSE){
  testStat = Cal(data = data)
  #nData = length(x)
  simus = rep(0, nPerm)
  for(i in 1:nPerm){   
    if(!is.null(strata)){
      #determine the permutation
      perm = data[, row[sample.int(.N)], by = strata]$V1
      #calculate the statistic
      simus[i] = Cal(perm, data)
    } else{
      #determine the permutation
      perm = data[, row[sample.int(.N)]]
      #calculate the statistic
      simus[i] = Cal(perm, data)
    }
  }
  p = CalP(testStat, simus, tail)
  if(returnSimus)
    result = list(testStat=testStat, p=p, simus=simus)
  else
    result = list(testStat=testStat, p=p)
  return(result)
}


#parameter analysis----------
SdAnalysis = function(trial, errorThresh, intervalThresh){
  res = list(regularSd = Inf, intercept = Inf,
             direction1 = Inf, direction1ForwardTrue = Inf, 
             regularSdP = Inf, interceptP = Inf,
             direction1P = Inf, direction1ForwardTrueP = Inf)
  trial[, qualified:=FALSE]
  trial[abs(error)<=errorThresh & interval <= intervalThresh,
        qualified:=TRUE]
  model = lm(error~direction, 
             trial[surprise==FALSE&start==FALSE&qualified==TRUE,])
  res$regularSd = model$coefficients[2]
  res$regularSdP = summary(model)$coefficients[2,4]
  
  model = lm(error2~direction*forward, 
             trial[surprise==TRUE&qualified==TRUE,])
  res$intercept = model$coefficients[1]
  res$direction1 = model$coefficients[2]
  res$direction1ForwardTrue = model$coefficients[4]
  res$interceptP = summary(model)$coefficients[1,4]
  res$direction1P = summary(model)$coefficients[2,4]
  res$direction1ForwardTrueP = summary(model)$coefficients[4,4]
  return(res)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

FitBias = function(trial){
  #fit dvm for the whole dataset-------------------------
  domain = 180
  stretch = 2*pi/domain
  kb0Initial = raToKb(15*stretch, -4)
  kb90Initial = raToKb(15*stretch, -4)
  param0 = c(unlist(kb0Initial), unlist(kb90Initial))
  res = optim(param0, OrientCorrectLoss, 
              orient = GetOrientation(trial$stimulus), 
              bias = trial$error,
              stretch = stretch,
              norm = "L2")
  raFit0 = kbToRa(unname(res$par[1]), unname(res$par[2]))
  raFit90 = kbToRa(unname(res$par[3]), unname(res$par[4]))
  (root0 = raFit0$root/stretch)
  (amplitude0 = -raFit0$amplitude)
  (root90 = raFit90$root/stretch)
  (amplitude90 = -raFit90$amplitude)
  
  #fit for each subject----------
  resAll = res
  param0 = resAll$par
  sbjBiasT = trial[, as.list(optim(param0, OrientCorrectLoss, 
                                   orient = GetOrientation(stimulus), 
                                   bias = error,
                                   stretch = stretch,
                                   norm = "L2")$par), 
                   keyby = .(sbjId)]
  cols = c('k0', 'b0', 'k90', 'b90')
  setnames(sbjBiasT, 2:5, cols)
  return(sbjBiasT)
}

FetchBias = function(trial, sbjBiasT){
  domain = 180
  stretch = 2*pi/domain
  cols = c('k0', 'b0', 'k90', 'b90')
  trial[, sbjIdTemp:=sbjId] #for disambiguity
  trial[, bias:=OrientBias(GetOrientation(stimulus), 
                           unlist(sbjBiasT[.(sbjIdTemp[1]), cols, 
                                           with=FALSE]), 
                           stretch), 
        by = .(sbjId)]
}