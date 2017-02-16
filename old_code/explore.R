library(ggplot2)
library(data.table)
if(!exists("AngularDiff", mode="function")) source("functions.R")

dataPath = 'data/'
dataName = "20161206_full"
#prepare data-------------
load(paste0(dataPath, dataName, '.RData'))
trial[, error:=OrientDiff(response, stimulus)]
trial[, response:=stimulus+error]
trial[, interval:=c(NA, (stimulusOverTime[-1] - stimulusOverTime[-.N])/1000), 
      by = .(sbjId, run)]
trial[, responseInterval:=(responseGivenTime - stimulusOverTime)/1000, 
      by = .(sbjId, run)]


#error distribution-----------
ggplot(trial, aes(x=error)) +
  geom_histogram() +
  facet_wrap(~sbjId)
ggplot(trial[abs(error)<50], aes(x=error)) +
  geom_histogram() +
  facet_wrap(~sbjId)


#interval distribution-------
ggplot(trial, aes(x=interval)) +
  geom_histogram() +
  facet_wrap(~sbjId, scales="free_x")
ggplot(trial[interval<20], aes(x=interval)) +
  geom_histogram() +
  facet_wrap(~sbjId, scales="free_x")

ggplot(trial, aes(x=responseInterval)) +
  geom_histogram() +
  facet_wrap(~sbjId, scales="free_x")
ggplot(trial[responseInterval<10], aes(x=responseInterval)) +
  geom_histogram() +
  facet_wrap(~sbjId, scales="free_x")

#check error with response interval
ggplot(trial, aes(responseInterval, abs(error))) +
  geom_point() +
  geom_smooth()
ggplot(trial[responseInterval<10,], aes(responseInterval, abs(error))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sbjId, scales = "free_x")


#check response distribution with stimulus distribution------------
visualT = melt(trial, measure.vars=c('stimulus', 'response'))
ggplot(visualT, aes(x=value%%360, fill=variable)) + 
  geom_histogram(center=0, binwidth=15, alpha=0.5, position='identity') +
  scale_x_continuous(breaks=seq(0,360,by=45), limits=c(0, 360)) +
  coord_polar() +
  facet_wrap(~sbjId)


#check the stimulus sequnce---------------
nTrialPerRun = max(trial$order)
visualT = copy(trial)
visualT[, stimulus:=stimulus%%360]

#deal with periodicity
visualT[, diff:=c(NA, stimulus[-1]-stimulus[-.N]), by=.(sbjId, run)]
visualT[, breakPoint:=diff>180|diff< -180]
visualT[is.na(breakPoint), breakPoint:=FALSE]
visualT[, group:=cumsum(breakPoint), by=.(sbjId,run)]

endPoint = visualT[breakPoint==TRUE,]
endPoint[, group:=group-1]
endPoint[diff>180, stimulus:=0]
endPoint[diff< -180, stimulus:=360]

startPoint = visualT[breakPoint==TRUE,]
startPoint[diff>180, stimulus:=360]
startPoint[diff< -180, stimulus:=0]

visualT = rbind(startPoint, visualT, endPoint)

#plot for all subjects
ggplot(visualT, aes(x=order, y=stimulus, group=group)) +
  geom_step() +
  coord_polar(theta='y') +
  scale_y_continuous(breaks=seq(0,360,45), limits=c(0,360)) +
  xlim(-0.3*nTrialPerRun, nTrialPerRun) +
  facet_grid(run~sbjId)
  
#plot for one subject one run
ggplot(visualT[sbjId=='R_1OuDffpB9nDKeMM'&run=='1'], 
       aes(x=order, y=stimulus, group=group)) +
  geom_step() +
  coord_polar(theta='y') +
  scale_y_continuous(breaks=seq(0,360,45), limits=c(0,360)) +
  xlim(-0.3*nTrialPerRun, nTrialPerRun)



#check stimulus and response------------
#prepare visualization data
visualT = copy(trial)
visualT[, start:=pmin(stimulus, response)]
visualT[, end:=pmax(stimulus, response)]
visualT[, start:=start%%360]
visualT[, end:=end%%360]
arcT = copy(visualT)
arcT[, group:=order]
temp = arcT[start>end, ]
nTrialPerRun = max(trial$order)
temp[, group:=group+nTrialPerRun]
temp[, start:=0]
arcT[start>end, end:=360]
arcT = rbind(arcT, temp)
arcT = melt(arcT, measure.vars=c('start','end'))

pointT = melt(visualT, measure.vars=c('stimulus','response'))
pointT[, value:=value%%360]
#point plot + arc plot
ggplot(mapping=aes(x=value, y=order)) +
  geom_point(data=pointT, aes(shape=variable), alpha=0.4)+
  geom_line(data=arcT, aes(group=group, color=error>0), size=1)+
  coord_polar() +
  scale_x_continuous(breaks = seq(0,360,by=45), limits=c(0,360)) +
  ylim(-0.3*nTrialPerRun, nTrialPerRun) +
  facet_grid(direction~sbjId)

#plot for only one subject
ggplot(mapping=aes(x=value, y=order)) +
  geom_point(data=pointT[sbjId=='5'&run=='2'], 
             aes(shape=variable), alpha=0.4)+
  geom_line(data=arcT[sbjId=='5'&run=='2'], 
            aes(group=group, color=error>0), size=1)+
  coord_polar() +
  scale_x_continuous(breaks = seq(0,360,by=45), limits=c(0,360)) +
  ylim(-0.3*nTrialPerRun, nTrialPerRun)


#error plot------------
ggplot(trial, aes(x = stimulus, y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  xlim(-180, 180)
#errors seem to be on average negative

ggplot(trial, aes(x = stimulus, y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  xlim(-180, 180) +
  facet_wrap(~sbjId)

#check the sign of the error
library(nlme)
model = lme(error~1, data=trial, random=~1|sbjId)
summary(model)

model = lme(error~run, data=trial, random=~1|sbjId)
summary(model)

model = lme(error~direction, data=trial, random=~1|sbjId)
summary(model)

#error~time-------------------
visualT = copy(trial)
visualT[, order:=(as.integer(run)-1)*nTrialPerRun+order]
ggplot(visualT, aes(x=order, y=abs(error))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~sbjId) 


#fit bias--------------
trial[, error:=error - mean(error, na.rm=TRUE), by=.(sbjId)]

domain = 180
stretch = 2*pi/domain
sbjBiasT = FitBias(trial)

visualSbj = unique(trial$sbjId)
orient = -90:90
tempT = CJ(sbjId=visualSbj, orient=orient)
cols = c('k0', 'b0', 'k90', 'b90')
tempT[, sbjIdTemp:=sbjId] #for disambiguity
tempT[, fitBias:=OrientBias(orient,
                            unlist(sbjBiasT[.(sbjIdTemp[1]), cols, with=FALSE]), 
                            stretch), by = .(sbjId)]

ggplot(trial, aes(x = GetOrientation(stimulus), y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  geom_line(data=tempT, aes(x=orient, y=fitBias), color='red') +
  scale_x_continuous(breaks=seq(-90,90,45)) +
  facet_wrap(~sbjId)


#corrected errors-------------------------
FetchBias(trial, sbjBiasT)
trial[, cError:=error - bias]


ggplot(trial, aes(x = stimulus, y = cError)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  xlim(-180, 180) +
  facet_wrap(~sbjId)

ggplot(trial, aes(x = stimulus, y = abs(cError))) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.2, 
              method.args = list(degree = 0)) +
  facet_wrap(~sbjId)

ggplot(trial, aes(x = stimulus, y = abs(cError))) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  coord_polar(start = -pi) +
  scale_x_continuous(breaks = seq(-180,180,45)) +
  facet_wrap(~sbjId)





########################################



#dvm fitting-------------------------
domain = 180
stretch = 2*pi/domain
kb0Initial = raToKb(20*stretch, -4)
kb90Initial = raToKb(20*stretch, -4)
param0 = c(unlist(kb0Initial), unlist(kb90Initial))
res = optim(param0, OrientCorrectLoss, 
            orient = GetOrientation(trial$stimulus), 
            bias = trial$error,
            stretch = stretch,
            norm = "L1")
raFit0 = kbToRa(unname(res$par[1]), unname(res$par[2]))
raFit90 = kbToRa(unname(res$par[3]), unname(res$par[4]))
(root0 = raFit0$root/stretch)
(amplitude0 = -raFit0$amplitude)
(root90 = raFit90$root/stretch)
(amplitude90 = -raFit90$amplitude)


x = -90:90
y = OrientBias(x, res$par, stretch)
tempT = data.table(x, y)
ggplot(trial, aes(x = GetOrientation(stimulus), y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.1, 
              method.args = list(degree = 0)) +
  geom_line(data=tempT, aes(x=x, y=y), color='red') 


trial[, cError:=error - OrientBias(GetOrientation(stimulus), 
                                   res$par, stretch)]
trial[, cError:=error - OrientBias(GetOrientation(stimulus), 
                                       res$par, stretch)]
ggplot(trial, aes(x = GetOrientation(stimulus), y = cError)) +
  geom_point(alpha = 0.2) + 
  geom_smooth()


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
sbj[, (cols):=sbjBiasT[.(sbj$sbjId), cols, with=FALSE]]

#visualization
visualSbj = unique(trial$sbjId)
orient = -90:90
tempT = CJ(sbjId=visualSbj, orient=orient)
cols = c('k0', 'b0', 'k90', 'b90')
tempT[, sbjIdTemp:=sbjId] #for disambiguity
tempT[, fitBias:=OrientBias(orient,
                            unlist(sbj[.(sbjIdTemp[1]), cols, with=FALSE]), 
                            stretch), by = .(sbjId)]

ggplot(trial, aes(x = GetOrientation(stimulus), y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method="loess", span=0.15, 
              method.args = list(degree = 0)) +
  geom_line(data=tempT, aes(x=orient, y=fitBias), color='red') +
  coord_equal() +
  facet_wrap(~sbjId)







ggplot(trial, aes(x=stimulus, y=error)) +
  geom_point() + geom_smooth() +
  coord_polar()

ggplot(trial, aes(x=stimulus%%360, y=error)) +
  geom_hline(yintercept = 0, color='red', size=2) +
  geom_point() +
  coord_polar() +
  scale_x_continuous(breaks = seq(0,360,by=45), limits=c(0,360))


#------------------------


trial[, error:=AngularDiff(response, stimulus)]
trial[, response:=stimulus+error]

ggplot(trial, aes(x = stimulus, y = response)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, 
              linetype = 2, color = 'blue', size = 0.8) +
  coord_equal()


ggplot(trial, aes(x = stimulus, y = error)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(color = 'black')


trial[, interval:=c(NA, (time[2:.N] - time[1:(.N-1)])/1000), 
      by = .(sbjId, run)]
hist(trial$interval)
