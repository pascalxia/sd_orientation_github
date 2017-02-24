# Calcluate column error. 
library(ggplot2)

trial = trialAll

trial[, error := OrientDiff(trial$response, trial$stimulus)]

hist(trial$error)
mean(trial$error, na.rm = TRUE)
sd(trial$error, na.rm = TRUE)/length(trial$error)^0.5

# x axis - 3 experiments. y axis - mean errors. 95% confidence interval bars too.

standard_errors = c(sd(trial[trial$exp == "exp2",]$error, na.rm = TRUE)/length(trial[trial$exp == "exp2",]$error)^0.5, 
                    sd(trial[trial$exp == "exp3",]$error, na.rm = TRUE)/length(trial[trial$exp == "exp3",]$error)^0.5, 
                    sd(trial[trial$exp == "exp4",]$error, na.rm = TRUE)/length(trial[trial$exp == "exp4",]$error)^0.5)

means = c(mean(trial[trial$exp == "exp2",]$error, na.rm = TRUE), 
          mean(trial[trial$exp == "exp3",]$error, na.rm = TRUE), 
          mean(trial[trial$exp == "exp4",]$error, na.rm = TRUE))

exp = c("exp2", "exp3", "exp4")

tableOfErrors = as.data.table(list(exp, means, standard_errors))
names(tableOfErrors) = c("exp", "means", "standard_errors")

tableOfErrors$exp = factpr(tableOfErrors$exp)

# Error bars represent standard error of the mean by experiment

ggplot(tableOfErrors, aes(x=exp, y=means)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=means-2*standard_errors, ymax=means+2*standard_errors),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))

summary = trial[!is.na(error), .(m = mean(error), se = sd(error)/sqrt(.N)), by = .(exp, run)]

# Error bars represent standard error of the mean by experiment and run

ggplot(summary, aes(x=exp, y=m, fill = run)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=m-2*se, ymax=m+2*se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


summaryOrientation = trial[!is.na(error), .(or = GetOrientation(stimulus), error, exp = factor(exp), run, id = sbjId)]

# Trends accross experiments

ggplot(summaryOrientation, aes(x = or, y = error, color = exp)) + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(.~run)

# Hist of stimulus orientation/angle

ggplot(summaryOrientation, aes(x = or)) + geom_histogram(colour = "black", fill = "white", binwidth = 5)

# Compare each subject to the average trend (facet_wrap(~subjectId))

ggplot(summaryOrientation, aes(x = or, y = error, colour = run)) + geom_point(alpha = 0.2) + geom_smooth() + facet_wrap(~id)

# Create Table of old and new Exp2

dataPath = '~/Desktop/URAP/sd_orientation_github/data/'

#prepare data of experiment 2 old-------------------
dataName = "exp2_20170208"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp2_old"]
trial[, exp:="exp2_old"]
sbj2 = sbj
trial2 = trial

#prepare data of experiment 2 new-------------------
dataName = "exp2_20170222"
load(paste0(dataPath, dataName, ".RData"))
sbj[, exp:="exp2_new"]
trial[, exp:="exp2_new"]
sbj2 = rbind(sbj2, sbj)
trial2 = rbind(trial2, trial)

sbj2[, exp:=factor(exp)]
trial2[, exp:=factor(exp)]

# 1. p-val for run bias for exp2old, exp2new, exp2old - exp2new

trial2[, error := OrientDiff(stimulus, response)]

summaryOrientation2 = trial2[!is.na(error), .(or = GetOrientation(stimulus), error, exp = factor(exp), run, id = sbjId, re = GetOrientation(response))]

ggplot(summaryOrientation2, aes(x = or, y = error, color = exp)) + geom_point(alpha = 0.2) + geom_smooth() + facet_grid(.~run)



linearModelOld = lm(or ~ re, data = summaryOrientation2[summaryOrientation2$exp == "exp2_old",])

linearModelNew = lm(or ~ re, data = summaryOrientation2[summaryOrientation2$exp == "exp2_new",])

modelOld = lm(error~run, data=summaryOrientation2[summaryOrientation2$exp == "exp2_old",])
summary(modelOld)

modelNew = lm(error~run, data=summaryOrientation2[summaryOrientation2$exp == "exp2_new",])
summary(modelNew)

summary2 = trial2[!is.na(error), .(m = mean(error), se = sd(error)/sqrt(.N)), by = .(exp, run)]

ggplot(summary2, aes(x=exp, y=m, fill = run)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=m-2*se, ymax=m+2*se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


summary3 = trial2[exp == "exp2_new", ][!is.na(error), .(m = mean(error), se = sd(error)/sqrt(.N)), by = .(sbjId, run)]


ggplot(summary3, aes(x=sbjId, y=m, fill = run)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=m-2*se, ymax=m+2*se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


ggplot(summaryOrientation2[exp == "exp2_new" & abs(error)<30,], aes(x = or, y = error, colour = run)) + geom_point(alpha = 0.2) + geom_smooth() + facet_wrap(~id)




