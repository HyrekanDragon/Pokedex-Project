
rm(list=ls()); gc()
setwd("C:/Users/Hyrek/OneDrive/Desktop/Pokedex Project")
dat =read.table("cleaned_pokedex.csv",sep=",",header=TRUE)

# I dont want ID and zip code in the data; we cant analyze them
dat = subset(dat, select = -c(X))

## If predictors are very different in scales,
## standardize them to have comparable scales.
## The code to do so is as follows:
dat[,!colnames(dat) %in% c("Viability")] = scale(dat[,!colnames(dat) %in% c("Viability")])

## simulate a data set
set.seed(8793)
error = rnorm( nrow(dat) )
# This is for student.por
#dat$Viability = 14.06420 - dat$failures*1.56743 - dat$higher_no*2.25467 - dat$school_MS*1.07593 - dat$Dalc*0.31749 - dat$health*0.22021 + dat$studytime_StudyGT5Hrs*.81581 - dat$schoolsup_yes*1.08314 - dat$reason_other*.97016 + dat$Medu_College*.73840 - dat$sex_M*.47239 - dat$address_R*.54745 + dat$reason_reputation*.57637 + dat$guardian_father*.43476 + error

# detach("package:class", unload=TRUE)
##### functions needed for knn prediction #######
library(dplyr)
library(FNN)
one.pred = function(xnew, xtrain, ytrain, k, algorithm) {
  ind = knnx.index(xtrain, matrix(xnew, 1), k=k, algorithm=algorithm)
  mean(ytrain[ind])
}

knn.predict = function(Xtrain, ytrain, Xtest, k=5, algorithm = 'kd_tree') {
  ypred = apply(Xtest, 1, one.pred, xtrain = Xtrain, ytrain = ytrain, k=k, algorithm=algorithm)
  return(ypred)
}

knn.predict.bestK = function(Xtrain, ytrain, Xtest, ytest, k.grid = 1:20, algorithm='kd_tree') {
  fun.tmp = function(x) {
    yhat = knn.predict(Xtrain, ytrain, Xtest, k = x, algorithm=algorithm) # run knn for each k in k.grid
    #rmse = (yhat - ytest)^2 %>% mean() %>% sqrt()
    #return(rmse)
    ypred.class = dichotomize(yhat, .5)
    err = mean(ypred.class != ytest) # misclassification error rate
    return(err)
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error),
             error.all = error)
  return(out)
}

knn.bestK = function(train, test, y.train, y.test, k.grid = 1:20, ct = .5) {
  # browser()
  fun.tmp = function(x) {
    y.tmp = knn(train, test, y.train, k = x, prob=T) # run knn for each k in k.grid
    prob = get.prob(y.tmp)
    y.hat = as.numeric( prob > ct ) + 1
    return( sum(y.hat != as.numeric(y.test)) )
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  names(error) = paste0('k=', k.grid)
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], 
             error.min = min(error)/length(y.test),
             error.all = error/length(y.test))
  return(out)
}

dichotomize = function(yhat, cutoff=.5) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}

sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}
#################################################

set.seed(8793)
n.train = floor( nrow(dat)*0.6 )
ind.train = sample(1:nrow(dat), n.train)
ind.test = setdiff(1:nrow(dat), ind.train)

Xtrain = dat[ind.train,!colnames(dat) %in% c("Viability")]
Xtest = dat[ind.test,!colnames(dat) %in% c("Viability")]
ytrain = dat[ind.train,c('Viability')]
ytest = dat[ind.test,c('Viability')]

ypred = knn.predict(Xtrain, ytrain, Xtest, k = 3)
plot(ytest, ypred)
abline(0, 1, col='red')

hist(ypred)

ypred.class = dichotomize(ypred, .5)
err = mean(ypred.class != ytest) # misclassification error rate
err

table(ypred.class, ytest)

sen(ytest, ypred.class)
spe(ytest, ypred.class)

obj1 = knn.predict.bestK(Xtrain, ytrain, Xtest, ytest, k.grid = 1:53) ## best k = 9
obj1

## rerun with the best k
ypred1 = knn.predict(Xtrain, ytrain, Xtest, k = obj1$k.optimal)
plot(ytest, ypred1)
abline(0, 1, col='red')

hist(ypred1)

ypred1.class = dichotomize(ypred1, .5)
err = mean(ypred1.class != ytest) # misclassification error rate
err

table(ypred1.class, ytest)

sen(ytest, ypred1.class)
spe(ytest, ypred1.class)




