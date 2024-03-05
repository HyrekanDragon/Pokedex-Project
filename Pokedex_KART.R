rm(list = ls()); gc()
library(rpart); library(rpart.plot)

setwd("C:/Users/Hyrek/OneDrive/Desktop/Pokedex Project")
dat =read.table("cleaned_pokedex.csv",sep=",",header=TRUE)

# I dont want ID and zip code in the data; we cant analyze them
dat = subset(dat, select = -c(X, BST))

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

################################################
# Regression Tree Example
set.seed(8793)
K = 10 # number of cross-validations

fit = rpart(formula = Viability ~ ., 
            method="anova", data=dat, xval=K, control = rpart.control(minsplit = 100))

data

# Minimum Error Tree
pfit.me = prune(fit, cp = fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
rpart.plot(pfit.me, main = 'Min Error Tree')

# Best Pruned Tree
ind = which.min(fit$cptable[,"xerror"]) # xerror: cross-validation error
se1 = fit$cptable[ind,"xstd"]/sqrt(K) # 1 standard error
xer1 = min(fit$cptable[,"xerror"]) + se1 # targeted error: min + 1 SE
ind0 = which.min(abs(fit$cptable[1:ind,"xerror"] - xer1)) # select the tree giving closest xerror to xer1
pfit.bp = prune(fit, cp = fit$cptable[ind0,"CP"])
rpart.plot(pfit.bp, main = 'Best Pruned Tree')

## How to predict? I am taking best pruned tree as an example.
yhat = predict(pfit.bp, dat) # replace "dat" by validation data if you have it
hist(yhat)

yhat.class = dichotomize(yhat, .5)
err = mean(yhat.class != dat$Viability) # misclassification error rate
err

table(yhat.class, dat$Viability)

sen(dat$Viability, yhat.class)
spe(dat$Viability, yhat.class)

yhat2 = predict(pfit.me, dat) # replace "dat" by validation data if you have 
hist(yhat2)

yhat2.class = dichotomize(yhat2, .5)
err = mean(yhat2.class != dat$Viability) # misclassification error rate
err

table(yhat2.class, dat$Viability)

sen(dat$Viability, yhat2.class)
spe(dat$Viability, yhat2.class)