# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: AssesVarsBS
# ------------------------------------------------------------------------------
# Description: Compares the effect of covariates by calculating the Brier score
# for several models (excluding one variable at a time), against the full model
# Model used for assessment - conditional inference forest
# ------------------------------------------------------------------------------
# Usage: 
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: A plot of the Brier Score for models with a different number of 
# covariates
# ------------------------------------------------------------------------------
# Keywords: Survival models, Covariates analysis, Brier Score, Conditional 
# inference forest.
# ------------------------------------------------------------------------------ 
# See also: 
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------

rm(list=ls())
library("data.table")
library("survival")
library("rpart")
library("partykit")
library("pec")
library("randomForestSRC")
library("party")

#Set working directory
wd<- "C:/Users/Nk/Documents/Uni/Qs/Qs_for_upload/AssessVarsBS/"
setwd(wd)

#Load dataset:
load("Metainfo.RData")
set.seed(22)

# Split in test and training ----------------------------------------------
train<- dat[,i:=.I,][sample(i, 19000)]
setkey(dat, "i")

valid<- dat[!train,]

# Select covariates -------------------------------------------------------

#Step 1 (full model)
fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age

#Step 2 (exclude 1 covariate)
fitform11<- Surv(newTOM,status)~ MS + DOP + Quantile
fitform12<- Surv(newTOM,status)~ MS + DOP + age
fitform13<- Surv(newTOM,status)~ MS + age + Quantile
fitform14<- Surv(newTOM,status)~ age + DOP + Quantile

# Asses after step 2 ------------------------------------------------------

fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=100, mtry=2))

fitcforest11 <- pecCforest(fitform11, data=train, controls=cforest_classical(ntree=100, mtry=2))
fitcforest12 <- pecCforest(fitform12, data=train, controls=cforest_classical(ntree=100, mtry=2))
fitcforest13 <- pecCforest(fitform13, data=train, controls=cforest_classical(ntree=100, mtry=2))
fitcforest14 <- pecCforest(fitform14, data=train, controls=cforest_classical(ntree=100, mtry=2))

# Calculate Brier Score for the first 30 days
fitpec <- pec(list("fitfull"=fitcforest,
                   "fit w/o age"=fitcforest11,
                   "fit w/o Quantile"=fitcforest12,
                   "fit w/o DOP"=fitcforest13, 
                   "fit w/o MS"=fitcforest14), 
              formula=Surv(newTOM, status)~MS+DOP+Quantile+age, 
              data=valid, 
              times=c(1:30)
              )
 
#Plot with smooth lines, move legend to the side

pdf("AssesVarsBS.pdf")
plot(fitpec, smooth=T, legend.x=35)
dev.off()







