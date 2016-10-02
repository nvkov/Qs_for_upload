# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: CalcBSandCI
# ------------------------------------------------------------------------------
# Description: Calculates the Brier Score and C-Index for 5 different survival
# models on market data for used cars.
# Survival models used are:
# (1) Cox proportional hazards (CoxPH)
# (2) Regression tree (rpart)
# (3) Conditional Inference tree (ctree)
# (4) Random Survival Forest (rsf)
# (5) Conditional inference forest (cforest)
# Model training and predictions are made on the basis of 5.000 observations for 
# used cars. A validation sample of 100 cars, 4900 are left for training. 
# Process is repeated 50 times with 50 disjoint validation samples.
# ------------------------------------------------------------------------------
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: BSlist.RData - a list with estimated Bries scores for all 5 models with 
# the 50 crossvalidation samples
# CIlist.RData - same for the C-Index
# ------------------------------------------------------------------------------
# Keywords: Cox proportional hazards, Regression trees, Conditional inference 
# trees, Ransom survival Forest, Conditional inference forest, Brier Score, C-Index
# Cendored data, survivala models, crossvalidation.
# ------------------------------------------------------------------------------ 
# See also: PlotBSandCI
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------
# Helper function: is.NullOb <- erases NULL entries in lists (e.g. when i index
# in loop bellow is not consecutive)
# add_legend <- adds a slim legend to the plot
# ------------------------------------------------------------------------------
# Helper functions: -------------------------------------------------------

is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects 
  rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
  
  }

# Helper functions --------------------------------------------------------
  
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  } 
  
#  ------------------------------------------------------------------------



#Remove all objects:

rm(list=ls())


library("data.table")
library("survival")
library("stargazer")
library("rpart")
library("partykit")
library("caret")
library("pec")
library("survAUC")
library("randomForestSRC")
library("party")


#Set working directory
setwd("C:/Users/Nk/Documents/Uni/Qs/CalcBSandCI/")

#Load dataset:
load("Metainfo.RData")

#Pick 5000 obs for calculation
dat<- dat[,i:=.I][sample(i, 5000)]


# Create partition for bootstrap ------------------------------------------
d<- c(1:nrow(dat))
data.nrows<- as.list(split(d, ceiling(seq_along(d)/100))) 


# Create empty objects for results ----------------------------------------
BSrpart<- NULL
BSReference<- NULL
BSCox<- NULL
BScforest<- NULL
BSrsf<- NULL
BSctree<- NULL

CIrpart<- NULL
CICox<- NULL
CIcforest<- NULL
CIrsf<- NULL
CIctree<- NULL


#  ------------------------------------------------------------------------

print("Warning: computationally expensive. Runs ca. 20 mins")

for(i in c(1:length(data.nrows))){
  
  # Remember rows for partition in run i:  
  slice<- unlist(data.nrows[i])
  
  # Split data in training and validation:
  print(paste0("Model ", i, " out of ", length(data.nrows)))
  train<- dat[!slice]
  valid<- dat[slice]

  # Define survival object with covars:  
  fitform <- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor 
  
  # Fit models to data:
  fitcox <- selectCox(fitform, data=train, rule="aic")
  fitrpart<- pecRpart(fitform, data=train)

  set.seed(13)
  fitrsf <- rfsrc(fitform,data=train,forest=TRUE,ntree=100, mtry=2)
  fitctree<- pecCtree(fitform, data=train)
  fitcforest <- pecCforest(fitform, data=train, controls=cforest_classical(ntree=100, mtry=2))

  extends <- function(...)TRUE
  set.seed(2006)

  print(paste0("Predict and calculate performance measures for run ", i, " out of ", length(data.nrows)))

  # Brier Score:
  print("Brier Score")
 
  # Calculate Brier Score for all models:
  BStemp <- pec(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
               formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, times=c(1:120))
 
  # Save results:
  BSrpart[[i]]<- BStemp$AppErr$rpart[1:85]
  BSrsf[[i]]<- BStemp$AppErr$rsf[1:85]
  BScforest[[i]]<- BStemp$AppErr$cforest[1:85]
  BSCox[[i]]<- BStemp$AppErr$Cox[1:85]
  BSReference[[i]]<- BStemp$AppErr$Reference[1:85]
  BSctree[[i]]<- BStemp$AppErr$ctree[1:85]
 
  
  # Concordance index:
  print("C-Index")
  
  # Calculate C-Index for all models:
  CItemp<- cindex(list("Cox"=fitcox,"rsf"=fitrsf,"cforest"=fitcforest, "rpart"=fitrpart, "ctree"=fitctree), 
             formula=Surv(newTOM,status)~MS +DOP + Quantile + age, data=valid, eval.times=c(1:120))
 
  # Save results:
  CIrpart[[i]]<- CItemp$AppCindex$rpart[1:85]
  CIrsf[[i]]<- CItemp$AppCindex$rsf[1:85]
  CIcforest[[i]]<- CItemp$AppCindex$cforest[1:85]
  CICox[[i]]<- CItemp$AppCindex$Cox[1:85]
  CIctree[[i]]<- CItemp$AppCindex$ctree[1:85]
}

# Save calculated objects -------------------------------------------------
# Rename columns for different models
BScforest<- as.data.frame(rmNullObs(BScforest))
colnames(BScforest)<- c(1:50)

BSctree<- as.data.frame(rmNullObs(BSctree))
colnames(BSctree)<- c(1:50)

BSrsf<- as.data.frame(rmNullObs(BSrsf))
colnames(BSrsf)<- c(1:50)

BSrpart<- as.data.frame(rmNullObs(BSrpart))
colnames(BSrpart)<- c(1:50)

BSCox<- as.data.frame(rmNullObs(BSCox))
colnames(BSCox)<- c(1:50)


#  ------------------------------------------------------------------------
# Pack as list for faster safe:
BSlist<- list(BSrpart=BSrpart, BSrsf=BSrsf, 
              BSctree=BSctree, BSCox=BSCox, 
              BScforest=BScforest)
save(BSlist, file="BSlist.RData")


#  ------------------------------------------------------------------------
# Example: Calculate and plot mean and sd values for Brier score: ---------

meanRpart<- apply(BSrpart, 1, mean)
sdRpart<- apply(BSrpart, 1, sd)

meanRsf<- apply(BSrsf, 1, mean)
sdRsf<- apply(BSrsf, 1, sd)

meanCtree<- apply(BSctree, 1, mean)
sdCtree<- apply(BSctree, 1, sd)

meanCox<- apply(BSCox, 1, mean)
sdCox<- apply(BSCox, 1, sd)

meanCforest<- apply(BSCforest, 1, mean)
sdCforest<- apply(BSCforest, 1, sd)


#  ------------------------------------------------------------------------
# Plot means (SDs are commented out - interpretable only for larger samples


plot(meanRsf, ylim=c(0,0.3), type="l")
#lines(I(meanRsf+1.98*sdRsf), col="black", lty=2)
#lines(I(meanRsf-1.98*sdRsf), col="black", lty=2)

lines(meanRpart, col="red")
#lines(I(meanRpart+1.98*sdRpart), col="red", lty=2)
#lines(I(meanRpart-1.98*sdRpart), col="red", lty=2)

lines(meanCtree, col="blue")
#lines(I(meanCtree+1.98*sdCtree), col="blue", lty=2)
#lines(I(meanCtree-1.98*sdCtree), col="blue", lty=2)

lines(meanCox, col="pink")
#lines(I(meanCox+1.98*sdCox), col="pink", lty=2)
#lines(I(meanCox-1.98*sdCox), col="pink", lty=2)

lines(meanCforest, col="green")
#lines(I(meanCforest+1.98*sdCforest), col="green", lty=2)
#lines(I(meanCforest-1.98*sdCforest), col="green", lty=2)
add_legend("topright", legend=c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), pch=19, 
           col=c("black", "green", "pink", "red", "blue"),
           horiz=TRUE, bty='n', cex=0.9)

