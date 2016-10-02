# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: CoxPH
# ------------------------------------------------------------------------------
# Description: Calculates a Cox Proportional hazards model for a dataset from 
# the German used car market. The analysis examines linearity and proportionality
# of hazards for 5 covariates on the time a used car spends on the market before 
# being sold. 
# Covariates: 
# (1) DOP (degree of overpricing) measures the ratio between the hedonic car price 
# and the asking price for the car online, 
# (2) MS (market size) measures the number of cars with similar characteristics, 
# simultaneously offered on the market
# (3) Quantile measures the quantile of the asking price for the car (estimated
# from the distribution of prices for cars with similar characteristics,
# simultaneously offered on the market)
# (4) age (in months)
# (5) Size of vendor (number of vehicles offered by the dealer)
# ------------------------------------------------------------------------------
# Usage: The plots are used to asses the linearity assumption (martingale) and 
# the proportionality assumption (Schoenfeld residuals) for the CoxPH model.
# Zero slope confirms respectively both assumptions.
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: A two-row plot with row 1) Martingale residuals vs. single covars and 
# row 2) Schoenfeld residuals against time
# ------------------------------------------------------------------------------
# Keywords: Cox proportional hazards, hazard ratio, linearity assumption, 
# proportionality assumption
# ------------------------------------------------------------------------------ 
# See also: CoxPH
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------

rm(list=ls())
library("data.table")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/Qs/CoxPHassmpChk/"

setwd(project_directory)

#Load dataset:
load("Metainfo.RData")

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor

# Calculate model ---------------------------------------------------------

coxfull<- coxph(fitform, data=dat)  

# Plot martingale residuals -----------------------------------------------

#Calculate Martingale residuals
res <- residuals(coxfull, type="martingale")

#create covariate matrix
X <- as.matrix(dat[, c("MS", "DOP", "Quantile", "age", "size_vendor"), with=F]) # matrix of covariates

#Save as pdf
pdf("CoxFullResiduals.pdf", width=12)

#  ------------------------------------------------------------------------


par(mfrow=c(2, 5), mar=c(5,1,1,1), oma=c(0,2,0,0))

#Plot Martingale residuals agains covariates
for (j in 1:5) { # residual plots
  plot(X[, j], res, xlab=c("MS", "DOP", "Quantile", "Age", "Size vendors")[j], 
       ylab="residuals", 
       ylim=c(-1, 1), 
       cex.lab=1.5, 
       type="n")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0), col="red", lwd=1.75)
}

#Plot Schoenfeld residuals against time
plot(cox.zph(coxfull,transform='log'), 
     col="red", 
     resid=F, 
     lwd=1.75)

#  ------------------------------------------------------------------------

dev.off() 
