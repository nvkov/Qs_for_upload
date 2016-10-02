# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: plotKMdiffDegs
# ------------------------------------------------------------------------------
# Description: An automated function that bins continuous covariates along their
# quartiles and plots Kaplan-Meier Curves for simple analysis for the effect of 
# covariates on the time-on-market for used cars.
# ------------------------------------------------------------------------------
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: a plot of KM curves for one specific covariate. Data is grouped in
# quartiles along the covariate's disrtibution.
# ------------------------------------------------------------------------------
# Keywords: Survival analysis, Kaplan-Meier Curves, Quartiles, Non-parametric
# estimation
# ------------------------------------------------------------------------------ 
# See also: CoxPH
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------

rm(list=ls())
library("data.table")
library("survival")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/Qs/plotKMdiffDegs/"

#Set working directory
setwd(project_directory)

#Load dataset:
load("Metainfo.RData")

# Helper functions --------------------------------------------------------

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
  } 

#  ------------------------------------------------------------------------

# Coerce data to data frame -----------------------------------------------
dat<- as.data.frame(dat)

# Fuction for plotting ----------------------------------------------------

plotKM<- function(var,dat){
  par(mar = c(4, 4, 1.6, 0.2))
  
  #Select bounds
  bounds<- quantile(dat[,var])
  
  #Select form
  fitform0<- Surv(newTOM,status)~1
  
  #Calculate survival curves
  KM.Q1<- survfit(fitform0, data=dat[dat[,var]<=bounds[2],])
  KM.Q2<- survfit(fitform0, data=dat[dat[,var]>=bounds[2] & dat[,var]<=bounds[3],])
  KM.Q3<- survfit(fitform0, data=dat[dat[,var]<=bounds[4] & dat[,var]>=bounds[3],])
  KM.Q4<- survfit(fitform0, data=dat[dat[,var]<=bounds[5] & dat[,var]>=bounds[4],])
  
  #Plot and save graphs
  pdf(paste0("KMcurve_",var, ".pdf"))
  plot(KM.Q1, xlab="Survival time", ylab="Survival probability")
  lines(KM.Q2, col="red")
  lines(KM.Q3, col="blue")
  lines(KM.Q4, col="green")
  
  add_legend("topright", 
             legend=c(bquote(Q1<.(round(bounds[2], digits=2))), 
                                  bquote(Q2<.(round(bounds[3], digits=2))),
                                  bquote(Q3<.(round(bounds[4], digits=2))),
                                  bquote(Q4<.(round(bounds[5], digits=2)))), 
             pch=19, 
             col=c("black", "red", "blue", "green"),
             horiz=TRUE, 
             bty='n', 
             cex=0.9)
  dev.off()
  
}

# Example -----------------------------------------------------------------
var.list<- c("MS", "DOP", "size_vendor")
lapply(var.list, plotKM, dat)



