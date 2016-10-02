# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: CoxPH
# ------------------------------------------------------------------------------
# Description: Calculates a Cox Proportional hazards model for a dataset from 
# the German used car market. The analysis presents the effect of 5 covariates on
# the time a used car spends on the market before being sold. 
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
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData (for calculation), 
# CoxTable.tex (basic document for generating the latex table) 
# ------------------------------------------------------------------------------
# Output: An automated Latex table with estimated hazard ratios, significance 
# level and summary statistics for CoxPH with the data (cox_table.tex).
# ------------------------------------------------------------------------------
# Keywords: Cox proportional hazards, Latex table, hazard ratio, used car market
# ------------------------------------------------------------------------------ 
# See also: SFEDeltahedgingLogic, SFEDeltahedgingdepend, SFEGBMProcess
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------

rm(list=ls())
library("data.table")
library("survival")
library("stargazer")

#Set working directory
project_directory<- "C:/Users/Nk/Documents/Uni/Qs"

setwd(project_directory)

#Load dataset:
load("Metainfo.RData")

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Select parameters -------------------------------------------------------
cox.Class<- c("A", "B", "C", "E",  "ML", "S", "SL", "SLK")
cox.Vars<- c("MS", "DOP", "Quantile", "age", "size_vendor")
# creating parameter grids
cox.grid <- expand.grid(Class=cox.Class)
grid   <- list(cox.grid) 

# creating model arrays
m.cox <- array(vector(mode = "list", length = 1), c(2, nrow(cox.grid)))

# Calculate models --------------------------------------------------------

print("Estimating Cox...")
for (i in 1:nrow(cox.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(cox.grid)))
  
  # training models
  m.cox[[i]] <-coxph(fitform, data=dat[dat$Class==cox.grid$Class[i],])
  
}  


# Save models in latex table ----------------------------------------------

sink("cox_table.tex")
stargazer(m.cox[[1]],m.cox[[2]],m.cox[[3]],m.cox[[4]],m.cox[[5]],m.cox[[6]],m.cox[[7]],m.cox[[8]],
          type="latex",
          apply.coef = exp,
          apply.se = exp, 
          label="tab:coxmodels",
          title="Cox proportional hazards. Comparing buyer preferences for major Mercedes-Benz Classes", 
          covariate.labels = c("Market size", "DOP", "Quantile",
                               "Age", "Size vendor"),
          dep.var.caption  = "Dependent variable",
          dep.var.labels   = "Time on market (in days)",
          column.labels = c("A", "B", "C", "E", "M", 
                            "S", "SL", "SLK"),  
          no.space = T,
          summary=F, 
          align=TRUE, 
          column.sep.width = "2pt", 
          float.env="sidewaystable", 
          font.size = "tiny", 
          t.auto=F, 
          p.auto=F, 
          report = "vc*", 
          omit.stat = c("lr", "wald", "ll", "logrank", "max.rsq"))
sink()


