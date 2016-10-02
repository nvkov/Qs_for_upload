# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: PlotRPartCtree
# ------------------------------------------------------------------------------
# Description: Grows and plots two survival trees on the same data: 
# (1) Rpart - Based on Breiman's CART algorithm
# (2) Ctree - Conditional inference tree (Hothorn, 2006)
# ------------------------------------------------------------------------------
# Usage: The plots are used to asses fast the effects of covariates on the time 
# of a used car on the market until it gets sold
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: Two plots for rpart and ctree survival trees. Terminal nodes present 
# Kaplan-Meier Curves for the observations in the nodes.
# ------------------------------------------------------------------------------
# Keywords: Survival models, Rpart, Ctree, 
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
library("randomForestSRC")
library("party")

#Set working directory
wd<- "C:/Users/Nk/Documents/Uni/Qs/Qs_for_upload/PlotRpartCtree"
setwd(wd)

#Load dataset:
load("Metainfo.RData")

set.seed(22)
# Select subsample for tests ----------------------------------------------
dat<- dat[,i:=.I][sample(i, 13500)]

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor

# Grow rpart tree ---------------------------------------------------------
rpart<- rpart(fitform, data=dat)

# Convert tree to party object for better plotting:
tree<- as.party(rpart)

# Save tree:
pdf("rpartSample.pdf")
plot(tree) 
dev.off()


# Grow ctree --------------------------------------------------------------
# Define mincriterion high for a slimmer tree
ctree<- ctree(fitform,contro=ctree_control(mincriterion = 0.999999), data=dat)

# Save as pdf, define width as high to evade overlapping of survival curves at the terminal nodes:
pdf("ctreeSample.pdf", width=20)
plot(ctree, gp = gpar(fontsize = 2),     # font size changed to 2
     inner_panel=node_inner,             # Change inner nodes
     ip_args=list(                       # Specify arguments to change
       abbreviate = F,                   # Write out full variable names
       id = T                            # Keep id for each inner node
       ), 
     terminal_panel=node_surv,           # Plot terminal nodes as KM Curves
     tp_args=list(id=F)                  # Erase id number for terminal nodes
)
dev.off()



