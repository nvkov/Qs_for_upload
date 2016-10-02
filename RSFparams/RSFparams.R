# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: RSFparams
# ------------------------------------------------------------------------------
# Description: Performs grid search for parameter selection of a random survival
# forest. Tuning for ntree and mtry
# ------------------------------------------------------------------------------
# Usage: 
# ------------------------------------------------------------------------------
# Inputs: Metainfo.RData 
# ------------------------------------------------------------------------------
# Output: A plot of the OOB Error of prediction for varying ntree and mtry.
# ------------------------------------------------------------------------------
# Keywords: Survival models, Parameter tuning, Random survival forests
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
wd<- "C:/Users/Nk/Documents/Uni/Qs/Qs_for_upload/RSFparams/"
setwd(wd)

#Load dataset:
load("Metainfo.RData")

# Select form -------------------------------------------------------------
fitform<- Surv(newTOM,status)~ MS + DOP + Quantile + age + size_vendor


# Draw sample -------------------------------------------------------------
train<- dat[,i:=.I,][sample(i, 13000),]


# Select parameters -------------------------------------------------------
rfs.ntree<- c(1:200)
rfs.mtry<- seq(2,4,1)
rfs.splitrule<-c("logrank")

# creating parameter grids
rfs.grid <- expand.grid(ntree = rfs.ntree, splitrule=rfs.splitrule, mtry=rfs.mtry)

forecsts<- NULL

# Calculate models --------------------------------------------------------

forecasts<- NULL
print("Training RSF...")
for (i in 1:nrow(rfs.grid)) {
  
  # displaying model number
  print(paste0("Model ", i, " out of ", nrow(rfs.grid)))
  
  # training models
  m.temp <-rfsrc(fitform, ntree=rfs.grid$ntree[i], splitrule=rfs.grid$splitrule[i], mtry=rfs.grid$mtry[i], data=train) 
  
  # extracting predictions
  forecasts[i] <- m.temp$err.rate[!is.na(m.temp$err.rate)]

}  

rfs.results<- cbind(rfs.grid, forecasts)

forecasts4<- rfs.results$forecasts[rfs.results$mtry==4]
forecasts2<- rfs.results$forecasts[rfs.results$mtry==2]
forecasts3<- rfs.results$forecasts[rfs.results$mtry==3]

pdf("rsf_ntree_stable.pdf")
plot(c(1:200), forecasts4, type="l", xlab="Time", ylab="Predicted error")
lines(c(1:200), forecasts3, type="l", col="red")
lines(c(1:200), forecasts2, type="l", col="blue")

legend(150, 0.42, legend=c("mtry=2", "mtry=3","mtry=4"),
       lty=c(1,1,1), 
       lwd=c(2.5,2.5, 2.5), col=c("blue","black","red"))
dev.off()




#m.temp <-rfsrc(fitform, ntree=10, splitrule="conserve", mtry=2, data=train[1:300,]) 
#m.temp <-rfsrc(fitform, ntree=120, mtry=2, data=train[20000:25000,]) 

#plot(as.party(m.temp))


plot(gg_vimp(m.temp)) +
  theme(legend.position = c(0.8, 0.2)) +
  labs(fill = "VIMP > 0")


varsel_pbc <- var.select(m.temp)
gg_md <- gg_minimal_depth(varsel_pbc)
print(gg_md)

plot(gg_minimal_vimp(gg_md)) +
  theme(legend.position=c(0.8, 0.2))

gg_v <- gg_variable(m.temp, time = c(10,20, 30, 60),
                    time.labels = c("10 days","20 days", "30 days", "60 days"))

plot(gg_v, xvar = "Quantile", alpha = 0.9) + labs(y = "Survival") +
  theme(legend.position = "none") +
  scale_color_manual(values = strCol, labels = event.labels) +
  scale_shape_manual(values = event.marks, labels = event.labels) +
  coord_cartesian(ylim = c(-0.01, 1.02))








