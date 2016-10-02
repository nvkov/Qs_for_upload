# ------------------------------------------------------------------------------
# Advisor: Prof. Dr. Stefan Lessmann
# ------------------------------------------------------------------------------
# Quantlet: PlotBSandCI
# ------------------------------------------------------------------------------
# Description: Creates a plot matrix for pairwise comparison of estimates 
# differences between Brier Scores/C-Index for 5 different survival models for
# the analysis of the time on market of German used cars.
# ------------------------------------------------------------------------------
# Usage: -
# ------------------------------------------------------------------------------
# Inputs: BSlist.RData/ CIlist.RData 
# ------------------------------------------------------------------------------
# Output: A diagonal matrix of plots with paired difference of means for 5 
# survival models.
# ------------------------------------------------------------------------------
# Keywords: Cox proportional hazards, Regression trees, Conditional inference 
# trees, Ransom survival Forest, Conditional inference forest, Brier Score, C-Index
# Censored data, survivala models, pairwise comparison, differences of means, 
# standard deviation.
# ------------------------------------------------------------------------------ 
# See also: CalcBSandCI
# ------------------------------------------------------------------------------
# Author: Nikoleta Kovachka, 2016/10/02
# ------------------------------------------------------------------------------
# Helper function: add_legend - adds a slim legend to the plot
# ------------------------------------------------------------------------------
# Helper functions --------------------------------------------------------

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
} 




rm(list=ls())

# Load packages:

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

# Set working directory:
setwd("C:/Users/Nk/Documents/Uni/Qs/PlotBSandCI/")

# Load data ---------------------------------------------------------------
load("BSlist.RData")

# Define patter for plot saving:
pattern = Sys.Date()

#Separate different models from BSlist
list2env(BSlist,envir=.GlobalEnv)

# Calculate means and sd for each data.frame

meanRpart<- apply(BSrpart, 1, mean)
sdRpart<- apply(BSrpart, 1, sd)

meanRsf<- apply(BSrsf, 1, mean)
sdRsf<- apply(BSrsf, 1, sd)

meanCtree<- apply(BSctree, 1, mean)
sdCtree<- apply(BSctree, 1, sd)

meanCox<- apply(BSCox, 1, mean)
sdCox<- apply(BSCox, 1, sd)


meanCforest<- apply(BScforest, 1, mean)
sdCforest<- apply(BScforest, 1, sd)

pdf(paste0("BSmeans", pattern, ".pdf"))
par(mar = c(4, 4, 1.6, 0.2))

plot(meanRsf, ylim=c(0,0.3), type="l", lwd=2, ylab = "Brier Score", xlab="days")
lines(meanRpart, col="red", lwd=2)
lines(meanCtree, col="blue", lwd=2)
lines(meanCox, col="pink", lwd=2)
lines(meanCforest, col="green", lwd=2)
abline(h=0.25, col="grey", lty=2)

add_legend("topright", legend=c("RSF", "Cforest", "Cox", "Rpart", "Ctree"), pch=19, 
           col=c("black", "green", "pink", "red", "blue"),
           horiz=TRUE, bty='n', cex=0.9)
dev.off()


# Build paired difference matrix -----------------------------------------

rsfcforest<- BSrsf- BScforest 
rsfctree<- BSrsf- BSctree
rsfrpart<- BSrsf- BSrpart
rsfcox<- BSrsf- BSCox

coxrsf<- BSCox - BSrsf
coxcforest<- BSCox - BScforest
coxctree<- BSCox - BSctree
coxrpart<- BSCox - BSrpart

cforestrsf<- BScforest - BSrsf 
cforestctree<- BScforest - BSctree
cforestrpart<- BScforest - BSrpart
cforestcox<- BScforest - BSCox

rpartrsf<- BSrpart - BSrsf
rpartctree<- BSrpart - BSctree
rpartcforest<- BSrpart - BScforest
rpartcox<- BSrpart - BSCox

ctreerpart<- BSctree - BSrpart
ctreersf<- BSctree - BSrsf
ctreecforest<- BSctree - BScforest
ctreecox<- BSctree - BSCox



# Calculate means and sds -------------------------------------------------
meanrsfcforest<- apply(rsfcforest, 1, mean) 
meanrsfctree<- apply(rsfctree, 1, mean)
meanrsfrpart<- apply(rsfrpart, 1, mean)
meanrsfcox<- apply(rsfcox, 1, mean)

meancoxrsf<- apply(coxrsf, 1, mean)
meancoxcforest<- apply(coxcforest, 1, mean)
meancoxctree<- apply(coxctree, 1, mean)
meancoxrpart<- apply(coxrpart, 1, mean)

meancforestrsf<- apply(cforestrsf, 1, mean) 
meancforestctree<- apply(cforestctree, 1, mean)
meancforestrpart<- apply(cforestrpart, 1, mean)
meancforestcox<- apply(cforestcox, 1, mean)

meanrpartrsf<- apply(rpartrsf, 1, mean)
meanrpartctree<- apply(rpartctree, 1, mean)
meanrpartcforest<- apply(rpartcforest, 1, mean)
meanrpartcox<- apply(rpartcox, 1, mean)

meanctreerpart<- apply(ctreerpart, 1, mean)
meanctreersf<- apply(ctreersf, 1, mean)
meanctreecforest<- apply(ctreecforest, 1, mean)
meanctreecox<- apply(ctreecox, 1, mean)


# Calculate standard errors -----------------------------------------------

sdrsfcforest<- apply(rsfcforest, 1, sd) 
sdrsfctree<- apply(rsfctree, 1, sd)
sdrsfrpart<- apply(rsfrpart, 1, sd)
sdrsfcox<- apply(rsfcox, 1, sd)

sdcoxrsf<- apply(coxrsf, 1, sd)
sdcoxcforest<- apply(coxcforest, 1, sd)
sdcoxctree<- apply(coxctree, 1, sd)
sdcoxrpart<- apply(coxrpart, 1, sd)

sdcforestrsf<- apply(cforestrsf, 1, sd) 
sdcforestctree<- apply(cforestctree, 1, sd)
sdcforestrpart<- apply(cforestrpart, 1, sd)
sdcforestcox<- apply(cforestcox, 1, sd)

sdrpartrsf<- apply(rpartrsf, 1, sd)
sdrpartctree<- apply(rpartctree, 1, sd)
sdrpartcforest<- apply(rpartcforest, 1, sd)
sdrpartcox<- apply(rpartcox, 1, sd)

sdctreerpart<- apply(ctreerpart, 1, sd)
sdctreersf<- apply(ctreersf, 1, sd)
sdctreecforest<- apply(ctreecforest, 1, sd)
sdctreecox<- apply(ctreecox, 1, sd)

# Plot differences --------------------------------------------------------
# Save under ceratain file name:
pdf(paste0("allBS",pattern,".pdf"))

# Prepare layout for matrix:
pp <- layout(matrix(c(1,0,0,0,0,
                      2,3,0,0,0, 
                      4,5,6,0,0,
                      7,8,9,10,0,
                      11,12,13,14,15),
                    5, 5, 
                    byrow=T))

#Inspect layout
#layout.show(pp)

# First line: RSF ---------------------------------------------------------
par(mar=c(0.5, 0.5, 0.5, 0.5))
par(oma=c(2.5,2.5,0,0))

# (1)
plot(c(0,85), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n') 
text(60, -0.035, "RSF", cex=1.2)

# Second line: Ctree ------------------------------------------------------

# (1)
plot(c(1:85), meancforestrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:85), I(meancforestrsf-1.98*sdcforestrsf), lty=2)
lines(c(1:85), I(meancforestrsf+1.98*sdcforestrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

# (2)
plot(c(0,85), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Cforest", cex=1.2)
text(5, 0, "Cforest", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")



# Third line: RPart -------------------------------------------------------

# (1)
plot(c(1:85), meanrpartrsf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:85), I(meanrpartrsf-1.98*sdrpartrsf), lty=2)
lines(c(1:85), I(meanrpartrsf+1.98*sdrpartrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

# (2)
plot(c(1:85), meanrpartcforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:85), I(meanrpartcforest-1.98*sdrpartcforest), lty=2)
lines(c(1:85), I(meanrpartcforest+1.98*sdrpartcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(3)
plot(c(0,85), c(-0.04,0.04), text("Cforest"), yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Rpart", cex=1.2)
text(5, 0, "Rpart", srt=270, cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")


# Fourth line: ctree ------------------------------------------------------

#(1)
plot(c(1:85), meanctreersf, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", xaxt='n')
lines(c(1:85), I(meanctreersf-1.98*sdctreersf), lty=2)
lines(c(1:85), I(meanctreersf+1.98*sdctreersf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(2)
plot(c(1:85), meanctreecforest, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:85), I(meanctreecforest-1.98*sdctreecforest), lty=2)
lines(c(1:85), I(meanctreecforest+1.98*sdctreecforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(3)
plot(c(1:85), meanctreerpart, ylim=c(-0.05, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n', xaxt='n')
lines(c(1:85), I(meanctreerpart-1.98*sdctreerpart), lty=2)
lines(c(1:85), I(meanctreerpart+1.98*sdctreerpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(4)
plot(c(0,85), c(-0.04,0.04),  yaxt='n', xaxt='n', frame.plot=F, type='n')
text(60, -0.035, "Ctree", cex=1.2)
text(5, 0, "Ctree", srt=270,  cex=1.2)
lines(x = c(0,60), y = c(-0.04,0), lty=3, col="grey")

# Last line: Cox ----------------------------------------------------------

#(1)
plot(c(1:85), meancoxrsf, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="")
lines(c(1:85), I(meancoxrsf-1.98*sdcoxrsf), lty=2)
lines(c(1:85), I(meancoxrsf+1.98*sdcoxrsf), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(2)
plot(c(1:85), meancoxcforest, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:85), I(meancoxcforest-1.98*sdcoxcforest), lty=2)
lines(c(1:85), I(meancoxcforest+1.98*sdcoxcforest), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(3)
plot(c(1:85), meancoxrpart, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:85), I(meancoxrpart-1.98*sdcoxrpart), lty=2)
lines(c(1:85), I(meancoxrpart+1.98*sdcoxrpart), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(4)
plot(c(1:85), meancoxctree, ylim=c(-0.1, 0.05), type="l", lwd=2, xlab="", ylab="", yaxt='n')
lines(c(1:85), I(meancoxctree-1.98*sdcoxctree), lty=2)
lines(c(1:85), I(meancoxctree+1.98*sdcoxctree), lty=2)
abline(h=0, col="red")
box(lty = '1373', col = 'gray')

#(5)
plot(c(0,85), c(-0.04,0.04), yaxt='n', xaxt='n', frame.plot=F, type='n')
text(5, 0, "CoxPH", srt=270, cex=1.2)

dev.off()

