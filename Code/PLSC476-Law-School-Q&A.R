######################################################
# This is some code from PLSC 476, dated
# March 9, 2021.
#
# It's about law schools.
#####################################################
# Load a few necessary R packages [if you haven't
# installed these by now, you should do so, 
# using -install.packages()-]:

library(RCurl)
library(car)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
#####################################################
# Data are from U.S. News, and are for the year
# 2015. (That means they are a bit dated, but in 
# general, law school rankings and other information
# doesn't change very quickly, so they're still
# broadly correct.)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/USNewsLawSchool2015.csv")
LS <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

# Next, make a scatterplot matrix of some key indicators
# of law schools:

pdf("LSScatterplotMatrix.pdf",7,6)
with(LS, 
     scatterplotMatrix(~Peer+LSAT_percentile+UGPA+Acceptance+employed9mo,
                var.labels=c("Peer Reputation",
                "Median LSAT Percentile","Median Undergrad GPA",
                "Acceptance Rate","Employed at Nine Months"),
                col="black",pch=20,cex=0.7))
dev.off()

##########################################################
# Here's a similar plot, using updated (2019-era) data,
# but that doesn't include the "Peer Reputation" variable.
# These data are from:
# 
# https://www.ilrg.com/rankings/law/1/desc/LSATMed
#

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchoolRankings2020.csv")
LS2 <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

LS2$Accept<-LS2$Accept*100       # make into percentages...
LS2$PassBar<-LS2$PassBar*100
LS2$EmployedAt10Mos<-LS2$EmployedAt10Mos*100

pdf("LSScatterplotMatrix2.pdf",7,6)
par(mar=c(4,4,2,2))
with(LS2, 
     scatterplotMatrix(~LSATMedian+GPAMedian+Accept+
                       PassBar+EmployedAt10Mos,
                       var.labels=c("Median LSAT Percentile",
                                    "Median Undergrad GPA",
                                    "Acceptance Rate",
                                    "Bar Passage Rate",
                                    "Employed at Ten Months"),
                       col="black",pch=20,cex=0.7))
dev.off()
