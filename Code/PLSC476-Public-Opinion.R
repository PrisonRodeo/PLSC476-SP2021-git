##############################################
# This is some code from PLSC 476,
# dated March 16, 2021.
#
# The subject matter is judicial behavior
# and public opinion.
##############################################
# Load necessary R packages...
#
# By now, you should already have installed the R 
# packages "RCurl", "readr", "plyr", and "stargazer".
# If you have not,uncomment and run this code:
#
# install.packages("RCurl")
# install.pcakages("readr")
# install.packages("plyr")
# install.packages("stargazer")
#
# Then do:

library(RCurl)
library(readr)
library(plyr)
library(stargazer)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# And be sure to use -setwd- to set the local / 
# working directory to where you need it to be.
#
#####################################################
# Read in SCOTUS votes + Segal-Cover ideology data:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2020_01/SCDB_2020_01_justiceCentered_Docket.csv.zip",ZIP)
Votes<-read.csv(unz(ZIP,"SCDB_2020_01_justiceCentered_Docket.csv"),
                stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)       # clean up...

# Segal-Cover scores data:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/Segal-Cover.csv")
SCScores <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

# Fix Rehnquist (remove his CJ line of data...)

SCScores<-SCScores[SCScores$Order!=32, ]

# Merge:

Master <- merge(Votes,SCScores,by=c("justice"),
                all.x=TRUE,all.y=FALSE)

# Read in annual "public mood" data:

Mood <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/PublicMood.csv")

# Create a {0,1} liberal vote variable:

Master$LibVote <- Master$direction - 1

# Now, as in McGuire & Stimson, we focus only on 
# Supreme Court *reversals*...
#
# Drop cases where SCOTUS affirmed the lower court:

Master$Rev <- ifelse(Master$caseDisposition>1 & 
                       Master$caseDisposition<5,1,0)
Reversals <- Master[Master$Rev==1,]

# Create three files for Criminal, CivLibs, and Economics:

CrimRev <- Reversals[Reversals$issueArea==1,]
CivlibRev <- Reversals[Reversals$issueArea==2,]
EconRev <- Reversals[Reversals$issueArea==8,]

# Aggregate the votes up to the term level...

CrimAnn <- ddply(CrimRev, "term", summarize,
                 SegalCover = mean(Ideology,na.rm=TRUE)*100,
                 CrimLibPct = (mean(LibVote,na.rm=TRUE))*100)
CLAnn <- ddply(CivlibRev, "term", summarize,
                 CLLibPct = (mean(LibVote,na.rm=TRUE))*100)
EconAnn <- ddply(EconRev, "term", summarize,
                 EconLibPct = (mean(LibVote,na.rm=TRUE))*100)

# Build annual "time series" dataset:

colnames(Mood) <- c("term","Mood")
TSData <- merge(Mood,CrimAnn,by=c("term"))
TSData <- merge(TSData,CLAnn,by=c("term"))
TSData <- merge(TSData,EconAnn,by=c("term"))

# Examine summary statistics, plots, and correlations:

summary(TSData)

pdf("MoodPlot.pdf",7,6)
par(mar=c(4,4,2,2))
with(TSData, plot(term,Mood,t="l",lwd=3,col="black",ylim=c(15,85),
                  xlab="Term",ylab="Liberalism"))
with(TSData, lines(term,CrimLibPct,lwd=2,col="blue"))
with(TSData, lines(term,CLLibPct,lwd=2,col="red"))
with(TSData, lines(term,EconLibPct,lwd=2,col="orange"))
legend("bottomleft",c("Mood","Criminal","Civil Rights","Economics"),
       lwd=c(3,2,2,2),col=c("black","blue","red","orange"),
       bty="n",cex=0.85)
dev.off()

# Correlations & scatterplots:

cor(TSData)

pdf("MoodScatters.pdf",7,3)
par(mar=c(4,4,2,2))
par(mfrow=c(1,3))
with(TSData, plot(Mood,CrimLibPct,pch=20,col="blue",
                  ylab="Criminal Liberalism",
                  xlim=c(50,70)))
with(TSData, plot(Mood,CLLibPct,pch=20,col="red",
                  ylab="Civil Rights Liberalism",
                  xlim=c(50,70)))
with(TSData, plot(Mood,EconLibPct,pch=20,col="orange",
                  ylab="Economic Liberalism",
                  xlim=c(50,70)))
dev.off()

# BUT: Need to control for Court ideology and a
# lagged "dependent variable"...
#
# Three linear regression analyses:

CrimFit<-with(TSData, lm(CrimLibPct~lag(CrimLibPct)+SegalCover+Mood))
CLFit<-with(TSData, lm(CLLibPct~lag(CLLibPct)+SegalCover+Mood))
EconFit<-with(TSData, lm(EconLibPct~lag(EconLibPct)+SegalCover+Mood))

# Now put those in a pretty table:

stargazer(CrimFit,CLFit,EconFit,type="text",
          column.labels=c("Criminal","Civil Liberties",
                          "Economics"),
          dep.var.labels=c("","",""))

# The LaTeX version:
# 
# stargazer(CrimFit,CLFit,EconFit,type="latex",
#           column.labels=c("Criminal","Civil Liberties",
#                           "Economics"),
#           dep.var.labels=c("","",""))
