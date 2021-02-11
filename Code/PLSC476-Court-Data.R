#########################################################
# PLSC 476: Empirical Legal Studies (Spring 2021)
#
# Code for February 11: Finding and working with
# court data.
#
# Set working directory (uncomment / change as necessary):

# setwd("~/Dropbox (Personal)/PLSC 476")

# Load packages (install as needed):

library(RCurl)
library(haven)
library(DescTools)
library(tidyverse)
library(lubridate)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 2) # show fewer decimal places
########################################################
# 1a. Supreme Court database (OT 1946-2010)...
#
# Case-Centered data, organized by docket number:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2020_01/SCDB_2020_01_caseCentered_Docket.csv.zip",ZIP)
scdb.cc<-read.csv(unz(ZIP,"SCDB_2020_01_caseCentered_Docket.csv"),
               stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)    # clean up...

# Data structure (not shown):

head(scdb.cc[1:5])

#########
# Justice-Centered data, organized by docket number:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2020_01/SCDB_2020_01_justiceCentered_Docket.csv.zip",ZIP)
scdb.jc<-read.csv(unz(ZIP,"SCDB_2020_01_justiceCentered_Docket.csv"),
               stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)    # clean up...

# "Legacy" data: OT1791-1945:
#
# Case-centered:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/Legacy_06/SCDB_Legacy_06_caseCentered_Citation.csv.zip",ZIP)
legacy.cc<-read.csv(unz(ZIP,"SCDB_Legacy_06_caseCentered_Citation.csv"),
                  stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)

# Justice-centered:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/Legacy_06/SCDB_Legacy_06_justiceCentered_Citation.csv.zip",ZIP)
legacy.jc<-read.csv(unz(ZIP,"SCDB_Legacy_06_justiceCentered_Citation.csv"),
                    stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)

######
# What can we do with all this?
#
# Modern, case-centered: Plot the number of Right to Privacy
# cases in each term, OT1946-2019:

privacy<-table(scdb.cc[scdb.cc$issueArea==5,]$term)

pdf("Notes and Slides/PrivacyCasesByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
plot(privacy,xlab="Term",ylab="Frequency")
dev.off()

# Modern, justice-centered: Plot the proportion of liberal
# votes cast by each justice in cases involving the
# First Amendment:

FirstA<-with(scdb.jc[scdb.jc$issueArea==3,],
             prop.table(xtabs(~justiceName+(direction-1)),1))
FirstA<-FirstA[order(FirstA[,2]),] # sort

pdf("Notes and Slides/FirstAmdtLibVoting.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(FirstA[,2]*100,horiz=TRUE,las=2,cex.names=0.5,
        xlim=c(0,100),xlab="Liberal Voting Percentage")
dev.off()

# Legacy, case-centered: Histogram of the number of cases
# involving Native American tribes as litigants (either 
# as petitioner or respondent):

NATdata<-legacy.cc[(legacy.cc$petitioner==170 | 
                        legacy.cc$respondent==170),]

pdf("Notes and Slides/NATCasesByTerm.pdf",6,5)
par(mar=c(4,4,2,2))
hist(NATdata$term,xlim=c(1790,1945),breaks=(1945-1790),
     xlab="Term",main=" ")
dev.off()

##############
# 1b. The Supreme Court Justices database:

Justices<-read_dta("http://epstein.wustl.edu/research/justicesdata.dta")

# Example:

pdf("Notes and Slides/SCOTUS-DateNom-AgeNom.pdf",6,5)
par(mar=c(4,4,2,2))
with(Justices[Justices$recess==0,], plot(yrnom,agenom,
     main=" ",pch=19,
     ylab="Age at Nomination",xlab="Year of Nomination"))
dev.off()

##############
# 1c. "Segal-Cover" and "Martin-Quinn" scores
#
# Segal-Cover:

url<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/Segal-Cover.csv")
SC<-read.csv(text = url,stringsAsFactors = FALSE)
rm(url)

# Martin-Quinn:

url<-getURL("https://mqscores.lsa.umich.edu/media/2019/justices.csv")
MQ<-read.csv(text = url,stringsAsFactors = FALSE)
rm(url)

# Example: Aggregate MQ scores to the justice level 
# (one score per justice for his/her whole career):

MQ.j <- aggregate(post_med~justice,data=MQ,mean)

# Merge with Segal-Cover data:

SC2<-merge(SC,MQ.j,by=c("justice"))

# Plot them against each other:

pdf("Notes and Slides/SCvsMQ.pdf",6,5)
par(mar=c(4,4,2,2))
with(SC2, plot(Ideology, post_med,pch=20,
               xlab="Segal-Cover (liberalism)",
               ylab="Martin-Quinn (conservatism)",
               xlim=c(-0.3,1.3),ylim=c(-5,3))
     )
with(SC2, text(Ideology,post_med,
               labels=SC2$Nominee,pos=1,cex=0.7)
     )
abline(lm(post_med~Ideology,data=SC2),lwd=2)
dev.off()

############################################
# 2. Federal Courts of Appeals...
#
# ...

############################################
# 3. All federal courts...
#
# The Federal Judicial Center's (FJC's)
# biographical database:
#
# https://www.fjc.gov/history/judges

FJC<-read_csv("https://www.fjc.gov/sites/default/files/history/judges.csv")

# Federal judges by Zodiac sign:

FJC$BDate<-paste(FJC$`Birth Month`,
                 FJC$`Death Day`,
                 FJC$`Birth Year`,
                 sep="-")
FJC$BDate<-mdy(FJC$BDate)
FJC$Sign<-Zodiac(FJC$BDate)

pdf("Notes and Slides/Judge-Zodiac.pdf",7,5)
par(mar=c(6,4,2,2))
barplot(table(FJC$Sign),las=2,ylab="Frequency")
abline(h=mean(table(FJC$Sign)),lty=2,col="red")
text(1,190,labels="Mean",col="red",cex=0.8)
dev.off()

########################################
# 4. State Supreme Courts...
# ...

########################################
# 5. Inter- and Cross-National Courts...
# ...
