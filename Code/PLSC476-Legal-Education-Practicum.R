######################################################
# This is some code from PLSC 476, for the class
# occurring April 1, 2021.
#
# It's about law schools.
#
# No, that's not a joke.
#####################################################
# Load a few necessary R packages [if you haven't
# installed these by now, you should do so, 
# using -install.packages()-]:

library(RCurl)
library(stargazer)
library(usmap)
library(tidyverse)
library(dplyr)

# Set a couple options:

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

# Also be sure to use -setwd()- to set your
# working directory (that is, where any files
# you create will be kept).
#
setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
#
#####################################################
# Data are from AccessLex, and are for the year
# 2019. They are downloadable here:
#
# https://analytix.accesslex.org/DataSet
#
# Read in the AccessLex law school data from the 
# course github repo. There are 12 total files;
# we'll pull all of them except the "Transfers"
# file (which is huge and messy):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Admissions.csv")
Admissions <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Attrition.csv")
Attrition <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

Attrition<-Attrition[c(1:3,5:8)] # pare down "Attrition"

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/BarPassage.csv")
BarPassage <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Curriculum.csv")
Curriculum <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Degrees.csv")
Degrees <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Employment.csv")
Employment <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Enrollment.csv")
Enrollment <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Faculty.csv")
Faculty <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/FinancialAid.csv")
FinancialAid <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/SchoolInformation.csv")
SchoolInformation <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/StudentExpenses.csv")
StudentExpenses <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

# Now merge all of these into a single dataset. In 
# each file, there is an identifier for each law school,
# called "schoolid" -- that's our friend:

SchoolInformation$calendaryear<-NULL
DF<-merge(SchoolInformation,Admissions,
          by=c("schoolid","schoolname"),
          all=FALSE)

DF<-merge(DF,Faculty,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

DF<-merge(DF,StudentExpenses,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

DF<-merge(DF,FinancialAid,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

DF<-merge(DF,Attrition,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

BarPassage$calendaryear<-NULL
DF<-merge(DF,BarPassage,
          by=c("schoolid","schoolname"),
          all=FALSE)

Employment$calendaryear<-Employment$cohort
DF<-merge(DF,Employment,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

rm(Admissions,Attrition,BarPassage,Curriculum,Degrees,
   Employment,Enrollment,Faculty,FinancialAid,
   SchoolInformation,StudentExpenses)

#############################################
# This is a **lot** of data, on more than 350 
# variables. Let's zoom in on a smaller set 
# of key variables:

Schools<-with(DF, data.frame(SchoolID=schoolid,
                        SchoolName=schoolname,
                        State=schoolstate,
                        TermType=termtype.x,
                        SchoolType=schooltype.x,
                        Applications=numapps,
                        Offers=numoffers,
                        Matriculants=nummatriculants,
                        NFirstYears=totalfirstyear,
                        PTFirstYears=ptfirstyear,
                        NStudents=numstudents,
                        MedianUGPA=uggpa50,
                        MedianLSAT=lsat50,
                        TotalFaculty=ftfactotal,
                        FemaleFaculty=ftfacwomen,
                        MinorityFaculty=ftfacminority,
                        ResTuition=ftrestuition,
                        NonResTuition=ftnonrestuition,
                        NGrads=gradtotal,
                        FirstTimeBarTakers=totalfirsttimetakers,
                        FirstTimeBarPassers=totalfirsttimepassers,
                        UltimateBarTakers=ultimatetakers,
                        UltimateBarPassers=ultimatepassers,
                        StatePassRate=(avgstatepasspct)*100))

row.names(Schools)<-Schools$SchoolName

##################################################
# Before we get into the schools, let's make a 
# state-level dataframe, for maps and things,
# by aggregating the school-level data:

states<-group_by(Schools,State)
States<-summarise(states,
                  NSchools=n(),
                  NGrads=sum(NGrads),
                  BarPassRate=round(mean(StatePassRate),3))
rm(states)

States$State<-as.character(States$State) # make character

# Add FIPS codes (needed for plotting):

States$abbr<-States$State
States<-merge(usmap::statepop,States,by=c("abbr"),all=TRUE)

# Alaska has no law schools, so they have no 
# school-level data on bar passage at the 
# state level; we'll add that information
# from this page:
#
# https://admissions.alaskabar.org/recent-2019-july

States$State<-ifelse(States$abbr=="AK","AK",States$State)
States$NSchools<-ifelse(States$abbr=="AK",0,States$NSchools)
States$NGrads<-ifelse(States$abbr=="AK",0,States$NGrads)
States$BarPassRate<-ifelse(States$abbr=="AK",64,States$BarPassRate)

# Also: Remove Puerto Rico...

States<-States[States$State!="PR",]

#############################################
# State-level maps:
#
# Specify legend colors, low and high:

low<-"yellow"
high<-"navy"

# Number of Law Schools:

pdf("SchoolsMap.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=States,values="NSchools",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="Number of Law Schools",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Number of 2019 Law Grads:

pdf("LawGradsMap.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=States,values="NGrads",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="2019 Law School Graduates",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Number of 2019 Law Grads per 100K population (DC is
# excluded because its number is so large that it messes
# up the plot...):

States$GradsPer100K<-States$NGrads/(States$pop_2015/100000)
StatesNoDC<-States[States$State!="DC",]

pdf("LawGradsMap2.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=StatesNoDC,values="GradsPer100K",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="2019 Law School Graduates Per\n100K Population (D.C. omitted)",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Bar Passage:

pdf("BarPassMap.pdf",10,8)
par(mar=c(2,2,2,2))
plot_usmap(data=States,values="BarPassRate",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="Bar Passage Rate",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()

# Is there a relationship between the number of schools
# or grads and the bar passage rate?

r1<-round(with(States,cor(NSchools,BarPassRate,
                          use="complete.obs")),2)

pdf("BarPass-Schools-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(States, plot(NSchools,BarPassRate,pch=20,
                  xlim=c(0,20),ylim=c(55,85),
                  xlab="Number of Law Schools",
                  ylab="Bar Passage Rate"))
with(States, text(NSchools,BarPassRate,pos=1,
                  labels=States$State,cex=0.7))
text(15,60,paste0("r = ",r1),cex=1.5)
dev.off()

# Same scatterplot, this time vs. law graduates:

r2<-round(with(States,cor(NGrads,BarPassRate,
                          use="complete.obs")),2)

pdf("BarPass-Grads-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(States, plot(GradsPer100K,BarPassRate,pch=20,
                  xlim=c(0,3700),ylim=c(55,85),
                  xlab="Number of 2019 Law Graduates",
                  ylab="Bar Passage Rate"))
with(States, text(NGrads,BarPassRate,pos=1,
                  labels=States$State,cex=0.7))
text(3000,60,paste0("r = ",r2),cex=1.5)
dev.off()

###############################################
# Now let's look at law *schools*:
#
# First generate both overall and first-time
# bar pass rate percentages for each school:

Schools$FTPassRate<-with(Schools,
                          round((FirstTimeBarPassers/FirstTimeBarTakers)*100),3)
Schools$BarPassRate<-with(Schools,
                     round((UltimateBarPassers/UltimateBarTakers)*100),3)

# What is the overall distribution of bar passage
# rates across schools?

pdf("FTBarPass-Histogram.pdf",4,5)
par(mar=c(4,4,2,2))
hist(Schools$FTPassRate,col="grey",main="",
     xlab="First-Time Bar Passage Rate",
     xlim=c(20,100),ylim=c(0,60),breaks=16)
abline(v=mean(Schools$FTPassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

pdf("TotalBarPass-Histogram.pdf",4,5)
par(mar=c(4,4,2,2))
hist(Schools$BarPassRate,col="grey",main="",
     xlab="Total Bar Passage Rate",
     xlim=c(20,100),ylim=c(0,60),breaks=8)
abline(v=mean(Schools$BarPassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

# Q: Which law schools had the highest and 
# lowest bar passage rates in 2019?
#
# Sort and list, first-time...

Schools<-Schools[order(-Schools$FTPassRate),]
head(Schools[,c(2,25)],10,addrownums=FALSE) # top 10
tail(Schools[,c(2,25)],10,addrownums=FALSE) # bottom 10

# and total...

Schools<-Schools[order(-Schools$BarPassRate),]
head(Schools[,c(2,26)],10) # top 10
tail(Schools[,c(2,26)],10) # bottom 10

# How do those two things looked when they're plotted
# against each other?

BPfit<-lm(BarPassRate~FTPassRate,data=Schools)

pdf("BarPassages-Scatterplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(FTPassRate,BarPassRate,pch=20,
                  xlab="First-Time Passage Rate",
                  ylab="Total Passage Rate"))
abline(a=0,b=1,lwd=2) # X=Y line
abline(BPfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2,1),lwd=2,col=c("red","black"),
       legend=c("Best Fit Line","X = Y"))
text(92,65,paste0("r = ",round(sqrt(summary(BPfit)$r.squared),2)),
     cex=1.5)
dev.off()

# OK, great... So, what are the factors that might
# lead to higher or lower bar passage rates for
# different schools?
#
# Let's start with some basic things, like the
# "quality" of the students, measured in terms of
# LSAT scores and undergraduate GPAs:

LSATfit<-lm(FTPassRate~MedianLSAT,data=Schools)

pdf("LSAT-BarPass-Scatterplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(MedianLSAT,FTPassRate,pch=20,
                  xlab="Median LSAT Score",
                  ylab="First-Year Bar Passage Rate"))
abline(LSATfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(166,36,paste0("r = ",round(sqrt(summary(LSATfit)$r.squared),2)),
     cex=1.5)
dev.off()

# Now, UGPA:

UGPAfit<-lm(FTPassRate~MedianUGPA,data=Schools)

pdf("UGPA-BarPass-Scatterplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(MedianUGPA,FTPassRate,pch=20,
                  xlab="Median Undergraduate GPA",
                  ylab="First-Year Bar Passage Rate"))
abline(UGPAfit,col="red",lwd=2,lty=2)
legend("topleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(3.75,36,paste0("r = ",round(sqrt(summary(UGPAfit)$r.squared),2)),
     cex=1.5)
dev.off()

# Now, selectivity. We'll measure this as the percentage
# of applicants a school admitted, as a percentage
# of those who applied. So, higher numbers = less
# selective:

Schools$AdmitPercent<-with(Schools,round((Offers/Applications)*100),4)

# Now plot that...

SelFit<-lm(FTPassRate~AdmitPercent,data=Schools)

pdf("Select-BarPass-Scatterplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(AdmitPercent,FTPassRate,pch=20,
                  xlab="Percent of Applicants Admitted",
                  ylab="First-Year Bar Passage Rate"))
abline(SelFit,col="red",lwd=2,lty=2)
legend("bottomleft",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(64,40,paste0("r = ",round(sqrt(summary(SelFit)$r.squared),2)),
     cex=1.5)
dev.off()

# What about public and private schools? We can 
# guess that it's unlikely that there are important
# bivariate differences, but let's look anyway:

Schools$Type<-ifelse(Schools$SchoolType=="PRI","Private","Public")

PubPri.t<-t.test(FTPassRate~Type,Schools)

pdf("BP-Type-Boxplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools, boxplot(FTPassRate~Type,ylim=c(20,100),
                      ylab="First-Year Bar Passage Rate",
                      xlab="School Type"))
text(1.5,28,paste0("t = ",round(PubPri.t$statistic,2),
                 "\nP = ",round(PubPri.t$p.value,3)),
     cex=1.2)
dev.off()

# School size (students and faculty)...

NFit<-lm(FTPassRate~NStudents,data=Schools)
NFacFit<-lm(FTPassRate~TotalFaculty,data=Schools)

pdf("NStud-BarPass-Scatterplot.pdf",4,5)
par(mar=c(4,4,2,2))
with(Schools,plot(NStudents,FTPassRate,pch=20,
                  xlab="Number of Students",
                  ylab="First-Year Bar Passage Rate"))
abline(NFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(1700,80,paste0("r = ",round(sqrt(summary(NFit)$r.squared),2)),
     cex=1.5)
dev.off()

pdf("NFac-BarPass-Scatterplot.pdf",4,5)
par(mar=c(4,4,2,2))
with(Schools,plot(TotalFaculty,FTPassRate,pch=20,
                  xlab="Number of Faculty",
                  ylab="First-Year Bar Passage Rate"))
abline(NFacFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(150,80,paste0("r = ",round(sqrt(summary(NFacFit)$r.squared),2)),
     cex=1.5)
dev.off()

# Next... COST:
#
# We'll define tuition as the average of resident
# and nonresident tuition; for private / one-price
# schools, this is equal to both. We'll express 
# the cost in thousands of dollars:

Schools$Cost<-with(Schools,(ResTuition+NonResTuition)/2000)

CostFit<-lm(FTPassRate~Cost,data=Schools)

# Plot:

pdf("Cost-BarPass-Scatterplot.pdf",6,5)
par(mar=c(4,4,2,2))
with(Schools,plot(Cost,FTPassRate,pch=20,
                  xlab="Tuition Per Year (Thousands of Dollars)",
                  ylab="First-Year Bar Passage Rate"))
abline(CostFit,col="red",lwd=2,lty=2)
legend("bottomright",bty="n",lty=c(2),lwd=2,col=c("red"),
       legend=c("Best Fit Line"))
text(65,45,paste0("r = ",round(sqrt(summary(CostFit)$r.squared),2)),
     cex=1.5)
dev.off()

# Now, let's fit some multivariate models to that, and see
# what happens...

FT.fit<-lm(FTPassRate~MedianLSAT+MedianUGPA+AdmitPercent+
                      Type+NStudents+TotalFaculty+
                      Cost,data=Schools)

stargazer(FT.fit,type="latex",model.numbers=FALSE,
          dep.var.caption="Dependent Variable",
          dep.var.labels="First-Time Bar Passage Rate",
          omit.stat=c("f","ser"))

# Finally, context... Most -- but not all -- law school
# graduates take the bar exam in the state where the
# law school is located. That means that some law grads
# face harder bar exams than others... We can get a sense
# of how a school does in relative terms by comparing its
# bar passage numbers to those for the state as a whole.
#
# If we subtract the school's bar passage rate from the
# overall state rate, we see whether a school is over-
# or under-performing relative to that state in general.
#
# We can examine this new number:

Schools$RelativePassRate<-with(Schools,
                               FTPassRate-StatePassRate)

# What does that look like?

pdf("RelativeBarPass-Histogram.pdf",6,5)
par(mar=c(4,4,2,2))
hist(Schools$RelativePassRate,col="grey",main="",
     xlab="Relative Bar Passage Rate",
     xlim=c(-40,40),ylim=c(0,40),breaks=16)
abline(v=mean(Schools$RelativePassRate,na.rm=TRUE),
       lwd=2,lty=2)
dev.off()

# Now let's do the same multivariate analysis,
# but for the "relative" passage scores:

FTR.fit<-lm(RelativePassRate~MedianLSAT+MedianUGPA+AdmitPercent+
             Type+NStudents+TotalFaculty+Cost,data=Schools)

stargazer(FT.fit,FTR.fit,type="text",model.numbers=FALSE,
          dep.var.caption="Dependent Variable",
          dep.var.labels=c("First-Time Bar Passage Rate",
                           "Relative Bar Passage Rate"),
          omit.stat=c("f","ser"))

# Lastly: Predictions!
#
# We can generate predicted bar passage rates (first-time
# and relative) from these models, and then compare the
# predictions against their actual bar passage rates to
# get a sense of which schools are doing well or poorly
# relative to expectations. These differences are 
# called "residuals":

FT.resids<-data.frame(School = rownames(FT.fit$model),
                      RelativePerf = round(FT.fit$residuals,2))

# Lay out the good and the bad...

good<-FT.resids[FT.resids$RelativePerf>0,]
good<-good[order(-good$RelativePerf),]
good$School<-NULL
head(good,10)

bad<-FT.resids[FT.resids$RelativePerf<0,]
bad<-bad[order(bad$RelativePerf),]
bad$School<-NULL
head(bad,10)

