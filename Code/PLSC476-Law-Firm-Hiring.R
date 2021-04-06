######################################################
# This is some code from PLSC 476, for the class
# occurring April 6, 2021.
#
# It's about law firm hiring and employment. 
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
# The first data we'll use are again from AccessLex, 
# and are for the year 2019. They are downloadable 
# here:
#
# https://analytix.accesslex.org/DataSet
#
# Read in the AccessLex law school data from the 
# course github repo. There are 12 total files;
# this time we'll only use five of them:

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Admissions.csv")
Admissions <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/BarPassage.csv")
BarPassage <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Degrees.csv")
Degrees <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/Employment.csv")
Employment <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC476-SP2021-git/master/Data/LawSchools2019/SchoolInformation.csv")
SchoolInformation <- read.csv(text = url,stringsAsFactors=FALSE) # read the Segal-Cover data
rm(url)      # clean up...

# Now merge all of these into a single dataset. In 
# each file, there is an identifier for each law school,
# called "schoolid" -- that's our friend:

SchoolInformation$calendaryear<-NULL
DF<-merge(SchoolInformation,Admissions,
          by=c("schoolid","schoolname"),
          all=FALSE)

BarPassage$calendaryear<-NULL
DF<-merge(DF,BarPassage,
          by=c("schoolid","schoolname"),
          all=FALSE)

Degrees$calendaryear<-NULL
DF<-merge(DF,Degrees,
          by=c("schoolid","schoolname"),
          all=FALSE)

Employment$calendaryear<-Employment$cohort
DF<-merge(DF,Employment,
          by=c("schoolid","schoolname","calendaryear"),
          all=FALSE)

rm(Admissions,BarPassage,Degrees,
   Employment,SchoolInformation)

#############################################
# Now zoom in on a smaller set of key 
# variables:

Schools<-with(DF, data.frame(SchoolID=schoolid,
                             SchoolName=schoolname,
                             State=schoolstate,
                             TermType=termtype,
                             SchoolType=schooltype,
                             Applications=numapps,
                             Offers=numoffers,
                             MedianUGPA=uggpa50,
                             MedianLSAT=lsat50,
                             JDs=total_grads,
                             N.LawFirm=solo_emp+sz_1_10_emp+sz_11_25_emp+sz_26_50_emp+sz_51_100_emp+sz_101_250_emp+sz_251_500_emp+sz_501up_emp+sz_unk_emp,
                             N.Business=bsind_emp,
                             N.Government=gov_emp,
                             N.PublicInterest=pub_emp,
                             N.Academic=acad_emp,
                             N.Clerks=fedclk_emp+locclk_emp+othclk_emp,
                             N.Unknown=unk_emp,
                             N.BPR.FTLT=bar_ftlt,
                             N.BPR=bar_emp,
                             N.JDA.FTLT=jda_ftlt,
                             N.JDA=jda_emp,
                             N.Professional=prof_emp,
                             N.NonProfessional=nprof_emp,
                             N.Unknown=emp_stat_unk,
                             N.Unemployed=(unemp_ns+unemp_seek),
                             N.Undetermined=undet_emp,
                             N.FTLT.All=total_ftlt,
                             N.TopState=state_emp1_num,
                             TopState=state_emp1))

# Next, create some percentages:

Schools$AdmitPercent<-with(Schools,(Offers/Applications)*100) # selectivity
Schools$N.Employed<-with(Schools,(N.LawFirm+N.Business+N.Government+N.PublicInterest+N.Academic+N.Clerks+N.Unknown))
Schools$LawFirmPct<-with(Schools,(N.LawFirm/N.Employed)*100)
Schools$BusinessPct<-with(Schools,(N.Business/N.Employed)*100)
Schools$GovernmentPct<-with(Schools,(N.Government/N.Employed)*100)
Schools$PublicInterestPct<-with(Schools,(N.PublicInterest/N.Employed)*100)
Schools$AcademicPct<-with(Schools,(N.Academic/N.Employed)*100)
Schools$ClerksPct<-with(Schools,(N.Clerks/N.Employed)*100)
Schools$UnknownPct<-with(Schools,(N.Unknown/N.Employed)*100)
Schools$BPR.FTLT.Percent<-with(Schools,(N.BPR.FTLT/JDs)*100) # full time, long-term, bar-passage required percent
Schools$BPR.Percent<-with(Schools,(N.BPR/JDs)*100) # ALL bar-passage required percent
Schools$JDA.FTLT.Percent<-with(Schools,(N.JDA.FTLT/JDs)*100) # full time, long-term, JD-advantaged percent
Schools$JDA.Percent<-with(Schools,(N.JDA/JDs)*100) # ALL JD-advantage percent
Schools$Prof.Percent<-with(Schools,(N.Professional/JDs)*100) # ALL professional percent
Schools$NonProf.Percent<-with(Schools,(N.NonProfessional/JDs)*100) # ALL non-professional percent
Schools$Unemployed.Percent<-with(Schools,(N.Unemployed/JDs)*100) # ALL unemployed percent
Schools$TopStatePercent<-with(Schools,(N.TopState/JDs)*100) # Percent of grads employed in that school's "top state"

row.names(Schools)<-Schools$SchoolName # row labels...

###################################################
# First: What do new JDs **do**?
#
# Plot the percentages by employer type for **all**
# of the graduates, in all 198 schools:

sums<-c(with(Schools,sum(N.LawFirm)),
        with(Schools,sum(N.Business)),
        with(Schools,sum(N.Government)),
        with(Schools,sum(N.PublicInterest)),
        with(Schools,sum(N.Academic)),
        with(Schools,sum(N.Clerks)),
        with(Schools,sum(N.Unknown)))
pct <- round(sums/sum(sums)*100,1)
labels<-c("Law Firms","Business","Government",
          "Public Interest","Academic","Clerks",
          "Unknown")
labels.all<-paste0(labels," ",pct,"%")

pdf("JDEmployment.pdf",5,5)
par(mar=c(1,5,2,5))
pie(sums, labels=labels.all,col=rainbow(length(sums)))
dev.off()

# The numbers for individual schools can be quite
# different from the totals:

# Penn State:

PSU<-Schools[Schools$SchoolName=="Pennsylvania State University-Penn State Law",]
psu<-t(PSU[,11:16])
psupct<-round(psu/sum(psu)*100,1)
labels.psu<-paste0(labels[1:6]," ",psupct,"%")

pdf("PSULaw.pdf",5,5)
par(mar=c(1,5,2,5))
pie(psu,labels=labels.psu,col=rainbow(length(psu)),
    main="Penn State Law",cex=0.8,init.angle=30)
dev.off()

# Northwestern:

NWU<-Schools[Schools$SchoolName=="Northwestern University",]
nwu<-t(NWU[,11:16])
nwupct<-round(nwu/sum(nwu)*100,1)
labels.nwu<-paste0(labels[1:6]," ",nwupct,"%")

pdf("NWULaw.pdf",5,5)
par(mar=c(1,5,2,5))
pie(nwu,labels=labels.nwu,col=rainbow(length(nwu)),
    main="Northwestern Law",cex=0.8,init.angle=20)
dev.off()

# Maryland:
UMD<-Schools[Schools$SchoolName=="University of Maryland",]
umd<-t(UMD[,11:16])
umdpct<-round(umd/sum(umd)*100,1)
labels.umd<-paste0(labels[1:6]," ",umdpct,"%")

pdf("UMDLaw.pdf",5,5)
par(mar=c(1,5,2,5))
pie(umd,labels=labels.umd,col=rainbow(length(umd)),
    main="University of Maryland Law",cex=0.8)
dev.off()

###################################################
# How about 

# Plot the percentages for **all** of the graduates,
# in all 198 schools:

sums2<-c(with(Schools,sum(N.BPR)),
         with(Schools,sum(N.JDA)),
         with(Schools,sum(N.Professional)),
         with(Schools,sum(N.NonProfessional)),
         with(Schools,sum(N.Unemployed)),
         with(Schools,sum(N.Undetermined)))
pct2 <- round(sums2/sum(sums2)*100,1)
labels2<-c("Bar Passage Required","JD-Advantage","Professional",
           "Non-Professional","Unemployed","Undetermined")
labels2.all<-paste0(labels2," (",pct2,"%)")

pdf("JDEmployment2.pdf",7,4)
par(mar=c(4,2,1,2))
dotchart(pct2,labels=labels2.all,pch=20,
         xlab="Percent of Graduates")
dev.off()

###################################################
# Now let's look at the gold standard first: full-time,
# long-term, bar-passage-required ("BPR") employment
# percentages:

pdf("BPR-FTLT-Histogram.pdf",6,5)
par(mar=c(6,4,2,2))
with(Schools,
     hist(BPR.FTLT.Percent,col="grey",main="",
          xlab="Percent Employed in Full-Time, Long-Term, 
          Bar-Passage-Required Positions"))
abline(v=mean(Schools$BPR.FTLT.Percent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# Top- and bottom-10 schools for "FTLT-BPR" positions
# in 2019:

rownames(Schools)<-NULL
Schools<-Schools[order(-Schools$BPR.FTLT.Percent),]
head(Schools[,c(2,39)],10,addrownums=FALSE) # top 10
tail(Schools[,c(2,39)],10,addrownums=FALSE) # bottom 10

# What percentage are employed in BPR positions of
# *any* kind:

pdf("BPR-All-Histogram.pdf",6,5)
par(mar=c(5,4,2,2))
with(Schools,
     hist(BPR.Percent,col="grey",main="",xlim=c(0,100),
          xlab="Percent Employed in All Bar-Passage-Required Positions"))
abline(v=mean(Schools$BPR.Percent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# Let's compare those two:

pdf("BPR-Scatterplot.pdf",6,5)
par(mar=c(6,6,2,2))
with(Schools,
     plot(BPR.FTLT.Percent,BPR.Percent,pch=20,
          ylim=c(0,100),
          xlab="Percent Employed in Full-Time, Long-Term, 
          Bar-Passage-Required Positions",
          ylab="Percent Employed in All\nBar-Passage-Required Positions"))
abline(a=0,b=1,lwd=2,col="red")
dev.off()

# Now do "JD-advantage" positions:

pdf("JDA-FTLT-Histogram.pdf",6,5)
par(mar=c(6,4,2,2))
with(Schools,
     hist(JDA.FTLT.Percent,col="grey",main="",
          xlab="Percent Employed in Full-Time, Long-Term, 
          JD-Advantage Positions"))
abline(v=mean(Schools$JDA.FTLT.Percent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topright",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# What percentage are employed in JDA positions of
# *any* kind:

pdf("JDA-All-Histogram.pdf",6,5)
par(mar=c(5,4,2,2))
with(Schools,
     hist(JDA.Percent,col="grey",main="",
          xlab="Percent Employed in All JD-Advantage Positions"))
abline(v=mean(Schools$JDA.Percent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topright",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# Let's compare those two:

pdf("JDA-Scatterplot.pdf",6,5)
par(mar=c(6,6,2,2))
with(Schools,
     plot(JDA.FTLT.Percent,JDA.Percent,pch=20,
          xlim=c(0,40),ylim=c(0,40),
          xlab="Percent Employed in Full-Time, Long-Term, 
          JD-Advantage Positions",
          ylab="Percent Employed in All\nJD-Advantage Positions"))
abline(a=0,b=1,lwd=2,col="red")
dev.off()

########################################
# Unemployment!

pdf("UE-Histogram.pdf",6,5)
par(mar=c(5,4,2,2))
with(Schools,
     hist(Unemployed.Percent,col="grey",main="",
          xlab="Percent Unemployed"))
abline(v=mean(Schools$Unemployed.Percent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topright",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# What correlates with unemployment?

r1<-round(with(Schools, cor(AdmitPercent,Unemployed.Percent,use="complete.obs")),2)
r2<-round(with(Schools, cor(MedianLSAT,Unemployed.Percent,use="complete.obs")),2)

# Plot:

pdf("UE-Correlates.pdf",8,5)
par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
with(Schools,plot(AdmitPercent,Unemployed.Percent,pch=20,
            xlab="Admission Percentage",ylab="Percent Unemployed"))
legend("topleft",bty="n",legend=paste0("r = ",r1))
with(Schools,plot(MedianLSAT,Unemployed.Percent,pch=20,
                  xlab="Median LSAT",ylab="Percent Unemployed"))
legend("topright",bty="n",legend=paste0("r = ",r2))
dev.off()

######################################################
# Now, the "states" analysis. We have data on the 
# number of graduates from each law school who went to 
# work in the "top" state for employing that school's
# graduates. 
#
# First: What is the distribution of the "top state"
# percentages?

pdf("TopState-Histogram.pdf",6,5)
par(mar=c(5,4,2,2))
with(Schools,
     hist(TopStatePercent,col="grey",main="",
          xlab="Percent in 'Top' State",
          xlim=c(0,100)))
abline(v=mean(Schools$TopStatePercent,na.rm=TRUE),
       lwd=3,lty=2)
legend("topleft",bty="n",lwd=3,lty=2,legend="Mean Percent")
dev.off()

# What are the schools that place the highest and lowest
# percentages of graduates in their "top" state?

rownames(Schools)<-NULL
Schools<-Schools[order(-Schools$TopStatePercent),]
head(Schools[,c(2,46)],10,addrownums=FALSE) # top 10
tail(Schools[,c(2,46)],10,addrownums=FALSE) # bottom 10

# Now: Let's look at this by state:

states<-group_by(Schools,TopState)
States<-summarise(states,
                  NSchools=n(),
                  NHires=sum(N.TopState))
rm(states)

States$State<-as.character(States$TopState) # make character

# Add FIPS codes (needed for plotting):

States$abbr<-States$State
States<-merge(usmap::statepop,States,by=c("abbr"),all=TRUE)

# Add AK:

States$State<-ifelse(States$abbr=="AK","AK",States$State)
States$NSchools<-ifelse(States$abbr=="AK",0,States$NSchools)
States$NHires<-ifelse(States$abbr=="AK",0,States$NHires)

# Also: Remove Puerto Rico...

States<-States[States$State!="PR",]

# Colors:

low<-"yellow"
high<-"navy"

# Map: Number of Law Schools for which each state is the 
# "top state":

pdf("TopStateMap.pdf",10,8)
par(mar=c(1,1,1,1))
plot_usmap(data=States,values="NSchools",color="black",
           labels=TRUE) + 
  scale_fill_continuous(low=low, high=high,
                        name="N of Law Schools' Top State",
                        label=scales::comma) + 
  theme(legend.position="right")
dev.off()
