#######################################
# This is R code for the PLSC 476
# practicum on "personal attribute
# models," dated February 18, 2021.
#
# It's code to do the analyses described 
# in the example research module, which
# is available on the course github
# repo, in the "Assignments" folder:
#
# https://github.com/PrisonRodeo/PLSC476-SP2021-git/tree/master/Assignments
#
# Note that anything on a line
# that starts with a "#" is a 
# comment, and will be ignored by
# the program (but, one hopes, not
# by you).
#######################################
# Load a useful R package:

library(RCurl)

# Set your working directory:

# setwd("My Files") # <- change this line, and 
                    #    uncomment it to run it

# Other options:

options(scipen = 6) # bias against scientific 
                    # notation
options(digits = 3) # show fewer decimal places

##############################################
# READ IN SOME DATA...
#
# SCDB data:

ZIP<-tempfile()
download.file("http://scdb.wustl.edu/_brickFiles/2020_01/SCDB_2020_01_justiceCentered_Docket.csv.zip",ZIP)
Votes<-read.csv(unz(ZIP,"SCDB_2020_01_justiceCentered_Docket.csv"),
                  stringsAsFactors = FALSE)
unlink(ZIP)
rm(ZIP)       # clean up...

# Biographical data:

Justices<-read_dta("http://epstein.wustl.edu/research/justicesdata.dta")

# Alternatively, you can download and unzip 
# the data into a folder on your computer 
# and read it in "locally" like this:
#
# Votes<-read.csv("SCDB_2020_01_justiceCentered_Docket.csv")
# Justices<-read_dta("justicesdata.dta")

##################################
# MERGING DATA 
#
# Create a variable for the justices' ID in the "Justices"
# data that matches the one in the "Votes" data:

Justices$justice <- Justices$spaethid

# Eliminate unnecessary / extraneous rows in the
# "Justices" data. This line gets rid of justices
# who didn't serve during the 1946-2019 terms:

Justices<-Justices[Justices$justice>=min(Votes$justice),]

# This line eliminates rows in the data corresponding
# to nominees who were not confirmed, etc.:

Justices<-Justices[Justices$success==1,]

# Now look at the justice ID variable:

table(Justices$justice)

# Notice that there are two lines of data with 
# "justice=102." That's justice Rehnquist; he's
# in the data twice because he was confirmed
# twice, once as an associate justice and once
# as chief justice. Because we're just using his
# biographical data, which doesn't change
# across the two nominations, we just need to
# get rid of one of these two rows (it doesn't 
# much matter which one we delete):

Justices<-Justices[Justices$id!=160, ]

# Now, MERGE those two together:

Master <- merge(Votes,Justices,by=c("justice"),
                all.x=TRUE,all.y=FALSE)

# Note that the new data frame called "Master" has 
# the same number of rows as the "Votes" data frame,
# but a number of columns equal to the number in
# "Votes" plus the number in "Justices" (minus one,
# since the merge variable "justice" appears in 
# both).
#
# Now, extract only the variables we are interested in and/or
# will need for the analysis. These include the justices' 
# backgrounds (urban / rural), the issue in each case, and the 
# vote of each justice, along with some justice and case
# identifiers.

MyVars <- c("justice","docketId","voteId","issue",
            "issueArea","direction","childsur")
Data<-Master[MyVars] # subsets the data

#########################################
# RECODING VARIABLES
#
# Next, we'll want to ensure that our variables measure things
# the way we want them to. First, let's look at the "vote"
# variable:

table(Data$direction)

# That is coded strangely, with liberal votes coded as "1"
# and conservative as "2". Let's make it better:

Data$LiberalVote <- Data$direction-1

# That should give us a variable that =1 if the justice 
# voted "liberally" and 0 if not. Let's check:

with(Data, xtabs(~direction+LiberalVote))

# Now liberal votes are "1" and conservative are "0". Next, 
# examine the "background" variable:

table(Data$childsur)

# There are three values present in the data: family 
# farm (=1), small town (=3), and urban (=5). Let's
# make a variable that =1 if the justice was from 
# an urban area, and 0 otherwise:

Data$Urban <- ifelse(Data$childsur==5,1,0)

# Check to make sure it "worked":

with(Data, xtabs(~childsur+Urban))

#########################################
# SUBSET THE DATA BY ISSUE
#
# Next, we want to draw out the cases in the data that 
# address the four issues we discussed. We can see from looking
# at the codebook that:

#  - Racial discrimination cases are issue numbers 20040 and
# 20050 (desegregation), 20070 (affirmative action), and 20080
# (protests re: racial discrimination). 

#  - Education cases are harder to locate; for now, we'll just
# look at those with issue code 20050 (school desegregation),
# 30180 (parochaid), and 20290 (immigrants' access to education).

#  - Crime is probably best just done using issueArea code #1
# (criminal cases)

#  - Gender discrimination issue codes are 20130 and 20140.
#
# Here's one way to subset the data...

Data$keep<-0
Data$keep<-ifelse(Data$issue==20040,1,Data$keep)
Data$keep<-ifelse(Data$issue==20050,1,Data$keep)
Data$keep<-ifelse(Data$issue==20070,1,Data$keep)
Data$keep<-ifelse(Data$issue==20080,1,Data$keep)
RaceData <- Data[Data$keep==1,]

# Note that we now have a "smaller" dataset comprising only
# cases from those four issue areas.

table(RaceData$issue)

# We can do a similar thing for the other issues:

Data$keep<-0
Data$keep<-ifelse(Data$issue==20050,1,Data$keep)
Data$keep<-ifelse(Data$issue==30180,1,Data$keep)
Data$keep<-ifelse(Data$issue==20290,1,Data$keep)
EdData <- Data[Data$keep==1,]  # Education

CrimeData<-Data[Data$issueArea==1,]  # Criminal cases

Data$keep<-0
Data$keep<-ifelse(Data$issue==20130,1,Data$keep)
Data$keep<-ifelse(Data$issue==20140,1,Data$keep)
SexData <- Data[Data$keep==1,]  # Gender

####################################################
# DESCRIPTION
#
# Now we can begin to look at the distribution of 
# the "outcomes" of interest for each data set. We'll
# start with liberal voting percentages in each type
# of case:

prop.table(table(RaceData$LiberalVote))*100
prop.table(table(EdData$LiberalVote))*100
prop.table(table(CrimeData$LiberalVote))*100
prop.table(table(SexData$LiberalVote))*100

# We can do the same thing with plots, e.g.:

barplot(prop.table(table(RaceData$LiberalVote))*100,
        names.arg=c("Conservative","Liberal"))

# We can also do a quick description of the "Urban"
# variable in the "entire" data set:

barplot(prop.table(table(Data$Urban))*100,
        names.arg=c("Rural","Urban"))

# And we can look at and compare the means of the "urban"
# variable across the four subsets of cases. These ought
# (we hope) to be roughly the same:

UrbanMeans<-c(mean(Data$Urban,na.rm=TRUE),
              mean(RaceData$Urban,na.rm=TRUE),
              mean(EdData$Urban,na.rm=TRUE),
              mean(CrimeData$Urban,na.rm=TRUE),
              mean(SexData$Urban,na.rm=TRUE))
UrbanMeans

# A picture is more compelling:

barplot(UrbanMeans*100,cex.names=0.6,
    names.arg=c("All","Race","Education","Criminal","Gender"))

#################################################
# BIVARIATE ASSOCIATION
#
# Finally, we can begin assessing the association between
# voting and urban / rural backgrounds. Since our two
# variables are both binary / dichotomous, we can do that
# most simply by looking at a cross-table of voting and
# urban/rural background.
#
# We'll start with "all" the data / every vote since 1946,
# even though this won't be part of our research
# module:

with(Data, prop.table(xtabs(~LiberalVote+Urban),2)) * 100

# This indicates that justices from rural backgrounds 
# vote liberally in all cases roughly 57.6 percent of
# the time, while those from urban backgrounds vote
# liberally a bit more than 50.3 percent of the time.
#
# We can look at the same crosstabs for the various
# subsets of cases that we created:

with(RaceData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(EdData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(CrimeData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
with(SexData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100

################################################################
# Now we'll walk through the actual analyses and 
# write-up of the results. The structure follows what
# is on the slides from the February 18 class: Begin 
# with the question / theory, then discuss the hypothesis,
# then the data and the measurement, then univariate
# descriptions, then bivariate analyses.
#
# One thing to recall is how to create graphics files.
# We put a "wrapper" around a graphics command that 
# creates a file. Type:
#
?png
#
# for more details. So, to create a PNG file of a 
# barplot of the liberal voting percentages in 
# racial discrimination cases, we use: 

png("RaceBarplot.png",640,640) #  <-- create a .PNG file
barplot(prop.table(table(RaceData$LiberalVote))*100,
        names.arg=c("Conservative","Liberal")) # <-- plot...
title(main="Liberal Voting Percentage: Racial 
      Discrimination Cases")   # <-- title...
dev.off()                      # <-- end creation of the .PNG

# A couple things here.
#
# 1. We can also create .JPGs, .PDFs, etc. by using
# the corresponding commands. So:
#
# > pdf("title.pdf",6,6)
# > ...
# > dev.off()
#
# ...creates a PDF file. 
#
# 2. The graphics files created will be saved in
# whatever your current "working" directory in R
# is. If you want them somewhere else, you have
# to specify a directory path.
#
# For our modules, sometimes it's useful to 
# combine several plots into a single file. We
# can do that using -mfrow-:

png("OutcomesBarplot.png",720,720)
par(mfrow=c(2,2))   # draw four figures: 2 rows, 2 cols
barplot(prop.table(table(RaceData$LiberalVote))*100, # <--#1
        names.arg=c("Conservative","Liberal"))
title(main="Liberal Voting Percentage: Racial 
      Discrimination Cases") # Race cases
barplot(prop.table(table(EdData$LiberalVote))*100,   # <--#2
        names.arg=c("Conservative","Liberal"))
title(main="Liberal Voting Percentage: Education
      Cases") # Education cases
barplot(prop.table(table(CrimeData$LiberalVote))*100, # <--#3
        names.arg=c("Conservative","Liberal"))
title(main="Liberal Voting Percentage: Criminal
      Cases") # Education cases
barplot(prop.table(table(SexData$LiberalVote))*100,  # <--#4
        names.arg=c("Conservative","Liberal"))
title(main="Liberal Voting Percentage: Gender
      Discrimination Cases") # Gender cases
par(mfrow=c(1,1)) # reset this to original
dev.off()

# This is Figure 1 in the example module.
#
# We'll also want to describe the justices in terms
# of urban vs. rural. We can do this by simply
# going back to the "Justices" data:

Justices$Urban<-ifelse(Justices$childsur==5,1,0)

# Now plot that:

png("UrbanBarplot.png",480,480)
barplot(prop.table(table(Justices$Urban))*100,
        names.arg=c("Non-Urban","Urban"),
        ylab="Percentage")
title(main="Urban and Non-Urban Backgrounds of the
      Justices, 1946-2019 (N=40)") # Gender cases
dev.off()

# This is Figure 2 in the example module.
#
# Finally, we want to analyze the association 
# between the two variables. First, the crosstabs:

RaceCorr<-with(RaceData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
EdCorr<-with(EdData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
CrimeCorr<-with(CrimeData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100
SexCorr<-with(SexData, prop.table(xtabs(~LiberalVote+Urban),2)) * 100

# We can use the values in these objects to calculate
# the differences in liberal voting between urban
# and rural justices. In particular, the second "row"
# of each crosstab has the percentage of times each
# type of justice case a liberal vote. For example: 

RaceCorr

# This means that justices with non-urban backgrounds
# (Urban=0, the first column of the table) cast
# liberal votes (LiberalVote=1) in 79.8% of all 
# racial discrimination cases, and cast conservative 
# votes (LiberalVote=0) in 20.2% of those cases. 
# In the second column, we see that justices from
# urban backgrounds (Urban=1) cast liberal votes
# 69.4% of the time in racial discrimination cases,
# and cast conservative votes 30.6% of the time.

# These tables also let us calculate the differences
# in the voting percentages across the two groups:

RaceCorr[2,2]-RaceCorr[2,1]
EdCorr[2,2]-EdCorr[2,1]
CrimeCorr[2,2]-CrimeCorr[2,1]
SexCorr[2,2]-SexCorr[2,1]

# Now, t-tests for differences in the means

with(RaceData, t.test(LiberalVote~Urban))
with(EdData, t.test(LiberalVote~Urban))
with(CrimeData, t.test(LiberalVote~Urban))
with(SexData, t.test(LiberalVote~Urban))

# Alternatively, we could have presented the same
# results as a four-way set of barplots:

png("VoteByUrban.png",720,720)
par(mfrow=c(2,2)) # 2x2 graphs
R<-barplot(RaceCorr[2,],names.arg=c("Rural","Urban"),
        ylim=c(0,90))
text(R,RaceCorr[2,],labels=round(RaceCorr[2,],1),pos=3)
title(main="Liberal Voting Percentages: Racial
      Discrimination Cases (t = 5.3)") # Race cases
E<-barplot(EdCorr[2,],names.arg=c("Rural","Urban"),
        ylim=c(0,90))
text(R,EdCorr[2,],labels=round(EdCorr[2,],1),pos=3)
title(main="Liberal Voting Percentages: Education
      Cases (t = 3.2)") # Race cases
C<-barplot(CrimeCorr[2,],names.arg=c("Rural","Urban"),
        ylim=c(0,90))
text(R,CrimeCorr[2,],labels=round(CrimeCorr[2,],1),pos=3)
title(main="Liberal Voting Percentages: Criminal 
      Cases (t = 7.6)") # Race cases
S<-barplot(SexCorr[2,],names.arg=c("Rural","Urban"),
        ylim=c(0,90))
text(R,SexCorr[2,],labels=round(SexCorr[2,],1),pos=3)
title(main="Liberal Voting Percentages: Sex
      Discrimination Cases (t = 1.7)") # Race cases
par(mfrow=c(1,1)) # reset
dev.off()

# This is the (alternative / hypothetical) Figure 3
# in the example module.