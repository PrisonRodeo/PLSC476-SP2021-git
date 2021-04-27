##############################################
# This is some code from PLSC 476,
# dated April 27, 2021.
#
# It contains code for scraping and 
# analyzing data from U.S. SEC litigation
# releases; a good landing page is:
#
# https://www.sec.gov/litigation/litreleases.htm
#
##############################################
# Load some useful R packages... (install as 
# necessary):

library(xml2)
library(rvest)
library(stringr)
library(lubridate)
library(slam)
library(tm)

# Also be sure to -setwd()-:
#
# setwd("~/Dropbox/Enemies/Carrot Top")
# setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")
###############################################
# Let's read some data. First, get a list of
# all the webpages for litigation releases
# during 2019:

url<-"https://www.sec.gov/litigation/litreleases/litrelarchive/litarchive2019.shtml"
pg<-read_html(url)
linx<-html_attr(html_nodes(pg, "a"), "href")
mylinx<-linx[grep("/litigation/litreleases/2019/",linx)]
mylinx<-mylinx[grep(".htm",mylinx)]
rm(url,pg,linx)
mylinx

# That's a reasonably big list of URLs -- 321 
# of them. We're going to read each of those webpages
# into R, retaining the main text content with the
# description of the litigation. 
#
# In order to prevent being mistaken for a DDOS
# attack, and to make things much, much faster, 
# we'll start by downloading all 321 web pages 
# to our local computer. That way, we can work 
# with local versions of the texts, and only have 
# to do the (slow) downloading part once. That 
# code looks something like this:

N<-length(mylinx)
dir.create("2019") # make a folder for them...

for(i in 1:N){  # NOTE: this takes a hot second to run...
  url<-paste0("http://sec.gov",mylinx[i])
  dest<-paste0("2019/SEC",i,".htm")
  download.file(url,dest)
}

rm(url,dest) # clean up

# (Once you've run that code once, be sure to 
# comment it out, so that you don't go grab the
# data from the website again.)
#
# Now we'll extract the content that we want 
# to examine. That information is included 
# in each webpage's source code, in the content
# of "<div id="main-content">". So:

metadata<-character(0)    # titles
mytexts<-character(0)   # full text

for(i in 1:N){    
  doc<-paste0("2019/SEC",i,".htm")
  page<-read_html(doc)
  main<-html_nodes(page,"#main-content")
  m1<-try(html_text(html_nodes(main,"h1")))
  if(inherits(m1,"try-error")){
    m1<-paste0("")
  }
  h2<-html_nodes(main,"h2")
  m2<-try(html_text(h2[1]))
  if(inherits(m1,"try-error")){
    m2<-paste0("")
  }
  m3<-try(html_text(h2[2]))
  if(inherits(m3,"try-error")){
    m1<-paste0("")
  }
  metadata[i]<-paste0(m1,"; ",m2,";; ",m3)
  mytexts[i]<-html_text(main,trim=TRUE) # all text
}

# Now remove the "\n"s in the text:

mytexts<-gsub("\n","",mytexts)
rm(i,doc,main,page,h2,m1,m2,m3)  # ...and clean up

# Great. The text file of that content isn't that
# large at all (about 850K), so it will be easy to 
# work with. 
#
# Now we can turn that mass of 321 texts into a data
# frame:

SEC.df<-data.frame(doc_id=seq(1,N,by=1),
                   text=mytexts,
                   stringsAsFactors=FALSE)

# Add some "metadata" -- characteristics of each of the
# various SEC announcements -- from the metadata object.
# For example, the title of the litigation press release:

titles<-sub("\\;.*", "", metadata)
SEC.df$Title<-titles

# We should probably add the SEC's "Litigation
# Release Number" if we can; that will make it
# easier for human beings to find specific releases
# later on...

foo<-str_match(metadata, ";\\s*(.*?)\\s*;;")[,2]
LRN<-str_match(foo, "No\\.\\s*(.*?)\\s*/")[,2]
SEC.df$LRN<-LRN
SEC.df$LRN<-as.numeric(SEC.df$LRN)

# (In case you're wondering what is going on there,
# those are regular expressions, which R handles
# similarly to other programming languages.)
#
# And dates:

dates<-trimws(sub(".*/", "", foo))
SEC.df$Date<-dates
SEC.df$Date<-mdy(SEC.df$Date)

rm(titles,foo,LRN,dates) # clean up

# Note that, thanks to some inconsistencies
# in the HTML formatting of the SEC press releases,
# there are some missing values in these
# metadata variables. Those are things that
# one might have to fix "by hand," if they
# were important.
#
# Here's a histogram of release dates for
# the press releases:

pdf("SEC-Date-Histogram.pdf",6,5)
par(mar=c(4,4,2,2))
hist(SEC.df$Date,breaks="months",freq=TRUE,
     col="grey",main="",ylim=c(0,50),
     xlab="Date",cex.axis=0.8)
dev.off()

#
# Now, turn that into a -tm- corpus:

SEC<-VCorpus(DataframeSource(SEC.df))
SEC

# Next, create a document-term matrix. Notice 
# that we're stemming / lemmatizing the words 
# here, and we're not retaining stopwords,
# capitalization, or numbers:

SEC.DTM<-DocumentTermMatrix(SEC,
             control=list(removePunctuation=TRUE,
                          tolower=TRUE,
                          stopwords=TRUE,
                          removeNumbers=TRUE,
                          stemming=TRUE))

rownames(SEC.DTM)<-SEC.df$LRN  # document IDs...
SEC.DTM  # print a description of this...

# Now, some basic "search"-type things. For example,
# if we want to see every single document that includes 
# any variant of the word "fraud" in it, we can do this:

frauds<-SEC.DTM[,grepl("fraud",SEC.DTM$dimnames$Terms)]
frauds
inspect(frauds)

# That gives us a DTM that only contains terms related
# to the word stem "fraud." 
#
# Which of those have the "most" focus on fraud?
# We can sort them by how often "fraud"-related
# terms appear in them:

frauds<-frauds[order(-row_sums(frauds)),]

pdf("SEC-Fraud-Barplot.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(row_sums(frauds[1:28,]),ylim=c(0,12),
        ylab="Instances of 'fraud'",las=2)
dev.off()

# Next, we might want to use TF-IDF weights, to
# single out documents that are especially related to 
# fraud:

SEC.TFIDF<-weightTfIdf(SEC.DTM) #TF-IDF weighting 
SEC.TFIDF
inspect(SEC.TFIDF)

# Great. Now, which documents are the most
# "fraud"-y, based on their TF-IDF score?
# Make an new "frauds" object, but now using
# the TF-IDF scores:

frauds.tfidf<-SEC.TFIDF[,grepl("fraud",SEC.TFIDF$dimnames$Terms)]
inspect(frauds.tfidf)

frauds.tfidf<-frauds.tfidf[order(-row_sums(frauds.tfidf)),]

pdf("SEC-Fraud-Barplot-TFIDF.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(row_sums(frauds.tfidf[1:20,]),
        ylab="TF-IDF for 'fraud'",las=2)
dev.off()

# We could do the same for the phrase "insider
# trading":

IT<-SEC.TFIDF[,grepl("insid",SEC.DTM$dimnames$Terms)]
inspect(IT)

IT<-IT[order(-row_sums(IT)),]

pdf("SEC-IT-Barplot-TFIDF.pdf",7,5)
par(mar=c(4,4,2,2))
barplot(row_sums(IT[1:20,]),
        ylab="TF-IDF for 'insider'",las=2)
dev.off()

# Now make the full TF-IDF DTM a standard data frame, to
# see what we can do with it:

SEC.TFIDF.df<-data.frame(as.matrix(SEC.TFIDF))
SEC.TFIDF.df$LRN<-SEC.df$LRN

# Let's see which documents are joint-highest 
# on both "fraud" and "insider":

pdf("SEC-Fraud-IT-Scatter.pdf",6,5)
par(mar=c(4,4,2,2))
with(SEC.TFIDF.df, 
     plot(fraud,insid,pch="",
          xlim=c(-0.001,0.035),
          xlab="`fraud' TF-IDF",
          ylab="`insider' TF-IDF"))
with(SEC.TFIDF.df, 
     text(fraud,insid,labels=SEC.TFIDF.df$LRN,
          cex=0.8))
dev.off()
