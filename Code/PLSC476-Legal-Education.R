#####################################
# PLSC 476 - ELS
#
# March 30, 2021: "Legal Education"
#####################################
# All this code does is draw the 
# little figure illustrating constant
# vs. increasing vs. decreasing
# returns...
#
# setwd():

setwd("~/Dropbox (Personal)/PLSC 476/Notes and Slides")

# Figure things...

X<-seq(0.1,4,by=0.05)
C<-X
I<-exp(X)
I<-(I/max(I))*4
D<-log(X)
D<-(D+abs(min(D)))*(4/3.69)

df<-data.frame(cbind(X,C,I,D))

pdf("Returns.pdf",6,5)
par(mar=c(4,4,2,2))
with(df, plot(X,C,t="l",lwd=2,xlab="Predictor Variable",
              ylab="LGPA",xaxt="n"))
with(df, lines(X,I,lwd=2,lty=2,col="darkblue"))
with(df, lines(X,D,lwd=2,lty=3,col="orange"))
text(1.4,2,"Linear Returns")
text(2.8,0.5,"Increasing Returns",col="darkblue")
text(1.8,3.6,"Decreasing Returns",col="orange")
dev.off()
