
# Explorative Data Analysis

# Name: Ali Darvishi Koushali


# Libraries
library(sp)
library(gstat)
library(MASS)

# data set
testdata <- read.table("testData123.csv", header = TRUE, sep = ",")
colnames(testdata)
sapply(testdata, class)
head(testdata)
summary(testdata)

# Description of the data set

# rows and columns
dim(testdata)
# variables
colnames(testdata)
# types
sapply(testdata, class)
# missing values
colSums(is.na(testdata))
# summary
summary(testdata)
# variance of variables
var(testdata$Co)
var(testdata$Ni)
# scatter plot
library(MASS)
eqscplot(testdata$x, testdata$y, pch = 19, xlab = "x", ylab = "y",
         main = "spatial distribution")

# spatial dataset 
ds = SpatialPointsDataFrame(testdata[,c("x","y")],testdata)


# spatial distribution
plot(ds)  
bubble(ds,"Co")
require("MASS")


# color funtion
cf = function(x) {
  crange = range(x)
  hsv(0.7*(crange[2]-x)/(crange[2]-crange[1]),0.7,0.7)
}

eqscplot(testdata$x,testdata$y,col=cf(testdata$Co),pch=19,xlab="x",ylab="y")
legend("right",title="Co",legend=round(seq(min(testdata$Co),max(testdata$Co),length.out=5),digits=2),
       fill=cf(round(seq(min(testdata$Co),max(testdata$Co),length.out=5),digits=2)))

eqscplot(testdata$x,testdata$y,col=cf(testdata$Ni),pch=19,xlab="x",ylab="y")
legend("right",title="Ni",legend=round(seq(min(testdata$Ni),max(testdata$Ni),length.out=5),digits=2),
       fill=cf(round(seq(min(testdata$Ni),max(testdata$Ni),length.out=5),digits=2)))




#check the distribution of the data and check for outliers
# for Co 
par(mfrow=c(2,2))
hist(testdata$Co, main="histogram for Co")
qqnorm(testdata$Co)
boxplot(testdata$Co, main="boxplot for Co")
#EDF
plot(ecdf(testdata$Co)) 

# for Ni
hist(testdata$Ni,main="histogram for Ni")
qqnorm(testdata$Ni)
boxplot(testdata$Ni,main="boxplot for Ni")
#edf
plot(ecdf(testdata$Ni))
par(mfrow=c(1,1))


boxplot(testdata$Co, main="boxplot for Co")
boxplot(testdata$Ni, main="boxplot for Ni")


# Correlation
pairs(testdata,pch=".") 
cor(testdata)
plot(testdata$Co,testdata$Ni,xlab="Co",ylab="Ni", main="correlation between Co and Ni")
modelC <- lm( testdata$Ni ~ testdata$Co)
modelC
abline(modelC, col = 4, lwd = 3)

