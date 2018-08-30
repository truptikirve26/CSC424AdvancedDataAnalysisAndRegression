
library(foreign)  # Allows us to read spss files!
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(stats)

PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}


# setting the path for working directory
setwd("C:\\Trupti MS\\CSC424AdvDataAnalaysis\\Assignments\\Assignment2\\ADVANCED DATA ANALYSIS - 2017-2018 Spring - 492018 - 544 PM")

#read dataset
bfi <- read.csv("C:/Trupti MS/CSC424AdvDataAnalaysis/Assignments/Assignment2/ADVANCED DATA ANALYSIS - 2017-2018 Spring - 492018 - 544 PM/bfi.csv")
View(bfi)


# redaing the first few records
head(bfi)

# removing the first col
dataset <- bfi[2:28]
dataset

# extracting only numeric data
numericdataset <- dataset[1:25]
numericdataset
str(numericdataset)

#plotting the numeric data
plot(numericdataset)
head(numericdataset)

reduceddata <- na.omit(numericdataset)


#problem 5 A
pbfi <- prcomp(reduceddata,center=T,scale=T)
print(pbfi)
plot(pbfi)
abline(1,0)

screeplot(pbfi)
abline(1,0)



summary(pbfi)


pbfi$center
pbfi$scale
pbfi$rotation



#problem 5 B
library(psych)
p2 = psych::principal(reduceddata, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.4, sort=T)
p2$loadings


#c
rescore = p2$scores
rescore
min(rescore[,1])
max(rescore[,1])
min(rescore[,2])
max(rescore[,2])
min(rescore[,3])
max(rescore[,3])
min(rescore[,4])
max(rescore[,4])
min(rescore[,5])
max(rescore[,5])





#D
fit = factanal(reduceddata, 5)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)


# some extra work around problem 5
#resultant PCA

biplot(pbfi,scale = 0)

std_dev <- pbfi$sdev

pr_var = std_dev^2


pr_var[1:25]

prop_varex  <- pr_var/sum(pr_var)

prop_varex[1:25]

#screeplot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")


#screeplot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


plot(pbfi)
abline(1,0)

#########################################################################################################################################

#problem 6



#part A
"
covData = prcomp(hbat, center=T, scale=F)
cov.covData=cov(hbat)
round(cov.covData,2)
data.cov.pca <- princomp(covmat=cov.covData) 
data.cov.pca$loadings

summary(covData)
print(covData)
"

censusdata <- read.csv("C:/Trupti MS/CSC424AdvDataAnalaysis/Assignments/Assignment2/ADVANCED DATA ANALYSIS - 2017-2018 Spring - 492018 - 544 PM/Census2.csv")
View(censusdata)

str(censusdata)

CovariantData = prcomp(censusdata,center=T,scale.=F)
CovariantData
covmat1 = cov(censusdata)
round(covmat1,2)

covmatpca = princomp(covmat=covmat1)
covmatpca$loadings

summary(CovariantData)
print(CovariantData)


#part B

scaleddata <- transform(censusdata, scaleddata=MedianHomeVal/100000)
head(scaleddata)
CovariantData1 = prcomp(scaleddata, center=T, scale=F)
cov.CovariantData1=cov(scaleddata)
round(cov.CovariantData1,2)
scaleddata.cov.pca <- princomp(covmat=cov.CovariantData1) 
scaleddata.cov.pca$loadings

summary(CovariantData1)
print(CovariantData1)
biplot(CovariantData1)

#Partc

corNewData = prcomp(censusdata, center=T, scale=T)
cor.corNewData=cor(censusdata)
cor.corNewData
Newdata.cor.pca <- princomp(covmat=cor.corNewData) 
Newdata.cor.pca$loadings

summary(corNewData)
print(corNewData)
biplot(corNewData)
############################################################################################################################################

#Problem 7


health <- read.csv("C:/Trupti MS/CSC424AdvDataAnalaysis/Assignments/Assignment2/ADVANCED DATA ANALYSIS - 2017-2018 Spring - 492018 - 544 PM/GSS_2002_Health_PCA.csv")
head(health)


str(health)

healthdatset = health[2:34]
head(healthdatset)

dim(healthdatset)
cor.health = cor(healthdatset)
cor.health


library(psych)
options("scipen"=100, "digits"=5)
round(cor.health, 2)
MCorrTest = corr.test(cor.health, adjust="none")
MCorrTest

M = MCorrTest$p
M

corrplot(cor.health, method="shade")

#p = prcomp(healthdata,center=T,scale.=T)
#p

M = cor.health$p
M

MTest = ifelse(M < .03, T, F)
MTest

colSums(MTest) - 1

#removing the variables which are not at all correlated, we can use those variables as it is
healthReduced = healthdatset[, -c(1, 7, 16,25)]

head(healthReduced)
dim(healthReduced)
corhealthreduced = cor(healthReduced)
round(corhealthreduced,2)


library(psych)
options("scipen"=100, "digits"=5)
round(corhealthreduced, 2)
MCorrTest = corr.test(corhealthreduced, adjust="none")
MCorrTest

M = MCorrTest$p
M

corrplot(corhealthreduced, method="number")

MTest = ifelse(M < .03, T, F)
MTest

colSums(MTest) - 1



#remove  7 docaskme, health30 22  These two variables are correlated to 1 and 2 other vraibles respectively but the
#correlation is weak and hence removing those variables

healthReduced1 = healthReduced[, -c( 7, 22)]
dim(healthReduced1)
head(healthReduced1)
corhealthreduced1 = cor(healthReduced1)
round(corhealthreduced1,2)



library(psych)
options("scipen"=100, "digits"=5)
round(corhealthreduced1, 2)
MCorrTest1 = corr.test(corhealthreduced1, adjust="none")
MCorrTest1

M1 = MCorrTest1$p
M1

corrplot(corhealthreduced1, method="number")

MTest1 = ifelse(M < .03, T, F)
MTest1

colSums(MTest1) - 1

#################################################################################################################################

helathpca = prcomp(healthReduced1,center=T,scale=T)
print(helathpca)
plot(helathpca)
abline(1,0)

screeplot(helathpca)
abline(1,0)

summary(p)
print(p)
biplot(helathpca)


library(psych)
p2 = psych::principal(healthReduced1, rotate="varimax", nfactors=5, scores=TRUE)
p2
print(p2$loadings, cutoff=.4, sort=T)



#B

#with 5 factors
fit = factanal(healthReduced1, 5)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)


#with 4 factors
fit = factanal(healthReduced1, 4)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)


#with 3 factors
fit = factanal(healthReduced1, 3)
print(fit$loadings, cutoff=.4, sort=T)
summary(fit)




