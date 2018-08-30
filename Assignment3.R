library(CCA)
library(yacca)
library(MASS)
library(ggplot2)
library(GGally)

ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}


canon <- read.delim("~/canon.txt")
View(canon)


head(canon)

is.na(canon)

reduceddata <- na.omit(canon)


Attitude = reduceddata[c(7,8,6,10)]
Health = reduceddata[c(5,4,2,3,9)]



head(Attitude) 
head(Health)

#correlation for individual dataframes
ggpairs(Attitude)
ggpairs(Health)


"c1 = cancor(Attitude, Health)
c1
"

#correlations within two variable sets using matcor
matcor(Attitude, Health)


ccCanon = cc(Attitude, Health)

#displays canonical correlations
ccCanon$cor

# raw canonical coeffients
ccCanon[3:4]
ls(ccCanon)



# compute canonical loadings
cc2 <- comput(Attitude,Health,ccCanon)
cc2[3:6]


# to understand the significance of the variates
wilksCan = ccaWilks(Attitude, Health, ccCanon)
round(wilksCan, 2)


#compute the scores 
loadingsCanon = comput(Attitude, Health, ccCanon)
ls(loadingsCanon)

loadingsCanon$corr.X.xscores

loadingsCanon$corr.Y.yscores
loadingsCanon$corr.X.yscores
loadingsCanon$corr.Y.xscores
loadingsCanon$xscores
loadingsCanon$yscores


# standardized coefficients
s1 = diag(sqrt(diag(cov(Attitude))))
s1 %*% ccCanon$xcoef

s2 = diag(sqrt(diag(cov(Health))))
s2 %*% ccCanon$ycoef



#######################################

library(yacca)
c2 = cca(Attitude,Health)
c2


c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)





###########################################################################################
#Correspondance Analysis
############################################################################################

# Reading data
mytable = matrix(c(230,329,177,34,6,400,471,237,28,12,1010,530,141,21,11,201,639,208,72,14,365,478,305,50,97),byrow = TRUE,ncol=5)
colnames(mytable) = c("Agree_Strongly","Agree","Neither_nor","Disagree","Disagree_Strongly")
rownames(mytable) = c("UK","USA","Russia","Spain","France")



#1 Mosaic Plot
library(vcd)
mosaic(mytable,shade=TRUE,length=TRUE)


#2 
chisq.test(mytable)

fit = ca(mytable)
summary(fit)
fit

#plot CA

plot(fit)



plot(fit, mass=T, contrib="absolute", 
     map="rowgreen", arrows=c(T, T))


plot.ca(fit, mass=T, contrib="relative", 
     map="rowgreen", arrows=c(T, F))

