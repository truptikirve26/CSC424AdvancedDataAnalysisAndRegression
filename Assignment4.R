library(classInt)
library(readxl)
library(corrplot)
library(CCA)
library(yacca)
library(MASS)
library(ggplot2)
library(GGally)

#a)

#reading the data from training tab
BondRating <- read_excel("C:/Trupti MS/CSC424AdvDataAnalaysis/Assignments/Assignment4/BondRating.xls")
View(BondRating)

dim(BondRating)



#performing LDA
BRLDA = lda(BondRating$RATING ~ BondRating$LOPMAR + BondRating$LFIXCHAR + BondRating$LGEARRAT + BondRating$LTDCAP + BondRating$LLEVER +BondRating$LCASHLTD + BondRating$LACIDRAT + BondRating$LCURRAT + BondRating$LRECTURN + BondRating$LASSLTD)
BRLDA

plot(BRLDA)

#plot(BRLDA, dimen=1, type="both")

#variables from col 4 to col13 are chosen as the col1 is observation no., col 2 is the grp name and col3 is grp no.
p = predict(BRLDA, newdata=BondRating[,4:13])$class
p

table(p, BondRating$RATING)

#validation data

p = predict(BRLDA, newdata=BondRatingNew[,4:13])$class
p

table(p, BondRatingNew$RATING)

#b)

#Validation dataset
#reading the data from validation tab
BondRatingNew <- read_excel("C:/Trupti MS/CSC424AdvDataAnalaysis/Assignments/Assignment4/BondRating.xls", sheet = "validation")
View(BondRatingNew)


dim(BondRatingNew)
BRLDAVal = lda(BondRatingNew$RATING ~ BondRatingNew$LOPMAR + BondRatingNew$LFIXCHAR + BondRatingNew$LGEARRAT + BondRatingNew$LTDCAP + BondRatingNew$LLEVER +BondRatingNew$LCASHLTD + BondRatingNew$LACIDRAT + BondRatingNew$LCURRAT + BondRatingNew$LRECTURN + BondRatingNew$LASSLTD)
BRLDAVal

plot(BRLDAVal)

#plot(BRLDAVal, dimen=1, type="both")

#variables from col 4 to col13 are chosen as the col1 is observation no., col 2 is the grp name and col3 is grp no.
p = predict(BRLDAVal, newdata=BondRatingNew[,4:13])$class
p

table(p, BondRatingNew$RATING)

#c)



BRLDA2 = lda(BondRating$RATING ~ BondRating$LOPMAR + BondRating$LFIXCHAR + BondRating$LGEARRAT + BondRating$LTDCAP + BondRating$LLEVER +BondRating$LCASHLTD + BondRating$LACIDRAT + BondRating$LCURRAT + BondRating$LRECTURN + BondRating$LASSLTD, data=BondRating, CV=T)
BRLDA2

table(BRLDA2$class, BondRating$RATING)

coef(BRLDA2)

