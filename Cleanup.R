#Packages Needed:
library(readr)
library(tidyverse)
library(caret)
library(rapportools)

#Load Data
train <- read_csv("C:/Users/isaac/Desktop/Kaggle Comp Housing Prices/train.csv")

#Check for Rows of NAs
summary(train)

sapply(train, function(x) sum(is.na(x)))

train %>%
    group_by(Alley) %>%   
    count()

#Remove variables with more than 90% NAs
many_missing = which(sapply(train, function(x) sum(is.na(x))) >= nrow(train)*.85)

train = train[,-many_missing]

#Remove variables with near zero variance

nzv <- nearZeroVar(train)
length(nzv) #21 Variables
train = train[,-nzv]

#Collect Numeric Variables
numcols = which(sapply(train, function(x) class(x)=="numeric" || class(x)=="integer"))
length(numcols)

#Check for correlated variables (highly)
corr = cor(train[,numcols], method='spearman', use='pairwise.complete.obs')
findCorrelation(corr, verbose=T, names=T, cutoff=0.9) #None are over .90 correlation

#collect character variables
charcols = which(sapply(train, function(x) class(x)=="character"))
length(charcols)

#Check for factors
#Test the correlation of 2 random factors
tab <- table(train[,c(6,54)])
tab
lt <- lambda.test(tab)
lt
pre <- min(c(lt$row, lt$col))
pre   # 0.354 not too bad

#Now test for the rest of the factors
combos <- combn(charcols,2)  #Create combos of all factors

for (i in 1:ncol(combos)) {
    tab = table(train[,combos[,i]])
    lt = lambda.test(tab)
    pre = min(c(lt$row, lt$col))
    if (pre > 0.8) cat(combos[1,i],' ',combos[2,i],' pre ',pre, '\n')
}


train[1,c(18,17)] #drop 18
train = train[,!names(train) %in% c("Exterior2nd")]

#Export the Cleaned Train 
clean_train = train
write.csv(clean_train, file = "clean_train.csv")
?write.csv
