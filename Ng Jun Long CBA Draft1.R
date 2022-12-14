## CBA AY 2022 Ng Jun Long U2110010D Draft1
install.packages("data.table")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("stringr")
install.packages("caret")
install.packages("skimr")
install.packages("dplyr")
install.packages("VIM")
#### Packages that might come in handy
install.packages("naniar")
install.packages("missMDA")
install.packages("Amelia")
install.packages("mice")
install.packages("missForest")
install.packages("FactoMineR")
install.packages("tidyverse")
install.packages("class")
install.packages("RANN")
install.packages("ggpubr")

## importing the relevant packages
library(data.table)
library(rpart)
library(rpart.plot) 
library(stringr)
library(ggplot2)
library(caret)
library(skimr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(class)
library(RANN)
library(VIM)
library(ggpubr)

## Importing the dataset
setwd("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/AY22 BC2406 CBA")
df1 = fread("/Users/junlongng/Desktop/NTU/Year 2/Semester 1/BC2406 Analytics 1/AY22 BC2406 CBA/homeloan2.csv")
summary(df1)


##Q1.a)
##Checking the names of all columns 
colnames(df1)

## Checking the type of all columns
sapply(df1, class)

##LoanID as character is okay
typeof(df1$Loan_ID)

## Data types that should be class recast to factorial 
factorcolumns = c("Gender","Married","Self_Employed","Credit_Score","Loan_Status","Education")

## Recasting the class to factor
df1[, (factorcolumns):= lapply(.SD, factor), .SDcols = factorcolumns]


##Need to change dependents to factored categorical data
## This at first glance should be an integervalue but there is an option for 3+..
## Therefore dependents can be a categorical data that determines how many people are depending on the person
df1$Dependents = factor(df1$Dependents, ordered = T, levels = c("0","1","2","3+",""))

## Similar to dependents, property area can be categorised into A,B,C
df1$Property_Area = factor(df1$Property_Area,ordered = F, levels = c("A","B","C"))
class(df1$Property_Area)


## Data type that should be converted to numeric for further analysis.
## Find a way to make this code work: df1[, (numericcol):= lapply(.SD, numeric), .SDcols = numericcol]

## Both income can be numeric as it can be represented as Income
df1$ApplicantIncome = as.numeric(df1$ApplicantIncome)
df1$CoapplicantIncome = as.numeric(df1$CoapplicantIncome)


## Loan amount can be numeric as you can represent loans in decimals
df1$LoanAmount = as.numeric(df1$LoanAmount)


## Loan amount should be numeric as you can represent a month in decimals i.e. 1.5 months
df1$Loan_Amount_Term = as.numeric(df1$Loan_Amount_Term)



## -------------- Q1.b) ------------- ##

## Checking for NA values
sum(is.na(df1))
## There are 63 values that are NA in the whole of df1

## But upon further inspection, there are more values that R did not pick up as they were turned into " ". 
summary(df1)
sapply(df1, class)

##Loan ID has no NA values

## Gender has 13 Blank values
## How should I solve it

## Married has 2 blank values
## How should I solve it

## Dependents have 13 Blank values
## How to solve it

## Education has no NA values

## Self Employed has 31 blank values
## How To Solve?

## Applicant Income has No NA values

## Coapplicant Income has no NA values

## Loan Amount has No NA values

## Loan Amount Term has 14 NAs
## How to solve?

## Credit Score has 49 NA's
## How to solve

## Property_Area has no NA values

## Loan Status has no NA values

## First step is to convert blank values to NA values 
df1[df1==""] = NA


## Now all columns that has " " has turned into NA
## Individually determine if to drop or fill NA

## Experimenting by creating a fork of df1
dfnew=df1
dfnew$Gender= factor(dfnew$Gender,levels = c("Male","Female"),labels=c(1,0))
summary(dfnew)


## Using KNN to change gender NA values
## Why use KNN for this variable?
knntest = kNN(dfnew, variable = c("Gender","Married","Dependents","Self_Employed","Loan_Amount_Term","Credit_Score"), k =10)
summary(knntest)



## Checking if there is still any NA values throughout the whole data set.
sum(is.na(knntest))
## No more NA values.

## Checking how many cases are left is the same as counting the number of rows in the data set
nrow(knntest)
dim(knntest)

## There are 592 cases
colnames(knntest)


## -------------- Q2 ------------- ##

## Viewing histogram
ggplot(data=knntest, aes(LoanAmount))+geom_histogram(bins=40,color="blue",fill="cyan")

## The below graph shows us that on average, there are more males that take loans
ggplot(data = knntest, aes(Gender_imp)) + geom_bar(color="blue", fill="cyan")

## Viewing categorical variables as a whole
factor = names(keep(knntest,is.factor))
gglist = list()
for(x in 1: (length(factor)-1)){gglist[x]=list(ggplot(data=knntest,aes_string(factor[x],fill='Loan_Status'))+ geom_bar(position = 'dodge')+ theme(legend.position = c(.9,.75),legend.background=element_rect(fill = alpha("white", 0.5)))+ scale_fill_brewer(palette = 'Paired'))}
ggarrange(plotlist=gglist)

## In general these are the findings
## There are more males that take loans and have their loans approved
## There are more married couples that take loans and have their loans approved
## Those with 0 dependents often have loan status granted and there are more of them
## Often graduates get their loan approved as compared to non-graduates
## None self-employed people get their loans approved
## People with a credit score get their loan approved more
## People living in property Area B get their loans approved more


## Viewing continuous variables as a whole
numeric = names(keep(knntest, is.numeric))
gglist1 = list()
gglist2 = list()
for(x in 1:length(numeric)){ 
  gglist1[x]=list(ggplot(data=knntest,aes_string(x='Loan_Status',y=numeric[x],color='Loan_Status'))+ geom_boxplot()+theme(legend.position = c(.85,.85),legend.background=element_rect(fill = alpha("white", 0.5)))+ scale_color_brewer(palette = 'Dark2'))
}

for(x in 1:length(numeric)){ 
gglist2[x]<-list(ggplot(data = knntest,aes_string(numeric[x],fill='Loan_Status'))+geom_density(alpha=0.5)+
                     theme(legend.position = c(.9,.85),legend.background=element_rect(fill = alpha("white", 0.5)))+
                     scale_fill_brewer(palette = 'Dark2'))
}
ggarrange(plotlist=gglist1)
ggarrange(plotlist=gglist2,ncol = 1, nrow = 3)
gglist2
#####################3








## To replace Gender, I've went with using mode, should use another way but to experiment...
# calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

# calc_mode(dfnew$Gender)
# dfnew$Gender = ifelse()

## Gender's NA values has now been replaced with mode of Male
# dfnew$Gender[is.na(dfnew$Gender)] = calc_mode(dfnew$Gender)

## Data Exploration




