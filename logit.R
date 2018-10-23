# Code for analyzing HRS data. Sep 2018.
# By: Estefania Vergara (github)
# Note: for downloading the data into the server run the python code
# The following code assumes that the data has already been downloaded


# Preliminaries -----------------------------------------------------------


rm(list=ls())  # clear environment

# UNCOMMENT TO INSTALL PACKAGES:
# from command line: sudo yum install openssl-devel
# from command line: sudo yum -y install libcurl libcurl-devel
# install.packages("data.table")
# install.packages("devtools")
# install.packages("haven")
# install.packages("Hmisc")
# install.packages("dplyr")
# install.packages("plyr")
# install.packages("xtable")

# SET DIRECTORY -----------------------------------------------------------
setwd("/home/ebv2/s_drive/SED/HRS/data") #change for location of the dataset

# ACTIVATE PACKAGES -----------------------------------------------------------
library(data.table)
library(devtools)
library(haven)
library(foreign) #read foreign datasets
library(dplyr)
library(plyr)
library(xtable)

# READ DATA  -----------------------------------------------------------
hrs <- read.dta('randhrs1992_2014v2.dta') # don't change, this uses name of downloaded file

# Create function "count_percent" that returns counts and percentages for YES/NO questions
# namevar: input corresponding to the variable in the hrs data. e.g. race is hrs$raracem. Namevar
# must be written as "hrs$" followed by the variable name (see Codebook for Longitudinal data).
# name: name you want to give to the variable, to be used in the table of results. Must be written
# in between quotation marks, e.g. "Race of respondent". Note: do not use excclamation marks or apostrophes
# in the name.

count_percent <- function(namevar,name) {
  options(warn=-1)
  options(digits=2)
  a<-as.data.frame(count(na.exclude(namevar)))
  b<-rbind(a,c('Total',1))
  b[,1]<-c('no','yes','Total')
  b[3,2] <- sum(as.numeric(b[1:2,2]))
  b[,2]<- as.numeric(b[,2])
  b[,3] <- format(round((b[,2]/b[3,2])*100,1),nsmall=1)
  colnames(b) <- c(name,'Count','%')
  return(b)
}

count_percent1 <- function(namevar,name) {
  options(warn=-1)
  options(digits=2)
  a<-as.data.frame(count(na.exclude(namevar)))
  b<-rbind(a,c('Total',1))
  b[length(b[,1]),2] <- sum(as.numeric(b[1:length(b[,1])-1,2]))
  b[,2]<- as.numeric(b[,2])
  b[,3] <- format(round((b[,2]/b[length(b[,1]),2])*100,1),nsmall=1)
  colnames(b) <- c(name,'Count','%')
  return(b)
}

# Create race variables
hrs$race_white <- as.numeric(hrs$raracem)==1 # Dummy for white: 1 if true (white)
hrs$race_black <- as.numeric(hrs$raracem)==2 # Dummy for white: 1 if true (black)
hrs$race_other <- as.numeric(hrs$raracem)==3 # Dummy for white: 1 if true (other)
# Create marital variables
hrs$married <- as.numeric(hrs$r11mstat)==1 | as.numeric(hrs$r11mstat)==2 | as.numeric(hrs$r11mstat)==3 # 1 if true 
hrs$separated <- as.numeric(hrs$r11mstat)==4 | as.numeric(hrs$r11mstat)==5 | as.numeric(hrs$r11mstat)==6
hrs$widowed <- as.numeric(hrs$r11mstat)==7 
hrs$single <- as.numeric(hrs$r11mstat)==8
# Create education variables
hrs$highschool <- as.numeric(hrs$raedyrs)>=12 # Dummy for white: 1 if true (graduated high school)
hrs$college4years <- as.numeric(hrs$raedyrs)>=12 & as.numeric(hrs$raedyrs)<15 # Dummy for white: 1 if true (completed 4 years college)
hrs$highereducat <- as.numeric(hrs$raedyrs)>=15 # Dummy for white: 1 if true (completed 4 years college)
# Create BMI variables
# Underweight: BMI is less than 18.5.
# Normal weight: BMI is 18.5 to 24.9.
# Overweight: BMI is 25 to 29.9.
# Obese: BMI is 30 or more.
hrs$underweight <- as.numeric(hrs$r11bmi)<=18.5
hrs$normalweight <- as.numeric(hrs$r11bmi)<=24.9 & as.numeric(hrs$r11bmi)>18.5
hrs$overweight <- as.numeric(hrs$r11bmi)<=29.9 & as.numeric(hrs$r11bmi)>25
hrs$obeseweight <- as.numeric(hrs$r11bmi)>=30
# Create dummies for subsets based on the age group in wave 11 (2012) and Medicare enrollment
hrs$sixtyfive <- as.numeric(hrs$r11agey_b>=65) # Dummy for age 65: 1 if >= 65
hrs$medicare <- as.numeric(hrs$r11govmr=='1.yes') # Dummy for R has medicare, 1: yes, 0 no
# Create dummies for census division: divisions are subdivisions of the 4 census regions
# There are 9 census divisions
hrs$newengland <- as.numeric(hrs$r11cendiv)==1
hrs$midatlantic <- as.numeric(hrs$r11cendiv)==2
hrs$encentral <- as.numeric(hrs$r11cendiv)==3
hrs$wncentral <- as.numeric(hrs$r11cendiv)==4
hrs$satlantic <- as.numeric(hrs$r11cendiv)==5
hrs$escentral <- as.numeric(hrs$r11cendiv)==6
hrs$wscentral <- as.numeric(hrs$r11cendiv)==7
hrs$mountain <- as.numeric(hrs$r11cendiv)==8
hrs$pacific <- as.numeric(hrs$r11cendiv)==9
hrs$us_na <- as.numeric(hrs$r11cendiv)==10
hrs$not_usterr <- as.numeric(hrs$r11cendiv)==11

# Create subsets for 65 and older
hrs_under <- subset(hrs,hrs$sixtyfive == 0)
hrs_over <-  subset(hrs,hrs$sixtyfive == 1) #65 or over

# Results: count of general demographic variables - non-age specific
output_general <- list(
  count_percent(hrs$sixtyfive,'R is over 65 years'),
  count_percent(hrs$ragender,'R gender'),
  count_percent(hrs$rahispan,'R is hispanic'),
  count_percent(hrs$race_white,'R is white'),
  count_percent(hrs$race_black,'R is black'),
  count_percent(hrs$race_other,'R is non-white non-black'),
  count_percent(hrs$highschool,'R completed high school'),
  count_percent(hrs$college4years,'R completed 4 years college'),
  count_percent(hrs$highereducat,'R post-college educ'),
  count_percent(hrs$married,'R married or partnered'),
  count_percent(hrs$separated,'R divorced or separated'),
  count_percent(hrs$widowed,'R widowed'),
  count_percent(hrs$single,'R never married')
)

# Results: count of selected variables - age specific
output_age <- list(
  cbind(count_percent(hrs$r11govmr,'Has Medicare: All'),
        count_percent(hrs_under$r11govmr,'Under 65'),
        count_percent(hrs_over$r11govmr,'Over 65')),
  cbind(count_percent(hrs$s11govmr,'Spouse has Medicare: All'),
        count_percent(hrs_under$s11govmr,'Under 65'),
        count_percent(hrs_over$s11govmr,'Over 65')),
  cbind(count_percent(hrs$r11govmd,'Has Medicaid: All'),
        count_percent(hrs_under$r11govmd,'Under 65'),
        count_percent(hrs_over$r11govmd,'Over 65')),
  cbind(count_percent(hrs$s11govmd,'Spouse has Medicaid: All'),
        count_percent(hrs_under$s11govmd,'Under 65'),
        count_percent(hrs_over$s11govmd,'Over 65')),
  cbind(count_percent(hrs$r11hearte,'Ever had heart problems: All'),
        count_percent(hrs_under$r11hearte,'Under 65'),
        count_percent(hrs_over$r11hearte,'Over 65')),
  cbind(count_percent(hrs$r11doctor,'Visited doctor past 2 yrs: All'),
        count_percent(hrs_under$r11doctor,'Under 65'),
        count_percent(hrs_over$r11doctor,'Over 65')),
  cbind(count_percent(hrs$r11flusht,'Flu shot: All'),
        count_percent(hrs_under$r11flusht,'Under 65'),
        count_percent(hrs_over$r11flusht,'Over 65')),
  cbind(count_percent(hrs$r11cholst,'Cholesterol: All'),
        count_percent(hrs_under$r11cholst,'Under 65'),
        count_percent(hrs_over$r11cholst,'Over 65')),
  cbind(count_percent(hrs$r11smoken,'Smokes now: All'),
        count_percent(hrs_under$r11smoken,'Under 65'),
        count_percent(hrs_over$r11smoken,'Over 65')),
  cbind(count_percent(hrs$r11drink,'Ever drunk alcohol: All'),
        count_percent(hrs_under$r11drink,'Under 65'),
        count_percent(hrs_over$r11drink,'Over 65')),
  cbind(count_percent(hrs$r11depres,'Felt depressed: All'),
        count_percent(hrs_under$r11depres,'Under 65'),
        count_percent(hrs_over$r11depres,'Over 65')),
  cbind(count_percent(hrs$r11psyche,'Ever had psychol problems: All'),
        count_percent(hrs_under$r11psyche,'Under 65'),
        count_percent(hrs_over$r11psyche,'Over 65')),
  cbind(count_percent(hrs$r11alzhee,'Ever had Alzheimer: All'),
        count_percent(hrs_under$r11alzhee,'Under 65'),
        count_percent(hrs_over$r11alzhee,'Over 65')),
  cbind(count_percent1(hrs$r11mobila,'Diff mobility: All'),
        count_percent1(hrs_under$r11mobila,'Under 65'),
        count_percent1(hrs_over$r11mobila,'Over 65')), 
  cbind(count_percent1(hrs$r11lgmusa,'Diff mobility large muscle: All'),
        count_percent1(hrs_under$r11lgmusa,'Under 65'),
        count_percent1(hrs_over$r11lgmusa,'Over 65')), 
  cbind(count_percent1(hrs$r11adlwa,'ADLs Wallace: All'),
        count_percent1(hrs_under$r11adlwa,'Under 65'),
        count_percent1(hrs_over$r11adlwa,'Over 65')), 
  cbind(count_percent1(hrs$r11adla,'Diff-ADLs /0-5: All'),
        count_percent1(hrs_under$r11adla,'Under 65'),
        count_percent1(hrs_over$r11adla,'Over 65')), 
  cbind(count_percent1(hrs$r11iadla,'Difficulties-IADLs-3: All'), #sum of (RwPHONEA, RwMONEYA, RwMEDSA);
        count_percent1(hrs_under$r11iadla,'Under 65'),
        count_percent1(hrs_over$r11iadla,'Over 65')) ,
  cbind(count_percent1(hrs$r11iadlza,'Difficulties-IADLs-5: All'), #sum (RwPHONEA, RwMONEYA, RwMEDSA, RwSHOPA, RwMEALA);
        count_percent1(hrs_under$r11iadlza,'Under 65'),
        count_percent1(hrs_over$r11iadlza,'Over 65')) ,
  cbind(count_percent1(hrs$r11grossa,'Walk1/R,Clim1,Bed,Bath/0-5: All'),
        count_percent1(hrs_under$r11grossa,'Under 65'),
        count_percent1(hrs_over$r11grossa,'Over 65')), 
  cbind(count_percent1(hrs$r11finea,'Dime/Eat/Dress /0-3: All'),
        count_percent1(hrs_under$r11finea,'Under 65'),
        count_percent1(hrs_over$r11finea,'Over 65')), 
  cbind(count_percent1(hrs$r11cesd,'CESD: All'),
        count_percent1(hrs_under$r11cesd,'Under 65'),
        count_percent1(hrs_over$r11cesd,'Over 65')) ,
  count_percent1(hrs$r11mstot,'Total mental status score: All'), 
  count_percent1(hrs_under$r11mstot,'Under 65'),
  count_percent1(hrs_over$r11mstot,'Over 65') ,
  count_percent1(hrs$r11cogtot,'Total cognitive score: All'), 
  count_percent1(hrs_under$r11cogtot,'Under 65'),
  count_percent1(hrs_over$r11cogtot,'Over 65') ,
  cbind(count_percent1(hrs$r11shltc,'Change in self-reported health: All'), 
        count_percent1(hrs_under$r11shltc,'Under 65'),
        count_percent1(hrs_over$r11shltc,'Over 65')) ,
  cbind(count_percent(hrs$underweight,'Underweight (BMI): All'),
        count_percent(hrs_under$underweight,'Under 65'),
        count_percent(hrs_over$underweight,'Over 65')),
  cbind(count_percent(hrs$normalweight,'Normal weight (BMI): All'),
        count_percent(hrs_under$normalweight,'Under 65'),
        count_percent(hrs_over$normalweight,'Over 65')),
  cbind(count_percent(hrs$overweight,'Overweight (BMI): All'),
        count_percent(hrs_under$overweight,'Under 65'),
        count_percent(hrs_over$overweight,'Over 65')),
  cbind(count_percent(hrs$obeseweight,'Obese (BMI): All'),
        count_percent(hrs_under$obeseweight,'Under 65'),
        count_percent(hrs_over$obeseweight,'Over 65')),
  cbind(count_percent1(hrs$r11conde,'Sum of conditions ever had: All'),
        count_percent1(hrs_under$r11conde,'Under 65'),
        count_percent1(hrs_over$r11conde,'Over 65')),
  cbind(count_percent1(hrs$r11conds,'Sum conditio since last wav: All'),
        rbind(count_percent1(hrs_under$r11conds,'Under 65'), rep(0,3)),
        count_percent1(hrs_over$r11conds,'Over 65'))
)

# Note: variables r11lmcoga,r12acgtot,r12liv85c r11liv75 are not available on wave 11

# Free memory: Now controlling for whether they have Medicare or not -----------------------------------------
hrs_under <- NULL
hrs_over <-  NULL
hrs <- hrs[ which(hrs$r11govmr=='1.yes' | hrs$r11govmr=='0.no'), ]
# Create subsets for Medicare groups
hrs_nomr <-  hrs[ which(hrs$medicare == 0), ] #Subset of those who don't have Medicr
hrs_yesmr <- hrs[ which(hrs$medicare == 1), ] # Subset of those that have Medicr

# Results: count of selected variables - age specific
output_bymedicare <- list(
  cbind(count_percent(hrs$s11govmr,'Spouse has Medicare: All'),
        count_percent(hrs_nomr$s11govmr,'No-Medicare'),
        count_percent(hrs_yesmr$s11govmr,'Yes-Medicare')),
  cbind(count_percent(hrs$r11govmd,'Has Medicaid: All'),
        count_percent(hrs_nomr$r11govmd,'No-Medicare'),
        count_percent(hrs_yesmr$r11govmd,'Yes-Medicare')),
  cbind(count_percent(hrs$s11govmd,'Spouse has Medicaid: All'),
        count_percent(hrs_nomr$s11govmd,'No-Medicare'),
        count_percent(hrs_yesmr$s11govmd,'Yes-Medicare')),
  cbind(count_percent(hrs$r11hearte,'Ever had heart problems: All'),
        count_percent(hrs_nomr$r11hearte,'No-Medicare'),
        count_percent(hrs_yesmr$r11hearte,'Yes-Medicare')),
  cbind(count_percent(hrs$r11doctor,'Visited doctor past 2 yrs: All'),
        count_percent(hrs_nomr$r11doctor,'No-Medicare'),
        count_percent(hrs_yesmr$r11doctor,'Yes-Medicare')),
  cbind(count_percent(hrs$r11flusht,'Flu shot: All'),
        count_percent(hrs_nomr$r11flusht,'No-Medicare'),
        count_percent(hrs_yesmr$r11flusht,'Yes-Medicare')),
  cbind(count_percent(hrs$r11cholst,'Cholesterol: All'),
        count_percent(hrs_nomr$r11cholst,'No-Medicare'),
        count_percent(hrs_yesmr$r11cholst,'Yes-Medicare')),
  cbind(count_percent(hrs$r11smoken,'Smokes now: All'),
        count_percent(hrs_nomr$r11smoken,'No-Medicare'),
        count_percent(hrs_yesmr$r11smoken,'Yes-Medicare')),
  cbind(count_percent(hrs$r11drink,'Ever drunk alcohol: All'),
        count_percent(hrs_nomr$r11drink,'No-Medicare'),
        count_percent(hrs_yesmr$r11drink,'Yes-Medicare')),
  cbind(count_percent(hrs$r11depres,'Felt depressed: All'),
        count_percent(hrs_nomr$r11depres,'No-Medicare'),
        count_percent(hrs_yesmr$r11depres,'Yes-Medicare')),
  cbind(count_percent(hrs$r11psyche,'Ever had psychol problems: All'),
        count_percent(hrs_nomr$r11psyche,'No-Medicare'),
        count_percent(hrs_yesmr$r11psyche,'Yes-Medicare')),
  cbind(count_percent(hrs$r11alzhee,'Ever had Alzheimer: All'),
        count_percent(hrs_nomr$r11alzhee,'No-Medicare'),
        count_percent(hrs_yesmr$r11alzhee,'Yes-Medicare')),
  cbind(count_percent1(hrs$r11mobila,'Diff mobility: All'),
        count_percent1(hrs_nomr$r11mobila,'No-Medicare'),
        count_percent1(hrs_yesmr$r11mobila,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11lgmusa,'Diff mobility large muscle: All'),
        count_percent1(hrs_nomr$r11lgmusa,'No-Medicare'),
        count_percent1(hrs_yesmr$r11lgmusa,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11adlwa,'ADLs Wallace: All'),
        count_percent1(hrs_nomr$r11adlwa,'No-Medicare'),
        count_percent1(hrs_yesmr$r11adlwa,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11adla,'Diff-ADLs /0-5: All'),
        count_percent1(hrs_nomr$r11adla,'No-Medicare'),
        count_percent1(hrs_yesmr$r11adla,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11grossa,'Walk1/R,Clim1,Bed,Bath/0-5: All'),
        count_percent1(hrs_nomr$r11grossa,'No-Medicare'),
        count_percent1(hrs_yesmr$r11grossa,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11finea,'Dime/Eat/Dress /0-3: All'),
        count_percent1(hrs_nomr$r11finea,'No-Medicare'),
        count_percent1(hrs_yesmr$r11finea,'Yes-Medicare')), 
  cbind(count_percent1(hrs$r11cesd,'CESD: All'),
        count_percent1(hrs_nomr$r11cesd,'No-Medicare'),
        count_percent1(hrs_yesmr$r11cesd,'Yes-Medicare')) ,
  count_percent1(hrs$r11mstot,'Total mental status score: All'), 
  count_percent1(hrs_nomr$r11mstot,'No-Medicare'),
  count_percent1(hrs_yesmr$r11mstot,'Yes-Medicare') ,
  count_percent1(hrs$r11cogtot,'Total cognitive score: All'), 
  count_percent1(hrs_nomr$r11cogtot,'No-Medicare'),
  count_percent1(hrs_yesmr$r11cogtot,'Yes-Medicare') ,
  cbind(count_percent1(hrs$r11shltc,'Change in self-reported health: All'), 
        count_percent1(hrs_nomr$r11shltc,'No-Medicare'),
        count_percent1(hrs_yesmr$r11shltc,'Yes-Medicare')) ,
  cbind(count_percent(hrs$underweight,'Underweight (BMI): All'),
        count_percent(hrs_nomr$underweight,'No-Medicare'),
        count_percent(hrs_yesmr$underweight,'Yes-Medicare')),
  cbind(count_percent(hrs$normalweight,'Normal weight (BMI): All'),
        count_percent(hrs_nomr$normalweight,'No-Medicare'),
        count_percent(hrs_yesmr$normalweight,'Yes-Medicare')),
  cbind(count_percent(hrs$overweight,'Overweight (BMI): All'),
        count_percent(hrs_nomr$overweight,'No-Medicare'),
        count_percent(hrs_yesmr$overweight,'Yes-Medicare')),
  cbind(count_percent(hrs$obeseweight,'Obese (BMI): All'),
        count_percent(hrs_nomr$obeseweight,'No-Medicare'),
        count_percent(hrs_yesmr$obeseweight,'Yes-Medicare')),
  cbind(count_percent1(hrs$r11conde,'Sum of conditions ever had: All'),
        count_percent1(hrs_nomr$r11conde,'No-Medicare'),
        count_percent1(hrs_yesmr$r11conde,'Yes-Medicare')),
  cbind(count_percent1(hrs$r11conds,'Sum conditio since last wav: All'),
        rbind(count_percent1(hrs_nomr$r11conds,'No-Medicare'), rep(0,3)),
        count_percent1(hrs_yesmr$r11conds,'Yes-Medicare'))
)

# Output results in .txt files
capture.output(output_general, file = "output_all.txt")
capture.output(output_age, file = "output_by_age.txt")
capture.output(output_bymedicare, file = "output_by_medicare.txt")

# Calculate p-values -----------------------------------------------
# Chi Square for factorial (cathegorical) variables function
# The test is applied when you have two categorical variables from a single 
# population. It is used to determine whether there is a significant association between the two variables.
# H0: there is no relationship between x and y (accepted if p.value > 0.05)
# x: dependent variable such as Medicare or hrs$medicare
# y: independent variable such as Race or hrs$ragender
# n: vector of variable names

base <- function(a){
  x <- a[,c("sixtyfive","ragender","rahispan","rameduc","r12mstat","s11govmr","r11govmd",
            "s11govmd","r11hearte","r11doctor","r11flusht","r11cholst","r11smoken",
            "r11drink","r11depres","r11psyche","r11alzhee","r11mobila","r11lgmusa","r11adlwa",
            "r11adla","r11grossa","r11finea","r11cesd","r11mstot","r11cogtot","r11shltc",
            "r11bmi","r11conde","r11conds")]
}
pvalue_table <- function(x,y,n) {
  a = matrix(nrow=length(x),ncol=3)
  colnames(a) <- c('Medicare and:',' P-value','Signif')
  a[,1] <- n
  #options(scipen = 999) #activate if results showing "e"
  for (i in 1:length(x)) {
    s <- na.exclude(as.data.frame(cbind(x[,i],y)))
    a[i,2] <- format(round(chisq.test(s[,1],s[,2])$p.value,5),nsmall=5) # 5 digits format
    if((as.numeric(a[i,2]) - 0.05) > 0){
      a[i,3] <- "No"}
    else {
      a[i,3] <-"Yes"}
  }
  return(as.data.frame(a))
}
# Specify the vector of variables to be used for the p-value table &
# create vector with the real names of the variables
base(hrs)
n <- c('R is over 65 years',"Gender","Hispanic","Education","Marital Stat","Spouse Medicare","R Medicaid",
       "Spouse Medicaid","Ever had heart problems","'Visited doctor past 2 yrs","Flu shot",
       "Cholesterol","Smokes now","Ever drunk alcohol","Felt depressed",
       "Ever had psychol problems","Ever had Alzheimer",
       "Diff mobility","Diff mobility large muscle","ADLs Wallace",
       "Diff-ADLs /0-5","Walk1/R,Clim1,Bed,Bath","Dime/Eat/Dress","CESD",
       "Total mental status score","Total cognitive score","Change in self-reported health",
       "BMI","Sum of conditions ever had","Sum conditio since last wav")

output_pval <- pvalue_table(x,hrs$medicare,n)
capture.output(output_pval, file = "output_pval.txt")

# Calculate Logit models -----------------------------------------------
logit <- function(x,name){
  #options(scipen = 999)
  options(digits=2)
  mylogit <- summary(glm(x ~ raracem+ragender+rahispan+raedyrs+r11mstat+r11agey_b+r11govmr+
                           r11govmd+s11govmr+s11govmd+newengland+midatlantic+encentral+wncentral+satlantic+
                           escentral+wscentral+mountain+pacific+not_usterr, data = hrs, family = binomial(link = "logit")))
  mylogit <- as.data.frame(mylogit$coef)
  #colnames(mylogit)[1] <- paste(name,"-estimate")
  rownames(mylogit) <- c(paste("intrc:",name),"black","other race","female","hispanic","years_education",
                         "married","partnered","age","Medicare","Medicaid","spouse Medicare",
                         "spouse Medicaid","New England","Mid Atlantic","EN Central", "WN Central", "S Atlantic",
                         "ES Central","WS Central","Mountain","Pacific") #,"Not US/inc US terr"
  #options(show.signif.stars=TRUE)
  return(mylogit)
}

output_logit <- list(
  logit(hrs$r11hearte, "Heart prob"),
  logit(hrs$r11doctor, "Visited doctor"),
  logit(hrs$r11flusht, "Flu shot"),
  logit(hrs$r11cholst, "Cholesterol"),
  logit(hrs$r11smoken,"Smokes now"),
  logit(hrs$r11drink,"Ever alcohol"),
  logit(hrs$r11depres,"Felt depressed"),
  logit(hrs$r11psyche,"Ever psychol prob"),
  logit(hrs$r11alzhee,"Ever alzheimer"),
  logit(hrs$underweight,"underweight"),
  logit(hrs$normalweight, "normalweight"),
  logit(hrs$overweight, "overweight"),
  logit(hrs$obeseweight,"obeseweight")
)
capture.output(output_logit, file = "output_logit.txt")


# Tables for age and medicare simultaneously. Create subsets:------
hrs<- NULL
hrs_under_yesmr <- subset(hrs_under,hrs_under$medicare == 1)
hrs_under_nomr <- subset(hrs_under,hrs_under$medicare == 0)
hrs_under <- NULL
hrs_over_yesmr <- subset(hrs_over,hrs_over$medicare == 1)
hrs_over_nomr <- subset(hrs_over,hrs_over$medicare == 0)
hrs_over <- NULL

output_age_medicare <- list(
  cbind( count_percent(hrs_under_yesmr$s11govmr,'Spouse has Medicare:Under 65 & Medicare'),
         count_percent(hrs_under_nomr$s11govmr,'Under 65 & No-Medicare'),
         count_percent(hrs_over_yesmr$s11govmr,'Over 65 & Medicare'),
         count_percent(hrs_over_nomr$s11govmr,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11govmd,'Has Medicaid:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11govmd,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11govmd,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11govmd,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$s11govmd,'Spouse has Medicaid:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$s11govmd,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$s11govmd,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$s11govmd,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11hearte,'Ever had heart problems:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11hearte,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11hearte,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11hearte,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11doctor,'Visited doctor past 2 yrs:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11doctor,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11doctor,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11doctor,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11flusht,'Flu shot:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11flusht,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11flusht,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11flusht,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11cholst,'Cholesterol:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11cholst,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11cholst,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11cholst,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11smoken,'Smokes now:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11smoken,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11smoken,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11smoken,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11drink,'Ever drunk alcohol:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11drink,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11drink,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11drink,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11depres,'Felt depressed:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11depres,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11depres,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11depres,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11psyche,'Ever had psychol problems:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11psyche,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11psyche,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11psyche,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$r11alzhee,'Ever had Alzheimer:Under 65 & Medicare'),
        count_percent(hrs_under_nomr$r11alzhee,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$r11alzhee,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$r11alzhee,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11mobila,'Diff mobility:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11mobila,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11mobila,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11mobila,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11lgmusa,'Diff mobility large muscle:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11lgmusa,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11lgmusa,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11lgmusa,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11adlwa,'ADLs Wallace:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11adlwa,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11adlwa,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11adlwa,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11adla,'Diff-ADLs /0-5:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11adla,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11adla,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11adla,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11iadla,'Difficulties-IADLs-3:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11iadla,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11iadla,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11iadla,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11iadlza,'Difficulties-IADLs-5:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11iadlza,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11iadlza,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11iadlza,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11grossa,'Walk1/R,Clim1,Bed,Bath/0-5:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11grossa,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11grossa,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11grossa,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11finea,'Dime/Eat/Dress /0-3:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11finea,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11finea,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11finea,'Over 65 & No-Medicare')),
  cbind(count_percent1(hrs_under_yesmr$r11cesd,'CESD:Under 65 & Medicare'),
        count_percent1(hrs_under_nomr$r11cesd,'Under 65 & No-Medicare'),
        count_percent1(hrs_over_yesmr$r11cesd,'Over 65 & Medicare'),
        count_percent1(hrs_over_nomr$r11cesd,'Over 65 & No-Medicare')),
  count_percent1(hrs_under_yesmr$r11mstot,'Total mental status score:Under 65 & Medicare'),
  count_percent1(hrs_under_nomr$r11mstot,'Under 65 & No-Medicare'),
  count_percent1(hrs_over_yesmr$r11mstot,'Over 65 & Medicare'),
  count_percent1(hrs_over_nomr$r11mstot,'Over 65 & No-Medicare'),
  count_percent1(hrs_under_yesmr$r11cogtot,'Total cognitive score:Under 65 & Medicare'),
  count_percent1(hrs_under_nomr$r11cogtot,'Under 65 & No-Medicare'),
  count_percent1(hrs_over_yesmr$r11cogtot,'Over 65 & Medicare'),
  count_percent1(hrs_over_nomr$r11cogtot,'Over 65 & No-Medicare'),
  cbind(count_percent(hrs_under_yesmr$underweight,'Underweight (BMI):Under 65 & Medicare'),
        count_percent(hrs_under_nomr$underweight,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$underweight,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$underweight,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$normalweight,'Normal weight (BMI):Under 65 & Medicare'),
        count_percent(hrs_under_nomr$normalweight,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$normalweight,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$normalweight,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$overweight,'Overweight (BMI):Under 65 & Medicare'),
        count_percent(hrs_under_nomr$overweight,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$overweight,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$overweight,'Over 65 & No-Medicare')),
  cbind(count_percent(hrs_under_yesmr$obeseweight,'Obese (BMI):Under 65 & Medicare'),
        count_percent(hrs_under_nomr$obeseweight,'Under 65 & No-Medicare'),
        count_percent(hrs_over_yesmr$obeseweight,'Over 65 & Medicare'),
        count_percent(hrs_over_nomr$obeseweight,'Over 65 & No-Medicare')),
  (count_percent1(hrs_under_yesmr$r11conde,'Sum of conditions ever had:Under 65 & Medicare')),
  count_percent1(hrs_under_nomr$r11conde,'Under 65 & No-Medicare'),
  count_percent1(hrs_over_yesmr$r11conde,'Over 65 & Medicare'),
  count_percent1(hrs_over_nomr$r11conde,'Over 65 & No-Medicare'),
  (count_percent1(hrs_under_yesmr$r11conds,'Sum conditio since last wav:Under 65 & Medicare')),
  (count_percent1(hrs_under_nomr$r11conds,'Under 65 & No-Medicare')),
  count_percent1(hrs_over_yesmr$r11conds,'Over 65 & Medicare'),
  count_percent1(hrs_over_nomr$r11conds,'Over 65 & No-Medicare')
)
capture.output(output_age_medicare, file = "output_age_medicare.txt")
