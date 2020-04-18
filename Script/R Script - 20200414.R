## install.packages("xlsx")
## install.packages("Hmisc")
## install.packages("plyr")
## install.packages("dplyr")
## install.packages("ggplot2")
## install.packages("gmodels")
## install.packages("productplots")
## install.packages("CGPfunctions")
## install.packages("doBy")

## Load libraries
rm(list = ls())
library("doBy")
library("CGPfunctions")
library("dplyr")
library("ggplot2")
library("productplots")
library("Hmisc")
library("xlsx")
library("plyr")
library("gmodels")

## Set working directory
setwd("C:/Users/william.mcfall/Desktop/Project Files/16. COVID19 RNA/R Script/") 
mydata <- read.xlsx("RNA Dataset - 20200414.xlsx", "Sheet1", header=TRUE)

## CrossTabulation Household No Chronic Illness or Disability by Nationality
print("Household Both Chronic Illness and Disability By Nationality")
CrossTable(mydata$HH.member.has.none_.dis_CI,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


## Cleaning Data - Errors
mydata$Refugee.vs.Jordanian <- revalue(mydata$Refugee.vs.Jordanian, c(refugee = "Refugee"))

mydata$Household.Members.disabled.or.chronically.ill <- revalue(mydata$Household.Members.disabled.or.chronically.ill, c(BOTH = "both", Disability = "disability", "no " = "no"))

## Cleaning Data - Reclassification
mydata$HH.Member.has.disability<-as.character(mydata$HH.Member.has.disability)
mydata$HH.Member.has.disability<- revalue(mydata$HH.Member.has.disability, c("1" = "Yes", "0" = "No"))
mydata$HH.Member.has.disability[is.na(mydata$HH.Member.has.disability)] <- "No"

mydata$HH..Member.has.chronic.ill<-as.character(mydata$HH..Member.has.chronic.ill)
mydata$HH..Member.has.chronic.ill<- revalue(mydata$HH..Member.has.chronic.ill, c("1" = "Yes", "0" = "No"))
mydata$HH..Member.has.chronic.ill[is.na(mydata$HH..Member.has.chronic.ill)] <- "No"

mydata$HH.member.has.both_dis_CI<-as.character(mydata$HH.member.has.both_dis_CI)
mydata$HH.member.has.both_dis_CI<- revalue(mydata$HH.member.has.both_dis_CI, c("1" = "Yes", "0" = "No"))
mydata$HH.member.has.both_dis_CI[is.na(mydata$HH.member.has.both_dis_CI)] <- "No"

mydata$HH.member.has.none_.dis_CI<-as.character(mydata$HH.member.has.none_.dis_CI)
mydata$HH.member.has.none_.dis_CI<- revalue(mydata$HH.member.has.none_.dis_CI, c("1" = "Yes", "0" = "No"))
mydata$HH.member.has.none_.dis_CI[is.na(mydata$HH.member.has.none_.dis_CI)] <- "No"

mydata$Nationality<- revalue(mydata$Nationality, c(jordanian = "JOR", Palestinian = "PSE", Pakistan = "PAK"))


## Cleaning Data - New Classifications
  mydata$FAMILY_SIZE <- NULL
  mydata$FAMILY_SIZE[mydata$Family.Size <= 3] <- "3 or less"
  mydata$FAMILY_SIZE[mydata$Family.Size == 4] <- "4 to 6"
  mydata$FAMILY_SIZE[mydata$Family.Size == 5] <- "4 to 6"
  mydata$FAMILY_SIZE[mydata$Family.Size == 6] <- "4 to 6"
  mydata$FAMILY_SIZE[mydata$Family.Size >= 7] <- "7 or more"
  mydata$FAMILY_SIZE <- as.factor(mydata$FAMILY_SIZE)

  ## Summarize the Data
  sink("Outputs/General Analysis Output.txt")
  summary(mydata)
  
  print("Enumerator Frequency")
  x <- table(mydata$Enumerator)
  x
  print("Enumerator Percentages")
  prop.table(x)
  
  print("Enumerator Agency Frequency")
  x <- table(mydata$Enumerator.Agency)
  x
  print("Enumerator Agency Percentages")
  prop.table(x)
  
  print("Consent Frequency")
  x <- table(mydata$Consent)
  x
  print("Consent Percentages")
  prop.table(x)
  
  print("Refugee Vs. Jordanian Frequency")
  x <- table(mydata$Refugee.vs.Jordanian)
  x
  print("Refugee Vs. Jordanian Percentages")
  prop.table(x)
  
  
  print("UNHCR Case ID Frequency")
  x <- table(mydata$X.UNHCR.Case.ID.)
  x
  print("UNHCR Case ID Percentages")
  prop.table(x)
  
  
  print("Nationality Frequency")
  x <- table(mydata$Nationality)
  x
  print("Nationality Percentages")
  prop.table(x)
  
  
  print("Governorate Frequency")
  x <- table(mydata$Governorate)
  x
  print("Governorate Percentages")
  prop.table(x)
  
  
  print("District Frequency")
  x <- table(mydata$District)
  x
  print("District Percentages")
  prop.table(x)
  
  
  print("Respondent Gender Frequency")
  x <- table(mydata$Respondent.Gender)
  x
  print("Respondent Gender Percentages")
  prop.table(x)
  
  
  print("Head of Household Gender Frequency")
  x <- table(mydata$HoH.Gender)
  x
  print("Head of Household Gender Percentages")
  prop.table(x)
  
  
  print("Head of Household Gender Frequency")
  x <- table(mydata$HoH.Gender)
  x
  print("Head of Household Gender Percentages")
  prop.table(x)
  
  
  print("Household Member Disability or Chronic Illness Frequency")
  x <- table(mydata$Household.Members.disabled.or.chronically.ill)
  x
  print("Household Member Disability or Chronic Illness Percentages")
  prop.table(x)
  
  
  print("Where Do you Live Currently Frequency")
  x <- table(mydata$Where.do.you.live.currently.)
  x
  print("Where Do you Live Currently Percentages")
  prop.table(x)
  

  
  sink()
  
  ########################################################################################
  ## Nationality Disaggregation
  ########################################################################################
  
  sink("Outputs/Nationality Analysis Output.txt")
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Refugee/Jordanian by Nationality
  print("Refugee By Nationality")
  CrossTable(mydata$Refugee.vs.Jordanian,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  jpeg("Graphs/Frequency/Refugee Jordanian By Nationality.jpg")
  PlotXTabs(mydata,Refugee.vs.Jordanian,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Refugee Jordanian By Nationality.jpg")
  PlotXTabs(mydata,Refugee.vs.Jordanian,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation UNHCR ID by Nationality
  print("UNHCR By Nationality")
  CrossTable(mydata$X.UNHCR.Case.ID.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  jpeg("Graphs/Frequency/UNHCR ID By Nationality.jpg")
  PlotXTabs(mydata,X.UNHCR.Case.ID.,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/UNHCR ID By Nationality.jpg")
  PlotXTabs(mydata,X.UNHCR.Case.ID.,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Governorate by Nationality
  print("Governorate By Nationality")
  CrossTable(mydata$Governorate,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

  jpeg("Graphs/Frequency/Governorate By Nationality.jpg")
  PlotXTabs(mydata,Governorate,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Governorate By Nationality.jpg")
  PlotXTabs(mydata,Governorate,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation District by Nationality
  print("District By Nationality")
  CrossTable(mydata$District,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Respondent Gender by Nationality
  print("Respondent Gender By Nationality")
  CrossTable(mydata$Respondent.Gender,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  jpeg("Graphs/Frequency/Respondent Gender By Nationality.jpg")
  PlotXTabs(mydata,Respondent.Gender,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Respondent Gender By Nationality.jpg")
  PlotXTabs(mydata,Respondent.Gender,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Head of Household by Nationality
  print("Head of Household By Nationality")
  CrossTable(mydata$HoH.Gender,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  jpeg("Graphs/Frequency/Head of Household By Nationality.jpg")
  PlotXTabs(mydata,HoH.Gender,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Head of Household By Nationality.jpg")
  PlotXTabs(mydata,HoH.Gender,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Family Size by Nationality
  print("Family Size By Nationality")
  CrossTable(mydata$Family.Size,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Girls Under 18 by Nationality
  print("Girls Under 18 By Nationality")
  CrossTable(mydata$X..of.girls.in.case...18.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  summaryBy(X..of.girls.in.case...18. ~ Nationality, data = mydata, 
            FUN = list(mean, median))
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Boys Under 18 by Nationality
  print("Boys Under 18 By Nationality")
  CrossTable(mydata$X..of.boys.in.case...18.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  summaryBy(X..of.boys.in.case...18. ~ Nationality, data = mydata, 
            FUN = list(mean, median))
  
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Females 18 - 59 by Nationality
  print("Females 18 - 59 By Nationality")
  CrossTable(mydata$X..of..adult.females...18.59.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  summaryBy(X..of..adult.females...18.59. ~ Nationality, data = mydata, 
            FUN = list(mean, median))
  
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Males 18-59 by Nationality
  print("Males 18 - 59 By Nationality")
  CrossTable(mydata$X..of.adult.males...18..59.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  
  summaryBy(X..of.adult.males...18..59. ~ Nationality, data = mydata, 
            FUN = list(mean, median))
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Adults 60+ by Nationality
  print("Adults 60+ By Nationality")
  CrossTable(mydata$X..of.elderly...60.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  
  summaryBy(X..of.elderly...60. ~ Nationality, data = mydata, 
            FUN = list(mean, median))
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Household Disability by Nationality
  print("Household Disability/Chronically Ill By Nationality")
  CrossTable(mydata$Household.Members.disabled.or.chronically.ill,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  
  jpeg("Graphs/Frequency/Household Disability or Chronic Illness By Nationality.jpg")
  PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Household Disability or Chronic Illness By Nationality.jpg")
  PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Household Disability by Nationality
  print("Household Disability By Nationality")
  CrossTable(mydata$HH.Member.has.disability,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Household Chronic Illness by Nationality
  print("Household Chronic Illness By Nationality")
  CrossTable(mydata$HH..Member.has.chronic.ill,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Household Both Chronic Illness and Disability by Nationality
  print("Household Both Chronic Illness and Disability By Nationality")
  CrossTable(mydata$HH.member.has.both_dis_CI,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Household No Chronic Illness or Disability by Nationality
  print("Household No Chronic Illness or Disability By Nationality")
  CrossTable(mydata$HH.member.has.none_.dis_CI,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation Where Do You Live Currently by Nationality
  print("Where Do You Live Currently By Nationality")
  CrossTable(mydata$Where.do.you.live.currently.,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  jpeg("Graphs/Frequency/Where Do You Live Currently By Nationality.jpg")
  PlotXTabs(mydata,Where.do.you.live.currently.,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/Where Do You Live Currently By Nationality.jpg")
  PlotXTabs(mydata,Where.do.you.live.currently.,Nationality, "percent")
  dev.off()
  
  print("#############################################################################################################")
  print("#############################################################################################################")
  ## CrossTabulation UNHCR Cash Beneficiary by Nationality
  print("UNHCR Cash Beneficiary By Nationality")
  CrossTable(mydata$UNHCR.Cash.Beneficiary,mydata$Nationality, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
  
  jpeg("Graphs/Frequency/UNHCR Cash Beneficiary By Nationality.jpg")
  PlotXTabs(mydata,UNHCR.Cash.Beneficiary,Nationality, "stack")
  dev.off()
  
  jpeg("Graphs/Percent/UNHCR Cash Beneficiary By Nationality.jpg")
  PlotXTabs(mydata,UNHCR.Cash.Beneficiary,Nationality, "percent")
  dev.off()
  
sink()


########################################################################################
## Gender Disaggregation
########################################################################################

sink("Outputs/Head of Household Gender Analysis Output.txt")

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Refugee/Jordanian by HoH.Gender
print("Refugee By HoH.Gender")
CrossTable(mydata$Refugee.vs.Jordanian,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Refugee Jordanian By HoH.Gender.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Refugee Jordanian By HoH.Gender.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR ID by HoH.Gender
print("UNHCR By HoH.Gender")
CrossTable(mydata$X.UNHCR.Case.ID.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
jpeg("Graphs/Frequency/UNHCR ID By HoH.Gender.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR ID By HoH.Gender.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Governorate by HoH.Gender
print("Governorate By HoH.Gender")
CrossTable(mydata$Governorate,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Governorate By HoH.Gender.jpg")
PlotXTabs(mydata,Governorate,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Governorate By HoH.Gender.jpg")
PlotXTabs(mydata,Governorate,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation District by HoH.Gender
print("District By HoH.Gender")
CrossTable(mydata$District,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Respondent Gender by HoH.Gender
print("Respondent Gender By HoH.Gender")
CrossTable(mydata$Respondent.Gender,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Respondent Gender By HoH.Gender.jpg")
PlotXTabs(mydata,Respondent.Gender,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Respondent Gender By HoH.Gender.jpg")
PlotXTabs(mydata,Respondent.Gender,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Head of Household by HoH.Gender
print("Head of Household By HoH.Gender")
CrossTable(mydata$HoH.Gender,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Head of Household By HoH.Gender.jpg")
PlotXTabs(mydata,HoH.Gender,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Head of Household By HoH.Gender.jpg")
PlotXTabs(mydata,HoH.Gender,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Family Size by HoH.Gender
print("Family Size By HoH.Gender")
CrossTable(mydata$Family.Size,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Girls Under 18 by HoH.Gender
print("Girls Under 18 By HoH.Gender")
CrossTable(mydata$X..of.girls.in.case...18.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.girls.in.case...18. ~ HoH.Gender, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Boys Under 18 by HoH.Gender
print("Boys Under 18 By HoH.Gender")
CrossTable(mydata$X..of.boys.in.case...18.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.boys.in.case...18. ~ HoH.Gender, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Females 18 - 59 by HoH.Gender
print("Females 18 - 59 By HoH.Gender")
CrossTable(mydata$X..of..adult.females...18.59.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of..adult.females...18.59. ~ HoH.Gender, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Males 18-59 by HoH.Gender
print("Males 18 - 59 By HoH.Gender")
CrossTable(mydata$X..of.adult.males...18..59.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.adult.males...18..59. ~ HoH.Gender, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Adults 60+ by HoH.Gender
print("Adults 60+ By HoH.Gender")
CrossTable(mydata$X..of.elderly...60.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.elderly...60. ~ HoH.Gender, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by HoH.Gender
print("Household Disability/Chronically Ill By HoH.Gender")
CrossTable(mydata$Household.Members.disabled.or.chronically.ill,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


jpeg("Graphs/Frequency/Household Disability or Chronic Illness By HoH.Gender.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Household Disability or Chronic Illness By HoH.Gender.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by HoH.Gender
print("Household Disability By HoH.Gender")
CrossTable(mydata$HH.Member.has.disability,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Chronic Illness by HoH.Gender
print("Household Chronic Illness By HoH.Gender")
CrossTable(mydata$HH..Member.has.chronic.ill,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Both Chronic Illness and Disability by HoH.Gender
print("Household Both Chronic Illness and Disability By HoH.Gender")
CrossTable(mydata$HH.member.has.both_dis_CI,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household No Chronic Illness or Disability by HoH.Gender
print("Household No Chronic Illness or Disability By HoH.Gender")
CrossTable(mydata$HH.member.has.none_.dis_CI,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Where Do You Live Currently by HoH.Gender
print("Where Do You Live Currently By HoH.Gender")
CrossTable(mydata$Where.do.you.live.currently.,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Where Do You Live Currently By HoH.Gender.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/Where Do You Live Currently By HoH.Gender.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,HoH.Gender, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR Cash Beneficiary by HoH.Gender
print("UNHCR Cash Beneficiary By HoH.Gender")
CrossTable(mydata$UNHCR.Cash.Beneficiary,mydata$HoH.Gender, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/UNHCR Cash Beneficiary By HoH.Gender.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,HoH.Gender, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR Cash Beneficiary By HoH.Gender.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,HoH.Gender, "percent")
dev.off()

sink()


########################################################################################
## Governorate Disaggregation
########################################################################################

sink("Outputs/Governorate Analysis Output.txt")

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Refugee/Jordanian by Governorate
print("Refugee By Governorate")
CrossTable(mydata$Refugee.vs.Jordanian,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Refugee Jordanian By Governorate.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Refugee Jordanian By Governorate.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR ID by Governorate
print("UNHCR By Governorate")
CrossTable(mydata$X.UNHCR.Case.ID.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
jpeg("Graphs/Frequency/UNHCR ID By Governorate.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR ID By Governorate.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Governorate by Governorate
print("Governorate By Governorate")
CrossTable(mydata$Governorate,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Governorate By Governorate.jpg")
PlotXTabs(mydata,Governorate,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Governorate By Governorate.jpg")
PlotXTabs(mydata,Governorate,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation District by Governorate
print("District By Governorate")
CrossTable(mydata$District,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Respondent Gender by Governorate
print("Respondent Gender By Governorate")
CrossTable(mydata$Respondent.Gender,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Respondent Gender By Governorate.jpg")
PlotXTabs(mydata,Respondent.Gender,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Respondent Gender By Governorate.jpg")
PlotXTabs(mydata,Respondent.Gender,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Head of Household by Governorate
print("Head of Household By Governorate")
CrossTable(mydata$HoH.Gender,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Head of Household By Governorate.jpg")
PlotXTabs(mydata,HoH.Gender,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Head of Household By Governorate.jpg")
PlotXTabs(mydata,HoH.Gender,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Family Size by Governorate
print("Family Size By Governorate")
CrossTable(mydata$Family.Size,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Girls Under 18 by Governorate
print("Girls Under 18 By Governorate")
CrossTable(mydata$X..of.girls.in.case...18.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.girls.in.case...18. ~ Governorate, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Boys Under 18 by Governorate
print("Boys Under 18 By Governorate")
CrossTable(mydata$X..of.boys.in.case...18.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.boys.in.case...18. ~ Governorate, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Females 18 - 59 by Governorate
print("Females 18 - 59 By Governorate")
CrossTable(mydata$X..of..adult.females...18.59.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of..adult.females...18.59. ~ Governorate, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Males 18-59 by Governorate
print("Males 18 - 59 By Governorate")
CrossTable(mydata$X..of.adult.males...18..59.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.adult.males...18..59. ~ Governorate, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Adults 60+ by Governorate
print("Adults 60+ By Governorate")
CrossTable(mydata$X..of.elderly...60.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.elderly...60. ~ Governorate, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by Governorate
print("Household Disability/Chronically Ill By Governorate")
CrossTable(mydata$Household.Members.disabled.or.chronically.ill,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


jpeg("Graphs/Frequency/Household Disability or Chronic Illness By Governorate.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Household Disability or Chronic Illness By Governorate.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by Governorate
print("Household Disability By Governorate")
CrossTable(mydata$HH.Member.has.disability,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Chronic Illness by Governorate
print("Household Chronic Illness By Governorate")
CrossTable(mydata$HH..Member.has.chronic.ill,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Both Chronic Illness and Disability by Governorate
print("Household Both Chronic Illness and Disability By Governorate")
CrossTable(mydata$HH.member.has.both_dis_CI,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household No Chronic Illness or Disability by Governorate
print("Household No Chronic Illness or Disability By Governorate")
CrossTable(mydata$HH.member.has.none_.dis_CI,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Where Do You Live Currently by Governorate
print("Where Do You Live Currently By Governorate")
CrossTable(mydata$Where.do.you.live.currently.,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Where Do You Live Currently By Governorate.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/Where Do You Live Currently By Governorate.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,Governorate, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR Cash Beneficiary by Governorate
print("UNHCR Cash Beneficiary By Governorate")
CrossTable(mydata$UNHCR.Cash.Beneficiary,mydata$Governorate, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/UNHCR Cash Beneficiary By Governorate.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,Governorate, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR Cash Beneficiary By Governorate.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,Governorate, "percent")
dev.off()

sink()

########################################################################################
## Family Size Disaggregation
########################################################################################

sink("Outputs/Family Size Analysis Output.txt")

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Refugee/Jordanian by FAMILY_SIZE
print("Refugee By FAMILY_SIZE")
CrossTable(mydata$Refugee.vs.Jordanian,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Refugee Jordanian By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/Refugee Jordanian By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Refugee.vs.Jordanian,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR ID by FAMILY_SIZE
print("UNHCR By FAMILY_SIZE")
CrossTable(mydata$X.UNHCR.Case.ID.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
jpeg("Graphs/Frequency/UNHCR ID By FAMILY_SIZE.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR ID By FAMILY_SIZE.jpg")
PlotXTabs(mydata,X.UNHCR.Case.ID.,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation FAMILY_SIZE by FAMILY_SIZE
print("FAMILY_SIZE By FAMILY_SIZE")
CrossTable(mydata$FAMILY_SIZE,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/FAMILY_SIZE By FAMILY_SIZE.jpg")
PlotXTabs(mydata,FAMILY_SIZE,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/FAMILY_SIZE By FAMILY_SIZE.jpg")
PlotXTabs(mydata,FAMILY_SIZE,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation District by FAMILY_SIZE
print("District By FAMILY_SIZE")
CrossTable(mydata$District,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Respondent Gender by FAMILY_SIZE
print("Respondent Gender By FAMILY_SIZE")
CrossTable(mydata$Respondent.Gender,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Respondent Gender By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Respondent.Gender,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/Respondent Gender By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Respondent.Gender,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Head of Household by FAMILY_SIZE
print("Head of Household By FAMILY_SIZE")
CrossTable(mydata$HoH.Gender,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Head of Household By FAMILY_SIZE.jpg")
PlotXTabs(mydata,HoH.Gender,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/Head of Household By FAMILY_SIZE.jpg")
PlotXTabs(mydata,HoH.Gender,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Family Size by FAMILY_SIZE
print("Family Size By FAMILY_SIZE")
CrossTable(mydata$Family.Size,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Girls Under 18 by FAMILY_SIZE
print("Girls Under 18 By FAMILY_SIZE")
CrossTable(mydata$X..of.girls.in.case...18.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.girls.in.case...18. ~ FAMILY_SIZE, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Boys Under 18 by FAMILY_SIZE
print("Boys Under 18 By FAMILY_SIZE")
CrossTable(mydata$X..of.boys.in.case...18.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of.boys.in.case...18. ~ FAMILY_SIZE, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Females 18 - 59 by FAMILY_SIZE
print("Females 18 - 59 By FAMILY_SIZE")
CrossTable(mydata$X..of..adult.females...18.59.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

summaryBy(X..of..adult.females...18.59. ~ FAMILY_SIZE, data = mydata, 
          FUN = list(mean, median))


print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Males 18-59 by FAMILY_SIZE
print("Males 18 - 59 By FAMILY_SIZE")
CrossTable(mydata$X..of.adult.males...18..59.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.adult.males...18..59. ~ FAMILY_SIZE, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Adults 60+ by FAMILY_SIZE
print("Adults 60+ By FAMILY_SIZE")
CrossTable(mydata$X..of.elderly...60.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


summaryBy(X..of.elderly...60. ~ FAMILY_SIZE, data = mydata, 
          FUN = list(mean, median))

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by FAMILY_SIZE
print("Household Disability/Chronically Ill By FAMILY_SIZE")
CrossTable(mydata$Household.Members.disabled.or.chronically.ill,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)


jpeg("Graphs/Frequency/Household Disability or Chronic Illness By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/Household Disability or Chronic Illness By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Household.Members.disabled.or.chronically.ill,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Disability by FAMILY_SIZE
print("Household Disability By FAMILY_SIZE")
CrossTable(mydata$HH.Member.has.disability,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Chronic Illness by FAMILY_SIZE
print("Household Chronic Illness By FAMILY_SIZE")
CrossTable(mydata$HH..Member.has.chronic.ill,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household Both Chronic Illness and Disability by FAMILY_SIZE
print("Household Both Chronic Illness and Disability By FAMILY_SIZE")
CrossTable(mydata$HH.member.has.both_dis_CI,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Household No Chronic Illness or Disability by FAMILY_SIZE
print("Household No Chronic Illness or Disability By FAMILY_SIZE")
CrossTable(mydata$HH.member.has.none_.dis_CI,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation Where Do You Live Currently by FAMILY_SIZE
print("Where Do You Live Currently By FAMILY_SIZE")
CrossTable(mydata$Where.do.you.live.currently.,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/Where Do You Live Currently By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/Where Do You Live Currently By FAMILY_SIZE.jpg")
PlotXTabs(mydata,Where.do.you.live.currently.,FAMILY_SIZE, "percent")
dev.off()

print("#############################################################################################################")
print("#############################################################################################################")
## CrossTabulation UNHCR Cash Beneficiary by FAMILY_SIZE
print("UNHCR Cash Beneficiary By FAMILY_SIZE")
CrossTable(mydata$UNHCR.Cash.Beneficiary,mydata$FAMILY_SIZE, prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)

jpeg("Graphs/Frequency/UNHCR Cash Beneficiary By FAMILY_SIZE.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,FAMILY_SIZE, "stack")
dev.off()

jpeg("Graphs/Percent/UNHCR Cash Beneficiary By FAMILY_SIZE.jpg")
PlotXTabs(mydata,UNHCR.Cash.Beneficiary,FAMILY_SIZE, "percent")
dev.off()

sink()
