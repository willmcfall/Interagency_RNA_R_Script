## Clear Workspace
rm(list = ls())

## Install libraries
## install.packages("xlsx")
## install.packages("Hmisc")
## install.packages("plyr")
## install.packages("dplyr")
## install.packages("ggplot2")
## install.packages("gmodels")
## install.packages("productplots")
## install.packages("CGPfunctions")
## install.packages("doBy")
## install.packages("tidyverse")
## install.packages("rlang")
## install.packages("psych")

## Load libraries
library("doBy")
library("CGPfunctions")
library("dplyr")
library("ggplot2")
library("productplots")
library("Hmisc")
library("xlsx")
library("plyr")
library("gmodels")
library("tidyverse")
library("rlang")
library(psych)

###########################################################################################################################
## Data Setup
###########################################################################################################################


## Set working directory
setwd("C:/Users/william.mcfall/Desktop/Project Files/16. COVID19 RNA/R Script/Interagency_RNA_R_Script") 

## Load data
data <- read.xlsx("../RNA Dataset - 20200414.xlsx", "Sheet1", header=TRUE)


###########################################################################################################################
## Data Cleaning
###########################################################################################################################


## Clean errors
data$Refugee.vs.Jordanian <- revalue(data$Refugee.vs.Jordanian, c(refugee = "Refugee"))
data$Refugee.vs.Jordanian <- revalue(data$Refugee.vs.Jordanian, c(no = "Non-Refugee"))
data$X.UNHCR.Case.ID. <- revalue(data$X.UNHCR.Case.ID., c(Yes = "yes"))
data$Consent <- revalue(data$Consent, c(Yes = "yes"))
data$Household.Members.disabled.or.chronically.ill <- revalue(data$Household.Members.disabled.or.chronically.ill, c(BOTH = "both", Disability = "disability", "no " = "no"))
data$Enumerator.Agency <- revalue(data$Enumerator.Agency, c("UNICEF -TdH" = "UNICEF - TdH"))
data$If.host.community..what.is.your.type.of.dwelling.possession. <- revalue(data$If.host.community..what.is.your.type.of.dwelling.possession., c("Unknown -- did not ask question to this case" = "unknown"))

## Clean recode
data$HH.Member.has.disability<-as.character(data$HH.Member.has.disability)
data$HH.Member.has.disability<- revalue(data$HH.Member.has.disability, c("1" = "Yes", "0" = "No"))
data$HH.Member.has.disability[is.na(data$HH.Member.has.disability)] <- "No"
data$HH.Member.has.disability <- as.factor(data$HH.Member.has.disability)

data$HH..Member.has.chronic.ill<-as.character(data$HH..Member.has.chronic.ill)
data$HH..Member.has.chronic.ill<- revalue(data$HH..Member.has.chronic.ill, c("1" = "Yes", "0" = "No"))
data$HH..Member.has.chronic.ill[is.na(data$HH..Member.has.chronic.ill)] <- "No"
data$HH..Member.has.chronic.ill<- as.factor(data$HH..Member.has.chronic.ill)

data$HH.member.has.both_dis_CI<-as.character(data$HH.member.has.both_dis_CI)
data$HH.member.has.both_dis_CI<- revalue(data$HH.member.has.both_dis_CI, c("1" = "Yes", "0" = "No"))
data$HH.member.has.both_dis_CI[is.na(data$HH.member.has.both_dis_CI)] <- "No"
data$HH.member.has.both_dis_CI <- as.factor(data$HH.member.has.both_dis_CI)

data$HH.member.has.none_.dis_CI<-as.character(data$HH.member.has.none_.dis_CI)
data$HH.member.has.none_.dis_CI<- revalue(data$HH.member.has.none_.dis_CI, c("1" = "Yes", "0" = "No"))
data$HH.member.has.none_.dis_CI[is.na(data$HH.member.has.none_.dis_CI)] <- "No"
data$HH.member.has.none_.dis_CI<-as.factor(data$HH.member.has.none_.dis_CI)
  
data$Nationality<- revalue(data$Nationality, c(jordanian = "JOR", Palestinian = "PSE", Pakistan = "PAK"))


###########################################################################################################################
## Data Transformation
###########################################################################################################################


## Reclassify Family Size
data$FAMILY_SIZE <- NULL
data$FAMILY_SIZE[data$Family.Size <= 3] <- "3 or less"
data$FAMILY_SIZE[data$Family.Size == 4] <- "4 to 6"
data$FAMILY_SIZE[data$Family.Size == 5] <- "4 to 6"
data$FAMILY_SIZE[data$Family.Size == 6] <- "4 to 6"
data$FAMILY_SIZE[data$Family.Size >= 7] <- "7 or more"
data$FAMILY_SIZE <- as.factor(data$FAMILY_SIZE)

## Reclassify Non-Syrian Refugees
data$REFUGEE_STATUS <- NULL
data$REFUGEE_STATUS [data$Nationality == ("SYR")] <- "Syrian Refugee"
data$REFUGEE_STATUS [data$Nationality == ("SOM") | data$Nationality == ("SUD") | data$Nationality == ("PAK") | data$Nationality ==("IRQ") | data$Nationality ==("YEM") | data$Nationality ==("PSE")] <- "Non-Syrian Refugee" 
data$REFUGEE_STATUS [data$Nationality == ("JOR")] <- "Non-Refugee"
data$REFUGEE_STATUS <- as.factor(data$REFUGEE_STATUS)

###########################################################################################################################
## Data Type Definition
###########################################################################################################################


# Define categorical data and disaggregate data
categorical_var <- c("Consent", "Enumerator", "Enumerator.Agency", "Refugee.vs.Jordanian", "Enumerator.Agency", "X.UNHCR.Case.ID.", "Nationality", "Governorate", "District", "Respondent.Gender", "HoH.Gender", "Household.Members.disabled.or.chronically.ill", "HH.Member.has.disability","HH..Member.has.chronic.ill","HH.member.has.both_dis_CI","HH.member.has.none_.dis_CI","Where.do.you.live.currently.", "If.host.community..what.is.your.type.of.dwelling.possession." , "UNHCR.Cash.Beneficiary", "FAMILY_SIZE", "REFUGEE_STATUS")
numerical_var <- c("Family.Size", "X..of.girls.in.case...18.", "X..of.boys.in.case...18.","X..of..adult.females...18.59.","X..of.adult.males...18..59.","X..of.elderly...60.","X..of.HH.members.with.disabilities", "X..of.HH.members.with.chronic.illness")
disaggregate_var <- c("Nationality", "Governorate", "HoH.Gender", "FAMILY_SIZE", "REFUGEE_STATUS", "Where.do.you.live.currently.")


###########################################################################################################################
## Data Analysis - Aggregated
###########################################################################################################################

sink("Outputs/Aggregated Analysis Output.txt")
 
 summary(data)
  
  for (i in categorical_var) {
    a <- paste0("data$", i) %>% parse_expr()
    title = paste(i, " Frequency")
    print("######################################################")
    print(title)
    print(table(eval(a)))
    print(prop.table(table(eval(a))))
  }
  
  
  for (i in numerical_var) {
    title = paste(i, " Statistics")
    print("######################################################")
    print(title)
    print(data$i)
    a <- as.double(data[,i])
    print(summary(a))
    plot_name = paste(i, " Plot")
    freq_file_name= paste("Graphs/Frequency/",plot_name,".jpg")
    jpeg(freq_file_name)
    hist(a, breaks=12, col="#2E9FDF")
    dev.off()
  }
  
sink()
  
  
###########################################################################################################################
## Data Analysis - Disaggregated
###########################################################################################################################
  

sink("Outputs/Disaggregated Analysis Output.txt")
  
  # Loop through combinations of categorical data and disaggregated data, running crosstable
  for (i in categorical_var) {
    for (j in disaggregate_var) {
      a <- paste0("data$", i) %>% parse_expr()
      b <- paste0("data$", j) %>% parse_expr()
      title = paste(i," by ",j)
      print("######################################################")
      print(title)
      CrossTable(eval(a),eval(b), prop.r=FALSE, prop.c=TRUE, prop.t=FALSE, prop.chisq = FALSE)
    }
  }
  
sink()
