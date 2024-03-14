###############################################################
# Done by : Boon Tze Suen           UOW ID  : 7311898         #
# Done by : Tan Zong YI             UOW ID  : 7224059         #
# Done by : Tan Zhen Xuan Shaun     UOW ID  : 7021690         #
# Done by :                         UOW ID  :          #
# Done by :                         UOW ID  :          #
# Done by :                         UOW ID  :          #
# Done From : 12 Feb 2023                                     #
###############################################################

# Install
#install.packages("dplyr") # <-- change as required

# Import
library(ggplot2)
library(kohonen)
library(sp)
library(maptools)
library(rgeos) # Interface to Geometry engine
library(MASS)  # Function and datasets to support Venables & Ripley 
library(Hmisc) # Contains many function useful for data analysis
library(caret)
library(tidyverse)
library(dplyr)

# Data preprocessing
# Load the data into Data_Frame
df <- read.csv("ac_2023_02_13.csv") # 5333rows, 167 columns

# Restrict model sold to: Aus
df <- df[grep("Australia", df$Sold_in),]

# after filter remove column
df = subset(df,select=-c(Sold_in)) # Observation: 5,224rows, 166columns

# Drop field model number, family name, GrandDate: not relevant, empty
df = subset(df,select=-c(Model_No,Family.Name,GrandDate)) # Observation: 5,224rows, 163columns

# check the Data type of each column
str(df)
# Observation:
# we observe that the all the attribute given is of char datatype
# conclusion: Seem like have to manually process the data ... 

# Attribute: ApplStandard, MEPSComp, N.Standard, Brand
# Attribute: avg_pwr_standby_mode
table(df$ApplStandard) # this function give a summary of the different categorical of data 
table(df$MEPSComp)
table(df$N.Standard)
table(df$Brand)
table(df$avg_pwr_standby_mode) #N/A 5224

# we will use caret package -- for One Hot Encoding
# dummify the data
splitMi = df[c(1:3,5)]
dumb <- dummyVars(" ~ .", data = splitMi)

processed_df <- data.frame(predict(dumb, newdata=splitMi))

# :) love manual but time to write function ... 
###############################################
#                  Function                   #
###############################################

# 1. Support function
dataCheck <- function(df, col_name){        
  column <- df[[col_name]]
  
  #Data check
  count <- nrow(df)
  NA_exist <- sum(df[[col_name]] == "N/A")
  existHypen = sum(df[[col_name]] == "-")
  existZero = sum(df[[col_name]] == "0.001")
  
  # Visual
  missing = NA_exist + existHypen
  percentilee = (missing/count)*100
  
  #Before
  print(paste("Total Rows:", count))
  print(paste("No of NA:", NA_exist))
  print(paste("No of -:", existHypen))
  print(paste("No of 0:", existZero))
  print(paste("NA_exist + existHypen", missing))
  print(paste("percentile of data missing:",percentilee))
} #working

# 2. support function
convert_XXX_to_YYY <- function(df, col_name, changeToValue, mediann){
  df[[col_name]] <- gsub(changeToValue, mediann, df[[col_name]])
  return(df)
}

extract_soul <- function(df, col_name){
  df[[col_name]] <- substr(df[[col_name]], 1, 6)
  return(df)
} #nt tested

convert <- function(df, col_name, changeToValue){
  # Convert "N/A" into 0
  df[[col_name]]<- gsub("N/A", changeToValue, df[[col_name]])
  return(df)
} #working

convert_1.0 <- function(df, col_name, changeToValue){
  # Convert "-" into 0
  df[[col_name]]<- gsub("-", changeToValue, df[[col_name]])
  return(df)
} #working

convert2 <- function(df, col_name){
  # cha to numeric
  df[[col_name]] <- as.numeric(df[[col_name]])
  return(df)
} #working

convert3 <- function(df, col_name){
  #get mean
  getMean <- mean(df[[col_name]])
  return(getMean)
} #working

convert4 <- function(df, col_name, getMean){
  # We perform data imputation
  df[[col_name]] <- gsub("0.0001", getMean, df[[col_name]])
  return(df)
}

mesh <- function(df, col_name, getMean){
  splitMi <- convert(df, col_name)
  splitMi <- convert_1.0(df, col_name)
  splitMi <- convert2(df, col_name)
  mean = convert3(df, col_name)
  splitMi <- convert4(df, col_name, mean)
  splitMi <- extract_soul(df, col_name)
  splitMi <- convert2(df, col_name)
  return(splitMi)
}
#splitMi <- mesh(splitMi, "H2_COP", mean)

# Attribute: C.Dehumid_Rated 6
table(df$C.Dehumid_Rated)
# 1. Observation data is in character data type need convert to numeric
# 2. The data set uses an unconventional N/A instead of NA
splitMi = df[c(6)]

dataCheck(splitMi, "C.Dehumid_Rated")
# Observation:
# No of NA: 3277
# percentile  of data missing: 62.72
splitMi <- convert(splitMi, "C.Dehumid_Rated", 0) # "N/A" into 0
splitMi <- convert2(splitMi, "C.Dehumid_Rated") #turn to num
str(splitMi)
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Configuration1, Configuration2, Configuration2.unitmount, Configuration3_Sink, 
# Attribute: Configuration3_Source, Country
table(df$Configuration1)
table(df$Configuration2)
table(df$Configuration2.unitmount)
table(df$Configuration3_Sink)
table(df$Configuration3_Source)
table(df$Country)

# Proposal: OHE on the above 6 attribute
# dummify the data
splitMi = df[c(7:12)]
dumb1 <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb1, newdata=splitMi))

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: C.Power_Inp_Rated  13
table(df$C.Power_Inp_Rated)

#Data Check
splitMi = df[c(13)]
dataCheck(splitMi, "C.Power_Inp_Rated")
# Observation
# "No of NA: 0"
# "No of -: 5"
# "No of 0: 0"
# "NA_exist + existHypen 5"
# "percentile of data missing: 0.0957120980091884"
#  There exist 5 missing data
#  proposal: We perform data imputation -- using mean
splitMi <- convert_1.0(splitMi, "C.Power_Inp_Rated", 0.0001)
splitMi <- convert2(splitMi, "C.Power_Inp_Rated") #turn to num
str(splitMi) #num
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: C.Sens_Cool_Rated  14
table(df$C.Sens_Cool_Rated)
splitMi = df[c(14)]
dataCheck(splitMi, "C.Sens_Cool_Rated")
# Observation
# "No of NA: 3277"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "C.Sens_Cool_Rated", 0.0001) # Convert "N/A" into unique
splitMi <- convert2(splitMi, "C.Sens_Cool_Rated")        #turn to num
mediann <- median(splitMi$C.Sens_Cool_Rated[splitMi$C.Sens_Cool_Rated != 0.0001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "C.Sens_Cool_Rated")
splitMi <- convert_XXX_to_YYY(splitMi, "C.Sens_Cool_Rated", 0.0001, mediann)

#test
splitMi <- convert2(splitMi, "C.Sens_Cool_Rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)



# Attribute: C.Total.Cool.Rated 15
table(df$C.Total.Cool.Rated)
splitMi = df[c(15)]
dataCheck(splitMi, "C.Total.Cool.Rated")
# Observation:
# 5missing data 
# proposal to perform data imputation -- using median
splitMi <- convert_1.0(splitMi, "C.Total.Cool.Rated", 0.0001) # Convert "-" into unique
splitMi <- convert2(splitMi, "C.Total.Cool.Rated")         #turn to num
mediann <- median(splitMi$C.Total.Cool.Rated[splitMi$C.Total.Cool.Rated != 0.0001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "C.Total.Cool.Rated", 0.0001, mediann)
splitMi <- convert2(splitMi, "C.Total.Cool.Rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Depth 16
table(df$Depth) # N/A 3860
sum(df$Depth == "N/A") # 3860
sum(df$Depth == "-") #752
# Observation: one of the requirement "size"
# proposal: data imputation --using median why: depth of aircon cannot be 0
splitMi = df[c(16)]
dataCheck(splitMi, "Depth")
str(splitMi)
splitMi <- convert(splitMi, "Depth", 0.001)     # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Depth", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Depth")            #turn to num
str(splitMi) 
mediann <- median(splitMi$Depth[splitMi$Depth != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Depth", 0.001, mediann)
splitMi <- convert2(splitMi, "Depth")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: H2_COP coefficient of performance? 17
table(df$H2_COP) 
splitMi = df[c(17)]
dataCheck(splitMi, "H2_COP")
splitMi <- convert(splitMi, "H2_COP", 0.001)      # Convert "N/A" into unique
splitMi <- convert2(splitMi, "H2_COP")            #turn to num
mediann <- median(splitMi$H2_COP[splitMi$H2_COP != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "H2_COP", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_COP")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute: H2_HeatPwrCapacity 18
table(df$H2_HeatPwrCapacity)
splitMi = df[c(18)]
dataCheck(splitMi, "H2_HeatPwrCapacity")
# Observation: missing data we do imputation
splitMi <- convert(splitMi, "H2_HeatPwrCapacity", 0.001)      # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_HeatPwrCapacity", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_HeatPwrCapacity")            #turn to num
mediann <- median(splitMi$H2_HeatPwrCapacity[splitMi$H2_HeatPwrCapacity != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "H2_HeatPwrCapacity", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_HeatPwrCapacity")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: H2_HeatPwrInput 19
table(df$H2_HeatPwrInput)
splitMi = df[c(19)]
dataCheck(splitMi, "H2_HeatPwrInput")
# Observation: missing data we do imputation
splitMi <- convert(splitMi, "H2_HeatPwrInput", 0.001)      # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_HeatPwrInput", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_HeatPwrInput")            #turn to num
mediann <- median(splitMi$H2_HeatPwrInput[splitMi$H2_HeatPwrInput != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "H2_HeatPwrInput", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_HeatPwrInput")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Height 20 
table(df$Height)
splitMi = df[c(20)]
dataCheck(splitMi, "Height")
# Observation: 88% missing data ... "size data" might be impt
splitMi <- convert(splitMi, "Height", 0.001)      # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Height", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Height")            #turn to num
mediann <- median(splitMi$Height[splitMi$Height != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Height", 0.001, mediann)
splitMi <- convert2(splitMi, "Height")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: H.Power_Inp_Rated 21
table(df$H.Power_Inp_Rated)
# Data check
splitMi = df[c(21)]
dataCheck(splitMi, "H.Power_Inp_Rated")
splitMi <- convert_1.0(splitMi, "H.Power_Inp_Rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H.Power_Inp_Rated")            #turn to num
mediann <- median(splitMi$H.Power_Inp_Rated[splitMi$H.Power_Inp_Rated != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "H.Power_Inp_Rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H.Power_Inp_Rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: H.Total.Heat.Rated 22
table(df$H.Total.Heat.Rated)
# Data check
splitMi = df[c(22)]
dataCheck(splitMi, "H.Total.Heat.Rated")
splitMi <- convert_1.0(splitMi, "H.Total.Heat.Rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H.Total.Heat.Rated")            #turn to num
mediann <- median(splitMi$H.Total.Heat.Rated[splitMi$H.Total.Heat.Rated != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "H.Total.Heat.Rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H.Total.Heat.Rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: indoorType 23
table(df$indoorType)
# Proposal: OHE
# Justification: Why OHE over label encoding to prevent the model from capturing numeric relationship
# we will use caret package -- for One Hot Encoding
# dummify the data
splitMi = df[c(23)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: EERtestAvg 24
table(df$EERtestAvg)
# Data check
splitMi = df[c(24)]
dataCheck(splitMi, "EERtestAvg")
splitMi <- convert_1.0(splitMi, "EERtestAvg", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "EERtestAvg")            #turn to num
mediann <- median(splitMi$EERtestAvg[splitMi$EERtestAvg != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "EERtestAvg", 0.001, mediann)
splitMi <- convert2(splitMi, "EERtestAvg")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: COPtestAvg 25
table(df$COPtestAvg)
# Data check
splitMi = df[c(25)]
dataCheck(splitMi, "COPtestAvg")
splitMi <- convert_1.0(splitMi, "COPtestAvg", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "COPtestAvg")            #turn to num
mediann <- median(splitMi$COPtestAvg[splitMi$COPtestAvg != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "COPtestAvg", 0.001, mediann)
splitMi <- convert2(splitMi, "COPtestAvg")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Invert 26
table(df$Invert)
# Observation:
# 1. might be interesting, will perform OHE
# we will use caret package -- for One Hot Encoding
# dummify the data
splitMi = df[c(26)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}

# Attribute: Setting_cool 27
table(df$Setting_cool)
splitMi = df[c(27)]
dataCheck(splitMi, "Setting_cool")
splitMi <- run_all(splitMi, "Setting_cool", 0.001)
str(splitMi)

mediann <- median(splitMi$Setting_cool[splitMi$Setting_cool != 0.001]) # exclude unique

splitMi <- convert_XXX_to_YYY(splitMi, "Setting_cool", 0.001, mediann)
str(splitMi)
splitMi <- convert2(splitMi,"Setting_cool")
str(splitMi)

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Setting_heat 28
table(df$Setting_heat)
# Data check
sum(df$Setting_heat == "N/A") # 5224
sum(df$Setting_heat == "-")   # 0
sum(df$Setting_heat == "0")   # 0
# Observation:
# column is filled with N/A we remove this column from our dataframe

# Attribute: Pnoc 29
table(df$Pnoc)
# Data check
splitMi = df[c(29)]
dataCheck(splitMi, "Pnoc")

splitMi <- run_all(splitMi, "Pnoc", 0.001)
mediann <- median(splitMi$Pnoc[splitMi$Pnoc != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "Pnoc", 0.001, mediann)
str(splitMi)
splitMi <- convert2(splitMi,"Pnoc")
str(splitMi)
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Pnoh 30 
table(df$Pnoh)
# Data check
splitMi = df[c(30)]
dataCheck(splitMi, "Pnoh")
splitMi <- run_all(splitMi, "Pnoh", 0.001)
mediann <- median(splitMi$Pnoh[splitMi$Pnoh != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "Pnoh", 0.001, mediann)
splitMi <- convert2(splitMi,"Pnoh")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: VSCP_EER50 31
table(df$VSCP_EER50)
# Data check
splitMi = df[c(31)]
dataCheck(splitMi, "VSCP_EER50")
splitMi <- run_all(splitMi, "VSCP_EER50", 0.001)
mediann <- median(splitMi$VSCP_EER50[splitMi$VSCP_EER50 != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "VSCP_EER50", 0.001, mediann)
splitMi <- convert2(splitMi,"VSCP_EER50")
str(splitMi)
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: VSCP_COP50 32 
table(df$VSCP_COP50)
# Data check
splitMi = df[c(32)]
dataCheck(splitMi, "VSCP_COP50")
splitMi <- run_all(splitMi, "VSCP_COP50", 0.001)
mediann <- median(splitMi$VSCP_COP50[splitMi$VSCP_COP50 != 0.001]) # exclude unique
splitMi <- convert_XXX_to_YYY(splitMi, "VSCP_COP50", 0.001, mediann)
splitMi <- convert2(splitMi,"VSCP_COP50")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)
str(splitMi)

# Attribute: eermepslev 33
table(df$eermepslev)
splitMi = df[c(33)]
dataCheck(splitMi, "eermepslev") # all NA
str(splitMi)
# proposal: remove the column

# Attribute: TestedOutputEER 34
table(df$TestedOutputEER)
splitMi = df[c(34)]
dataCheck(splitMi, "TestedOutputEER") # All NA remove column
str(splitMi)
# proposal: remove the column

# Attribute: TestedOutputCOP 35
table(df$TestedOutputCOP)
splitMi = df[c(35)]
dataCheck(splitMi, "TestedOutputCOP") # All NA remove 
# proposal: remove the column

names(df)
# Attribute: AnnualOutputEER 36
table(df$AnnualOutputEER)
# Data check
splitMi = df[c(36)]
dataCheck(splitMi, "AnnualOutputEER")
splitMi <- extract_soul(splitMi, "AnnualOutputEER")
splitMi <- run_all(splitMi, "AnnualOutputEER", 0.001)

mediann <- median(splitMi$AnnualOutputEER[splitMi$AnnualOutputEER != 0.001]) # exclude unique
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "AnnualOutputEER", 0.001, mediann)
splitMi <- convert2(splitMi,"AnnualOutputEER")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: AnnualOutputCOP 37
table(df$AnnualOutputCOP)
# Data check
splitMi = df[c(37)]
dataCheck(splitMi, "AnnualOutputCOP")
splitMi <- extract_soul(splitMi, "AnnualOutputCOP")
splitMi <- run_all(splitMi, "AnnualOutputCOP", 0.001)
mediann <- median(splitMi$AnnualOutputCOP[splitMi$AnnualOutputCOP != 0.001])
splitMi <- convert_XXX_to_YYY(splitMi, "AnnualOutputCOP", 0.001, mediann)
splitMi <- convert2(splitMi,"AnnualOutputCOP")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: PL_EERMEPS 38
table(df$PL_EERMEPS)
# Data check
splitMi = df[c(38)]
dataCheck(splitMi, "PL_EERMEPS")
# observation: all NA we remove from dataframe

# Attribute: PL_COPMEPS 39
table(df$PL_EERMEPS)
# Data check
splitMi = df[c(39)]
dataCheck(splitMi, "PL_COPMEPS")
# observation: all NA we remove from dataframe

# Attribute: Star2010_Cool 42  --- relevant feature according to da question
table(df$Star2010_Cool)
# Data check
splitMi = df[c(42)]
dataCheck(splitMi, "Star2010_Cool")
splitMi <- extract_soul(splitMi, "Star2010_Cool")
splitMi <- run_all(splitMi, "Star2010_Cool", 0.001)
mediann <- median(splitMi$Star2010_Cool[splitMi$Star2010_Cool != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Star2010_Cool", 0.001, mediann)
splitMi <- convert2(splitMi,"Star2010_Cool")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Star2010_Heat 43 --- relevant feature according to da question
table(df$Star2010_Heat)
# Data check
splitMi = df[c(43)]
dataCheck(splitMi, "Star2010_Heat")
splitMi <- extract_soul(splitMi, "Star2010_Heat")
splitMi <- run_all(splitMi, "Star2010_Heat", 0.001)
mediann <- median(splitMi$Star2010_Heat[splitMi$Star2010_Heat != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Star2010_Heat", 0.001, mediann)
splitMi <- convert2(splitMi,"Star2010_Heat")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: outdoortype 44
table(df$outdoortype)
# We perform OHE -- dummify the data
splitMi = df[c(44)]
dumb1 <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb1, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Phase 45   ---Interesting can use presentation---
table(df$Phase)

# Observation: 4 categorical "-" "single" "three" "three-phase"
# Assumption:  three and three phase are the same we merge that category
splitMi = df[c(45)]
splitMi$Phase <- gsub("Three-phase", "Three", splitMi$Phase)
table(splitMi$Phase) # output: "-" "Single" "Three"

# Proposal: label encoding - numeric relation is applicable here.
processed_df$Phase <- as.numeric(factor(splitMi$Phase))

# Attribute: Refrigerant 46
table(df$Refrigerant)
# Data check
splitMi = df[c(46)]
dataCheck(splitMi, "Refrigerant")
# proposal: to use OHE - dk if numerical replation apply anot
# dummify the data
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Submit_ID 47 
table(df$Submit_ID)
# Observation: does not affect SRI values remove column

# Attribute: ExpDate 48
table(df$ExpDate)
# Observation: does not affect SRI values remove column

# Attribute: SubmitStatus 49 
table(df$SubmitStatus) # Approved 5224
# proposal: remove column all approved

# Attribute: Type 50
table(df$Type) # three cotogory "-" "cooling Only" "Reverse Cycle"
# reverse cycle mean can heat and cool
# proposal: OHE
# dummify the data
splitMi = df[c(50)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Width 51
table(df$Width)
# Data check
splitMi = df[c(51)]
dataCheck(splitMi, "Width")
splitMi <- extract_soul(splitMi, "Width")
splitMi <- run_all(splitMi, "Width", 0.001)
mediann <- median(splitMi$Width[splitMi$Width != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Width", 0.001, mediann)
splitMi <- convert2(splitMi,"Width")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Product.Class 52
table(df$Product.Class)
# Data check
splitMi = df[c(52)]
# Proposal: label encoding - numeric relation is applicable here.
processed_df$Product.Class <- as.numeric(factor(splitMi$Product.Class))

# Attribute: Demand.Response.Capability 53
table(df$Demand.Response.Capability)
# Demand.Response.Capability to respond to remote communications that increase or decrease
# proposal: OHE
splitMi = df[c(53)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Demand.Response.1 to 7 
table(df$Demand.Response.1)
# proposal: remove does provide much useful data  

names(df)

# Attribute: PartNumber 60
table(df$PartNumber)
# Data check
splitMi = df[c(60)]
# proposal: OHE
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: EER 61
table(df$EER)
# Data check
splitMi = df[c(61)]
dataCheck(splitMi, "EER")
# Energy Efficiency Rating(EER) 
splitMi <- extract_soul(splitMi, "EER")
splitMi <- run_all(splitMi, "EER", 0.001)
mediann <- median(splitMi$EER[splitMi$EER != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "EER", 0.001, mediann)
splitMi <- convert2(splitMi,"EER")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Availability.Status 62
table(df$Availability.Status)
# proposal: remove column does not affect SRI values


# Attribute: star2000_cool 63
table(df$star2000_cool) #output: 5224 N/A
# proposal: remove feature

# Attribute: star2000_heat 64
table(df$star2000_heat) #output: 5224 N/A
# proposal: remove feature

# Attribute: Product.Website 65
table(df$Product.Website)
# proposal: remove website does not affect a product sri values

# Attribute: Representative.Brand.URL 66
table(df$Representative.Brand.URL)
# proposal: remove Brand.URL does not affect a product sri values

# Attribute: Variable.Output.Compressor 67
table(df$Variable.Output.Compressor)
# Proposal: label encoding - numeric relation is applicable here.
splitMi = df[c(67)]
processed_df$Variable.Output.Compressor <- as.numeric(factor(splitMi$Variable.Output.Compressor))

# Attribute: Star.Image.Large 68
table(df$Star.Image.Large)
# proposal: remove does not affect product sri

# Attribute: Star.Image.Small 69
table(df$Star.Image.Small)
# proposal: remove does not affect product sri

# Attribute: Registration.Number 70
table(df$Registration.Number)
# proposal: remove unique variable

# Attribute: Is.variable.speed 71
table(df$Is.variable.speed)
# Proposal: label encoding - numeric relation is applicable here.
splitMi = df[c(71)]
processed_df$Is.variable.speed <- as.numeric(factor(splitMi$Is.variable.speed))

# Attribute: Type.variable.output 72
table(df$Type.variable.output)
# proposal: OHE
splitMi = df[c(72)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Var.output.compressor 73
table(df$Var.output.compressor)
# proposal: OHE
splitMi = df[c(73)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

names(df)

# Attribute: Variable.output.rated.as.fixed.sp 74 
table(df$Variable.output.rated.as.fixed.sp)
# Proposal: label encoding - numeric relation is applicable here.
splitMi = df[c(74)]
processed_df$Variable.output.rated.as.fixed.sp <- as.numeric(factor(splitMi$Variable.output.rated.as.fixed.sp))

# Attribute: No.HSPF 75
table(df$No.HSPF)
# HSPF is a metric used in evaluation of air source heat pump in heating mode
# proposal: OHE
splitMi = df[c(75)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Rated.Total.Cool.Capacity.W 76
table(df$Rated.Total.Cool.Capacity.W)
# Data check
splitMi = df[c(76)]
dataCheck(splitMi, "Rated.Total.Cool.Capacity.W")
# Observation: some might be heatpump does not cool
# proposal: N/A, - data to be set as 0
splitMi <- extract_soul(splitMi, "Rated.Total.Cool.Capacity.W")
splitMi <- run_all(splitMi, "Rated.Total.Cool.Capacity.W", 0.001)
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.Total.Cool.Capacity.W", 0.001, 0)
splitMi <- convert2(splitMi,"Rated.Total.Cool.Capacity.W")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute: Rated.cooling.power.input.kW 77 
table(df$Rated.cooling.power.input.kW)
# Data check
splitMi = df[c(77)]
dataCheck(splitMi, "Rated.cooling.power.input.kW")
# Observation: some does not cool
# proposal: - data to be set as 0
splitMi <- extract_soul(splitMi, "Rated.cooling.power.input.kW")
splitMi <- run_all(splitMi, "Rated.cooling.power.input.kW", 0.001)
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.cooling.power.input.kW", 0.001, 0)
splitMi <- convert2(splitMi,"Rated.cooling.power.input.kW")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.T1.half.cap.results 78 
table(df$Have.T1.half.cap.results)
# Data check
splitMi = df[c(78)]
dataCheck(splitMi, "Have.T1.half.cap.results")
# Observation: all data is in N/A or - 
# Proposal: remove column

# Attribute: T1_half_cap_power_rated 79
table(df$T1_half_cap_power_rated)
# Data check
splitMi = df[c(79)]
dataCheck(splitMi, "T1_half_cap_power_rated")
splitMi <- extract_soul(splitMi, "T1_half_cap_power_rated")
splitMi <- run_all(splitMi, "T1_half_cap_power_rated", 0.001)
mediann <- median(splitMi$T1_half_cap_power_rated[splitMi$T1_half_cap_power_rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "T1_half_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi,"T1_half_cap_power_rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: T1_half_cap_cooling_cap_rated 80
table(df$T1_half_cap_cooling_cap_rated)
# Data check
splitMi = df[c(80)]
dataCheck(splitMi, "T1_half_cap_cooling_cap_rated")
splitMi <- extract_soul(splitMi, "T1_half_cap_cooling_cap_rated")
splitMi <- run_all(splitMi, "T1_half_cap_cooling_cap_rated", 0.001)
mediann <- median(splitMi$T1_half_cap_cooling_cap_rated[splitMi$T1_half_cap_cooling_cap_rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "T1_half_cap_cooling_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi,"T1_half_cap_cooling_cap_rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.T1.min.cap.results 81
table(df$Have.T1.min.cap.results)
# Data check
splitMi = df[c(81)]
# Proposal: label encoding - numeric relation is applicable here.
processed_df$Have.T1.min.cap.results <- as.numeric(factor(splitMi$Have.T1.min.cap.results))

# Attribute: T1_min_cap_power_rated 82
table(df$T1_min_cap_power_rated)
# Data check
splitMi = df[c(82)]
dataCheck(splitMi, "T1_min_cap_power_rated")
splitMi <- extract_soul(splitMi, "T1_min_cap_power_rated")
splitMi <- run_all(splitMi, "T1_min_cap_power_rated", 0.001)
mediann <- median(splitMi$T1_min_cap_power_rated[splitMi$T1_min_cap_power_rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "T1_min_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi,"T1_min_cap_power_rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute: T1_min_cap_cooling_cap_rated 83
table(df$T1_min_cap_cooling_cap_rated)
# Data check
splitMi = df[c(83)]
dataCheck(splitMi, "T1_min_cap_cooling_cap_rated")
splitMi <- extract_soul(splitMi, "T1_min_cap_cooling_cap_rated")
splitMi <- run_all(splitMi, "T1_min_cap_cooling_cap_rated", 0.001)
mediann <- median(splitMi$T1_min_cap_cooling_cap_rated[splitMi$T1_min_cap_cooling_cap_rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "T1_min_cap_cooling_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi,"T1_min_cap_cooling_cap_rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.low.temp.cool.full.cap.results 84
table(df$Have.low.temp.cool.full.cap.results)
# Data check
splitMi = df[c(84)]
# Proposal: label encoding - numeric relation is applicable here.
processed_df$Have.low.temp.cool.full.cap.results <- as.numeric(factor(splitMi$Have.low.temp.cool.full.cap.results))

# Attribute: Low.temp.cooling.full.cap.power.rated 85
table(df$Low.temp.cooling.full.cap.power.rated)
# Data check
splitMi = df[c(85)]
dataCheck(splitMi, "Low.temp.cooling.full.cap.power.rated")
splitMi <- extract_soul(splitMi, "Low.temp.cooling.full.cap.power.rated")
splitMi <- run_all(splitMi, "Low.temp.cooling.full.cap.power.rated", 0.001)
mediann <- median(splitMi$Low.temp.cooling.full.cap.power.rated[splitMi$Low.temp.cooling.full.cap.power.rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.full.cap.power.rated", 0.001, mediann)
splitMi <- convert2(splitMi,"Low.temp.cooling.full.cap.power.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Low.temp.cooling.full.cap.rated 86
table(df$Low.temp.cooling.full.cap.rated)
# Data check
splitMi = df[c(86)]
dataCheck(splitMi, "Low.temp.cooling.full.cap.rated")
splitMi <- extract_soul(splitMi, "Low.temp.cooling.full.cap.rated")
splitMi <- run_all(splitMi, "Low.temp.cooling.full.cap.rated", 0.001)
mediann <- median(splitMi$Low.temp.cooling.full.cap.rated[splitMi$Low.temp.cooling.full.cap.rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.full.cap.rated", 0.001, mediann)
splitMi <- convert2(splitMi,"Low.temp.cooling.full.cap.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.low.temp.cool.half.cap.results 87
table(df$Have.low.temp.cool.half.cap.results)
# Data check
splitMi = df[c(87)]
# Proposal: label encoding - numeric relation is applicable here.
processed_df$Have.low.temp.cool.half.cap.results <- as.numeric(factor(splitMi$Have.low.temp.cool.half.cap.results))


# Attribute: Low.temp.cooling.half.cap.power.rated 88
table(df$Low.temp.cooling.half.cap.power.rated)
# Data check
splitMi = df[c(88)]
dataCheck(splitMi, "Low.temp.cooling.half.cap.power.rated")
splitMi <- extract_soul(splitMi, "Low.temp.cooling.half.cap.power.rated")
splitMi <- run_all(splitMi, "Low.temp.cooling.half.cap.power.rated", 0.001)
mediann <- median(splitMi$Low.temp.cooling.half.cap.power.rated[splitMi$Low.temp.cooling.half.cap.power.rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.half.cap.power.rated", 0.001, mediann)
splitMi <- convert2(splitMi,"Low.temp.cooling.half.cap.power.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Low.temp.cooling.half.cap.rated 89
table(df$Low.temp.cooling.half.cap.power.rated)
# Data check
splitMi = df[c(89)]
dataCheck(splitMi, "Low.temp.cooling.half.cap.rated")
splitMi <- extract_soul(splitMi, "Low.temp.cooling.half.cap.rated")
splitMi <- run_all(splitMi, "Low.temp.cooling.half.cap.rated", 0.001)
mediann <- median(splitMi$Low.temp.cooling.half.cap.rated[splitMi$Low.temp.cooling.half.cap.rated != 0.001])
print(mediann)
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.half.cap.rated", 0.001, mediann)
splitMi <- convert2(splitMi,"Low.temp.cooling.half.cap.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.low.temp.cool.min.cap.results 90
table(df$Have.low.temp.cool.min.cap.results)
# Data check
splitMi = df[c(90)]
# Proposal: label encoding - numeric relation is applicable here.
processed_df$Have.low.temp.cool.min.cap.results <- as.numeric(factor(splitMi$Have.low.temp.cool.min.cap.results))

###############################################################################
# Attribute: Low.temp.cooling.min.cap.power.rated  91
table(df$Low.temp.cooling.min.cap.power.rated) # N/A 2386
sum(df$Low.temp.cooling.min.cap.power.rated == "N/A") # 2386
sum(df$Low.temp.cooling.min.cap.power.rated == "-") # 2008
# Have data : 830 (15.8%)

table(df$Low.temp.cooling.min.cap.power.rated)
splitMi = df[c(91)]
dataCheck(splitMi, "Low.temp.cooling.min.cap.power.rated")
# Observation
# "No of NA: 2386"
# "No of - : 2008"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")        #turn to num
str(splitMi)
mediann <- median(splitMi$Low.temp.cooling.min.cap.power.rated[splitMi$Low.temp.cooling.min.cap.power.rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001, mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# Attribute: Have.low.temp.cool.min.cap.results 90
table(df$Have.low.temp.cool.min.cap.results) # N/A 2386
sum(df$Have.low.temp.cool.min.cap.results == "N/A") # 2386
sum(df$Have.low.temp.cool.min.cap.results == "-") # 890

splitMi = df[c(90)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  91
table(df$Low.temp.cooling.min.cap.power.rated) # N/A 2386
sum(df$Low.temp.cooling.min.cap.power.rated == "N/A") # 2386
sum(df$Low.temp.cooling.min.cap.power.rated == "-") # 2008
# Have data : 830 (15.8%)

table(df$Low.temp.cooling.min.cap.power.rated)
splitMi = df[c(91)]
dataCheck(splitMi, "Low.temp.cooling.min.cap.power.rated")
# Observation
# "No of NA: 2386"
# "No of - : 2008"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")        #turn to num
str(splitMi)
mediann <- median(splitMi$Low.temp.cooling.min.cap.power.rated[splitMi$Low.temp.cooling.min.cap.power.rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.min.cap.power.rated", 0.001, mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.power.rated")
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)
str(processed_df$Low.temp.cooling.min.cap.power.rated)


# Attribute:  92
table(df$Low.temp.cooling.min.cap.rated) # N/A 2386
sum(df$Low.temp.cooling.min.cap.rated == "N/A") # 2386
sum(df$Low.temp.cooling.min.cap.rated == "-") # 2008
# Have data : 830 (15.8%)

table(df$Low.temp.cooling.min.cap.rated)
splitMi = df[c(92)]
dataCheck(splitMi, "Low.temp.cooling.min.cap.rated")
# Observation
# "No of NA: 2386"
# "No of - : 2008"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "Low.temp.cooling.min.cap.rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Low.temp.cooling.min.cap.rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.rated")        #turn to num
mediann <- median(splitMi$Low.temp.cooling.min.cap.rated[splitMi$Low.temp.cooling.min.cap.rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.rated")
splitMi <- convert_XXX_to_YYY(splitMi, "Low.temp.cooling.min.cap.rated", 0.001, mediann)
splitMi <- convert2(splitMi, "Low.temp.cooling.min.cap.rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  93
table(df$Rated.Heating.Capacity.watts) # N/A 1887
sum(df$Rated.Heating.Capacity.watts == "N/A") # 1887
sum(df$Rated.Heating.Capacity.watts == "-") # 400
# Have data : 2937 (56.2%)

table(df$Rated.Heating.Capacity.watts)
splitMi = df[c(93)]
dataCheck(splitMi, "Rated.Heating.Capacity.watts")
# Observation
# "No of NA: 1887"
# "No of NA: 400"
# proposal to perform data imputation -- using median Q2
splitMi <- extract_soul(splitMi, "Rated.Heating.Capacity.watts")
splitMi <- convert(splitMi, "Rated.Heating.Capacity.watts", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "Rated.Heating.Capacity.watts", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Rated.Heating.Capacity.watts")        #turn to num
mediann <- median(splitMi$Rated.Heating.Capacity.watts[splitMi$Rated.Heating.Capacity.watts != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Rated.Heating.Capacity.watts")
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.Heating.Capacity.watts", 0.001, mediann)
splitMi <- convert2(splitMi, "Rated.Heating.Capacity.watts")
str(splitMi)

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)
str(processed_df$Rated.Heating.Capacity.watts)


# Attribute:  94
table(df$Rated.heating.power.input.kW) # N/A 0
sum(df$Rated.heating.power.input.kW == "N/A") # 0
sum(df$Rated.heating.power.input.kW == "-") # 420
# Have data : 4804 (91.9%)

table(df$Rated.heating.power.input.kW)
splitMi = df[c(94)]
dataCheck(splitMi, "Rated.heating.power.input.kW")
# Observation
# "No of NA: 3277"
# proposal to perform data imputation -- using median Q2

splitMi <- convert_1.0(splitMi, "Rated.heating.power.input.kW", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "Rated.heating.power.input.kW")        #turn to num
mediann <- median(splitMi$Rated.heating.power.input.kW[splitMi$Rated.heating.power.input.kW != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Rated.heating.power.input.kW")
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.heating.power.input.kW", 0.001, mediann)
splitMi <- convert2(splitMi, "Rated.heating.power.input.kW")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  95
table(df$H1_half_cap_power_rated) # N/A 2386
sum(df$H1_half_cap_power_rated == "N/A") # 2386
sum(df$H1_half_cap_power_rated == "-") # 456
# Have data : 2382 (45.5%)

table(df$H1_half_cap_power_rated)
splitMi = df[c(95)]
dataCheck(splitMi, "H1_half_cap_power_rated")
# Observation
# "No of NA: 2386"
# "No of -: 456"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H1_half_cap_power_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H1_half_cap_power_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H1_half_cap_power_rated")        #turn to num
mediann <- median(splitMi$H1_half_cap_power_rated[splitMi$H1_half_cap_power_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H1_half_cap_power_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H1_half_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H1_half_cap_power_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  96
table(df$H1_half_cap_heat_cap_rated)
splitMi = df[c(96)]
dataCheck(splitMi, "H1_half_cap_heat_cap_rated")
# Observation
# "No of NA: 2386"
# "No of -: 456"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H1_half_cap_heat_cap_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H1_half_cap_heat_cap_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H1_half_cap_heat_cap_rated")        #turn to num
mediann <- median(splitMi$H1_half_cap_heat_cap_rated[splitMi$H1_half_cap_heat_cap_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H1_half_cap_heat_cap_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H1_half_cap_heat_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H1_half_cap_heat_cap_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  97
table(df$Have.H1.min.cap.results)
splitMi = df[c(97)]
dataCheck(splitMi, "Have.H1.min.cap.results")
# Observation
# "No of NA: 2386"
# "No of -: 467"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(97)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  98
table(df$H1_min_cap_power_rated)
splitMi = df[c(98)]
dataCheck(splitMi, "H1_min_cap_power_rated")
# Observation
# "No of NA: 2386"
# "No of -: 2104"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H1_min_cap_power_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H1_min_cap_power_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H1_min_cap_power_rated")        #turn to num
mediann <- median(splitMi$H1_min_cap_power_rated[splitMi$H1_min_cap_power_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H1_min_cap_power_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H1_min_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H1_min_cap_power_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  99
table(df$H1_min_cap_heat_cap_rated)
splitMi = df[c(99)]
dataCheck(splitMi, "H1_min_cap_heat_cap_rated")
# Observation
# "No of NA: 2386"
# "No of -: 2104"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H1_min_cap_heat_cap_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H1_min_cap_heat_cap_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H1_min_cap_heat_cap_rated")        #turn to num
mediann <- median(splitMi$H1_min_cap_heat_cap_rated[splitMi$H1_min_cap_heat_cap_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H1_min_cap_heat_cap_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H1_min_cap_heat_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H1_min_cap_heat_cap_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  100
table(df$Have.H2.extended.mode.results)
splitMi = df[c(100)]
dataCheck(splitMi, "Have.H2.extended.mode.results")
# Observation
# "No of NA: 2386"
# "No of -: 466"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(100)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  101
table(df$H2_ext_cap_power_rated)
splitMi = df[c(101)]
dataCheck(splitMi, "H2_ext_cap_power_rated")
# Observation
# "No of NA: 2386"
# "No of -: 761"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H2_ext_cap_power_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_ext_cap_power_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_ext_cap_power_rated")        #turn to num
mediann <- median(splitMi$H2_ext_cap_power_rated[splitMi$H2_ext_cap_power_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H2_ext_cap_power_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H2_ext_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_ext_cap_power_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  102
table(df$H2_ext_cap_heat_cap_rated)
splitMi = df[c(102)]
dataCheck(splitMi, "H2_ext_cap_heat_cap_rated")
# Observation
# "No of NA: 2386"
# "No of -: 761"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H2_ext_cap_heat_cap_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_ext_cap_heat_cap_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_ext_cap_heat_cap_rated")        #turn to num
mediann <- median(splitMi$H2_ext_cap_heat_cap_rated[splitMi$H2_ext_cap_heat_cap_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H2_ext_cap_heat_cap_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H2_ext_cap_heat_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_ext_cap_heat_cap_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  103
table(df$Have.H2.full.mode.results)
splitMi = df[c(103)]
dataCheck(splitMi, "Have.H2.full.mode.results")
# Observation
# "No of NA: 2386"
# "No of -: 786"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(103)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  104
table(df$H2_full_cap_power_rated)
splitMi = df[c(104)]
dataCheck(splitMi, "H2_full_cap_power_rated")
# Observation
# "No of NA: 2386"
# "No of -: 2368"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H2_full_cap_power_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_full_cap_power_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_full_cap_power_rated")        #turn to num
mediann <- median(splitMi$H2_full_cap_power_rated[splitMi$H2_full_cap_power_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H2_full_cap_power_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H2_full_cap_power_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_full_cap_power_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  105
table(df$H2_full_cap_heat_cap_rated)
splitMi = df[c(105)]
dataCheck(splitMi, "H2_full_cap_heat_cap_rated")
# Observation
# "No of NA: 2386"
# "No of -: 2368"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "H2_full_cap_heat_cap_rated", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "H2_full_cap_heat_cap_rated", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "H2_full_cap_heat_cap_rated")        #turn to num
mediann <- median(splitMi$H2_full_cap_heat_cap_rated[splitMi$H2_full_cap_heat_cap_rated != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "H2_full_cap_heat_cap_rated")
splitMi <- convert_XXX_to_YYY(splitMi, "H2_full_cap_heat_cap_rated", 0.001, mediann)
splitMi <- convert2(splitMi, "H2_full_cap_heat_cap_rated")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  106
table(df$Have.H2.half.capacity.results)
splitMi = df[c(106)]
dataCheck(splitMi, "Have.H2.half.capacity.results")
# Observation
# "No of NA: 2386"
# "No of -: 484"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(106)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  107
table(df$H2_half_cap_power_rated)
# proposal : Remove (99% NA)


# Attribute:  108
table(df$H2_half_cap_heat_cap_rated)
# proposal : Remove (99% NA)


# Attribute:  109
table(df$Have.H2.min.capacity.results)
# proposal : Remove (99% NA)


# Attribute:  110
table(df$H2_min_cap_power_rated)
# proposal : N/A, Removed


# Attribute:  111
table(df$H2_min_cap_heat_cap_rated)
# proposal : N/A, Removed


# Attribute:  112
table(df$Have.H3.extended.mode.results)
splitMi = df[c(112)]
dataCheck(splitMi, "Have.H3.extended.mode.results")
# Observation
# "No of NA: 2386"
# "No of -: 821"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(112)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  113
table(df$H3_ext_cap_power_rated)
# proposal : Remove (99% NA)


# Attribute:  114
table(df$H3_ext_cap_heat_cap_rated)
# proposal : Remove (99% NA)


# Attribute:  115
table(df$Have.H3.full.mode.results)
splitMi = df[c(115)]
dataCheck(splitMi, "Have.H3.full.mode.results")
# Observation
# "No of NA: 2386"
# "No of -: 324"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(115)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  116
table(df$H3_full_cap_power_rated)
# proposal : Remove (99% NA)


# Attribute:  117
table(df$H3_full_cap_heat_cap_rated)
# proposal : Remove (99% NA)


# Attribute:  118
table(df$Have.H3.half.capacity.results)
splitMi = df[c(118)]
dataCheck(splitMi, "Have.H3.half.capacity.results")
# Observation
# "No of NA: 2386"
# "No of -: 490"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(118)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


# Attribute:  119
table(df$H3_half_cap_power_rated)
# proposal : Remove (99% NA)


# Attribute:  120
table(df$H3_half_cap_heat_cap_rated)
# proposal : Remove (99% NA)


# Attribute:  121
table(df$indoor_sound_level)
splitMi = df[c(121)]
dataCheck(splitMi, "indoor_sound_level")
# Observation
# "No of NA: 2488"
# "No of -: 854"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "indoor_sound_level", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "indoor_sound_level", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "indoor_sound_level")        #turn to num
mediann <- median(splitMi$indoor_sound_level[splitMi$indoor_sound_level != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "indoor_sound_level")
splitMi <- convert_XXX_to_YYY(splitMi, "indoor_sound_level", 0.001, mediann)
splitMi <- convert2(splitMi, "indoor_sound_level")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  122
table(df$outdoor_sound_level)
splitMi = df[c(122)]
dataCheck(splitMi, "outdoor_sound_level")
# Observation
# "No of NA: 2446"
# "No of -: 640"
# proposal to perform data imputation -- using median Q2

splitMi <- convert(splitMi, "outdoor_sound_level", 0.001) # Convert "N/A" into unique
splitMi <- convert_1.0(splitMi, "outdoor_sound_level", 0.001) # Convert "-" into unique
splitMi <- convert2(splitMi, "outdoor_sound_level")        #turn to num
mediann <- median(splitMi$outdoor_sound_level[splitMi$outdoor_sound_level != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "outdoor_sound_level")
splitMi <- convert_XXX_to_YYY(splitMi, "outdoor_sound_level", 0.001, mediann)
splitMi <- convert2(splitMi, "outdoor_sound_level")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  123
table(df$Residential.TCSPF_cold)
splitMi = df[c(123)]
dataCheck(splitMi, "Residential.TCSPF_cold")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.TCSPF_cold", 0.001)
splitMi$Residential.TCSPF_cold <- ifelse(is.na(splitMi$Residential.TCSPF_cold), 0.001, splitMi$Residential.TCSPF_cold)
splitMi <- convert2(splitMi, "Residential.TCSPF_cold")        #turn to num
mediann <- median(splitMi$Residential.TCSPF_cold[splitMi$Residential.TCSPF_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.TCSPF_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

names(df)
# Attribute:  124
table(df$Residential.TCSPF_mixed)
splitMi = df[c(124)]
dataCheck(splitMi, "Residential.TCSPF_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.TCSPF_mixed", 0.001)
splitMi$Residential.TCSPF_mixed <- ifelse(is.na(splitMi$Residential.TCSPF_mixed), 0.001, splitMi$Residential.TCSPF_mixed)
splitMi <- convert2(splitMi, "Residential.TCSPF_mixed")        #turn to num
mediann <- median(splitMi$Residential.TCSPF_mixed[splitMi$Residential.TCSPF_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.TCSPF_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  125
table(df$Residential.TCSPF_hot)
splitMi = df[c(125)]
dataCheck(splitMi, "Residential.TCSPF_hot")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.TCSPF_hot", 0.001)
splitMi$Residential.TCSPF_hot <- ifelse(is.na(splitMi$Residential.TCSPF_hot), 0.001, splitMi$Residential.TCSPF_hot)
splitMi <- convert2(splitMi, "Residential.TCSPF_hot")        #turn to num
mediann <- median(splitMi$Residential.TCSPF_hot[splitMi$Residential.TCSPF_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.TCSPF_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.TCSPF_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


names(df)
# Attribute:  126
table(df$Commercial.TCSPF_cold)
splitMi = df[c(126)]
dataCheck(splitMi, "Commercial.TCSPF_cold")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.TCSPF_cold", 0.001)
splitMi$Commercial.TCSPF_cold <- ifelse(is.na(splitMi$Commercial.TCSPF_cold), 0.001, splitMi$Commercial.TCSPF_cold)
splitMi <- convert2(splitMi, "Commercial.TCSPF_cold")        #turn to num
mediann <- median(splitMi$Commercial.TCSPF_cold[splitMi$Commercial.TCSPF_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.TCSPF_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  127
table(df$Commercial.TCSPF_mixed)
splitMi = df[c(127)]
dataCheck(splitMi, "Commercial.TCSPF_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.TCSPF_mixed", 0.001)
splitMi$Commercial.TCSPF_mixed <- ifelse(is.na(splitMi$Commercial.TCSPF_mixed), 0.001, splitMi$Commercial.TCSPF_mixed)
splitMi <- convert2(splitMi, "Commercial.TCSPF_mixed")        #turn to num
mediann <- median(splitMi$Commercial.TCSPF_mixed[splitMi$Commercial.TCSPF_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.TCSPF_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  128
table(df$Commercial.TCSPF_hot)
splitMi = df[c(128)]
dataCheck(splitMi, "Commercial.TCSPF_hot")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.TCSPF_hot", 0.001)
splitMi$Commercial.TCSPF_hot <- ifelse(is.na(splitMi$Commercial.TCSPF_hot), 0.001, splitMi$Commercial.TCSPF_hot)
splitMi <- convert2(splitMi, "Commercial.TCSPF_hot")        #turn to num
mediann <- median(splitMi$Commercial.TCSPF_hot[splitMi$Commercial.TCSPF_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.TCSPF_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.TCSPF_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

names(df)
# Attribute:  129
table(df$Residential.tcec_cold)
splitMi = df[c(129)]
dataCheck(splitMi, "Residential.tcec_cold")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.tcec_cold", 0.001)
splitMi$Residential.tcec_cold <- ifelse(is.na(splitMi$Residential.tcec_cold), 0.001, splitMi$Residential.tcec_cold)
splitMi <- convert2(splitMi, "Residential.tcec_cold")        #turn to num
mediann <- median(splitMi$Residential.tcec_cold[splitMi$Residential.tcec_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.tcec_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.tcec_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.tcec_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  130
table(df$Residential.tcec_mixed)
splitMi = df[c(130)]
dataCheck(splitMi, "Residential.tcec_mixed")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.tcec_mixed", 0.001)
splitMi$Residential.tcec_mixed <- ifelse(is.na(splitMi$Residential.tcec_mixed), 0.001, splitMi$Residential.tcec_mixed)
splitMi <- convert2(splitMi, "Residential.tcec_mixed")        #turn to num
mediann <- median(splitMi$Residential.tcec_mixed[splitMi$Residential.tcec_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.tcec_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.tcec_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.tcec_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  131
table(df$Residential.tcec_hot)
splitMi = df[c(131)]
dataCheck(splitMi, "Residential.tcec_hot")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.tcec_hot", 0.001)
splitMi$Residential.tcec_hot <- ifelse(is.na(splitMi$Residential.tcec_hot), 0.001, splitMi$Residential.tcec_hot)
splitMi <- convert2(splitMi, "Residential.tcec_hot")        #turn to num
mediann <- median(splitMi$Residential.tcec_hot[splitMi$Residential.tcec_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.tcec_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.tcec_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.tcec_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  132
table(df$Commercial.tcec_cold)
splitMi = df[c(132)]
dataCheck(splitMi, "Commercial.tcec_cold")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.tcec_cold", 0.001)
splitMi$Commercial.tcec_cold <- ifelse(is.na(splitMi$Commercial.tcec_cold), 0.001, splitMi$Commercial.tcec_cold)
splitMi <- convert2(splitMi, "Commercial.tcec_cold")        #turn to num
mediann <- median(splitMi$Commercial.tcec_cold[splitMi$Commercial.tcec_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.tcec_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  133
table(df$Commercial.tcec_mixed)
splitMi = df[c(133)]
dataCheck(splitMi, "Commercial.tcec_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.tcec_mixed", 0.001)
splitMi$Commercial.tcec_mixed <- ifelse(is.na(splitMi$Commercial.tcec_mixed), 0.001, splitMi$Commercial.tcec_mixed)
splitMi <- convert2(splitMi, "Commercial.tcec_mixed")        #turn to num
str(splitMi)
mediann <- median(splitMi$Commercial.tcec_mixed[splitMi$Commercial.tcec_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.tcec_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  134
table(df$Commercial.tcec_hot)
splitMi = df[c(134)]
dataCheck(splitMi, "Commercial.tcec_hot")
# Observation
# "No of NA: 2386"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.tcec_hot", 0.001)
splitMi$Commercial.tcec_hot <- ifelse(is.na(splitMi$Commercial.tcec_hot), 0.001, splitMi$Commercial.tcec_hot)
splitMi <- convert2(splitMi, "Commercial.tcec_hot")        #turn to num
mediann <- median(splitMi$Commercial.tcec_hot[splitMi$Commercial.tcec_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.tcec_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.tcec_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  135
table(df$c_star_cold)
splitMi = df[c(135)]
dataCheck(splitMi, "c_star_cold")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "c_star_cold", 0.001)
mediann <- median(splitMi$c_star_cold[splitMi$c_star_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "c_star_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "c_star_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "c_star_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  136
table(df$c_star_mixed)
splitMi = df[c(136)]
dataCheck(splitMi, "c_star_mixed")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "c_star_mixed", 0.001)
mediann <- median(splitMi$c_star_mixed[splitMi$c_star_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "c_star_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "c_star_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "c_star_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  137
table(df$c_star_hot)
splitMi = df[c(137)]
dataCheck(splitMi, "c_star_hot")
# Observation
# "No of NA: 1929"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "c_star_hot", 0.001)
mediann <- median(splitMi$c_star_hot[splitMi$c_star_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "c_star_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "c_star_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "c_star_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  138
table(df$Residential.HSPF_cold)
splitMi = df[c(138)]
dataCheck(splitMi, "Residential.HSPF_cold")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.HSPF_cold", 0.001)
mediann <- median(splitMi$Residential.HSPF_cold[splitMi$Residential.HSPF_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.HSPF_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  139
table(df$Residential.HSPF_mixed)
splitMi = df[c(139)]
dataCheck(splitMi, "Residential.HSPF_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.HSPF_mixed", 0.001)
mediann <- median(splitMi$Residential.HSPF_mixed[splitMi$Residential.HSPF_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.HSPF_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  140
table(df$Residential.HSPF_hot)
splitMi = df[c(140)]
dataCheck(splitMi, "Residential.HSPF_hot")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.HSPF_hot", 0.001)
mediann <- median(splitMi$Residential.HSPF_hot[splitMi$Residential.HSPF_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.HSPF_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.HSPF_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  141
table(df$Commercial.HSPF_cold)
splitMi = df[c(141)]
dataCheck(splitMi, "Commercial.HSPF_cold")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.HSPF_cold", 0.001)
splitMi$Commercial.HSPF_cold <- ifelse(is.na(splitMi$Commercial.HSPF_cold), 0.001, splitMi$Commercial.HSPF_cold)
splitMi <- convert2(splitMi, "Commercial.HSPF_cold")        #turn to num
mediann <- median(splitMi$Commercial.HSPF_cold[splitMi$Commercial.HSPF_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.HSPF_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  142
table(df$Commercial.HSPF_mixed)
splitMi = df[c(142)]
dataCheck(splitMi, "Commercial.HSPF_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.HSPF_mixed", 0.001)
mediann <- median(splitMi$Commercial.HSPF_mixed[splitMi$Commercial.HSPF_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.HSPF_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  143
table(df$Commercial.HSPF_hot)
splitMi = df[c(143)]
dataCheck(splitMi, "Commercial.HSPF_hot")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.HSPF_hot", 0.001)
mediann <- median(splitMi$Commercial.HSPF_hot[splitMi$Commercial.HSPF_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.HSPF_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.HSPF_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  144
table(df$Residential.thec_cold)
splitMi = df[c(144)]
dataCheck(splitMi, "Residential.thec_cold")
# Observation
# "No of NA: 1929"
# "No of -: 481"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.thec_cold", 0.001)
mediann <- median(splitMi$Residential.thec_cold[splitMi$Residential.thec_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.thec_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.thec_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.thec_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  145
table(df$Residential.thec_mixed)
splitMi = df[c(145)]
dataCheck(splitMi, "Residential.thec_mixed")
# Observation
# "No of NA: 1929"
# "No of -: 481"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.thec_mixed", 0.001)
mediann <- median(splitMi$Residential.thec_mixed[splitMi$Residential.thec_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.thec_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.thec_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.thec_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  146
table(df$Residential.thec_hot)
splitMi = df[c(146)]
dataCheck(splitMi, "Residential.thec_hot")
# Observation
# "No of NA: 1929"
# "No of -: 481"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Residential.thec_hot", 0.001)
mediann <- median(splitMi$Residential.thec_hot[splitMi$Residential.thec_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Residential.thec_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Residential.thec_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Residential.thec_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  147
table(df$Commercial.thec_cold)
splitMi = df[c(147)]
dataCheck(splitMi, "Commercial.thec_cold")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.thec_cold", 0.001)
splitMi$Commercial.thec_cold <- ifelse(is.na(splitMi$Commercial.thec_cold), 0.001, splitMi$Commercial.thec_cold)
splitMi <- convert2(splitMi, "Commercial.thec_cold")        #turn to num
mediann <- median(splitMi$Commercial.thec_cold[splitMi$Commercial.thec_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.thec_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.thec_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.thec_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

names(df)
# Attribute:  148
table(df$Commercial.thec_mixed)
splitMi = df[c(148)]
dataCheck(splitMi, "Commercial.thec_mixed")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.thec_mixed", 0.001)
mediann <- median(splitMi$Commercial.thec_mixed[splitMi$Commercial.thec_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.thec_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.thec_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.thec_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  149
table(df$Commercial.thec_hot)
splitMi = df[c(149)]
dataCheck(splitMi, "Commercial.thec_hot")
# Observation
# "No of NA: 2386"
# "No of -: 295"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Commercial.thec_hot", 0.001)
mediann <- median(splitMi$Commercial.thec_hot[splitMi$Commercial.thec_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Commercial.thec_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "Commercial.thec_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "Commercial.thec_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  150
table(df$h_star_cold)
splitMi = df[c(150)]
dataCheck(splitMi, "h_star_cold")
# Observation
# "No of NA: 1929"
# "No of -: 393"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "h_star_cold", 0.001)
mediann <- median(splitMi$h_star_cold[splitMi$h_star_cold != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "h_star_cold")
splitMi <- convert_XXX_to_YYY(splitMi, "h_star_cold", 0.001, mediann)
splitMi <- convert2(splitMi, "h_star_cold")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  151
table(df$h_star_mixed)
splitMi = df[c(151)]
dataCheck(splitMi, "h_star_mixed")
# Observation
# "No of NA: 1929"
# "No of -: 393"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "h_star_mixed", 0.001)
mediann <- median(splitMi$h_star_mixed[splitMi$h_star_mixed != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "h_star_mixed")
splitMi <- convert_XXX_to_YYY(splitMi, "h_star_mixed", 0.001, mediann)
splitMi <- convert2(splitMi, "h_star_mixed")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  152
table(df$h_star_hot)
splitMi = df[c(152)]
dataCheck(splitMi, "h_star_hot")
# Observation
# "No of NA: 1929"
# "No of -: 393"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "h_star_hot", 0.001)
mediann <- median(splitMi$h_star_hot[splitMi$h_star_hot != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "h_star_hot")
splitMi <- convert_XXX_to_YYY(splitMi, "h_star_hot", 0.001, mediann)
splitMi <- convert2(splitMi, "h_star_hot")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  153
table(df$Outdoor.unit.only)
splitMi = df[c(153)]
dataCheck(splitMi, "Outdoor.unit.only")
# Observation
# "No of NA: 2945"
# "No of -: 238"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(153)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)

# we try make a function to package our task
run_all <- function(df, col_name, changeToValue){
  
  df <- convert(df, col_name, changeToValue)
  df <- convert_1.0(df, col_name, changeToValue)
  df <- convert2(df, col_name)
  return(df)
}


names(df)
# Attribute:  154
table(df$Have_water_tank)
splitMi = df[c(154)]
dataCheck(splitMi, "Have_water_tank")
# Observation
# "No of NA: 2443"
# "No of -: 2324"
# proposal to perform data imputation -- using median Q2

splitMi = df[c(154)]
dumb <- dummyVars(" ~ .", data = splitMi)
splitMi <- data.frame(predict(dumb, newdata=splitMi))
# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  155
table(df$Rated.cool.power.input.with.water)
# proposal : N/A, Removed


# Attribute:  156
table(df$Rated.cool.cap.with.water.W)
# proposal : N/A, Removed


# Attribute:  157
table(df$Rated.cool.cap.with.water.kW)
# proposal : N/A, Removed


# Attribute:  158
table(df$Residential.tcec_cold.with.water)
# proposal : N/A, Removed


# Attribute:  159
table(df$Residential.tcec_mixed.with.water)
# proposal : N/A, Removed


# Attribute:  160
table(df$Residential.tcec_hot.with.water)
# proposal : N/A, Removed


# Attribute:  161
table(df$PIA.inoperative.power)
splitMi = df[c(161)]
dataCheck(splitMi, "PIA.inoperative.power")
# Observation
# "No of NA: 2344"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "PIA.inoperative.power", 0.001)
mediann <- median(splitMi$PIA.inoperative.power[splitMi$PIA.inoperative.power != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "PIA.inoperative.power")
splitMi <- convert_XXX_to_YYY(splitMi, "PIA.inoperative.power", 0.001, mediann)
splitMi <- convert2(splitMi, "PIA.inoperative.power")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


# Attribute:  162
table(df$Rated.AEER)
splitMi = df[c(162)]
dataCheck(splitMi, "Rated.AEER")
# Observation
# "No of NA: 0"
# "No of -: 5"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Rated.AEER", 0.001)
mediann <- median(splitMi$Rated.AEER[splitMi$Rated.AEER != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Rated.AEER")
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.AEER", 0.001, mediann)
splitMi <- convert2(splitMi, "Rated.AEER")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)


names(df)
# Attribute:  163
table(df$Rated.ACOP)
splitMi = df[c(163)]
dataCheck(splitMi, "Rated.ACOP")
# Observation
# "No of NA: 0"
# "No of -: 400"
# proposal to perform data imputation -- using median Q2

splitMi <- run_all(splitMi, "Rated.ACOP", 0.001)
mediann <- median(splitMi$Rated.ACOP[splitMi$Rated.ACOP != 0.001])   # exclude unique 
print(mediann)
splitMi <- convert2(splitMi, "Rated.ACOP")
splitMi <- convert_XXX_to_YYY(splitMi, "Rated.ACOP", 0.001, mediann)
splitMi <- convert2(splitMi, "Rated.ACOP")

# Append data frame to each other using bind_cols
processed_df = bind_cols(processed_df, splitMi)



############# This is our "label" what we trying to predict ####################
# Attribute: sri2010_cool 40 
table(df$sri2010_cool)
table(df$sri2010_heat)
# Data check
splitMi = df[c(40)]
splitMii = df[c(41)]
dataCheck(splitMi, "sri2010_cool")
dataCheck(splitMii, "sri2010_heat")
str(splitMi)
str(splitMii)
# Observation:
# 1. Data is of chr type
# 2. we need convert into numeric
# 3. remove 0 and NA data place in new dataframe
# 4. split the remain to train and test

# replace "N/A" with a unique "0.0010"
splitMi <- convert_XXX_to_YYY(splitMi, "sri2010_cool", "N/A", 0.001)
splitMii <- convert_XXX_to_YYY(splitMii, "sri2010_heat", "N/A", 0.001)
splitMii <- convert_XXX_to_YYY(splitMii, "sri2010_heat", "-", 0.001)
splitMi <- convert2(splitMi, "sri2010_cool") #nurmeric
splitMii <- convert2(splitMii, "sri2010_heat")
str(splitMi) #num
str(splitMii)
#replace processed_df col with processed dataframe column
processed_df$sri2010_cool <- splitMi$sri2010_cool #success
processed_df$sri2010_heat <- splitMii$sri2010_heat
# new dataframe for thing to be predict
predict_cool <- subset(processed_df, sri2010_cool == 0.0010)
predict_heat <- subset (processed_df, sri2010_heat == 0.0010)
str(predict_cool$sri2010_cool)
str(predict_heat$sri2010_heat)
dataCheck(predict_cool, "sri2010_cool")
dataCheck(predict_heat, "sri2010_heat")

# processed_df remove the attribute "sri2010_cool" = 0.0010, this is what we have to predict
# remove them from processed_df (cannot have unknown for training/testing)
dataCheck(processed_df, "sri2010_cool") # 3860 to be remove
dataCheck(processed_df, "sri2010_heat") # 3870 to be remove
processed_df <- processed_df[processed_df$sri2010_cool != 0.001, ]
processed_df <- processed_df[processed_df$sri2010_heat != 0.001, ]
dataCheck(processed_df, "sri2010_cool") # 1354 remain
dataCheck(processed_df, "sri2010_heat") # 1354 remain

#### After data clean do this######################################
#remove them from processed_df (cannot have unknow for training/testing)
#processed_df <- df %>% filter(sri2010_cool != "N/A")

################################################################################

############# This is our "label" what we trying to predict ####################
# Attribute: sri2010_heat 41


# processed_df remove the attribute "sri2010_heat" = 0.0010, this is what we have to predict
# remove them from processed_df (cannot have unknown for training/testing)



#### After data clean do this######################################
#remove them from processed_df (cannot have unknow for training/testing)
#processed_df <- df %>% filter(sri2010_heat != "N/A")
################################################################################

write.csv(processed_df, "process.csv", row.names=FALSE)
write.csv(predict_cool, "predict_sri2010_cool.csv", row.names=FALSE)
write.csv(predict_heat, "predict_sri2010_heat.csv", row.names=FALSE)

