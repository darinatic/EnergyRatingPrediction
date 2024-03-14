# Load the data into Data_Frame
library(dplyr)
dataset <- read.csv("process.csv")
testdataset <- read.csv("Yearly Energy Consumption/Newpredict_sriheat_2010.csv")

#find attribute that have correlation to "AnnualOutputEER", 
corTable = abs(cor(testdataset, y = testdataset$AnnualOutputEER))
corTable = corTable[order(corTable, decreasing = TRUE),,drop = FALSE]
head(corTable,11)

# Select relevant features
features <- c("EER","AnnualOutputCOP","Configuration3_SinkWater.loop","Configuration3_SourceWater.Loop","outdoortypeWater.Loop.Equipment","EERtestAvg","sri2010_cool","AnnualOutputEER")

extract_feature=dataset[features]
extract_feature_of_test=testdataset[features]

#Linear Regression Model
model= lm(AnnualOutputEER~., data=extract_feature)
Pred <- predict(model, extract_feature_of_test)  # predict distance

actuals_preds <- data.frame(cbind(actuals=extract_feature_of_test$AnnualOutputEER, energy_heat=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 82.7%
head(actuals_preds)


Annual_Energy_Consumption=testdataset[c("Rated.Heating.Capacity.watts")]
energy_consumption_cool=Annual_Energy_Consumption/actuals_preds[c("energy_heat")]
names(energy_consumption_cool)[names(energy_consumption_cool) == "Rated.Heating.Capacity.watts"] <- "Yearly_Energy_Consumption"

# Append data frame to each other using bind_cols
testdataset = bind_cols(testdataset, energy_consumption_cool)
write.csv(testdataset,"Yearly Energy Consumption/Yearly_Energy_Heat_2010.csv",row.names=FALSE)
