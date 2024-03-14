###############################################################
# Name: Gabriel / Jason Bobo Kyaw                             #
# UOW ID:  / 7435277                                          #
###############################################################

# Load the data into Data_Frame
library(dplyr)
dataset <- read.csv("process.csv")
testdataset1 <- read.csv("predict_sri2010_cool.csv")
testdataset2 <- read.csv("predict_sri2010_heat.csv")

#find attribute that have correlation to "AnnualOutputEER", 
EERcorTable = abs(cor(testdataset2, y = testdataset2$AnnualOutputEER))
EERcorTable = EERcorTable[order(EERcorTable, decreasing = TRUE),,drop = FALSE]
head(EERcorTable,11)

#find attribute that have correlation to "AnnualOutputCOP", 
COPcorTable = abs(cor(testdataset1, y = testdataset1$AnnualOutputCOP))
COPcorTable = COPcorTable[order(COPcorTable, decreasing = TRUE),,drop = FALSE]
head(COPcorTable,11)

##############################################################################

#Train and predict Annual EER Model
EER_model= lm(formula=AnnualOutputEER ~ + EER + Rated.cooling.power.input.kW + 
                C.Sens_Cool_Rated + EERtestAvg + sri2010_cool + 
                C.Total.Cool.Rated, data=dataset)
Pred_EER <- predict(EER_model, testdataset1)  # predict distance

EER_model_residuals = EER_model$residuals
hist(EER_model_residuals)

# Plot the residuals & Q-Q line
qqnorm(EER_model_residuals)
qqline(EER_mEER_model_residualsodel)

#get model evaluation metrics
summary(EER_model) 
#R squared value: 0.9928
#RSE: 0.04478

# Calculate MAPE and Min-Max Accuracy
actuals_preds <- data.frame(cbind(actuals=testdataset1$AnnualOutputEER, predicteds=Pred_EER))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(paste("min_max_accuracy: ", min_max_accuracy)) # 0.8497
print(paste("mean absolute percentage error: ",mape)) # 0.1503

##############################################################################################

#Train and predict Annual COP Model
COP_model= lm(formula=AnnualOutputCOP ~ C.Dehumid_Rated + 
                C.Power_Inp_Rated +    
                Depth + H.Power_Inp_Rated + H.Total.Heat.Rated +
                COPtestAvg + Rated.Heating.Capacity.watts + 
                Rated.heating.power.input.kW, data=dataset)
Pred_COP <- predict(COP_model, testdataset2)  # predict distance

# Get the model residuals and plot histogram
COP_model_residuals = COP_model$residuals
hist(COP_model_residuals)

# Plot the residuals & Q-Q line
qqnorm(COP_model_residuals)
qqline(COP_model_residuals)

#get model evaluation metrics
summary(COP_model) 
#R squared value: 0.9978
#RSE: 0.05324

# Calculate MAPE and Min-Max Accuracy
actuals_preds <- data.frame(cbind(actuals=testdataset1$AnnualOutputEER, predicteds=Pred_EER))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
print(paste("min_max_accuracy: ", min_max_accuracy)) # 0.8497 (84%)
print(paste("mean absolute percentage error: ",mape))# 0.1503 (15%)


# Append data frame to each other using bind_cols
# testdataset = bind_cols(testdataset, energy_consumption_cool)
# write.csv(testdataset,"Yearly Energy Consumption/Yearly_Energy_Heat_2010.csv",row.names=FALSE)
