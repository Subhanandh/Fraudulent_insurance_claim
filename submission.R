rm(list=ls(all=TRUE))

library(caret)
library(DMwR)


# Read Train Data
demographics = read.csv(file = "TrainData/Train_Demographics.csv", header = T, sep = ',')
claim = read.csv(file = "TrainData/Train_Claim.csv", header = T, sep = ',',na.strings = c('?','-5','MISSINGVALUE','MISSEDDATA'))
policy = read.csv(file = "TrainData/Train_Policy.csv", header = T, sep = ',',na.strings = c('-1','MISSINGVAL','NA'))
vehicle = read.csv(file = "TrainData/Train_Vehicle.csv", header = T, sep = ',',na.strings = '???')
outcome = read.csv(file = "TrainData/Train.csv", header = T, sep = ',')

# Read Test Data
test_demographics = read.csv(file = "TestData/Test_Demographics.csv", header = T, sep = ',')
test_policy = read.csv(file = "TestData/Test_Policy.csv", header = T, sep = ',',na.strings = c('-1','MISSINGVAL','NA'))
test_claim = read.csv(file = "TestData/Test_Claim.csv", header = T, sep = ',',na.strings = c('?','-5','MISSINGVALUE','MISSEDDATA'))
test_vehicle = read.csv(file = "TestData/Test_Vehicle.csv", header = T, sep = ',',na.strings = '???')
test_outcome = read.csv(file = "TestData/Test.csv", header = T, sep = ',')

# Processing Vehicle columns - train
VehicleID = subset(vehicle, VehicleAttribute == "VehicleID")
VehicleMake = subset(vehicle, VehicleAttribute == "VehicleMake")
VehicleModel = subset(vehicle, VehicleAttribute == "VehicleModel")
VehicleYOM = subset(vehicle, VehicleAttribute == "VehicleYOM")

# Merging column subsets
merge1 = merge(x = VehicleID, y = VehicleMake, by = "CustomerID", all = TRUE)
merge2 = merge(x = VehicleModel, y = VehicleYOM, by = "CustomerID", all = TRUE)
merged = merge(x = merge1, y = merge2 , by = "CustomerID", all =TRUE)
vehicle_new = merged[,!(colnames(merged) %in% c("VehicleAttribute.x.x","VehicleAttribute.y.x","VehicleAttribute.x.y","VehicleAttribute.y.y"))]
names(vehicle_new) = c("CustomerID","VehicleID","VehicleMake","VehicleModel","VehicleYOM")
nrow(vehicle_new)
str(vehicle_new)


# Cnverting columns to categories
vehicle_new$VehicleID = as.factor(as.character(vehicle_new$VehicleID))
vehicle_new$VehicleMake = as.factor(as.character(vehicle_new$VehicleMake))
vehicle_new$VehicleModel = as.factor(as.character(vehicle_new$VehicleModel))
vehicle_new$VehicleYOM = as.factor(as.character(vehicle_new$VehicleYOM))
str(vehicle_new)


# Repeating the same for test
test_VehicleID = subset(test_vehicle, VehicleAttribute == "VehicleID")
test_VehicleMake = subset(test_vehicle, VehicleAttribute == "VehicleMake")
test_VehicleModel = subset(test_vehicle, VehicleAttribute == "VehicleModel")
test_VehicleYOM = subset(test_vehicle, VehicleAttribute == "VehicleYOM")

merge1 = merge(x = test_VehicleID, y = test_VehicleMake, by = "CustomerID", all = TRUE)
merge2 = merge(x = test_VehicleModel, y = test_VehicleYOM, by = "CustomerID", all = TRUE)
merged = merge(x = merge1, y = merge2 , by = "CustomerID", all =TRUE)
test_vehicle_new = merged[,!(colnames(merged) %in% c("VehicleAttribute.x.x","VehicleAttribute.y.x","VehicleAttribute.x.y","VehicleAttribute.y.y"))]
names(test_vehicle_new) = c("CustomerID","VehicleID","VehicleMake","VehicleModel","VehicleYOM")
nrow(test_vehicle_new)
str(test_vehicle_new)

test_vehicle_new$VehicleID = as.factor(as.character(test_vehicle_new$VehicleID))
test_vehicle_new$VehicleMake = as.factor(as.character(test_vehicle_new$VehicleMake))
test_vehicle_new$VehicleModel = as.factor(as.character(test_vehicle_new$VehicleModel))
test_vehicle_new$VehicleYOM = as.factor(as.character(test_vehicle_new$VehicleYOM))
str(test_vehicle_new)

# Remove unnecessary columns
rm(VehicleID, VehicleMake, VehicleModel, VehicleYOM, test_VehicleID, test_VehicleMake, 
   test_VehicleModel, test_VehicleYOM)

# Combining the final train and test datasets
train_data = Reduce(function(x, y) merge(x, y, all=TRUE), 
                    list(demographics,claim,policy,vehicle_new, outcome))
test_data = Reduce(function(x,y) merge(x,y, all=TRUE), 
                    list(test_demographics,test_claim,test_policy,test_vehicle_new,test_outcome))
test_outcome$id=1:nrow(test_outcome)
test_datax=merge(test_outcome,test_data,by = "CustomerID",sort = FALSE)
test_data=test_datax[order(test_datax$id),]

# Remove unnecessary columns
rm(merge1, merge2, merged, demographics, claim, policy, outcome, vehicle, vehicle_new, 
   test_demographics, test_claim, test_policy, test_outcome, test_vehicle, test_vehicle_new, test_datax)

##Summary
summary(train_data)
str(train_data)

summary(test_data)
str(test_data)

# check for NA values
sum(is.na(train_data))
colSums(is.na(train_data))

sum(is.na(test_data))
colSums(is.na(test_data))

sum(is.na(train_data))
sum(is.na(test_data))

# Converting few columns to categories
train_data$IncidentTime = as.factor(as.character(train_data$IncidentTime))
train_data$NumberOfVehicles = as.factor(as.character(train_data$NumberOfVehicles))
train_data$BodilyInjuries = as.factor(as.character(train_data$BodilyInjuries))
train_data$Witnesses = as.factor(as.character(train_data$Witnesses))
names(train_data)

test_data$IncidentTime=as.factor(as.character(test_data$IncidentTime))
test_data$NumberOfVehicles=as.factor(as.character(test_data$NumberOfVehicles))
test_data$BodilyInjuries=as.factor(as.character(test_data$BodilyInjuries))
test_data$Witnesses=as.factor(as.character(test_data$Witnesses))
names(test_data)

# Removing CustomerID, Country, VehicleID, InsurancePolicyNumber
train_data=train_data[,!names(train_data) %in% c('CustomerID','InsurancePolicyNumber','Country','VehicleID')]
test_data=test_data[,!names(test_data) %in% c('id', 'CustomerID','InsurancePolicyNumber','Country','VehicleID')]

# Removing Correlated columns 
# AmountOfTotalClaim, AmountOfVehicleDamage
train_data = train_data[,!names(train_data) %in% c('AmountOfTotalClaim','AmountOfVehicleDamage')]
test_data = test_data[,!names(test_data) %in% c('AmountOfTotalClaim','AmountOfVehicleDamage')]

# Converting target values to 1 and 0
train_data$ReportedFraud = ifelse(train_data$ReportedFraud == 'Y',1,0)
train_data$ReportedFraud = as.factor(as.character(train_data$ReportedFraud))

# Exploring numerical columns and treating outliers
# AmountOfInjuryClaim
boxplot(train_data$AmountOfInjuryClaim, test_data$AmountOfInjuryClaim)
# Train
summary(train_data$AmountOfInjuryClaim)
plot(train_data$AmountOfInjuryClaim)
# Test
summary(test_data$AmountOfInjuryClaim)
plot(test_data$AmountOfInjuryClaim)

# Limiting to boundary of 17000
train_data$AmountOfInjuryClaim=ifelse(train_data$AmountOfInjuryClaim > 17000,'NA',train_data$AmountOfInjuryClaim)
train_data$AmountOfInjuryClaim=as.numeric(train_data$AmountOfInjuryClaim)   

test_data$AmountOfInjuryClaim=ifelse(test_data$AmountOfInjuryClaim > 17000,'NA',test_data$AmountOfInjuryClaim)
test_data$AmountOfInjuryClaim=as.numeric(test_data$AmountOfInjuryClaim)   

boxplot(train_data$AmountOfInjuryClaim,test_data$AmountOfInjuryClaim)


# AmountOfPropertyClaim
boxplot(train_data$AmountOfPropertyClaim,test_data$AmountOfPropertyClaim)
# Train
summary(train_data$AmountOfPropertyClaim)
plot(train_data$AmountOfPropertyClaim)
# Test
summary(test_data$AmountOfPropertyClaim)
plot(test_data$AmountOfPropertyClaim)

# Limiting to boundary of 16500
train_data$AmountOfPropertyClaim=ifelse(train_data$AmountOfPropertyClaim > 16500,'NA',train_data$AmountOfPropertyClaim)
train_data$AmountOfPropertyClaim=as.numeric(train_data$AmountOfPropertyClaim)

test_data$AmountOfPropertyClaim=ifelse(test_data$AmountOfPropertyClaim > 16500,'NA',test_data$AmountOfPropertyClaim)
test_data$AmountOfPropertyClaim=as.numeric(test_data$AmountOfPropertyClaim)

boxplot(train_data$AmountOfPropertyClaim,test_data$AmountOfPropertyClaim)

# CustomerLoyaltyPeriod
boxplot(train_data$CustomerLoyaltyPeriod,test_data$CustomerLoyaltyPeriod)
# Train
summary(train_data$CustomerLoyaltyPeriod)
plot(train_data$CustomerLoyaltyPeriod)
# Test
summary(test_data$CustomerLoyaltyPeriod)
plot(test_data$CustomerLoyaltyPeriod)

# Limiting to boundary of 470
train_data$CustomerLoyaltyPeriod=ifelse(train_data$CustomerLoyaltyPeriod > 470,'NA',train_data$CustomerLoyaltyPeriod)
train_data$CustomerLoyaltyPeriod=as.numeric(train_data$CustomerLoyaltyPeriod)

test_data$CustomerLoyaltyPeriod=ifelse(test_data$CustomerLoyaltyPeriod > 470,'NA',test_data$CustomerLoyaltyPeriod)
test_data$CustomerLoyaltyPeriod=as.numeric(test_data$CustomerLoyaltyPeriod)

boxplot(train_data$CustomerLoyaltyPeriod,test_data$CustomerLoyaltyPeriod)

# Policy_Deductible
boxplot(train_data$Policy_Deductible,test_data$Policy_Deductible)

# PolicyAnnualPremium
boxplot(train_data$PolicyAnnualPremium,test_data$PolicyAnnualPremium)
# Train
summary(train_data$PolicyAnnualPremium)
plot(train_data$PolicyAnnualPremium)
# Test
summary(test_data$PolicyAnnualPremium)
plot(test_data$PolicyAnnualPremium)

## Limit to boundary of range 800 and1600
train_data$PolicyAnnualPremium=ifelse((train_data$PolicyAnnualPremium < 800) | (train_data$PolicyAnnualPremium > 1600),'NA',train_data$PolicyAnnualPremium)
train_data$PolicyAnnualPremium=as.numeric(train_data$PolicyAnnualPremium)

test_data$PolicyAnnualPremium=ifelse((test_data$PolicyAnnualPremium < 800) | (test_data$PolicyAnnualPremium  > 1600),'NA',test_data$PolicyAnnualPremium)
test_data$PolicyAnnualPremium=as.numeric(test_data$PolicyAnnualPremium)

boxplot(train_data$PolicyAnnualPremium,test_data$PolicyAnnualPremium)

# UmbrellaLimit
boxplot(train_data$UmbrellaLimit,test_data$UmbrellaLimit)
# Train
summary(train_data$UmbrellaLimit)
plot(train_data$UmbrellaLimit)
# Test
summary(test_data$UmbrellaLimit)
plot(test_data$UmbrellaLimit)

# Plot with Target
plot(train_data$ReportedFraud,train_data$UmbrellaLimit)

# UmbrellaLimit
boxplot(train_data$UmbrellaLimit,test_data$UmbrellaLimit)
plot(train_data$UmbrellaLimit)

# Umbrella limit range category - new attribute (-1,0,2)
train_data$UmbrellaRange = ifelse(train_data$UmbrellaLimit > 0, 2, train_data$UmbrellaLimit)
train_data$UmbrellaRange = ifelse(train_data$UmbrellaRange < 0, -1, train_data$UmbrellaRange)
train_data$UmbrellaRange = as.factor(as.character(train_data$UmbrellaRange))

test_data$UmbrellaRange = ifelse(test_data$UmbrellaLimit > 0, 2, test_data$UmbrellaLimit)
test_data$UmbrellaRange = ifelse(test_data$UmbrellaRange < 0, -1, test_data$UmbrellaRange)
test_data$UmbrellaRange = as.factor(as.character(test_data$UmbrellaRange))


# InsuredZipCode
plot(train_data$InsuredZipCode)
plot(test_data$InsuredZipCode)

# Reducing the levels 
train_data$InsuredZipCode = round(train_data$InsuredZipCode / 1000)
train_data$InsuredZipCode =as.factor(as.character(train_data$InsuredZipCode))
summary(train_data$InsuredZipCode)

test_data$InsuredZipCode = round(test_data$InsuredZipCode / 1000)
test_data$InsuredZipCode =as.factor(as.character(test_data$InsuredZipCode))
summary(test_data$InsuredZipCode)

# Checking CapitalGains
boxplot(train_data$CapitalGains,test_data$CapitalGains)
summary(train_data$CapitalGains)
plot(train_data$CapitalGains)

# Checking CapitalLoss
boxplot(train_data$CapitalLoss,test_data$CapitalLoss)
summary(train_data$CapitalLoss)
plot(train_data$CapitalLoss)

# Calculating Net Balance from loss and gain.
train_data$NetBalance = train_data$CapitalGains - train_data$CapitalLoss
train_data = train_data[,!names(train_data) %in% c('CapitalGains','CapitalLoss')]
str(train_data)
test_data$NetBalance = test_data$CapitalGains - test_data$CapitalLoss
test_data=test_data[,!names(test_data) %in% c('CapitalGains','CapitalLoss')]
str(test_data)

# Limiting to boundary value of 110000.
boxplot(train_data$NetBalance, test_data$NetBalance)
train_data$NetBalance=ifelse(train_data$NetBalance > 110000,'NA',train_data$NetBalance)
train_data$NetBalance=as.numeric(train_data$NetBalance)

test_data$NetBalance=ifelse(test_data$NetBalance > 110000,'NA',test_data$NetBalance)
test_data$NetBalance=as.numeric(test_data$NetBalance)

boxplot(train_data$NetBalance, test_data$NetBalance)

# Checking NA values
sum(is.na(train_data))
sum(is.na(test_data))

# Date columns
# Calculating duration between DateOfIncident and DateOfPolicyCoverage
# Train
train_data$PolicyCoverage_Days = as.Date(as.character(train_data$DateOfIncident),format="%Y-%m-%d")- as.Date(as.character(train_data$DateOfPolicyCoverage), format="%Y-%m-%d")
train_data$PolicyCoverage_Days = as.numeric(train_data$PolicyCoverage_Days)
train_data = train_data[,!names(train_data) %in% c('DateOfIncident','DateOfPolicyCoverage')]
str(train_data)
summary(train_data)

# Test
test_data$PolicyCoverage_Days = as.Date(as.character(test_data$DateOfIncident),format="%Y-%m-%d")-as.Date(as.character(test_data$DateOfPolicyCoverage), format="%Y-%m-%d")
test_data$PolicyCoverage_Days = as.numeric(test_data$PolicyCoverage_Days)
test_data=test_data[,!names(test_data) %in% c('DateOfIncident','DateOfPolicyCoverage')]
str(test_data)
summary(test_data)

# Policy_CombinedSingleLimit - split and creating new attribute by inverse division - maximum per person coverage for injury claim
library(splitstackshape)
train_data = cSplit(train_data, "Policy_CombinedSingleLimit", "/")
train_data$MaximumInjuryPersonCover = round(train_data$Policy_CombinedSingleLimit_2 / train_data$Policy_CombinedSingleLimit_1)
train_data$MaximumInjuryPersonCover = as.factor(as.character(train_data$MaximumInjuryPersonCover))

test_data = cSplit(test_data, "Policy_CombinedSingleLimit", "/")
test_data$MaximumInjuryPersonCover = round(test_data$Policy_CombinedSingleLimit_2 / test_data$Policy_CombinedSingleLimit_1)
test_data$MaximumInjuryPersonCover = as.factor(as.character(test_data$MaximumInjuryPersonCover))
detach("package:splitstackshape", unload=TRUE)

# Imputing missing values
train_data = centralImputation(train_data)
test_data = centralImputation(test_data)

# Standardizing the dataset
target = 'ReportedFraud'
preProcess = preProcess(train_data[, setdiff(names(train_data), target)], method=c("center", "scale"))
train_data = predict(preProcess, train_data)
summary(train_data)
str(train_data)

test_preProcess = preProcess(test_data[, setdiff(names(test_data), target)], method=c("center", "scale"))
test_data = predict(test_preProcess, test_data)
summary(test_data)
str(test_data)


# Moving target to last column
temp = train_data
train_data$ReportedFraud=NULL
train_data = cbind(train_data, ReportedFraud=temp$ReportedFraud)

rm(data, temp)

str(train_data)
str(test_data)


# Using H2o to build model
library(h2o)
library(ROCR)
library(vegan)

# starting H2o
h2o.shutdown()
h2o.init(nthreads = -1, max_mem_size = '5g')

# creating h2o objects for train and test
train.h2o = as.h2o(train_data)
test.h2o  = as.h2o(test_data)
test.h2o
train.h2o
# Independent variable names
x = c(1:36)

# Target name
y = 37

# Random forest using Grid search
search_criteria = list(strategy = "RandomDiscrete", max_models = 30, seed = 1122)
# hyper parameters
grid_space <- list()
grid_space$ntrees <- c(5, 10, 30)
grid_space$max_depth <- c(4, 1, 5)
grid_space$nbins <- c(6, 4, 3)
grid_space$mtries <- c(2, 4, 3)
grid_space$sample_rate <- c(0.3, 0.7, 0.9)

drf_grid <- h2o.grid("randomForest", 
                     grid_id = 'drf_grid3',
                     training_frame = train.h2o,
                     x = x, 
                     y = y,
                     nfolds = 3,
                     stopping_rounds = 3,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "AUC",
                     score_tree_interval = 50,
                     search_criteria = search_criteria,
                     hyper_params=grid_space)
# get best model
rf.sorted.grid <- h2o.getGrid(grid_id = "drf_grid3", sort_by = "f1", decreasing = TRUE)
print(rf.sorted.grid)
best_model <- h2o.getModel(rf.sorted.grid@model_ids[[1]])
summary(best_model)
# get important variables
imp = h2o.varimp(best_model)
predict.rf <- as.data.frame(h2o.predict(best_model, test.h2o))
# predict on test
result_test = predict.rf$predict
result_test = ifelse(result_test == 1,'Y','N' )
result_test = as.character(as.factor(result_test))
summary(result_test)

write.csv(result_test, 'submit.csv')
write.csv(imp, 'variable_importance1.csv')
# check hyper paramters
h2o.performance (best_model)
h2o.gainsLift(best_model, train.h2o)


# Building with XgBoost
# choose hyper parameters
hyper_params <- list(ntrees = seq(10, 100, 10),
                     learn_rate = c(0.5),
                     sample_rate = c(0.7, 0.9),
                     col_sample_rate = c(0.7, 0.9))
search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 10, 
                        seed = 1122)
xgb_grid <- h2o.grid(algorithm = "xgboost",
                     x = x, y = y,
                     training_frame = train.h2o,
                     nfolds = 3,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

# get best model
grid <- h2o.getGrid(grid_id = xgb_grid@grid_id, sort_by = "f1", decreasing = TRUE)
grid_top_model <- grid@summary_table[1, "model_ids"]
print(grid)
best_model <- h2o.getModel(grid@model_ids[[1]])
summary(best_model)
# performance
h2o.performance (best_model)
h2o.gainsLift(best_model, train.h2o)
# predict
predict_test = as.data.frame(h2o.predict(best_model, test.h2o))


### Build using GBM

sp <- h2o.splitFrame(train.h2o,ratios = 0.7)
split_val <- sp[[2]]
# Choose hyperparameters
ntrees <- seq(20, 180, 20)
max_depth_opts <- seq(7,10)
min_rows_opts <- c(20,50,100)
learn_rate_opts <- c(0.1,0.3)
sample_rate_opts <- c(0.7, 0.9)
col_sample_rate_opts <- c(0.7, 0.9)
col_sample_rate_per_tree_opts = c(0.7, 0.9)

hyper_params = list( ntrees = ntrees,
                     max_depth = max_depth_opts,
                     min_rows = min_rows_opts,
                     learn_rate = learn_rate_opts,
                     sample_rate = sample_rate_opts,
                     col_sample_rate = col_sample_rate_opts,
                     col_sample_rate_per_tree = col_sample_rate_per_tree_opts
)

search_criteria = list(strategy = "RandomDiscrete", max_models = 30, seed = 123456)
# create grid
gbm.grid <- h2o.grid("gbm",
                     grid_id = "mygrid",
                     x = x,
                     y = y,
                     training_frame = train.h2o,
                     nfolds = 3,
                     stopping_rounds = 3,
                     stopping_tolerance = 1e-3,
                     stopping_metric = "AUC",
                     score_tree_interval = 50, 
                     seed = 123456,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
# Get best model
gbm.sorted.grid <- h2o.getGrid(grid_id = "mygrid", sort_by = "f1", decreasing = TRUE)
print(gbm.sorted.grid)
best_model <- h2o.getModel(gbm.sorted.grid@model_ids[[1]])
summary(best_model)

# View model performance
h2o.performance (best_model)
predict.gbm <- as.data.frame(h2o.predict(best_model, test.h2o))

# Predict on test
result_test = predict.gbm$predict
result_test = ifelse(result_test == 1,'Y','N' )
result_test = as.character(as.factor(result_test))
summary(result_test)

write.csv(result_test, 'submission.csv')

## Building Decision tree for rule mining and pattern identification
## Train_data taken without standardizing or normalizing
library(C50)

build_c50_tree <- function(train_data)
{
  c5_tree = C5.0(train_data$ReportedFraud ~ . , train_data)
  c5_rules = C5.0(train_data$ReportedFraud ~ . , train_data, rules = T)
  print (summary(c5_rules))
  print (C5imp(c5_tree, metric = "usage"))
  #plot(c5_tree)
}


# Train without address
build_c50_tree(train_data)

# Train without address
train_wo_address = train_data[ , !(names(train_data) %in% 'IncidentAddress')]
build_c50_tree(train_wo_address)

# Length of total fraudulent confirmation observations => 7785
length(train_data$ReportedFraud[train_data$ReportedFraud == 1])
