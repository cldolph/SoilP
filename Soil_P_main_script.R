
# Main script for predicting soil P across the Upper Mississippi River Basin, USA
#using a random forest model

#All input files and scripts on the GitHub repository: https://github.com/cldolph/SoilP.git

#updated 9/14/22

# Clear memory
rm(list=ls())

#################
# LOAD PACKAGES #
#################

library(tidyverse)
library(stringr)
library(data.table) #note: make sure it works ok to load this, if it conflicts
library(tidymodels)
library(workflows)
library(tune)
library(ranger)
library(missRanger) 
###################
# SET DIRECTORIES #
###################

# Define the input & output directory (change as appropriate to your machine)
input_dir <- "C:/Users/dolph/Documents/SoilP/Soil_P_input"
output_dir <- "C:/Users/dolph/Documents/SoilP/Soil_P_output"

###################
# STEP 1: RUN MODULE TO PRE-PROCESS USGS SOIL P DATA #
#note: takes ~ 15 seconds to run 
#generates an intermediate output file: 'soil_USGSgeochem_since2000_assumed_depths.csv'
setwd(input_dir)
source('module1_preprocess_soilP_data.R')

#####################
# STEP 2: RUN MODULE TO PRE-PROCESS gSSURGO ATTRIBUTES #
#note: takes ~ 9 minutes
#generates an intermediate output file: 'SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv'
setwd(input_dir)
source('module2_preprocess_SSURGO.R')

#######################
# STEP 3: RUN MODULE TO PRE-PROCESS STREAMCAT ATTRIBUTES #
#note: takes ~ 8 minutes
#generates an intermediate output file: 'StreamCat_variables_for_SOIL_P.csv'
setwd(input_dir)
source('module3_preprocess_StreamCat.R')

########################
# STEP 4: LOAD IN SOIL P DATA w/SPATIAL ATTRIBUTES #
#& MERGE WITH SSURGO & STREAMCAT ATTRIBUTES #
#note: takes ~ 1 minute
#generates an intermediate output file: 'Soil_P_data_for_modelling.csv'
setwd(input_dir)
source('module4_merge_covariates.R')

#########################
#STEP 5: EXPLORE THE DATA #

#Read in soil P data for modelling (prepped in modules above):
setwd(output_dir)

P_orig<-read.delim('./Soil_P_data_for_modelling.csv', sep=",", header=TRUE)
head(P_orig)
nrow(P_orig)
#make a copy to work with
P_clean<-P_orig
names(P_clean)

summary(P_clean$P_mgkg)

#Histogram of P
P.hist<-
  ggplot(P_clean, aes(P_mgkg))+
  geom_histogram(binwidth = 1,colour = "lightgray", fill = "darkgrey") +
  geom_vline(aes(xintercept=mean(P_mgkg)), linetype="dashed", color="blue")+
  xlab("P (mg/kg)") +
  ylab ("Frequency") +
  #ggtitle("Near Channel Soil P distribution")+
  #ggtitle("Soil P Samples distribution") +
  theme(axis.title = element_text(size=26))+
  theme(axis.text=element_text(size=26))+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))
P.hist

# Formal calculation of skewness (e1071 package)
#(library(e1071) #note this package masks several objects from needed pacakges
#The skewness for a normal distribution = zero
#and any symmetric data should have a skewness near zero. 
#Negative values for the skewness indicate data that are skewed left 
#positive values for the skewness indicate data that are skewed right. 
#skewness(P_clean$P_mgkg)

#write fig to file
setwd(output_dir)
tiff("./Soil_totalP_histogram_usgs_ncss.tiff", units="in", width=8, height=8, res=300)
P.hist
dev.off()

#Compare riparian and non riparian soils:
P.rip<-
  ggplot(P_clean, aes(StreamType, P_mgkg))+
           geom_boxplot()
P.rip

#number & mean of surface and subsurface samples
P_clean %>%
  group_by(grp = cut(Depth_cm, c(-Inf, 30, Inf))) %>%
  summarise(mean=mean(P_mgkg), n=n()) 

#number of samples with high soil P >1000 mg/kg
nrow(filter(P_clean, P_mgkg>1000))
ggplot(filter(P_clean, P_mgkg>1000))+
  geom_point(aes(Depth_cm, P_mgkg)) #high soil P as a function of sample depth


########################
#STEP 6: SPLIT DATA INTO TRAIN AND TEST, AND 
#ESTIMATE MISSING VALUES USING IMPUTATION 
#Note: takes ~16 minutes 

setwd(input_dir)
source('module6_split_and_impute.R')

#########################
##STEP 7 TUNE RANDOM FOREST MODELS

setwd(input_dir)

#prep datasets for modelling
source('module7_prep_model_data.R')

#Input datasets are:
#1. Training Data: 'P.Predictors2'
#2. Test Data: 'test.prep2'

#Note: These datasets exclude samples with soil P > 1000 mg/kg

###
#DEFINE A RECIPE 
#Notes:
#standardize predictors
#note nominal variables include factor and character variables
#update: not using step_dummy b/c this is reported to affect model run time
#see https://community.rstudio.com/t/tidymodels-slow-hyperparameter-tuning-with-wide-data-and-group-cv-fold/108087/5
#see https://juliasilge.com/blog/sf-trees-random-tuning/ for helpful processing steps
#not collapsing categorical levels because none of them have an extremely high number of levels

#not imputing missing values as you already did that separately 

#Note: best practice is to scale only using the training dataset - 
#eg: https://datascience.stackexchange.com/questions/39932/feature-scaling-both-training-and-test-data
#If you use the whole dataset to figure out the feature mean and variance, 
#you're using knowledge about the distribution of the test set to set the scale of the training set - 'leaking' information.
#then you can "bake" out the centered/scaled test data later, when you need to test independent randomForest models.


P_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(P_mgkg ~ ., data = P.Predictors2) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())
  #Other options excluded for now, eg:
  #%>%
  #step_dummy(all_nominal()) #%>%
  #step_impute_bag(all_predictors(), trees=100) #option to impute missing values, using all other predictors
P_recipe


###
#EXTRACT THE PREPROCESSED DATASET
P_train_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors2) %>%
  # extract the pre-processed training dataset
  juice()
P_train_preprocessed

#write pre-processed training data to file
setwd(output_dir)
write.table(as.data.frame(P_train_preprocessed), "./P_train_preprocessed.csv", sep=",", row.names=FALSE)
names(P_train_preprocessed)

#Important: How you prep the test data:
#The bake() function takes a prepped recipe (one that has had all quantities estimated from training data) 
#and applies it to new_data. 
#That new_data could be the training data again...
#Or it could be the testing data. In this case, the column means from the training data are applied to the testing data;
#To do otherwise is data leakage.

#So need to bake() and not juice
P_test_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors2) %>%
  # extract the pre-processed testing dataset
  bake(new_data=test.prep2)

#View(P_test_preprocessed)

#write pre-processed testing data to file, so can use later for model evaluation
write.table(as.data.frame(P_test_preprocessed), "./P_test_preprocessed.csv", sep=",", row.names=FALSE)


###
#SPECIFY THE MODEL

#example from Julia Silge: 
#https://juliasilge.com/blog/sf-trees-random-tuning/

#note: tuning the number of trees is not necessary
#see https://stats.stackexchange.com/questions/348245/do-we-have-to-tune-the-number-of-trees-in-a-random-forest
#"Tuning the number of trees is unnecessary; instead, simply set the number of trees to a large, computationally feasible number, and let the asymptotic behavior of LLN do the rest."

#Use parallel processing to make tuning go faster
#install.packages("doParallel", repos="https://cloud.r-project.org")
#install.packages("foreach", repos="https://cloud.r-project.org")

library(doParallel)
library(foreach)

doParallel::registerDoParallel()

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

#specify cross validation for the training dataset, note that default is k=10 folds
set.seed(234)
P_folds <- vfold_cv(P.Predictors2)

###
#PUT EVERYTHING TOGETHER IN A WORKFLOW

tune_wf <- workflow() %>%
  add_recipe(P_recipe) %>%
  add_model(tune_spec)

###
#TUNE THE PARAMETERS

#Check start time for initial tuning
start_time <- Sys.time()
start_time

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = P_folds,
  grid = 20,
  control = control_grid(verbose = TRUE)
)
tune_res

#Check end time for initial tuning: 
end_time <- Sys.time()
end_time - start_time

#save the tune results
saveRDS(tune_res, "./rf_tune_results_INITIAL_range.rds")

#load in tune results (as needed, if skipping ahead to this step)
#tune_res<-readRDS("./rf_tune_results_INITIAL_ranger.rds")
#tune_res

#Check out initial tuning results
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  arrange(mean) %>%
  print(n=40) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")


#find the best set:
best_rmse1<-select_best(tune_res, "rmse")
best_rmse1


#Option: Tune one more time, narrowing in on good tuning
#Note: for now, not selecting this option (as haven't seen increase
#in performance)

rf_grid <- grid_regular(
  mtry(range = c(50,95)),
 min_n(range = c(3, 17)),
  levels = 5)

rf_grid %>% 
print(n=25)

#Check start time for fine tuning
start_time <- Sys.time()
start_time
set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = P_folds,
  grid = rf_grid,
  control = control_grid(verbose = TRUE))

regular_res
#save the tune results
saveRDS(regular_res, "./rf_fine_tuned_results_ranger.rds")


#Check end time for fine tuning: 
end_time <- Sys.time()
end_time - start_time

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "R2")

best_rmse2 <- select_best(regular_res, "rsq")
best_rmse2



####
#FINALIZE THE WORKFLOW

final_rf <- finalize_model(
  tune_spec,
  best_rmse2
)

final_rf

final_wf <- workflow() %>%
  add_recipe(recipe(P_mgkg ~ ., data = P_train_preprocessed)) %>%
  add_model(final_rf)

###
#FIT THE MODEL BASED ON TRAINING DATA
rf_fit <- final_wf %>%
  fit(P_train_preprocessed)
rf_fit

#Save the final model 
saveRDS(rf_fit, "./rf_fit.ranger_MODEL_training_less1000.rds")

###
#APPLY MODEL TO TEST DATA
#predict soil P for the test dataset
rf_pred <-predict(rf_fit, new_data=P_test_preprocessed)
rf_pred  
#add predictions to test data:
test.pred<-as.data.frame(cbind(P_test_preprocessed, rf_pred))

#check R2 value for actual vs predicted
reg<- with(test.pred,lm(P_mgkg~.pred))
summary(reg)

#unscale by multiplying values by the standard deviation and adding the mean of the training dataset
test.pred$P_mgkg_unscaled<-test.pred$P_mgkg* sd(P.Predictors2$P_mgkg) + mean(P.Predictors2$P_mgkg)
test.pred$pred_unscaled<-test.pred$.pred* sd(P.Predictors2$P_mgkg) + mean(P.Predictors2$P_mgkg)
head(test.pred)
nrow(test.pred)

#View actual vs predicted soil P for test data
ActualvsPredict.plot<-
  ggplot(test.pred)+
  geom_point(aes(P_mgkg_unscaled, pred_unscaled))+
  #scaled and centered data:
  #geom_point(aes(P_mgkg, .pred))+
  geom_abline(intercept=0, slope=1)+
  xlim(0,1000)+
  ylim(0,1000)+
  theme_bw()+
  xlab("Actual P (mg/kg)") +
  ylab ("Predicted P (mg/kg)") +
  theme_bw()+
  theme(axis.title=element_text(size=24), axis.text=element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ActualvsPredict.plot

#write fig to file
tiff("./Actual_vs_model_predicted_P_test_data.tiff", units="in", width=8, height=8, res=300)
ActualvsPredict.plot
dev.off()

#calculate rmse
RMSE = mean((test.pred$P_mgkg - test.pred$.pred)^2) %>% sqrt()
RMSE

######################################
# STEP 8: COMPARE RANGER AND RANDOM FOREST MODELS #

#See 'module8_compare_ranger_randomForest.R'
#Note: this module is not necessary for the implementation of the remaining script
#But is just for documentation purposes

#####################################
# STEP 9: VARIABLE IMPORTANCE (using permimp)

#see https://cran.r-project.org/web/packages/permimp/vignettes/permimp-package.html

setwd(output_dir)

library(party)
library(permimp)

#need a randomForest object
#need to set hyperparameters to those tuned by ranger previously 
#note in tidy models for engine rand_forest:
#min_n = An integer for the minimum number of data points in a node that are required for the node to be split further.
#is this equivalent in randomForest to nodesize = Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
#note: need pre-processed (centered, scaled) train and test data (created in recipe above):

#Run Model in randomForest
library(randomForest)

start_time <- Sys.time()
start_time

set.seed(542863)
rfP_model <- randomForest(P_mgkg ~ ., data = as.data.frame(P_train_preprocessed), mtry = 50, ntree=1000, replace = FALSE, 
                          nodesize = 3
                          , keep.forest = TRUE, keep.inbag = TRUE)
end_time <- Sys.time()
end_time - start_time

summary(rfP_model)

#SAVE FINAL RANDOM FOREST MODEL!
saveRDS(rfP_model, "./FINALMODEL_randomForest_less1000.rds")

#Check performance of the model (should match performance of the random_Forest() model tuned above, using tidymodels?)
rf.pred<-predict(rfP_model, newdata=P_test_preprocessed)
head(rf.pred)

#Match actual to predicted data:
predicted.P<-cbind(as.data.frame(P_test_preprocessed), rf.pred)
head(predicted.P)

#actual vs predicted
reg.check<-with(predicted.P, lm(P_mgkg~rf.pred))
summary(reg.check)
#it matches! R2=0.30
#calculate rmse
RMSE = mean((predicted.P$P_mgkg - predicted.P$rf.pred)^2) %>% sqrt()
RMSE


##RUN CONDITIONAL PERMUTATION IMPORTANCE:
#read in saved model if needed;

#rfP_model<-readRDS("./FINALMODEL_randomforest_less1000.rds")

#Note: permimp does require training data to run!

start_time <- Sys.time()
start_time

CPI_permimpRF <- permimp(rfP_model, 
conditional = TRUE, progressBar = TRUE, do_check=FALSE)

CPI_permimpRF
end_time <- Sys.time()
end_time - start_time

#save permimp object!
saveRDS(CPI_permimpRF, "./permimp_results_less1000.rds")


#read in permimp results if needed
CPI_permimpRF<-readRDS("./permimp_results_less1000.rds")


#Rank importance values and plot a subset:
#Make a dot plot:
library(dplyr)
library(ggplot2)
VarImp<-as.data.frame(sort(CPI_permimpRF$values, decreasing=TRUE), optional=T)
colnames(VarImp)[1]<-"Imp"
rownames(VarImp)
VarImp2 <- cbind(rownames(VarImp), data.frame(VarImp, row.names=NULL))
colnames(VarImp2)[1]<-"Var"
head(VarImp2)
View(VarImp)
VarImp2[1:25,]

Importance.plot<-
  VarImp2[1:17,] %>% 
  #slice(25, log10(Imp)) %>% 
  ggplot() + aes(x=reorder(Var, Imp), y=log10(Imp)) + geom_point()+coord_flip()+
  ylab("Importance, log scale")+
  xlab("Covariate")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(text = element_text(size = 14))
Importance.plot

#write fig to file
tiff("./Covariate_importance_plot_less1000.tiff", units="in", width=10, height=8, res=300)
Importance.plot
dev.off()

###############################
# STEP 10 PREDICT SOIL P FOR 100M GRID ACROSS UMRB

#updated: 9/19/2022

#DESCRIPTION
#This script loads in a 100m grid of points spaced equally across the
#Upper Mississippi River Basin (UMRB), with some attributes that
#were added in ArcGIS
#The script then 
#1. Divides the UMRB grid file into chunks, to decrease memory demands (each chunk processed separately)
#2. Loads in P training data that was used to build a random forest model for predicting soil P
#1. provides for some additional processing of attributes in the UMRB grid file
#2. merges the grid file with additional attributes from ssurgo and streamcat files
#3. imputes missing values for the grid points using rough fix method
#4. centers and scales covariate attributes for UMRB grid data
#5. predicts soil P for UMRB grid points using an existing random forest model, in a tidymodels framework 

#on home machine:
#setwd(output_dir)

#on MSI:
#Specify the library path, so can load locally installed packages:
.libPaths(c('/panfs/roc/groups/14/jfinlay/dolph008/r-packages',.libPaths())) 
setwd('/panfs/roc/groups/14/jfinlay/dolph008/SoilP')
#library(data.table)
library(tidymodels)
library(tidyverse)
library(randomForest)

#Methods for working with large data files:
#note that UMRB grid file is ~ 9 GB
#Strategy is to chunk UMRB Grid into workable subunits, 
#and run code (and model), for each subunit;

#note: data.table is much faster for reading in data, but messes with some of the tidymodels commands


#Note for Se Jong: working on making this section modular 
#module is grid_functions.R script
#for parallel processing - coming soon 


