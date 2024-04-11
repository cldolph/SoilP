
# Main script for predicting soil P across the Upper Mississippi River Basin, USA
#using a random forest model

#All input files and scripts on the GitHub repository: https://github.com/cldolph/SoilP.git

#updated 9/20/22

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
  #geom_histogram(binwidth = 1,colour = "lightgray", fill = "darkgrey") +
  geom_histogram(binwidth = 1,colour = "black", fill ="black") +
  #geom_vline(aes(xintercept=mean(P_mgkg)), linetype="dashed", color="blue")+
  geom_vline(aes(xintercept=mean(P_mgkg)), linetype="dashed", color="purple", size=1.5)+
  #geom_vline(aes(xintercept=1000), linetype="dotdash", color="black")+
  geom_vline(aes(xintercept=1000), linetype="dotdash", color="darkgray", size=1.5)+
  xlab("P (mg/kg)") +
  ylab ("Frequency") +
  #ggtitle("Near Channel Soil P distribution")+
  #ggtitle("Soil P Samples distribution") +
  theme(axis.title = element_text(size=36))+
  theme(axis.text=element_text(size=36))+
  #theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #theme(plot.title = element_text(size = 26, face = "bold", hjust=0.5))
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
jpeg("./Soil_totalP_histogram_usgs_ncss.jpg", units="in", width=8, height=8, res=300)
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
nrow(P_test_preprocessed)
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

setwd(output_dir)

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
tune_res<-readRDS("./rf_tune_results_INITIAL_ranger.rds")
tune_res

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
#Tho note: in this case, the fine tuned models did not perform
#better than the original model selected above 
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

#load in tune results (as needed, if skipping ahead to this step)
regular_res<-readRDS("./rf_fine_tuned_results_ranger.rds")
regular_res

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

regular_res %>% 
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter") %>%
  arrange(mean) %>%
  print(n=40) 


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


best_rmse2 <- select_best(regular_res, "rmse")
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
rf_fit<-readRDS("./rf_fit.ranger_MODEL_training_less1000.rds")
rf_fit
###
#APPLY MODEL TO TEST DATA
#predict soil P for the test dataset
rf_pred <-predict(rf_fit, new_data=P_test_preprocessed)
rf_pred  
#add predictions to test data:
test.pred<-as.data.frame(cbind(P_test_preprocessed, rf_pred))

#check R2 value for actual vs predicted
#(using unscaled data)
reg<- with(test.pred,lm(P_mgkg_unscaled~pred_unscaled))
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
RMSE = mean((test.pred$P_mgkg_unscaled - test.pred$pred_unscaled)^2) %>% sqrt()
RMSE
mean(test.pred$P_mgkg_unscaled)

#calculate RMSE as percentage:
#https://search.r-project.org/CRAN/refmans/forestmangr/html/rmse_per.html
library(forestmangr)
names(test.pred)
class(test.pred)
rmse_per(test.pred, "P_mgkg_unscaled", "pred_unscaled", na.rm = TRUE)

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

#LOAD IN MODEL (if needed)
rfP_model<-readRDS("./FINALMODEL_randomForest_less1000.rds")
rfP_model

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
setwd(output_dir)
rfP_model<-readRDS("./FINALMODEL_randomforest_less1000.rds")
rfP_model

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
VarImp2[1:75,]

#rename 30m grid scale NLCD attribute so it doesn't get confused with catchment-scale attributes
VarImp3<-
  VarImp2 %>% 
  mutate(Var=ifelse(Var=="NLCD06Cat", "NLCD06", Var))
head(VarImp3)

Importance.plot<-
  VarImp3[1:65,] %>% 
  #slice(25, log10(Imp)) %>% 
  ggplot() + aes(x=reorder(Var, Imp), y=log10(Imp)) + geom_point()+coord_flip()+
  ylab("Importance, log scale")+
  xlab("Covariate")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(size=16),
          axis.title=element_text(size=18,face="bold"))
Importance.plot

#write fig to file
jpeg("./Covariate_importance_plot_less1000_NEW.jpeg", units="in", width=17, height=15, res=300)
Importance.plot
dev.off()

###############################
# STEP 10 PREDICT SOIL P FOR 100M GRID ACROSS UMRB

#Processing UMRB 100m grid points for attributes, for use in predictive modelling
#author: Christy Dolph
#updated: 9/21/2022

#DESCRIPTION
#This script loads in a 100m grid of points spaced equally across the
#Upper Mississippi River Basin (UMRB), with some attributes that
#were added in ArcGIS
#The script then 
#1. Divides the UMRB grid file into chunks, to decrease memory demands (each chunk processed separately)
#2. Loads in P training data that was used to build a random forest model for predicting soil P
#1. provides for some additional processing of attributes in the UMRB grid file
#2. merges the grid file with additional attributes from ssurgo and streamcat files
#3. imputes missing values using missranger package
#4. centers and scales covariate attributes for UMRB grid data
#5. predicts soil P for UMRB grid points using an existing random forest model, using a tidymodels framework 

#on home machine:
#setwd('C:/Users/dolph/Documents/SoilP')

#on MSI:
#Specify the library path, so can load locally installed packages:
#(replace with your library path here)

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
#alternatively -experimenting with fread below

#note: data.table is much faster for reading in data, but messes with some of the tidymodels commands


#read in training data: (for use in processing prediction data)
P.Predictors<-read.delim("./P_Predictors_imputed.csv",sep=",", header=TRUE)
head(P.Predictors)
nrow(P.Predictors)

#rename P column to match for train and test
names(P.Predictors)
colnames(P.Predictors)[1]<- "P_mgkg"

#format covariates to correct class:
P.Predictors <- transform(
  P.Predictors,
  P_mgkg=as.numeric(P_mgkg),
  slope_r=as.numeric(slope_r),
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  ffd_r=as.numeric(ffd_r),
  comppct_r=as.numeric(comppct_r),
  soc150_999=as.numeric(soc150_999),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  om_r=as.numeric(om_r),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  aws0_5 = as.numeric(aws0_5), 
  aws5_20=as.numeric(aws5_20),
  aws20_50= as.numeric(aws20_50), 
  aws50_100=as.numeric(aws50_100), 
  aws100_150=as.numeric(aws100_150), 
  aws0_20=as.numeric(aws0_20),
  aws0_30=as.numeric(aws0_30),
  aws0_100=as.numeric(aws0_100), 
  aws0_150=as.numeric(aws0_150) , 
  aws0_999=as.numeric(aws0_999), 
  soc0_5=as.numeric(soc0_5), 
  soc5_20=as.numeric(soc5_20),
  soc20_50=as.numeric(soc20_50), 
  soc50_100=as.numeric(soc50_100),
  soc100_150=as.numeric(soc100_150), 
  soc0_20=as.numeric(soc0_20), 
  soc0_30=as.numeric(soc0_30),
  soc0_100=as.numeric(soc0_100), 
  soc0_150=as.numeric(soc0_150),
  soc0_999=as.numeric(soc0_999), 
  nccpi3corn=as.numeric(nccpi3corn),
  nccpi3soy=as.numeric(nccpi3soy), 
  nccpi3cot=as.numeric(nccpi3cot),
  nccpi3sg=as.numeric(nccpi3sg), 
  nccpi3all=as.numeric(nccpi3all),
  pctearthmc=as.numeric(pctearthmc), 
  rootznemc=as.numeric(rootznemc),
  rootznaws=as.numeric(rootznaws), 
  droughty=as.numeric(droughty),
  Depth_cm=as.numeric(Depth_cm),
  PctNonCarbResidCat=as.numeric(PctNonCarbResidCat)
)


#transform categorical variables
P.Predictors <- transform(
  P.Predictors,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  #taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind)
)

#EDIT 7.13.22
#Exclude attributes with factor levels that do not match factor levels in UMRB grid dataset
Exclude.grid<-c('taxpartsize', 'texcl', 'pmkind', 'taxreaction')
P.Predictors2<-P.Predictors[,!(names(P.Predictors) %in% Exclude.grid)]

#ADDITIONAL EDIT 7.13.22
#Exclude samples with soil P >1000 to see how model performance is affected
P.Predictors2<-P.Predictors2[P.Predictors2$P_mgkg<1000,]
summary(P.Predictors2$P_mgkg)

#make ID column 
P.Predictors2$id<-as.numeric(rownames(P.Predictors2))
names(P.Predictors2)

#Define a recipe      
P_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(P_mgkg ~ ., data = P.Predictors2) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  update_role(id, new_role = "id variable") %>%  #retain ID variable
  update_role(-id, new_role = 'predictor')  #but exclude it from prediction!!
#Other options excluded for now:
#%>%
#step_dummy(all_nominal()) #%>%
#step_impute_bag(all_predictors(), trees=100) #trying to impute missing values, using all other predictors

P_recipe

P_train_preprocessed <- P_recipe %>%
  # apply the recipe to the training data
  prep(P.Predictors2) %>%
  # extract the pre-processed training dataset
  juice()
head(P_train_preprocessed)


#Read in UMRB grid file:
#using data.table (takes ~30 seconds to read)
library(data.table)
UMRBgrid<-fread("./UMRB_grid_NWI_NLCD16_NHDv2100b_ssurgo_NLCD06_CatchmentID.csv",
                select = c('UMRB_GRID_ID', 'MUTKEY', 'FTYPE', 'FCODE', 'WETLAND_TYPE', 
                           'NLCD06', 'FEATUREID_1'))
UMRBgrid<-as.data.frame(UMRBgrid)
nrow(UMRBgrid)
class(UMRBgrid)
head(UMRBgrid)

#rows = 51336964

#1. Option: Split the dataset into chunks
#https://stackoverflow.com/questions/8762719/divide-a-dataset-into-chunks
chunks<-split(UMRBgrid, 1:8)

#CHUNK1
#reassign chunk:
grid.att<-chunks$"1"
nrow(grid.att)

#Or if using whole dataset:
#grid.att<-UMRBgrid
#nrow(grid.att)


#Processing UMRB grid for predictive modelling:

#2. Additional columns needed:
#Add Depth column
grid.att$Depth_cm<-rep(30, nrow(grid.att))

#Fix name for FEATUREID column
colnames(grid.att)[7]<-'FEATUREID'




#3. Create Levels for Attribute data

#set NA to 0 for FCODE
grid.att$FCODE[is.na(grid.att$FCODE)] <- 0

#Create levels for streams layer;
#levels are: 
#levels(factor(grid.att$FCODE))
#see definitions here: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm

#compare to training dataset attribute levels
#Training data attributes = c('0', '33600', '46003', '46006', '55800', '56600')
#levels in UMRB grid not in training data:
#33400 - Connector: see definition here: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/NHDFlowline/Connector.htm
#33601 - Canal Ditch (note: Canal/Ditch type|aqueduct)
#42800 - Pipeline  
#42801 - Pipeline (Product|water; aqueduct; at or near surface
#42805 - Pipeline; Product|water; general case; at or near surface
#42813 - Pipeline; product|water; siphon; unspecified relation to surface
#42823 - Pipeline: underground stormwater
#46000 - Stream/River; feature type only; no attribute

#assign pipelines and aqueducts 'Nonstream'; since aqueducts and pipelines are 
#unlikely to interact with the surrounding environment the way streams are
#if they are located within dams that should show up in NLCD or NWI
#hm note: predicting soil P for grid points in water bodies doesnt make sense
#so cut those predictions out of final model; restrict to land areas! (ie, not open water)
#may want to go back after the fact to address open water areas; predictions likely not useful
#assign 46000 t0 46006 (assume perennial stream)

#Reassign attributes:
grid.att$FCODE[grid.att$FCODE=="33400"] <- "0"
grid.att$FCODE[grid.att$FCODE=="33601"] <- "0"
grid.att$FCODE[grid.att$FCODE=="42800"] <- "0"
grid.att$FCODE[grid.att$FCODE=="42801"] <- "0"
grid.att$FCODE[grid.att$FCODE=="42805"] <- "0"
grid.att$FCODE[grid.att$FCODE=="42813"] <- "0"
grid.att$FCODE[grid.att$FCODE=="42823"] <- "0"
grid.att$FCODE[grid.att$FCODE=="46000"] <- "46006"

#levels(factor(grid.att$FCODE))

grid.att$StreamType<-factor(grid.att$FCODE, levels=c('0', '33600', '46003', '46006', '55800', '56600'),
                            labels=c('NonStream', 'Canal/Ditch', 'Intermittent_Stream', 'Perennial_Stream', 'Artificial_Path', 'Coastline'))


#Create categorical variable for NWI categories
names(grid.att)
#levels(factor(grid.att$WETLAND_TYPE))
#only category missing from training data is "Other"
#for now assign to 'NonWetland'
grid.att$WETLAND_TYPE[grid.att$WETLAND_TYPE=="Other"]<-""
#levels(factor(grid.att$WETLAND_TYPE))

grid.att$WetlandType<-factor(grid.att$WETLAND_TYPE, levels=c('', 'Freshwater Emergent Wetland', 
                                                             'Freshwater Forested/Shrub Wetland','Freshwater Pond', 'Lake', 'Riverine'),
                             labels=c('NonWetland','Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland',
                                      'Freshwater Pond', 'Lake', 'Riverine' ))
#levels(grid.att$WetlandType)


#NLCD 2006 Land Use category - note this category has some overlap with NWI
#check number of levels
#levels(factor(grid.att$NLCD06))
#same levels as training data
#create categorical variable
#0 - 11 - Open Water
#1 - 21 - Developed, Open Space
#2 - 22 - Developed, Low Intensity
#3 - 23 - Developed, Medium Intensity
#4 - 24 - Developed, High Intensity
#5 - 31 - Barren Land
#6 - 41 - Deciduous Forest
#7 - 42 - Evergreen Forest
#8 - 43 - Mixed Forest
#9 - 52 - Shrub/Scrub
#10 - 71 - Grassland/Herbaceous
#11 - 81 - Pasture/Hay
#12 - 82 - Cultivated Crops
#13 - 90 - Woody Wetlands
#14 - 95 - Emergent Herbaceous Wetlands
grid.att$NLCD06Cat<-factor(grid.att$NLCD06, levels=c('11','21','22','23','24','31','41','42','43','52',
                                                     '71','81','82','90','95'),
                           labels=c('Open Water', 'Developed, Open Space', 'Developed, Low Intensity',
                                    'Developed, Medium Intensity', 'Developed, High Intensity', 'Barren Land',
                                    'Deciduous Forest','Evergreen Forest','Mixed Forest','Shrub/Scrub','Grassland/Herbaceous',
                                    'Pasture/Hay','Cultivated Crops','Woody Wetlands','Emergent Herbaious Wetlands'))


#4. Load in ssurgo attributes and merge to grid points
ssurgo<-fread('./SSURGO_attributes_unique_for_mapunit_CEAP_regionalextent.csv')
ssurgo<-as.data.frame(ssurgo)
grid.att<-merge(grid.att, ssurgo, by=c('MUTKEY'))
head(grid.att)

#Exclude sites in grid with drainagcl=="Subaqueous", ie., include sites located in water
grid.att2<-droplevels(grid.att[!grid.att$drainagecl=="Subaqueous",])
nrow(grid.att2)


#4. ADD IN STREAM CAT VARIABLES 
streamcat<-fread('./StreamCat_variables_for_SOIL_P.csv')
streamcat<-as.data.frame(streamcat)

#Make a FEATUREID column equivalent to COMID
streamcat$FEATUREID<-streamcat$COMID

#MERGE STREAMCAT TO ALL SOIL P DATA
All.Grid<-merge(grid.att2, streamcat, by=c('FEATUREID'))
nrow(All.Grid)
#Compare to initial chunk size:
#nrow(Chunk1)
#retaining 86% of UMRB grid points
#what points are being lost?

#Check columns:
names(All.Grid)
nrow(All.Grid)

#5. SET ID TO UMRB_GRID_ID (formerly OBJECTID for the UMRB grid input file), for future tracking purposes
All.Grid$id<-as.numeric(All.Grid$UMRB_GRID_ID)
names(All.Grid)

#6. Match final attribute list to attributes used in training model

Keep.List<-names(as.data.frame(P.Predictors2[,c(2:270)])) ##check this to make sure ID is included!
Keep.List

griddata<-All.Grid[, names(All.Grid) %in% Keep.List]
nrow(griddata)

#7. #format covariates to correct class:
griddata <- transform(
  griddata,
  slope_r=as.numeric(slope_r),
  elev_r=as.numeric(elev_r),
  airtempa_r=as.numeric(airtempa_r),
  ffd_r=as.numeric(ffd_r),
  comppct_r=as.numeric(comppct_r),
  soc150_999=as.numeric(soc150_999),
  sieveno4_r=as.numeric(sieveno4_r),
  sieveno10_r=as.numeric(sieveno10_r),
  sieveno40_r=as.numeric(sieveno40_r),
  sieveno200_r=as.numeric(sieveno200_r),
  sandtotal_r=as.numeric(sandtotal_r),
  silttotal_r=as.numeric(silttotal_r),
  claytotal_r=as.numeric(claytotal_r),
  om_r=as.numeric(om_r),
  ksat_r=as.numeric(ksat_r),
  kffact=as.numeric(kffact),
  caco3_r=as.numeric(caco3_r),
  gypsum_r=as.numeric(gypsum_r),
  aws0_5 = as.numeric(aws0_5), 
  aws5_20=as.numeric(aws5_20),
  aws20_50= as.numeric(aws20_50), 
  aws50_100=as.numeric(aws50_100), 
  aws100_150=as.numeric(aws100_150), 
  aws0_20=as.numeric(aws0_20),
  aws0_30=as.numeric(aws0_30),
  aws0_100=as.numeric(aws0_100), 
  aws0_150=as.numeric(aws0_150) , 
  aws0_999=as.numeric(aws0_999), 
  soc0_5=as.numeric(soc0_5), 
  soc5_20=as.numeric(soc5_20),
  soc20_50=as.numeric(soc20_50), 
  soc50_100=as.numeric(soc50_100),
  soc100_150=as.numeric(soc100_150), 
  soc0_20=as.numeric(soc0_20), 
  soc0_30=as.numeric(soc0_30),
  soc0_100=as.numeric(soc0_100), 
  soc0_150=as.numeric(soc0_150),
  soc0_999=as.numeric(soc0_999), 
  nccpi3corn=as.numeric(nccpi3corn),
  nccpi3soy=as.numeric(nccpi3soy), 
  nccpi3cot=as.numeric(nccpi3cot),
  nccpi3sg=as.numeric(nccpi3sg), 
  nccpi3all=as.numeric(nccpi3all),
  pctearthmc=as.numeric(pctearthmc), 
  rootznemc=as.numeric(rootznemc),
  rootznaws=as.numeric(rootznaws), 
  droughty=as.numeric(droughty),
  Depth_cm=as.numeric(Depth_cm),
  PctNonCarbResidCat=as.numeric(PctNonCarbResidCat)
)


#transform categorical variables
griddata <- transform(
  griddata,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  #taxpartsize=as.factor(taxpartsize),
  #taxreaction=as.factor(taxreaction),
  #texcl=as.factor(texcl),
  #pmkind=as.factor(pmkind),
  taxceactcl=as.factor(taxceactcl)
)

lapply(griddata, class)


#7. Impute missing values
#skipping for now as it's too computationally intensive! 
#instead using na.roughfix option below
#library(missRanger)
#GridImputed <- missRanger(griddata,
#formula = . ~ . ,
# num.trees = 100, 
#verbose = 1, seed = 111)
#fwrite(GridImputed, "./UMRB_chunk1_Grid_imputed.csv", sep=",", row.names=FALSE)


grid.roughfix<-na.roughfix(griddata)
head(grid.roughfix)

#fwrite(grid.roughfix, './UMRB_chunk1_grid_roughimpute.csv', sep=",", row.names=FALSE)
#fwrite(grid.roughfix, './UMRB_grid_roughimpute.csv', sep=",", row.names=FALSE)



#Check levels for training dataset

#setdiff(levels(grid.roughfix$StreamType), levels(factor(P.Predictors2$StreamType)))
#setdiff(levels(grid.roughfix$WetlandType), levels(factor(P.Predictors2$WetlandType)))
#setdiff(levels(grid.roughfix$NLCD06Cat), levels(factor(P.Predictors2$NLCD06Cat)))
#setdiff(levels(grid.roughfix$hydricrating), levels(factor(P.Predictors2$hydricrating)))
#setdiff(levels(grid.roughfix$drainagecl), levels(factor(P.Predictors2$drainagecl)))
#setdiff(levels(grid.roughfix$taxpartsize), levels(factor(P.Predictors2$taxpartsize)))
#setdiff(levels(grid.roughfix$taxceactcl), levels(factor(P.Predictors2$taxceactcl)))
#setdiff(levels(grid.roughfix$texcl), levels(factor(P.Predictors2$texcl)))
#setdiff(levels(grid.roughfix$pmkind), levels(factor(P.Predictors2$pmkind)))
#setdiff(levels(grid.roughfix$taxreaction), levels(factor(P.Predictors2$taxreaction)))



#Make an empty P_mgkg column?
grid.roughfix$P_mgkg<-as.numeric(rep("0", nrow(grid.roughfix)))
head(grid.roughfix)

#Bake the prediction dataset!
Grid_preprocessed <- P_recipe %>%
  #create the recipe from the training data
  prep(P.Predictors2) %>%
  # apply the recipe to the new prediction dataset
  bake(new_data=grid.roughfix)
head(Grid_preprocessed)
#fwrite(Grid_preprocessed, "./Grid_preprocessed_chunk1.csv")

#try to exclude id and P_mgkg columns
Grid_preprocessed<-as.data.frame(Grid_preprocessed)
names(Grid_preprocessed[,c(1:268)])
names(Grid_preprocessed[,c(269:270)])


#load existing model: 
rfP_model<-readRDS('./FINALMODEL_randomForest_less1000.rds')
rfP_model

#Run model for grid chunk:
grid.pred<-predict(rfP_model, newdata=Grid_preprocessed[,c(1:268)])
head(grid.pred)

#bind back to "id" for mapping later
#nrow(All.Grid)
#length(grid.pred)

grid.pred.ID<-as.data.frame(cbind(grid.roughfix$id, grid.pred))
colnames(grid.pred.ID)[1]<-"UMRB_GRID_ID"
head(grid.pred.ID)

#Unscale predicted P values:
grid.pred.ID$P_mgkg_unscaled<-grid.pred.ID$grid.pred* sd(P.Predictors2$P_mgkg) + mean(P.Predictors2$P_mgkg)
head(grid.pred.ID)

fwrite(grid.pred.ID, "./UMRB_grid_chunk1_predicted.csv", sep=",")
