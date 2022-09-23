#STEP 6: SPLIT DATA INTO TRAIN AND TEST, AND 
#ESTIMATE MISSING VALUES USING IMPUTATION 

setwd(output_dir)
P_clean<-read.delim('./Soil_P_data_for_modelling.csv', sep=",", header=TRUE)
head(P_clean)
nrow(P_clean)

#format covariates to correct class:
P_clean <- transform(
  P_clean,
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
P_clean <- transform(
  P_clean,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  #taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind))

#Check levels for factors (to see if they should be aggregated)
levels(P_clean$StreamType)
levels(P_clean$taxpartsize)
levels(P_clean$WetlandType)
levels(P_clean$NLCD06Cat)
levels(P_clean$hydricrating)
levels(P_clean$drainagecl)
levels(P_clean$taxpartsize)
levels(P_clean$texcl)
levels(P_clean$pmkind)

#Split into training and testing datasets
#90% for model training, 10% for model testing

DATA<-P_clean
samplesize = 0.90*nrow(DATA)
set.seed(100)
index = sample(seq_len(nrow(DATA)), size = samplesize)
#Creating training and test set 
datatrain = DATA[index,]
datatest = DATA[-index,]

nrow(datatrain) #n=6194
nrow(datatest)  #n=689

#Check P distribution for training and test datasets

summary(datatrain$P_mgkg) #rare very high values pushing the average here 
summary(datatest$P_mgkg)

#check for distribution of high values across train and test
nrow(datatrain[datatrain$P_mgkg>1000,]) #n=323
nrow(datatest[datatest$P_mgkg>1000,]) #n=29

#IMPUTE MISSING VALUES FOR REMAINING COVARIATES
#use missRanger to interpolate
#see https://cran.r-project.org/web/packages/missRanger/vignettes/missRanger.html

#By default missRanger uses all columns in the data set to impute all columns with missings. 

#exclude P_mgkg from imputation
#first impute training dataset
#then use imputed training dataset to impute test dataset (to prevent data leakage)

#Separate out predictor variables:
names(datatrain)
Predictors<-datatrain %>% 
  select(-P_mgkg)

#R with parameters set to run faster
#no pmm.k, num.trees=100; took ~ 3.5 min

start_time <- Sys.time()
start_time
dataImputed <- missRanger(Predictors,
                          formula = . ~ . ,
                          num.trees = 100, 
                          verbose = 1, seed = 111)
head(dataImputed)

end_time <- Sys.time()
end_time - start_time

nrow(dataImputed)

#rematch to soil P
P.DATA<-datatrain[,c(1)]
P.Predictors<-cbind(P.DATA, dataImputed)
head(P.Predictors)

summary(P.Predictors)

#WRITE IMPUTED DATA TO FILE
#(note this is training data only)
write.table(P.Predictors, "./P_Predictors_imputed.csv", sep=",", row.names=FALSE)

###
#Impute *testing data* separately (for use in model testing later)

names(datatest)

#recreate dataImputed
dataImputed<-P.Predictors %>% 
  select(-P.DATA)
names(dataImputed)
nrow(datatest)

Test.impute<-rbind(
  (datatest %>% 
    select(-P_mgkg)),
dataImputed)
nrow(Test.impute)


#Impute missing values for test data (combined with imputed training data)
start_time <- Sys.time()
start_time
data.Test.Imputed <- missRanger(Test.impute,
                                formula = . ~ . ,
                                num.trees = 100, 
                                verbose = 1, seed = 111)
head(data.Test.Imputed)

end_time <- Sys.time()
end_time - start_time

#select out test data; and attach to soil P
test<-data.Test.Imputed[c(1:689),]
P.test<-datatest[,c(1)]
P.test
test.prep<-cbind(P.test, test)
nrow(test.prep)
head(test.prep)

write.table(test.prep, "./P_TEST_imputed.csv", sep=",", row.names=FALSE)

names(test.prep)
#check these 3 variables
check.list<-c("SuperfundDensCat",
"SuperfundDensCatRp100",
"MineDensCat")

summary(test.prep[,names(test.prep) %in% check.list])
#Ok values not all zeroes


