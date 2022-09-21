
#STEP 7: prep attributes for modelling


setwd(output_dir)

#read in imputed data

#training data:
P.Predictors<-read.delim("./P_Predictors_imputed.csv",sep=",", header=TRUE)
head(P.Predictors)
nrow(P.Predictors)


#independent test data:
test.prep<-read.delim("./P_TEST_imputed.csv", sep=",", header=TRUE)
head(test.prep)
nrow(test.prep)

#rename P column to match for train and test
names(P.Predictors)
names(test.prep)
colnames(P.Predictors)[1]<- "P_mgkg"
colnames(test.prep)[1] <-"P_mgkg"

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
  taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind)
)

#check class type of all columns
lapply(P.Predictors, class)

test.prep <- transform(
  test.prep,
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
test.prep <- transform(
  test.prep,
  StreamType=as.factor(StreamType),
  WetlandType=as.factor(WetlandType),
  NLCD06Cat=as.factor(NLCD06Cat),
  hydricrating=as.factor(hydricrating),
  drainagecl=as.factor(drainagecl),
  taxpartsize=as.factor(taxpartsize),
  taxceactcl=as.factor(taxceactcl),
  taxreaction=as.factor(taxreaction),
  texcl=as.factor(texcl),
  pmkind=as.factor(pmkind)
)


####

#EDIT 7.13.22
#Exclude attributes with factor levels that do not match factor levels in UMRB grid dataset:
Exclude.grid<-c('taxpartsize', 'texcl', 'pmkind', 'taxreaction')
P.Predictors2<-P.Predictors[,!(names(P.Predictors) %in% Exclude.grid)]
test.prep2<-test.prep[,!(names(test.prep) %in% Exclude.grid)]

#Exclude samples with soil P >1000 to see how model performance is affected
P.Predictors2<-P.Predictors2[P.Predictors2$P_mgkg<1000,]
test.prep2<-test.prep2[test.prep2$P_mgkg<1000,]
