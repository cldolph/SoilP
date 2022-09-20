# STEP 8: COMPARE RANGER AND RANDOM FOREST MODELS #

#Define recipe to use in both model types 
short_recipe<-
  recipe(P_mgkg ~ ., data = P_train_preprocessed) 

#specify model (equivalent was tune_spec)
short_model_RANGER<-
  rand_forest(mtry = 91, min_n=3, trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger") 

short_model_RF<-
  rand_forest(
    mtry = 91, 
    min_n=3, 
    trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("randomForest")

#put into workflow
short_wf_ranger<-workflow()%>%
  add_recipe(short_recipe) %>%
  add_model(short_model_RANGER)

short_wf_RF<-workflow()%>%
  add_recipe(short_recipe) %>%
  add_model(short_model_RF)


#run models
ranger_fit<-short_wf_ranger %>%
  fit(P_train_preprocessed)
ranger_fit
summary(ranger_fit)

RF_fit<-short_wf_RF %>%
  fit(P_train_preprocessed)
RF_fit
summary(RF_fit)


#Check performance
ranger_pred <-predict(ranger_fit, new_data=P_test_preprocessed)
randfor_pred<-predict(RF_fit, new_data=P_test_preprocessed) 
#add predictions to test data:
ranger.pred<-as.data.frame(cbind(P_test_preprocessed, ranger_pred))
head(ranger.pred)
RF.pred<-as.data.frame(cbind(P_test_preprocessed, randfor_pred))

#check R2 value for actual vs predicted
reg2<- with(ranger.pred,lm(P_mgkg~.pred))
summary(reg2)
reg3<- with(RF.pred,lm(P_mgkg~.pred))
summary(reg3)

#the test results match very closely
