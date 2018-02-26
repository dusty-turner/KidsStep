library(tidyverse) 
library(caret)
library(survival)
options(scipen=999)
kidsraw = read_csv("CADENCE-Kids data.csv")
  
names(kidsraw)

str(kidsraw)
kids = kidsraw %>%
  filter(complete.cases(.)) %>% ## filter out 2 rows of missing data (only 1 subject) - not worth imputing IMO
  mutate(Stage = as.factor(Stage), Sex = as.factor(Sex), Race = as.factor(Race),Agecat = as.factor(Agecat),Obese_status = as.factor(Obese_status)) %>%
  group_by(id) %>%
  top_n(1, TreadmillSpeed_MPH) %>%
  ungroup() %>%
  select(Sex, Age_years, Race, HeightCMAvg, WeightKGAvg, WaistCMAvg, Cadence_stepsmin, leglengthCM, Tanita.Avg_percentbodyfat, BMI_rawscore, Obese_status) ## removed because of high correlation with BMI and two linearly dependent variables

## Individual missing the data
kidsraw %>%
  filter(!complete.cases(.)) %>%
  select(id, Race, HeightCMAvg, WeightKGAvg, WaistCMAvg, SittingHeightCMAvg)

names(kids)

kidslong = kidsraw %>%
  filter(complete.cases(.)) %>% ## filter out 2 rows of missing data (only 1 subject) - not worth imputing IMO
  mutate(Stage = as.factor(Stage), Sex = as.factor(Sex), Race = as.factor(Race),Agecat = as.factor(Agecat),Obese_status = as.factor(Obese_status)) %>%
  group_by(id) %>%
  top_n(1, TreadmillSpeed_MPH) %>%
  ungroup()


write.csv(kidslong, "KidsRunAllPredictors.csv")

##do we have all the data -- missing 2
sum(complete.cases(kidsraw)==FALSE)
sum(complete.cases(kids)==FALSE)

kidsraw[which(complete.cases(kidsraw)==FALSE),]  

##near zero variance -- good
nzv <- nearZeroVar(kidsraw, saveMetrics= TRUE)

## Correlations?  -- removed two BMI metrics because of this
str(kids)
numkids = kids %>%
  select(-Sex, -Race, -Obese_status)

descrCor <-  cor(numkids)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .95, na.rm = TRUE)

round(descrCor,3)

plot(data.frame(kidsraw$BMI_percentile,kidsraw$BMI_rawscore,kidsraw$BMI_zscore))
cor(data.frame(kidsraw$BMI_percentile,kidsraw$BMI_rawscore,kidsraw$BMI_zscore))

## linear dependencies  ## need to remove METSAdult and leglengthCM 
findLinearCombos(numkids)
numkids$

ggpairs(kids)
ggpairs(numkids)

## split data
set.seed(206)
trainIndex <- createDataPartition(kids$Cadence_stepsmin, p = .75, 
                                  list = FALSE, 
                                  times = 1)

modelbuildingTrain <- kids[ trainIndex,]
modelbuildingTest  <- kids[-trainIndex,]
## survival##################
names(kids)
head(kids)
S = Surv(time = modelbuildingTrain$Cadence_stepsmin, event = rep(1,length(modelbuildingTrain$Cadence_stepsmin)))

modelbuildingTrain = modelbuildingTrain %>%
  select(-Cadence_stepsmin)

tail(modelbuildingTrain)

## Univariate

summary(coxph(S~Sex, data = modelbuildingTrain)) ## bad
summary(coxph(S~Age_years, data = modelbuildingTrain)) ## bad
summary(coxph(S~Race, data = modelbuildingTrain)) ## race white is good
summary(coxph(S~HeightCMAvg, data = modelbuildingTrain)) ## .21
summary(coxph(S~WeightKGAvg, data = modelbuildingTrain)) ## .0891
summary(coxph(S~WaistCMAvg, data = modelbuildingTrain)) ## .0319
summary(coxph(S~leglengthCM, data = modelbuildingTrain)) ## .115
summary(coxph(S~Tanita.Avg_percentbodyfat, data = modelbuildingTrain)) ## .126
summary(coxph(S~BMI_rawscore, data = modelbuildingTrain)) ## .0552
summary(coxph(S~Obese_status, data = modelbuildingTrain)) ## 'over' is good obese and under are bad


names(modelbuildingTrain)

modelsaturated = coxph(S~., data = modelbuildingTrain)
summary(modelsaturated)

##remove leg length
names(modelbuildingTrain)
model1 = coxph(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status, data = modelbuildingTrain)
summary(model1)

##remove 'over' of obese
modelbuildingTrain$Obese_status2 = ifelse(modelbuildingTrain$Obese_status=="Over","Normal",as.character(modelbuildingTrain$Obese_status))
model2 = coxph(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status2, data = modelbuildingTrain)
summary(model2)


##remove 'under' of obese
modelbuildingTrain$Obese_status3 = ifelse(modelbuildingTrain$Obese_status2=="Under","Normal",as.character(modelbuildingTrain$Obese_status2))
model3 = coxph(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status3, data = modelbuildingTrain)
summary(model3)

## remove 'other' from race
modelbuildingTrain$Race2 = ifelse(modelbuildingTrain$Race=="Other", "Black or African American", as.character(modelbuildingTrain$Race))
model4 = coxph(S~Sex+Age_years+Race2+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status2, data = modelbuildingTrain)
summary(model4)

modelbuildingTest$Race2 = ifelse(modelbuildingTest$Race=="Other", "Black or African American", as.character(modelbuildingTest$Race))
modelbuildingTest$Obese_status2 = ifelse(modelbuildingTest$Obese_status=="Over","Normal",as.character(modelbuildingTest$Obese_status))
modelbuildingTest$Obese_status2 = ifelse(modelbuildingTest$Obese_status2=="Under","Normal",as.character(modelbuildingTest$Obese_status2))

predict(model4,modelbuildingTest)

###########survival CV#################



##########Survival -- Weibull###########
library(survival)
options(scipen=1)

weibull1 = survreg(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+leglengthCM+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status, data = modelbuildingTrain, dist = "weibull")
summary(weibull1)

## collapse obese over
modelbuildingTrain$Obese_status2 = ifelse(modelbuildingTrain$Obese_status=="Over","Normal",as.character(modelbuildingTrain$Obese_status))
weibull2 = survreg(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+leglengthCM+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status2, data = modelbuildingTrain, dist = "weibull")
summary(weibull2)

## remove leglengthcm
weibull3 = survreg(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status2, data = modelbuildingTrain, dist = "weibull")
summary(weibull3)

## remove obese under
modelbuildingTrain$Obese_status3 = ifelse(modelbuildingTrain$Obese_status2=="Under","Normal",as.character(modelbuildingTrain$Obese_status2))
weibull4 = survreg(S~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status3, data = modelbuildingTrain, dist = "weibull")
summary(weibull4)

## reomve race
weibull5 = survreg(S~Sex+Age_years+HeightCMAvg+WeightKGAvg+WaistCMAvg+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status3, data = modelbuildingTrain, dist = "weibull")
summary(weibull5)

## reomve bodyfat
weibull6 = survreg(S~Sex+Age_years+HeightCMAvg+WeightKGAvg+WaistCMAvg+BMI_rawscore+Obese_status3, data = modelbuildingTrain, dist = "weibull")
summary(weibull6)

## reomve waiste
weibull7 = survreg(S~Sex+Age_years+HeightCMAvg+WeightKGAvg+BMI_rawscore+Obese_status3, data = modelbuildingTrain, dist = "weibull")
summary(weibull7)

## reomve gender
weibull8 = survreg(S~Age_years+HeightCMAvg+WeightKGAvg+BMI_rawscore+Obese_status3, data = modelbuildingTrain, dist = "weibull")
summary(weibull8)


modelbuildingTest$Obese_status2 = ifelse(modelbuildingTest$Obese_status=="Over","Normal",as.character(modelbuildingTest$Obese_status))
modelbuildingTest$Obese_status3 = ifelse(modelbuildingTest$Obese_status=="Under","Normal",as.character(modelbuildingTest$Obese_status2))
data.frame(predict(weibull8, modelbuildingTest),modelbuildingTest$Cadence_stepsmin)

##########LM TIME from CARET#############

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

names(modelbuildingTrain)

leapslm <- train(Cadence_stepsmin ~ ., data = modelbuildingTrain, 
                 method = "leapSeq", 
                 trControl = fitControl, nmax = 10)
summary(leapslm$finalModel)
leapslm$finalModel

lmmodel = lm(modelbuildingTrain$Cadence_stepsmin~modelbuildingTrain$Age_years+modelbuildingTrain$WaistCMAvg)
summary(lm(modelbuildingTrain$Cadence_stepsmin~modelbuildingTrain$Age_years+modelbuildingTrain$WaistCMAvg))
plot(lmmodel)

library(car)

residualPlots(lmmodel, type = "rstudent")


library(leaps)
leapsnoncaret = regsubsets(Cadence_stepsmin~Sex+Age_years+Race+HeightCMAvg+WeightKGAvg+WaistCMAvg+Cadence_stepsmin+leglengthCM+Tanita.Avg_percentbodyfat+BMI_rawscore+Obese_status, data = kids, nvmax = 10, method = "exhaustive")
leapsnoncaretsum = summary(leapsnoncaret)
leapsnoncaretsum$bic
leapsnoncaretsum$adjr2

##best model (BIC) 5 factors
kids$Racelimited = ifelse(as.character(kids$Race)=="Other","Black or African American",as.character(kids$Race))
fac5linmodel = lm(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat, data = kids)
summary(fac5linmodel)

cv.fac5linmodel = train(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat, method = "lm", data = kids, trControl = trainControl(method = "LOOCV"))
summary(cv.fac5linmodel)
cv.fac5linmodel

coef(fac5linmodel)[2:6]*c(1,mean(kids$Age_years),mean(kids$WaistCMAvg),1,mean(kids$Tanita.Avg_percentbodyfat))


## second best model 6 factors (BIC)
kids$obeselimited = ifelse(kids$Obese_status=="Obese","Obese","Normal")
fac6linmodel = lm(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat+obeselimited, data = kids)
summary(fac6linmodel)

cv.fac6linmodel = train(Cadence_stepsmin~Sex+Age_years+WaistCMAvg+Racelimited+Tanita.Avg_percentbodyfat+obeselimited, method = "lm", data = kids, trControl = trainControl(method = "LOOCV"))
summary(cv.fac6linmodel)
cv.fac6linmodel

## 3 factors for simplicity (BIC)
fac3linmodel = lm(Cadence_stepsmin~Age_years+WaistCMAvg+Racelimited, data = kids)
summary(fac3linmodel)

cv.fac3linmodel = train(Cadence_stepsmin~Age_years+WaistCMAvg+Racelimited, method = "lm", data = kids, trControl = trainControl(method = "LOOCV"))
summary(cv.fac3linmodel)
cv.fac3linmodel


############PCA##########  -- I don't know how to remove a principal component that isn't significant
library(FactoMineR)

row.names(modelbuildingTrain)

pr.out = prcomp(modelbuildingTrain)
AFDM(modelbuildingTrain)

library(caret)

pproc = preProcess(modelbuildingTrain, method = c("pca"))
names(pproc)

str(pproc)

pproc$rotation

test = train(Cadence_stepsmin~., data = modelbuildingTrain,
             method = "lm",
             preProcess=c("pca"),
             trControl = fitControl)

test = train(Cadence_stepsmin~., data = modelbuildingTrain,
             method = "lm",
             preProcess=c("pca"),
             trControl = fitControl)

str(test$preProcess)

test$preProcess$rotation



summary(test$finalModel)

test$finalModel

testpred = predict(test, modelbuildingTest)

summary(test$finalModel)

str(test)

# ###########Lasso############3
# 
# set.seed(206)
# names(modelbuildingTrain)
# glmnet1 <- train(Cadence_stepsmin ~ ., data = modelbuildingTrain, 
#                  method = "glmnet", 
#                  # preProc = preProcess(modelbuildingTrain, method = c("knnImpute"), k = 3),
#                  trControl = fitControl
#                  ## This last option is actually one
#                  ## for gbm() that passes through
# )
# summary(glmnet1)
# glmnet1$bestTune
# str(glmnet1$finalModel)
# glmnet1$finalModel$beta
# glmnet1$finalModel
#   coef(glmnet1$finalModel)
# varImp(glmnet1)
# coefplot::coefplot(glmnet1$finalModel)
# 
# 
# coef(glmnet1$finalModel, glmnet1$bestTune$.lambda)
# # ma206predictedglm = predict(glmnet1, testTransformed)
# # postResample(pred = ma206predictedglm, obs = testTransformed$ma206)
# modelbuildingTrainpred = modelbuildingTrain[,7]
# modelbuildingTrain1 = modelbuildingTrain[,-7]
# glmnet(x=model.matrix(modelbuildingTrain1), y=modelbuildingTrainpred)
# 
# modelbuildingTrain1 = as.data.frame(modelbuildingTrain1)
# 
# glmnet.lasso = glmnet(x=model.matrix(modelbuildingTrain1),
#                     y = model.matrix(modelbuildingTrain[,7]),
#                     alpha = 1)
# 
# 
# cvfit.lasso = cv.glmnet(x=data.matrix(modelbuildingTrain1), 
#                         y = modelbuildingTrain[,7],
#                         family = "gaussian",
#                         alpha = 1)
# ##






#### try this without  obese from start  probably should rerun this without cadence...?


###randomforrest survival
## this website for blog
## https://pedroconcejero.wordpress.com/2015/11/12/survival-random-forests-for-churn-prediction-3/
install.packages("randomForestSRC")
library(randomForestSRC)
length(modelbuildingTrain$id)
modelbuildingTrain$stop = rep(1,91)

train = data.frame(sex = modelbuildingTrain$Sex, age = modelbuildingTrain$Age_years, stop = modelbuildingTrain$stop, TreadmillSpeed_MPH = modelbuildingTrain$TreadmillSpeed_MPH)

survtree = rfsrc(Surv(TreadmillSpeed_MPH, stop)~., data = train)
summary(survtree)

survtree$predicted.oob

imp.rsf.1 <- sort(survtree$importance, 
                  decreasing = T)
imp.rsf.1

plot(gg_vimp(out.rsf.1))