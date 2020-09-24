library(tidyverse)  
library(GGally)
library(caret)
library(forcats)
options(scipen=999)
kidsraw = read_csv("Predict_Running_Transition_Final.csv")



#### Linear discriminate analysis


kids = kidsraw %>%
  filter(Transitioned_FullStage==1) %>%
  mutate_if(is.character,as.factor)

names(kids)

# ggpairs(kids[,-c(1)])

kidsnofac = kids%>%select(-id,-Sex,-Agecat,-Race,-Obesecat,-Walk_Stage,-Run_Stage,-Transitioned, -Transitioned_FullStage)
# 
# 
# descrCor = cor(kidsnofac)>.9
# highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9)
# summary(descrCor[upper.tri(descrCor)])
# 
# names(kidsnofac)
# 
# highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
# filteredDescr <- kidsnofac[,-highlyCorDescr]
# 
# str(filteredDescr)
# 
# descrCor2 <- cor(filteredDescr)
# summary(descrCor2[upper.tri(descrCor2)])
# 
# kidsfinal = filteredDescr %>% bind_cols(kids%>%select(id,Sex,Agecat,Race,Obesecat,Walk_Stage,Run_Stage,Transitioned, Transitioned_FullStage))
# ## HeightCMAvg -- SittingHeightAvg, Leglength
# ## WeightKGAvg -- WaistAvg
# setdiff(names(kids),names(kidsfinal))

dim(kids)
names(kidsnofac)
comboInfo = findLinearCombos(kidsnofac)

kidsnofac = kidsnofac[, -comboInfo$remove]
dim(kidsnofac)

## $linearCombos[[1]]
# [1] 6 2 5
#  Leglength, Height, Sitting height
# $linearCombos[[2]]
# [1] 13 12
#  first run and last full stage
# $linearCombos[[3]]
# [1] 14 11
# walk speed and last walk
# $linearCombos[[4]]
# [1] 17 16
#  , Walk_METSAdult, Walk_VO2mlkgmin 
# $linearCombos[[5]]
# [1] 21 12
# run speed, first run
# $linearCombos[[6]]
# [1] 24 23
# Run_METSAdult, Run_VO2mlkgmin <dbl>
# 
# $remove
# [1]  6 13 14 17 21 24
# leg length, last full stage, walk speed, Walk_METSAdult, Run_Speed, Run_METSAdult
####

descrCor = cor(kidsnofac)>.9
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9)
summary(descrCor[upper.tri(descrCor)])
# 
names(kidsnofac)
# 
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
# filteredDescr <- kidsnofac[,-highlyCorDescr]
dim(filteredDescr)

filteredDescr <- kidsnofac[,-c(4,5,8,10,14,15,20)]

# [1]  4  2  8 10 14 15 20
# waistavg, height, bmiperc, lastwalk, walk_mets_youth2, run_metsyouth2

# 
# str(filteredDescr)
# 
# descrCor2 <- cor(filteredDescr)
# summary(descrCor2[upper.tri(descrCor2)])
# 
# kidsfinal = filteredDescr %>% bind_cols(kids%>%select(id,Sex,Agecat,Race,Obesecat,Walk_Stage,Run_Stage,Transitioned, Transitioned_FullStage))
# ## HeightCMAvg -- SittingHeightAvg, Leglength
# ## WeightKGAvg -- WaistAvg
# setdiff(names(kids),names(kidsfinal))

names(filteredDescr)

kidsfac = kids%>%select(Sex,Agecat,Race,Obesecat,Walk_Stage,Run_Stage,Transitioned, Transitioned_FullStage)

kidsbound = cbind(filteredDescr,kidsfac)

library(leaps)

names(kidsbound)

head(kidsbound)

dim(kidsfinal)
## use run cadence as the dependent variable -- took walk cadence out

kidsfinal = kidsbound[,-c(7,11,16,17,19:22)] ## for walk_cadence
kidsfinal = kidsbound[,-c(7,8,16,17,19:22)] ## for run_cadence

names(kidsfinal)
library(leaps)
model = regsubsets(Walk_Cadence~., data = kidsfinal, method = "exhaustive")
model = regsubsets(Run_Cadence~., data = kidsfinal, method = "exhaustive")
summodel = summary(model)
summodel$bic

str(summodel)
summodel$outmat

# bestmod = lm(Run_Cadence~Tanita.Avg+BMIz+Run_VO2mlkgmin+Run_METSYouth3,data = kidsfinal)
bestmodrun = lm(Run_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data = kidsfinal)
bestmodinterp = lm(Walk_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data = kidsfinal)
bestmodnoninterp = lm(Walk_Cadence~Tanita.Avg+BMIz+Walk_VO2mlkgmin+Run_METSYouth3,data = kidsfinal)
summary(bestmodinterp)
summary(bestmodnoninterp)
BIC(bestmodinterp)
BIC(bestmodnoninterp)

kidsfinal %>%
  ggplot(aes(x=Tanita.Avg,y=BMIz)) +
  geom_point()

###Example Prediction of walk interp
age=10
height=161.4
weight=40
gender="M"
BMI=weight/(height/100)^2
predict.lm(bestmodinterp,data.frame(Age=age, HeightCMAvg=height, WeightKGAvg=weight,BMIz=y2z(BMI,age,gender,ref=cdc.bmi)),interval="prediction")

### walk cadence interpretable
set.seed(2007) ## set seed to make this reproducible
nfolds = 10 ## number of folds
cv.error.10 = rep(0,10) #create a vector to store our error
#separate the folds
library(pracma)
fold.assignment = randperm(rep(1:nfolds,length.out=dim(kidsfinal)[1]),dim(kidsfinal)[1])
model.data = kidsfinal %>%
  select(Walk_Cadence,Age,HeightCMAvg,WeightKGAvg,BMIz) %>%
  mutate(fold.assignment = fold.assignment)
SSEFold = NA
# i=1
for(i in 1:nfolds){
  training.data = model.data[fold.assignment!=i,]#separate out fold i
  test.data = model.data[fold.assignment==i,]#everyone but fold i
  fit = lm(Walk_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data=training.data)#fit model to the training data
  predicted.walkcadence = predict.lm(fit,test.data) #predict HBatting in the test set
  SSEFold[i] = sum(predicted.walkcadence-test.data$Walk_Cadence)^2 # how badly did we miss our estimate squared (SSE)
}
interpmodelsse = mean(SSEFold) ## average SSE on the data

### walk cadence non-interpretable
set.seed(2007) ## set seed to make this reproducible
nfolds = 10 ## number of folds
cv.error.10 = rep(0,10) #create a vector to store our error
#separate the folds
library(pracma)
fold.assignment = randperm(rep(1:nfolds,length.out=dim(kidsfinal)[1]),dim(kidsfinal)[1])
model.data = kidsfinal %>%
  select(Walk_Cadence,Tanita.Avg,BMIz,Walk_VO2mlkgmin,Run_METSYouth3) %>%
  mutate(fold.assignment = fold.assignment)
SSEFold = NA
# i=1
for(i in 1:nfolds){
  training.data = model.data[fold.assignment!=i,]#separate out fold i
  test.data = model.data[fold.assignment==i,]#everyone but fold i
  fit = lm(Walk_Cadence~Tanita.Avg+BMIz+Walk_VO2mlkgmin+Run_METSYouth3,data=training.data)#fit model to the training data
  predicted.walkcadence = predict.lm(fit,test.data) #predict HBatting in the test set
  SSEFold[i] = sum(predicted.walkcadence-test.data$Walk_Cadence)^2 # how badly did we miss our estimate squared (SSE)
}
noninterpmodelsse = mean(SSEFold) ## average SSE on the data



interpmodelsse
noninterpmodelsse

##################################################
wt = 60.1
ht = 160
age = 10
library(AGD)
BMItemp=wt/(ht/100)^2
BMIzcalc=y2z(BMItemp,age,sex="M",ref=cdc.bmi)

newdata = data.frame(Age=age,HeightCMAvg=ht,WeightKGAvg=wt,BMIz=BMIzcalc)

predict.lm(bestmodinterp, newdata = newdata, interval = "prediction")[2]
predict.lm(bestmodrun, newdata = newdata, interval = "prediction")


head(kidsfinal)

bestmodrun

#### Logistic Regression Interpretable

logdata=kidsbound %>%
  select(Age,HeightCMAvg,WeightKGAvg,Walk_Cadence,Run_Cadence,Sex,BMIz) %>%
  gather(Walk_or_Run,Cadence,c(Walk_Cadence,Run_Cadence)) %>%
  mutate(walk_or_run=case_when(
    Walk_or_Run=="Run_Cadence" ~ 1,
    Walk_or_Run=="Walk_Cadence" ~ 0)
  ) %>%
  select(-Walk_or_Run) 
logdata$walk_or_run=as.factor(logdata$walk_or_run)
logmodel=glm(walk_or_run~., data=logdata,family=binomial(link="logit"))
summary(logmodel)

testingdata = 
logdata %>%
  summarise(Age = mean(Age), HeightCMAvg = mean(HeightCMAvg), WeightKGAvg = mean(WeightKGAvg),BMIz = mean(BMIz))

nextdata = 
expand.grid(Age = testingdata$Age, 
            HeightCMAvg = testingdata$HeightCMAvg,
            WeightKGAvg = testingdata$WeightKGAvg, 
            BMIz = testingdata$BMIz, 
            Sex = c("M","F"),
            Cadence = seq(min(logdata$Cadence),max(logdata$Cadence),1)) 
nextdata %>% 
  as_tibble() %>%
  mutate(predictions = predict.glm(logmodel, newdata = nextdata , type = "response")) %>%
  group_by(Sex) %>%
  filter(predictions>.2&predictions<.8)

####################################################
library(caret)
preds = predict.lm(bestmod,newdata = kidsfinal)
postResample(preds, kidsfinal$Run_Cadence)


kidsfinal$Run_METSYouth1
cor(kidsfinal$Tanita.Avg,kidsfinal$BMIz)

library(car)
residualPlots(bestmod)
qqPlot(bestmod, distribution = "norm")


kidsfinal$predictions = predict.lm(bestmod,data = kidsfinal)

####  Visualize Run Cadence based on Age Group and Height Tertile
dodge=position_dodge(width=.5)
kidsfinal %>% mutate(AgeGroup=cut(Age,breaks=c(5,9,12,16,21),labels=c("6-9","10-12","13-16","17-20"))) %>%
  group_by(AgeGroup) %>%  mutate(HeightTertile=ntile(HeightCMAvg,3) )  %>% 
  mutate(HeightTertile=case_when(HeightTertile==1 ~ "Low",HeightTertile==2 ~ "Medium",HeightTertile==3 ~ "High")) %>% 
  mutate(HeightTertile=fct_relevel(HeightTertile,"Low","Medium","High")) %>%
  select (AgeGroup, HeightTertile, Run_Cadence, predictions) %>%
  group_by(AgeGroup, HeightTertile) %>%
  # summarise(RunCadence=mean(Run_Cadence), upperbound=mean(Run_Cadence)+sd(Run_Cadence),lowerbound=mean(Run_Cadence)-sd(Run_Cadence)) %>%
  summarise(PredRunCadence=mean(predictions), upperbound=mean(predictions)+sd(predictions),lowerbound=mean(predictions)-sd(predictions)) %>%
  # ggplot(aes(x=AgeGroup,y=RunCadence, color=HeightTertile),position = dodge) + geom_point(position = dodge) + geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge) 

  ggplot(aes(x=AgeGroup,y=PredRunCadence, color=HeightTertile),position = dodge) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge, size = 2.5) +
  geom_point(position = dodge, size =4) +
  ylab("Predicted Transition Cadence") + xlab("Age") + 
  scale_color_discrete("Height Tertile") +
  ggtitle("Predicted Transition Cadence by Age over Height")


dodge=position_dodge(width=.5)
kidsfinal %>% mutate(AgeGroup=cut(Age,breaks=c(5,9,12,16,21),labels=c("6-9","10-12","13-16","17-20"))) %>%
  group_by(AgeGroup) %>% mutate(WeightTertile=ntile(WeightKGAvg,3) )  %>% 
  mutate(WeightTertile=case_when(WeightTertile==1 ~ "Low",WeightTertile==2 ~ "Medium",WeightTertile==3 ~ "High")) %>%
  mutate(WeightTertile=fct_relevel(WeightTertile,"Low","Medium","High")) %>%
  select (AgeGroup, WeightTertile, Run_Cadence, predictions) %>%
  group_by(AgeGroup, WeightTertile) %>% 
  # summarise(RunCadence=mean(Run_Cadence), upperbound=mean(Run_Cadence)+sd(Run_Cadence),lowerbound=mean(Run_Cadence)-sd(Run_Cadence)) %>%
  summarise(PredRunCadence=mean(predictions), upperbound=mean(predictions)+sd(predictions),lowerbound=mean(predictions)-sd(predictions)) %>%
  # ggplot(aes(x=AgeGroup,y=RunCadence, color=HeightTertile),position = dodge) + geom_point(position = dodge) + geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge) 
  ggplot(aes(x=AgeGroup,y=PredRunCadence, color=WeightTertile),position = dodge) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge, size = 2.5) +
  geom_point(position = dodge, size = 4) +
  ylab("Predicted Transition Cadence") + xlab("Age") + 
  scale_color_discrete("Weight Tertile") +
  ggtitle("Predicted Transition Cadence by Age over Weight")

dodge=position_dodge(width=.5)
kidsfinal %>% mutate(AgeGroup=cut(Age,breaks=c(5,9,12,16,21),labels=c("6-9","10-12","13-16","17-20"))) %>%
  group_by(AgeGroup) %>% mutate(BMITertile=ntile(BMIz,3) )  %>% 
  mutate(BMITertile=case_when(BMITertile==1 ~ "Low",BMITertile==2 ~ "Medium",BMITertile==3 ~ "High")) %>%
  mutate(BMITertile=fct_relevel(BMITertile,"Low","Medium","High")) %>%
  select (AgeGroup, BMITertile, Run_Cadence, predictions) %>%
  group_by(AgeGroup, BMITertile) %>% 
  # summarise(RunCadence=mean(Run_Cadence), upperbound=mean(Run_Cadence)+sd(Run_Cadence),lowerbound=mean(Run_Cadence)-sd(Run_Cadence)) %>%
  summarise(PredRunCadence=mean(predictions), upperbound=mean(predictions)+sd(predictions),lowerbound=mean(predictions)-sd(predictions)) %>%
  # ggplot(aes(x=AgeGroup,y=RunCadence, color=HeightTertile),position = dodge) + geom_point(position = dodge) + geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge) 
  ggplot(aes(x=AgeGroup,y=PredRunCadence, color=BMITertile),position = dodge) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge, size = 2.5) +
  geom_point(position = dodge, size = 4) +
  ylab("Predicted Transition Cadence") + xlab("Age") + 
  scale_color_discrete("BMIz Tertile") +
  ggtitle("Predicted Transition Cadence by Age over BMIz")

dodge=position_dodge(width=.5)
kidsfinal %>% mutate(HeightTertile=ntile(HeightCMAvg,3) )  %>% 
  mutate(HeightTertile=case_when(HeightTertile==1 ~ "Low",HeightTertile==2 ~ "Medium",HeightTertile==3 ~ "High")) %>% 
  mutate(HeightTertile=fct_relevel(HeightTertile,"Low","Medium","High")) %>%
  group_by(HeightTertile) %>% mutate(BMITertile=ntile(BMIz,3) )  %>% 
  mutate(BMITertile=case_when(BMITertile==1 ~ "Low",BMITertile==2 ~ "Medium",BMITertile==3 ~ "High")) %>%
  mutate(BMITertile=fct_relevel(BMITertile,"Low","Medium","High")) %>%
  select (HeightTertile, BMITertile, Run_Cadence, predictions) %>%
  group_by(HeightTertile, BMITertile) %>% 
  # summarise(RunCadence=mean(Run_Cadence), upperbound=mean(Run_Cadence)+sd(Run_Cadence),lowerbound=mean(Run_Cadence)-sd(Run_Cadence)) %>%
  summarise(PredRunCadence=mean(predictions), upperbound=mean(predictions)+sd(predictions),lowerbound=mean(predictions)-sd(predictions)) %>%
  # ggplot(aes(x=AgeGroup,y=RunCadence, color=HeightTertile),position = dodge) + geom_point(position = dodge) + geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge) 
  ggplot(aes(x=HeightTertile,y=PredRunCadence, color=BMITertile),position = dodge) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge, size = 2.5) +
  geom_point(position = dodge, size = 4) +
  ylab("Predicted Transition Cadence") + xlab("Height Tertile") + 
  scale_color_discrete("BMIz Tertile") +
  ggtitle("Predicted Transition Cadence by Height over BMIz")

dodge=position_dodge(width=.5)
kidsfinal %>% mutate(WeightTertile=ntile(WeightKGAvg,3) )  %>% 
  mutate(WeightTertile=case_when(WeightTertile==1 ~ "Low",WeightTertile==2 ~ "Medium",WeightTertile==3 ~ "High")) %>% 
  mutate(WeightTertile=fct_relevel(WeightTertile,"Low","Medium","High")) %>%
  group_by(WeightTertile) %>% mutate(BMITertile=ntile(BMIz,3) )  %>% 
  mutate(BMITertile=case_when(BMITertile==1 ~ "Low",BMITertile==2 ~ "Medium",BMITertile==3 ~ "High")) %>%
  mutate(BMITertile=fct_relevel(BMITertile,"Low","Medium","High")) %>%
  select (WeightTertile, BMITertile, Run_Cadence, predictions) %>%
  group_by(WeightTertile, BMITertile) %>% 
  summarise(RunCadence=mean(Run_Cadence), upperbound=mean(Run_Cadence)+sd(Run_Cadence),lowerbound=mean(Run_Cadence)-sd(Run_Cadence)) %>%
  #summarise(PredRunCadence=mean(predictions), upperbound=mean(predictions)+sd(predictions),lowerbound=mean(predictions)-sd(predictions)) %>%
  ggplot(aes(x=WeightTertile,y=RunCadence, color=BMITertile),position = dodge) + 
  #ggplot(aes(x=WeightTertile,y=PredRunCadence, color=BMITertile),position = dodge) + 
  geom_errorbar(aes(ymin=lowerbound,ymax=upperbound),position = dodge, size = 2.5) +
  geom_point(position = dodge, size = 4) +
  ylab("Predicted Transition Cadence") + xlab("Weight Tertile") + 
  scale_color_discrete("BMIz Tertile") +
  ggtitle("Predicted Transition Cadence by Weight over BMIz")
