library(tidyverse)  
library(GGally)
library(caret)
library(forcats)
options(scipen=999)
kidsraw = read_csv("Predict_Running_Transition_Final.csv")



#### Logistic Regression


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


filteredDescr <- kidsnofac[,-c(4,5,8,10,14,15,20)]

dim(filteredDescr)

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

kidsbound = cbind(filteredDescr,kidsfac) %>% as_tibble()

## use run cadence as the dependent variable -- took walk cadence out

kidsfinal = kidsbound[,-c(7,11,16,17,19:22)] ## for walk_cadence
kidsfinal = kidsbound[,-c(7,8,16,17,19:22)] ## for run_cadence

#### Logistic Regression Interpretable

logdata= kidsbound %>%
  # select(Age,HeightCMAvg,WeightKGAvg,Walk_Cadence,Run_Cadence,Sex,BMIz) %>%
  gather(WalkOrRun,Cadence,c(Walk_Cadence,Run_Cadence)) %>%
  mutate(WalkOrRun=case_when(
    WalkOrRun=="Run_Cadence" ~ 1,
    WalkOrRun=="Walk_Cadence" ~ 0)
  ) %>%
  mutate(WalkOrRun=as.factor(WalkOrRun)) %>%
  select(-c(FirstRun,Agecat,Walk_Stage,Run_Stage,Transitioned,Transitioned_FullStage))

summary(logdata)

## Full Model - doesn't work
logmodel=glm(WalkOrRun~., data=logdata,family=binomial(link="logit"))
summary(logmodel)


## origional full model was fully determined
logdatasub = logdata %>% 
  select(-c(starts_with("Walk_"),starts_with("Run_")))

## Full Model
logmodel=glm(WalkOrRun~., data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

## remove race
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIcont+BMIz+Sex+Obesecat+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

## remove obese cat
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIcont+BMIz+Sex+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

## remove bmicont
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIz+Sex+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

## remove sex
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIz+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

## remove tanita
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+BMIz+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)


##### Second look
## remove bmicont
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIz+Sex+Obesecat+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)
## remove obesecat
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIz+Sex+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)
## remove sex
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+Tanita.Avg+BMIz+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)
## remove tanita
logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+BMIz+Cadence, data=logdatasub,family=binomial(link="logit"))
summary(logmodel)

logmodel=glm(WalkOrRun~Age+HeightCMAvg+WeightKGAvg+BMIz+Cadence, data=logdata,family=binomial(link="logit"))
summary(logmodel)


testingdata = 
  logdata %>%
  summarise(Age = mean(Age), HeightCMAvg = mean(HeightCMAvg), WeightKGAvg = mean(WeightKGAvg),BMIz = mean(BMIz))

pymin=min(logdata$Cadence)-20
pymax=max(logdata$Cadence)+20

nextdata = 
  expand.grid(Age = testingdata$Age, 
              HeightCMAvg = testingdata$HeightCMAvg,
              WeightKGAvg = testingdata$WeightKGAvg, 
              BMIz = testingdata$BMIz,
              Cadence = seq(min(logdata$Cadence),max(logdata$Cadence),1)) %>% 
  as_tibble() 

nextdata = nextdata %>%
  mutate(predictions = predict.glm(logmodel, newdata = nextdata , type = "response")) 

cut =
nextdata %>%
  mutate(close = abs(.5-predictions)) %>% 
  arrange(close) %>%
  filter(row_number()==1)
cut = cut$Cadence

cuthigh=
  nextdata %>%
  mutate(close = abs(.9-predictions)) %>% 
  arrange(close) %>%
  filter(row_number()==1)
cuthigh = cuthigh$Cadence

cutlow=
  nextdata %>%
  mutate(close = abs(.1-predictions)) %>% 
  arrange(close) %>%
  filter(row_number()==1)
cutlow = cutlow$Cadence


nextdata %>%
  ggplot(aes(x=Cadence,y=predictions)) +
  geom_line() +
  # geom_hline(yintercept = c(0,1)) +
  geom_ribbon(aes(ymin = 0, ymax = predictions), fill = "orange", alpha = .5) +
  geom_ribbon(aes(ymin = predictions, ymax = 1), fill = "blue", alpha = .5) +
  geom_segment(aes(x= cut, y = 0, xend = cut, yend = .5), color = "red", size = 2) +
  geom_segment(aes(x= cut, y = .5, xend = min(nextdata$Cadence ), yend = .5), color = "red", size = 1) +
  geom_rect(aes(xmin=cutlow, xmax=cuthigh, ymin=0, ymax=1),alpha=.01,fill="red") +
  geom_label(aes(x=min(nextdata$Cadence+10),y=.9, label = "Walking")) +
  geom_label(aes(x=max(nextdata$Cadence-10),y=.9, label = "Running")) +
  geom_label(aes(x=cut,y=0, label = cut)) +
  geom_label(aes(x=cut,y=.9, label = "Transition Band")) +
  labs(x = "Cadence", y = "Probability", title = "Transition Probability as Cadence Increases") 



#######################################


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
