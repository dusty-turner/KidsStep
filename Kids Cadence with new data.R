library(tidyverse)  
library(GGally)
library(caret)
options(scipen=999)
kidsraw = read_csv("Predict_Running_Transition_Final.csv")

kids = kidsraw %>%
  filter(Transitioned_FullStage==1) %>%
  mutate_if(is.character,as.factor)

kids %>%
  select(Run_Cadence,Transitioned_FullStage)

hist(kids$BMIz)
(kids$BMIcont-mean(kids$BMIcont))/sd(kids$BMIcont)
lm(kids$BMIz~
kids$BMIcont)
plot(kids$BMIz~
kids$BMIcont)

names(kids)

## selecting only things we can easily measure before 
kidslogical = kids%>%select(Run_Cadence,Sex, Age,Race,HeightCMAvg,WeightKGAvg,WaistAvg,BMIcont,BMIperc,BMIz, Obesecat)

head(kidslogical)

kidslogicalnofac = kidslogical %>% select(-Sex,-Race,-Obesecat)

comboInfo = findLinearCombos(kidslogicalnofac)

# kidsnofac = kidsnofac[, -comboInfo$remove]
# dim(kidsnofac)

####

descrCor = cor(kidslogicalnofac)>.9
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9)
summary(descrCor[upper.tri(descrCor)])
# 
names(kidslogicalnofac)
kidslogicalnofac$BMIcont
# 
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
filteredDescr <- kidslogicalnofac[,-highlyCorDescr]
dim(filteredDescr)
# 

names(filteredDescr)

kidsfac = kids%>%select(Sex,Race,Obesecat)

kidsfinal = cbind(filteredDescr,kidsfac)

library(leaps)

names(kidsfinal)

head(kidsfinal)

dim(kidsfinal)
## use run cadence as the dependent variable -- took walk cadence out

model = regsubsets(Run_Cadence~., data = kidsfinal)
summodel = summary(model)
summodel$bic

summodel$outmat

bestmod4 = lm(Run_Cadence~Age:Sex+HeightCMAvg+WeightKGAvg+BMIz:Sex,data = kidsfinal)
bestmod4 = lm(Run_Cadence~HeightCMAvg+WeightKGAvg+BMIz+Age,data = kidsfinal)
# bestmod4a = lm(Run_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data = kidsfinal)
summary(bestmod4)
summary(bestmod4a)
# bestmod4 = lm(Run_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz+Obesecat,data = kidsfinal)
# summary(bestmod4)

AIC(bestmod4)

plot(kidsfinal$BMIcont,kidsfinal$BMIz)

predict.lm(bestmod4, newdata = kidsfinal, interval = "prediction")

kidsfinal %>%
  ggplot(aes(x=HeightCMAvg,y=Run_Cadence, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm")
kidsfinal %>%
  ggplot(aes(x=WeightKGAvg,y=Run_Cadence, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm")
kidsfinal %>%
  ggplot(aes(x=Age,y=Run_Cadence, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm")
kidsfinal %>%
  ggplot(aes(x=BMIz,y=Run_Cadence, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm")


summary(kidsfinal)

library(car)
residualPlots(bestmod4, type = "rstudent")
qqPlot(bestmod4, distribution = "norm")
