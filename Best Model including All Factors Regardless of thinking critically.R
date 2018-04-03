library(tidyverse)  
library(GGally)
library(caret)
options(scipen=999)
kidsraw = read_csv("Predict_Running_Transition_Final.csv")

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

####

descrCor = cor(kidsnofac)>.9
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .9)
summary(descrCor[upper.tri(descrCor)])
# 
names(kidsnofac)
# 
highlyCorDescr <- findCorrelation(descrCor, cutoff = .9)
filteredDescr <- kidsnofac[,-highlyCorDescr]
dim(filteredDescr)
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

kidsfinal = kidsbound[,-c(7,8,16,19:22)]

names(kidsfinal)

model = regsubsets(Run_Cadence~., data = kidsfinal)
summodel = summary(model)
summodel$bic

str(summodel)
summodel$outmat

bestmod = lm(Run_Cadence~Tanita.Avg+BMIz+Run_VO2mlkgmin+Run_METSYouth3,data = kidsfinal)
summary(bestmod)


kidsfinal$Run_METSYouth1
cor(kidsfinal$Tanita.Avg,kidsfinal$BMIz)

library(car)
residualPlots(bestmod)
qqPlot(bestmod, distribution = "norm")
