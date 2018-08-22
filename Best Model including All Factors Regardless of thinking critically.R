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

kidsfinal = kidsbound[,-c(7,8,16,19:22)]

names(kidsfinal)
library(leaps)
model = regsubsets(Run_Cadence~., data = kidsfinal, method = "exhaustive")
summodel = summary(model)
summodel$bic

str(summodel)
summodel$outmat

# bestmod = lm(Run_Cadence~Tanita.Avg+BMIz+Run_VO2mlkgmin+Run_METSYouth3,data = kidsfinal)
bestmod = lm(Run_Cadence~Age+HeightCMAvg+WeightKGAvg+BMIz,data = kidsfinal)
summary(bestmod)

library(caret)
preds = predict.lm(bestmod,newdata = kidsfinal)
postResample(preds, kidsfinal$Run_Cadence)


kidsfinal$Run_METSYouth1
cor(kidsfinal$Tanita.Avg,kidsfinal$BMIz)

library(car)
residualPlots(bestmod)
qqPlot(bestmod, distribution = "norm")

