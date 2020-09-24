library(stats)
library(mclust)
library(dbscan)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rgl)
library(dbscan)

setwd("C:/Users/James.Pleuss/Documents/Research/Kids Cadence")
kids=read.csv("KidsRunStatic.csv")
kids2=read.csv("KidsRunAllPredictors.csv")
CorMat=kids %>%
  select(Age_years,HeightCMAvg,WeightKGAvg,WaistCMAvg,Cadence_stepsmin,leglengthCM,Tanita.Avg_percentbodyfat,BMI_rawscore) %>%
  cor()
CorMat1=kids %>%
  select(Age_years,WeightKGAvg,WaistCMAvg,Cadence_stepsmin,leglengthCM,Tanita.Avg_percentbodyfat,BMI_rawscore) %>%
  cor()
CorMat1 
# Let's look at getting rid of height, waist, and BMI


# interesting relationship between Tanita and Weight
kids %>% 
  select(Age_years,leglengthCM,WeightKGAvg,Tanita.Avg_percentbodyfat, BMI_rawscore, HeightCMAvg) %>%
  ggplot(aes(x=WeightKGAvg,y=Tanita.Avg_percentbodyfat), color="black") + 
  #coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point()

kids %>% 
  select(Sex,Tanita.Avg_percentbodyfat,WeightKGAvg) %>%
  plot3d(size =5)

#Tanita vs. Weight with Sex and Age. Males are the alternate cluster which is not surprising
kids %>% 
  ggplot(aes(x=WeightKGAvg,y=Tanita.Avg_percentbodyfat)) + 
  #coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(aes(color=Sex,size=Age_years))

kids2 %>% filter(Cadence_stepsmin>65) %>%
  ggplot(aes(x=WeightKGAvg,y=Tanita.Avg_percentbodyfat)) + 
  #coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(aes(color=Obese_status,size=Cadence_stepsmin,shape=Sex))

kids2 %>% 
  filter(Cadence_stepsmin>65) %>%
  ggplot(aes(x=WeightKGAvg,y=leglengthCM)) + 
  #coord_cartesian(xlim=c(11,35),ylim=c(56,80)) +
  geom_point(aes(color=Tanita.Avg_percentbodyfat,size=Cadence_stepsmin))

BIC= kids2 %>% 
  select(Cadence_stepsmin,Tanita.Avg_percentbodyfat,WeightKGAvg,Age_years,leglengthCM) %>%
  scale() %>% mclustBIC()
  
#Clustering of the 5 main variables based on BIC. 
kidsmod = kids2 %>% mutate(SexN=as.numeric(Sex)) %>%
  select(Cadence_stepsmin,Tanita.Avg_percentbodyfat,WeightKGAvg,Age_years,leglengthCM) %>%
  Mclust()

kidsmod2 = kids2 %>% mutate(SexN=as.numeric(Sex)) %>%
  select(Tanita.Avg_percentbodyfat,WeightKGAvg,Age_years,leglengthCM) %>%
  Mclust()

summary(kidsmod2, parameters = TRUE)
plot(kidsmod2, what="classification")

kidsmod2$classification
plot(kidsmod, what="classification")
plot(kidsmod$BIC)

kids2$cluster2=kidsmod$classification

summary(lm(Cadence_stepsmin~Sex + as.factor(cluster2) + Tanita.Avg_percentbodyfat + Age_years + leglengthCM  + WeightKGAvg, data=kids2))

clus2kids=kids2 %>% filter(cluster==3)
summary(lm(Cadence_stepsmin~ Sex + Tanita.Avg_percentbodyfat + Age_years + leglengthCM  + WeightKGAvg, data=clus2kids))



kids2 %>% filter(Sex=="F", cluster==2) %>% select(Sex, cluster, Tanita.Avg_percentbodyfat,Age_years,leglengthCM, WeightKGAvg)
kids2 %>% filter(Sex=="M", cluster==1) %>% select(Sex, cluster, Tanita.Avg_percentbodyfat,Age_years,leglengthCM, WeightKGAvg)

#geom_point(data=PBRCMutate, aes(color="PBRC")) +
  #geom_point(data=PBRCMutate %>% filter(BMI > 30), aes(color="BMI > 30")) +
  #scale_color_manual(name="Legend", values=c("FJ"="black","PBRC"="blue","BMI > 30"="red"), limits=c("FJ","PBRC","BMI > 30")) +
  #labs(title="BMI Over 30", x="Waist to Hip (inches)",y="Body Height (inches)") +
#  theme(legend.position = c(.9,.15))


#Linear Model of speed, cadence, and leglength
summary(lm(kids2$Cadence_stepsmin~kids2$TreadmillSpeed_MPH+kids2$leglengthCM))
