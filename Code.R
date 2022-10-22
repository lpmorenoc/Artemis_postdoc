rm(list=ls(all=TRUE))
library(tidyverse)
library(caret)
setwd('D:/OneDrive - University of Florida/Documents/Job applications/CIAT/Tanzania/test')
oneacrefund <- read.csv("Artemis_postdoc/One_Acre_Fund_-_Agronomic_Survey_Data_-_2016_to_2021.csv")


table(oneacrefund$crop)
#Start analyzing maize and then climbing beans (more farmers growing those crops)
#However, maize is the only one in all the countries (cimbing bean just Burundi and Rwanda)
ggplot(oneacrefund,aes(x=country,y=yield_kg_ph))+
  geom_boxplot()+facet_wrap(~crop)

ggplot(oneacrefund,aes(x=country,y=yield_kg_pa))+
  geom_boxplot()+facet_wrap(~crop)

#Create a new column with all the yield in kg/ha
oneacrefund$yield_kg_ph_all <- oneacrefund$yield_kg_ph + (oneacrefund$yield_kg_pa*2.47105)
ggplot(oneacrefund,aes(x=country,y=yield_kg_ph_all))+
  geom_boxplot()+facet_wrap(~crop)


#Start with maize analysis

oneacrefundmz <- oneacrefund %>% filter(crop =='maize')

#Verify effect of drought and electricity
aggregate(rep(1, nrow(oneacrefund)), by = list(oneacrefund$drought,oneacrefund$crop), sum)
ggplot(oneacrefundmz,aes(x=as.factor(drought),y=yield_kg_ph_all))+
  geom_boxplot()+facet_wrap(~electricity)



#Evaluate effect fertilizer application (NPK)
#one column with all fertilizers in kg. Acres converted to ha but the ones without area are kept the same (room for some errors)
oneacrefundmz$npk_kg_ph_all <- oneacrefundmz$npk_kg + oneacrefundmz$npk_kg_ph + (oneacrefundmz$npk_kg_pa*2.47105)

ggplot(oneacrefundmz,aes(x=npk_kg_ph_all,y=yield_kg_ph_all,color=as.factor(year)))+
  geom_point()+facet_wrap(~country)


#Urea
oneacrefundmz$urea_kg_ph_all <- oneacrefundmz$urea_kg + oneacrefundmz$urea_kg_ph + (oneacrefundmz$urea_kg_pa*2.47105)
ggplot(oneacrefundmz,aes(x=urea_kg_ph_all,y=yield_kg_ph_all,color=as.factor(year)))+
  geom_point()+facet_wrap(~country)

#Compost
ggplot(oneacrefundmz,aes(x=as.factor(compost),y=yield_kg_ph_all))+
  geom_boxplot()

#Check if weeding has an effect on yield (apparently not for maize with the data collected)
ggplot(oneacrefundmz,aes(x=weed,y=yield_kg_ph_all))+
  geom_point()

#Having cows have an effect on yield? Farmer with more funds?
ggplot(oneacrefundmz,aes(x=as.factor(cows_binary),y=yield_kg_ph_all))+
  geom_boxplot()
ggplot(oneacrefundmz,aes(x=cows,y=yield_kg_ph_all))+
  geom_point()

#What about goats?
ggplot(oneacrefundmz,aes(x=as.factor(goats_binary),y=yield_kg_ph_all))+
  geom_boxplot()

#Using hybrid seeds contribute to higher yield?
ggplot(oneacrefundmz,aes(x=as.factor(hybrid),y=yield_kg_ph_all))+
  geom_boxplot()
ggplot(oneacrefundmz,aes(x=hybridseed_kg_ph_all,y=yield_kg_ph_all))+
  geom_point()
oneacrefundmz$hybridseed_kg_ph_all <- oneacrefundmz$hybridseed_kg_ph  + (oneacrefundmz$hybridseed_kg_pa*2.47105)


#Intercropping?
ggplot(oneacrefundmz,aes(x=as.factor(intercrop),y=yield_kg_ph_all))+
  geom_boxplot()

#Number of household members
ggplot(oneacrefundmz,aes(x=hh_num,y=yield_kg_ph_all,color=as.factor(year)))+
  geom_point()+facet_wrap(~country)


#Disease
ggplot(oneacrefundmz,aes(x=as.factor(pest_disease),y=yield_kg_ph_all))+
  geom_boxplot()

#plant space
ggplot(oneacrefundmz,aes(x=plant_spacing,y=yield_kg_ph_all,color=as.factor(year)))+
  geom_point()#+facet_wrap(~country)


tomodel1 <-subset(oneacrefundmz,select=c(intercrop,hybrid,hybridseed_kg_ph_all,cows_binary, cows, urea_kg_ph_all, npk_kg_ph_all, drought, electricity, yield_kg_ph_all, country,year, yield_kg_ph_all))   
tomodel <-subset(oneacrefundmz,select=c(intercrop,hybrid,cows_binary, urea_kg_ph_all, npk_kg_ph_all, drought, electricity, country,year, yield_kg_ph_all))   
tomodel=na.omit(tomodel)
names <- c("intercrop" ,"hybrid","cows_binary","drought","electricity")
tomodel[,names] <- lapply(tomodel[,names] , factor)
str(tomodel)
set.seed(1)
ind = createDataPartition(tomodel$yield_kg_ph_all, p=0.8,list = FALSE)
training = tomodel[ind, ]
testing = tomodel[-ind, ]

control = trainControl(method='repeatedcv', number=10, repeats = 5, selectionFunction= "oneSE")
control_best = trainControl(method='repeatedcv', number=10, repeats = 5)

#Testing linear model

lmfit = train(yield_kg_ph_all ~ ., data=training, 
              method = "lm", trControl=control)
summary(lmfit)
lmfit$finalModel

lmpredict <- predict(lmfit, newdata = testing)
y.test= testing$yield_kg_ph_all

predicted.test <- cbind(testing,lmpredict)
colnames(predicted.test)[11] <- "predicted.test"
alldata2 <- merge(tomodel,predicted.test,all.x=T)
alldata2 <- alldata2[!duplicated(alldata2),]

ggplot(alldata2,aes(x=yield_kg_ph_all,y=predicted.test))+
  geom_point(aes(colour=country),alpha=0.7)+
  labs(x="Observed maize yield (kg/ha)",y="Simulated yield (kg/ha)", colour="Country")


#Testing random forest
randomGrid <-  data.frame(mtry = c(1:5))

randomforfit = train(yield_kg_ph_all ~ ., data=training, 
                     method = 'rf', verbose=T, trControl=control, 
                     tuneGrid=randomGrid)

randomforfit
randomforfit$finalModel

yhat.forest <- predict(randomforfit, newdata = testing, mtry=randomforfit$bestTune$mtry)
y.test = testing$yield_kg_ph_all
predicted.test <-cbind(testing,yhat.forest)
colnames(predicted.test)[11] <- "predicted.test"
alldata2 <- merge(tomodel,predicted.test,all.x=T)
alldata2 <- alldata2[!duplicated(alldata2),]

ggplot(alldata2,aes(x=yield_kg_ph_all,y=predicted.test))+
  geom_point(aes(colour=country),alpha=0.7)+
  labs(x="Observed maize yield (kg/ha)",y="Simulated yield (kg/ha)", colour="Country")




