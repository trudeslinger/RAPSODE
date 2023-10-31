#Looking at air pollution & epilepsy

#Clear, load libraries and files.
rm(list=ls())
library(Hmisc)
epilep<-read.csv("...dataframe_final.csv")


polls<-c("no2_annual","o3_annual","pm10_annual","pm25_annual")

#


#Basic demographics - current sample size (1143) matches

#Epilepsy diagnosis

table(epilep$diagnosis) #406 with, 737 without 
table(epilep$epilepsy_etiology) #1 = Genetic, 2 = structural, 3 = metabolic, 6 = UNK. Numbers match

#Some quick explorations of the data
for (i in 1:length(polls)){
  curr<-polls[i]
  print(curr)
  print(summary(epilep[curr]))
}


xtabs(~diagnosis + epilepsy_type, data = epilep)
xtabs(~diagnosis + epilepsy_etiology, data = epilep)
xtabs(~diagnosis + genetic_etiology_subtype, data = epilep)

tapply(epilep$pm25_annual,epilep$diagnosis,summary)


#Table 1. Basic demographics (including those in model)
{
rundemo<-function(x){
  paste(round(mean(epilep[,x],na.rm = TRUE),1)," (",round(sd(epilep[,x],na.rm = TRUE),1),"),",
        round(mean(epilep[,x][epilep$diagnosis==1],na.rm = TRUE),1)," (", round(sd(epilep[,x][epilep$diagnosis==1],na.rm = TRUE),1),"),",
        round(mean(epilep[,x][epilep$diagnosis==0],na.rm = TRUE),1)," (", round(sd(epilep[,x][epilep$diagnosis==0],na.rm = TRUE),1),")",sep="")
}

table1cont<-c("age","seswoa_avg", "no2_annual","o3_annual","pm10_annual","pm25_annual")

print(paste("Female,",
            nrow(epilep[which(epilep$sex==0),])," (",round(nrow(epilep[which(epilep$sex==0),])/nrow(epilep)*100,1),"%),",
            nrow(epilep[which(epilep$sex==0 & epilep$diagnosis==1),]), " (",
            round(nrow(epilep[which(epilep$sex==0 & epilep$diagnosis==1),])/nrow(epilep[which(epilep$diagnosis==1),])*100,1),"%),",
            nrow(epilep[which(epilep$sex==0 & epilep$diagnosis==0),]), " (",
            round(nrow(epilep[which(epilep$sex==0 & epilep$diagnosis==0),])/nrow(epilep[which(epilep$diagnosis==0),])*100,1),"%)",
            sep=""))

print(paste("Male,",
            nrow(epilep[which(epilep$sex==1),])," (",round(nrow(epilep[which(epilep$sex==1),])/nrow(epilep)*100,1),"%),",
            nrow(epilep[which(epilep$sex==1 & epilep$diagnosis==1),]), " (",
            round(nrow(epilep[which(epilep$sex==1 & epilep$diagnosis==1),])/nrow(epilep[which(epilep$diagnosis==1),])*100,1),"%),",
            nrow(epilep[which(epilep$sex==1 & epilep$diagnosis==0),]), " (",
            round(nrow(epilep[which(epilep$sex==1 & epilep$diagnosis==0),])/nrow(epilep[which(epilep$diagnosis==0),])*100,1),"%)",
            sep=""))

for(i in 1:length(table1cont)){
  curr<-table1cont[i]
  test<-t.test(epilep[,curr]~epilep$diagnosis)
  print(paste(curr,",",rundemo(curr),",p = ",round(test$p.value,4), ", missing = ",nrow(epilep[which(is.na(epilep[,curr])==TRUE),]),sep=""))
}

}

#Age skews younger - do Wilcoxin test

test<-wilcox.test(age~diagnosis, data = epilep)
test


#Simple proportion test to see if proportions by gender are any different

prop.test(x=c(229,398),n = c(406,737),alternative = "two.sided")
prop.test(x=c(177,339),n = c(406,737),alternative = "two.sided") #Ok, so not really seeing a difference in diagnosis by gender



#Correlation matrix of pollutants!
corrme<-epilep[,c("no2_annual","o3_annual","pm10_annual","pm25_annual","seswoa_avg")]
cor(corrme, use = "pairwise.complete.obs")


#Logistic regression

#Table 2 - Main models
#Model 1 - just pollution + diagnosis 

for (i in 1:length(polls)){
  curr<-polls[i]
  model<-glm(diagnosis~epilep[,curr], data = epilep, family = "binomial")
  #print(summary(model))
  OR <-exp(coef(model)[2])
  CIL<-exp(confint(model)[2])
  CIH<-exp(confint(model)[4])
  print(curr)
  print(paste(round(OR,2)," (",round(CIL,2),",",round(CIH,2),")",sep=""))
}

#Model 2 - SES + age + gender 
for (i in 1:length(polls)){
  curr<-polls[i]
  model<-glm(diagnosis~epilep[,curr] + age + sex + seswoa_avg, data = epilep, family = "binomial")
  #print(summary(model))
  OR <-exp(coef(model)[2])
  CIL<-exp(confint(model)[2])
  CIH<-exp(confint(model)[7])
  print(curr)
  print(paste(round(OR,2)," (",round(CIL,2),",",round(CIH,2),")",sep=""))
}

#Table 3 - epilepsy subtypes

etiology<-c(1,2,3,6)

for(i in 1:length(polls)){
        curr<-polls[i]
        print(curr)
                for(inx in 1:length(etiology)){
                        type<-etiology[inx]
                        data <-epilep[which(epilep$epilepsy_etiology==type|epilep$diagnosis==0),]
                        model1<-glm(diagnosis~data[,curr], data = data, family = "binomial")
                        OR1 <-exp(coef(model1)[2])
                        CIL1<-exp(confint(model1)[2])
                        CIH1<-exp(confint(model1)[4])
                        model2 <-glm(diagnosis~data[,curr] + age + sex + seswoa_avg, data = data, family = "binomial")
                        OR2 <-exp(coef(model2)[2])
                        CIL2<-exp(confint(model2)[2])
                        CIH2<-exp(confint(model2)[7])
                        print(paste("Etiology: ",type,sep=""))
                        print(paste("Unadjusted: ",round(OR1,2)," (",round(CIL1,2),",",round(CIH1,2),")",
                                    ", Adjusted: ",round(OR2,2)," (",round(CIL2,2),",",round(CIH2,2),")",sep=""))
                        
                }
}

#And sensitivity test with those restricted to 2014 - 2019
epilep$presentation_year<-substr(epilep$presentation_date,7,11)
data<-epilep[which(epilep$presentation_year>2013 & epilep$presentation_year<2020),]

for(i in 1:length(polls)){
        curr<-polls[i]
        model1<-glm(diagnosis~data[,curr], data = data, family = "binomial")
        OR1 <-exp(coef(model1)[2])
        CIL1<-exp(confint(model1)[2])
        CIH1<-exp(confint(model1)[4])
        model2 <-glm(diagnosis~data[,curr] + age + sex + seswoa_avg, data = data, family = "binomial")
        OR2 <-exp(coef(model2)[2])
        CIL2<-exp(confint(model2)[2])
        CIH2<-exp(confint(model2)[7])
        print(paste(curr,"*",round(OR1,2)," (",round(CIL1,2),",",round(CIH1,2),")*",
                    round(OR2,2)," (",round(CIL2,2),",",round(CIH2,2),")",sep=""))
        
}


