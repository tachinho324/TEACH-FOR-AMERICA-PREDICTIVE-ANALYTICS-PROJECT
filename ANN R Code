library(caret)
library(klaR)
library(C50)
library(kernlab) 
library(caret)
library(dplyr)
library(fastDummies)
library(caret)
library(neuralnet)
getwd()
setwd("C:/Users/Smush/Desktop/P DJ Class")
OG<-read.csv("CaseStudy.csv")
str(OG)
summary(OG)

#ANN
ANNOG<- OG[,-c(1,2,6)]
ANNOG$schoolsel[is.na(ANNOG$schoolsel)] <- 0
#DUMMY DF
ANNOG_D <- ANNOG %>%
  dummy_cols(select_columns = c("major1", "major2", "minor", "major1group", "major2group", "minorgroup", "undergrad_uni", "appdeadline"),
             remove_selected_columns = TRUE, remove_first_dummy = TRUE)

#TRAIN
set.seed(1947)
annidx <- createDataPartition(ANNOG_D $completedadm , p=0.7, list=FALSE)
anntrain <- ANNOG_D[annidx, ]
anntest <- ANNOG_D[-annidx, ]
names(anntrain) <- gsub(" ", "_", names(anntrain))
names(anntest) <- gsub(" ", "_", names(anntest))
names(anntest)
str(anntrain)

#---------------------------------------------------------------------------------------
#Persona 1 -  Social Sciences Enthusiast
#ANN 1&1
accOG_SSE1 <- neuralnet(completedadm ~ gpa + stem + schoolsel + essay1length + essay2length +
                        essay3length + essayuniquewords + essayssentiment + signupdate +
                        starteddate + submitteddate + attendedevent + major1_political_science + 
                        major1_history + minor_philosophy + major1group_social_sciences +
                        appdeadline_January, data = anntrain, linear.output = TRUE)
#Plot
plot(accOG_SSE1)
pred.test <- predict(accOG_SSE1, anntest)
class.test <- apply(pred.test, 1, which.max)-1
# Confusion matrix
confusionMatrix(factor(class.test), factor(anntest$completedadm))

#ANN 3
#Model 
accOG_SSE3<- neuralnet(completedadm ~ gpa + stem + schoolsel + essay1length + essay2length +
                         essay3length + essayuniquewords + essayssentiment + signupdate +
                         starteddate + submitteddate + attendedevent + major1_political_science + 
                         major1_history + minor_philosophy + major1group_social_sciences +
                         appdeadline_January, data = anntrain,hidden=3)
#Plot
plot(accOG_SSE3)
pred3.test <- predict(accOG_SSE3, anntest)
class3.test <- apply(pred3.test, 1, which.max)-1
# Confusion matrix
confusionMatrix(factor(class3.test), factor(anntest$completedadm))


 #-----------------------------------------------------------------------------------------
 #Persona 2 -  STEM and Business Focus
 #ANN 1&1
 accOG_SBF1 <- neuralnet(completedadm ~ gpa +stem+ schoolsel+ essay1length+ essay2length+
                         essay3length+ essayuniquewords+ essayssentiment+ signupdate+
                         starteddate+ submitteddate+ attendedevent+ major1_business+
                         major1_biology+ minor_management+ major1group_stem+major1group_other+
                         appdeadline_October,
                         data = anntrain, linear.output = TRUE)
 #Plot
 plot(accOG_SBF1)
 pred.test_sbf1 <- predict(accOG_SBF1, anntest)
 class.test_sbf1 <- apply(pred.test_sbf1, 1, which.max)-1
 # Confusion matrix
 confusionMatrix(factor(class.test_sbf1), factor(anntest$completedadm))
 
 #ANN 3
 #Model 
 accOG_SBF3<- neuralnet(completedadm ~ gpa +stem+ schoolsel+ essay1length+ essay2length+
                          essay3length+ essayuniquewords+ essayssentiment+ signupdate+
                          starteddate+ submitteddate+ attendedevent+ major1_business+
                          major1_biology+ minor_management+ major1group_stem+major1group_other+
                          appdeadline_October,
                        data = anntrain ,hidden=3)
 #Plot
 plot(accOG_SBF3)
 pred3.test_sbf3 <- predict(accOG_SBF3, anntest)
 class3.test_sbf3 <- apply(pred3.test_sbf3, 1, which.max)-1
 # Confusion matrix
 confusionMatrix(factor(class3.test_sbf3), factor(anntest$completedadm))
 
 
 #-----------------------------------------------------------------------------------------
 #Persona 3 -  Humanities and Education Altruist
 #ANN 1&1
 accOG_HEA1 <- neuralnet(completedadm ~ gpa + stem + schoolsel + essay1length + essay2length +
                           essay3length + essayuniquewords + essayssentiment + signupdate +
                           starteddate + submitteddate + attendedevent + major1_education_(elementary) +
                           major1_english + minor_english_literature + major1group_humanities +
                           major1group_education + appdeadline_March, 
                         data = anntrain, linear.output = TRUE)
 #Plot
 plot(accOG_HEA1)
 pred.test_hea1 <- predict(accOG_HEA1, anntest)
 class.test_hea1 <- apply(pred.test_hea1, 1, which.max)-1
 # Confusion matrix
 confusionMatrix(factor(class.test_hea1), factor(anntest$completedadm))
 
 #ANN 3
 #Model 
 accOG_HEA3<- neuralnet(completedadm ~ gpa + stem + schoolsel + essay1length + essay2length +
                          essay3length + essayuniquewords + essayssentiment + signupdate +
                          starteddate + submitteddate + attendedevent + major1_education_(elementary) +
                          major1_english + minor_english_literature + major1group_humanities +
                          major1group_education + appdeadline_March, 
                        data = anntrain, hidden=3 )
 #Plot
 plot(accOG_HEA3)
 pred3.test_hea3 <- predict(accOG_HEA3, anntest)
 class3.test_hea3 <- apply(pred3.test, 1, which.max)-1
 # Confusion matrix
 confusionMatrix(factor(class3.test_hea3), factor(anntest$completedadm))
