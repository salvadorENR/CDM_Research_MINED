
rm (list = ls(all=TRUE))

#set your working directory

setwd("")

#install necessary packages

install.packages("ltm")
install.packages("irtoys")
install.packages("psych")
install.packages("plink")
install.packages("estimatr")
install.packages("ggplot2")
install.packages("mice")
install.packages("miceadd")
install.packages("mfx")
install.packages("texreg")
install.packages("dplyr")
install.packages("car")
install.packages("quantreg")

library(ltm)
library(irtoys)
library(psych)
library(plink)
library(estimatr)
library(ggplot2)
library(mice)
library(miceadd)
library(mfx)
library(texreg)
library(dplyr)
library(car)
library(quantreg)

###Table 3 and 4####

###store the csv files in your working directory.

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_7_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_7_baseline_t <- subset(student_test_7_baseline,
                                    treatment_7==1)

student_test_7_baseline_c <- subset(student_test_7_baseline,
                                    treatment_7==0)

#shift

mean(student_test_7_baseline_t$Shift*100, na.rm = T)
mean(student_test_7_baseline_c$Shift*100, na.rm = T)

Diff_shift <- mean(student_test_7_baseline_t$Shift*100, na.rm = T)-
  mean(student_test_7_baseline_c$Shift*100, na.rm = T)

reg_Shift_baseline <- lm_robust(Shift~
                                  treatment_7+
                                  department*urban.rural, 
                                
                                data=student_test_7_baseline, 
                                clusters = ID_IE, se_type = "stata")

#age

mean(student_test_7_baseline_t$Age, na.rm = T)
mean(student_test_7_baseline_c$Age, na.rm = T)

sd(student_test_7_baseline_t$Age, na.rm = T)
sd(student_test_7_baseline_c$Age, na.rm = T)

Diff_age <- mean(student_test_7_baseline_t$Age, na.rm = T)-
  mean(student_test_7_baseline_c$Age, na.rm = T)

reg_Age_baseline <- lm_robust(Age~
                                treatment_7+
                                department*urban.rural, 
                              
                              data=student_test_7_baseline, 
                              clusters = ID_IE, se_type = "stata")

#sex

mean(student_test_7_baseline_t$Sex*100, na.rm = T)
mean(student_test_7_baseline_c$Sex*100, na.rm = T)

Diff_sex <- mean(student_test_7_baseline_t$Sex*100, na.rm = T)-
  mean(student_test_7_baseline_c$Sex*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Sex~
                                treatment_7+
                                department*urban.rural, 
                              
                              data=student_test_7_baseline, 
                              clusters = ID_IE, se_type = "stata")

#Repeated

mean(student_test_7_baseline_t$Repeated*100, na.rm = T)
mean(student_test_7_baseline_c$Repeated*100, na.rm = T)

Diff_sex <- mean(student_test_7_baseline_t$Repeated*100, na.rm = T)-
  mean(student_test_7_baseline_c$Repeated*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Repeated~
                                treatment_7+
                                department*urban.rural, 
                              
                              data=student_test_7_baseline, 
                              clusters = ID_IE, se_type = "stata")

#N. of siblings

mean(student_test_7_baseline_t$no_elder, na.rm = T)
mean(student_test_7_baseline_c$no_elder, na.rm = T)

sd(student_test_7_baseline_t$no_elder, na.rm = T)
sd(student_test_7_baseline_c$no_elder, na.rm = T)

Diff_no_elder <- mean(student_test_7_baseline_t$no_elder, na.rm = T)-
  mean(student_test_7_baseline_c$no_elder, na.rm = T)

reg_no_elder_baseline <- lm_robust(no_elder~
                                     treatment_7+
                                     department*urban.rural, 
                                   
                                   data=student_test_7_baseline, 
                                   clusters = ID_IE, se_type = "stata")

#younger brothers or/and sisters

mean(student_test_7_baseline_t$no_younger, na.rm = T)
mean(student_test_7_baseline_c$no_younger, na.rm = T)

sd(student_test_7_baseline_t$no_younger, na.rm = T)
sd(student_test_7_baseline_c$no_younger, na.rm = T)

Diff_no_younger <- mean(student_test_7_baseline_t$no_younger, na.rm = T)-
  mean(student_test_7_baseline_c$no_younger, na.rm = T)

reg_no_younger_baseline <- lm_robust(no_younger~
                                       treatment_7+
                                       department*urban.rural, 
                                     
                                     data=student_test_7_baseline, 
                                     clusters = ID_IE, se_type = "stata")

#test scores (20 items)

mean(student_test_7_baseline_t$total_score_7, na.rm = T)
mean(student_test_7_baseline_c$total_score_7, na.rm = T)

sd(student_test_7_baseline_t$total_score_7, na.rm = T)
sd(student_test_7_baseline_c$total_score_7, na.rm = T)

Diff_total_score_7 <- mean(student_test_7_baseline_t$total_score_7, na.rm = T)-
  mean(student_test_7_baseline_c$total_score_7, na.rm = T)

reg_total_score_baseline <- lm_robust(total_score_7~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#stndardized test scores

mean(student_test_7_baseline_t$ztest_score_baseline, na.rm = T)
mean(student_test_7_baseline_c$ztest_score_baseline, na.rm = T)

Diff_ztest_score_baseline <- mean(student_test_7_baseline_t$ztest_score_baseline, na.rm = T)-
  mean(student_test_7_baseline_c$ztest_score_baseline, na.rm = T)

reg_total_score_baseline <- lm_robust(ztest_score_baseline~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#textbook last year

mean(student_test_7_baseline_t$Math_textbook_LY_e*100, na.rm = T)
mean(student_test_7_baseline_c$Math_textbook_LY_e*100, na.rm = T)

Diff_Math_textbook_LY_e_baseline <- 
  mean(student_test_7_baseline_t$Math_textbook_LY_e*100, na.rm = T)-
  mean(student_test_7_baseline_c$Math_textbook_LY_e*100, na.rm = T)

reg_Math_textbook_LY_e_baseline <- lm_robust(Math_textbook_LY_e~
                                               treatment_7+
                                               department*urban.rural, 
                                             
                                             data=student_test_7_baseline, 
                                             clusters = ID_IE, se_type = "stata")

#notebook last year

mean(student_test_7_baseline_t$Math_Notebook_LY_e*100, na.rm = T)
mean(student_test_7_baseline_c$Math_Notebook_LY_e*100, na.rm = T)

Diff_Math_Notebook_LY_e_baseline <- 
  mean(student_test_7_baseline_t$Math_Notebook_LY_e*100, na.rm = T)-
  mean(student_test_7_baseline_c$Math_Notebook_LY_e*100, na.rm = T)

reg_Math_Notebook_LY_e_baseline <- lm_robust(Math_Notebook_LY_e~
                                               treatment_7+
                                               department*urban.rural, 
                                             
                                             data=student_test_7_baseline, 
                                             clusters = ID_IE, se_type = "stata")

#student desk at home

mean(student_test_7_baseline_t$Student_desk_home*100, na.rm = T)
mean(student_test_7_baseline_c$Student_desk_home*100, na.rm = T)

Diff_Student_desk_home_baseline <- 
  mean(student_test_7_baseline_t$Student_desk_home*100, na.rm = T)-
  mean(student_test_7_baseline_c$Student_desk_home*100, na.rm = T)

reg_Student_desk_home_baseline <- lm_robust(Student_desk_home~
                                                  treatment_7+
                                                  department*urban.rural, 
                                                
                                                data=student_test_7_baseline, 
                                                clusters = ID_IE, se_type = "stata")

#smartphone

mean(student_test_7_baseline_t$Item_House__1*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_7_baseline_t$Item_House__1*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_7_baseline_t$Item_House__2*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_7_baseline_t$Item_House__2*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_7_baseline_t$Item_House__3*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_7_baseline_t$Item_House__3*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_7_baseline_t$Item_House__4*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_7_baseline_t$Item_House__4*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_7_baseline_t$Item_House__5*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_7_baseline_t$Item_House__5*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_7_baseline_t$Item_House__6*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_7_baseline_t$Item_House__6*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_7_baseline_t$Item_House__7*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_7_baseline_t$Item_House__7*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_7_baseline_t$Item_House__8*100, na.rm = T)
mean(student_test_7_baseline_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_7_baseline_t$Item_House__8*100, na.rm = T)-
  mean(student_test_7_baseline_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline, 
                               clusters = ID_IE, se_type = "stata")

#Not using fire for cooking

mean(student_test_7_baseline_t$Cooking_Equipment__1_r*100, na.rm = T)
mean(student_test_7_baseline_c$Cooking_Equipment__1_r*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_7_baseline_t$Cooking_Equipment__1_r*100, na.rm = T)-
  mean(student_test_7_baseline_c$Cooking_Equipment__1_r*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1_r~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#balance of student household economic index

###compute composite index of household economic status####

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_7_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_7_p <- cbind(student_test_7_baseline$Item_House__1,
                          student_test_7_baseline$Item_House__2,
                          student_test_7_baseline$Item_House__3,
                          student_test_7_baseline$Item_House__4,
                          student_test_7_baseline$Item_House__5,
                          student_test_7_baseline$Item_House__6,
                          student_test_7_baseline$Item_House__7,
                          student_test_7_baseline$Item_House__8,
                          student_test_7_baseline$Cooking_Equipment__1_r)

row.names(student_test_7_p) <- as.numeric(c(student_test_7_baseline$ID_IE_test))

student_test_7_p <- na.omit(student_test_7_p)

result <- prcomp(student_test_7_p, scale=T, na.rm=T, center=T)

summary(result)

result2 <- round(result$rotation, 3)
result3 <- round(result$x, digits=3)

pc1 <- result$x[,1] # first principal component
pc2 <- result$x[,2] # second principal component
pc3 <- result$x[,3] # third principal component

student_id <- as.numeric(rownames(student_test_7_p))

student_test_7_p <- cbind.data.frame(student_id, student_test_7_p, pc1, pc2, pc3)

student_test_7_p <- subset(student_test_7_p,
                           select=c("student_id",
                                    "pc1"))

names(student_test_7_p) <- c("ID_IE_test",
                             "pc1")

student_test_7_baseline <- merge(student_test_7_baseline, student_test_7_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_7_baseline$ID_IE_test

write.csv(student_test_7_p,
          "student_test_7_p.csv",
          row.names = F)

#check balance of student household economic index

student_test_7_baseline_t <- subset(student_test_7_baseline,
                                    treatment_7==1)

student_test_7_baseline_c <- subset(student_test_7_baseline,
                                    treatment_7==0)

mean(student_test_7_baseline_t$pc1, na.rm = T)
mean(student_test_7_baseline_c$pc1, na.rm = T)

Diff_pc1 <- 
  mean(student_test_7_baseline_t$pc1, na.rm = T)-
  mean(student_test_7_baseline_c$pc1, na.rm = T)

reg_pc1 <- lm_robust(pc1~
                       treatment_7+
                       department*urban.rural, 
                     
                     data=student_test_7_baseline, 
                     clusters = ID_IE, se_type = "stata")

###Table C.2 in Appendix C####

rm (list = ls(all=TRUE))

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_p <- 
  read.csv("student_test_7_p.csv", header=T, stringsAsFactors = F)

student_test_7_baseline <- merge(student_test_7_baseline, student_test_7_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_7_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_7_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_7_baseline_pc_low <- subset(student_test_7_baseline,
                                           pc1<(-1))

student_test_7_baseline_pc_low_t <- subset(student_test_7_baseline,
                                           pc1<(-1)&
                                             treatment_7==1)

student_test_7_baseline_pc_low_c <- subset(student_test_7_baseline,
                                           pc1<(-1)&
                                             treatment_7==0)

student_test_7_baseline_pc_med <- subset(student_test_7_baseline,
                                         pc1>=(-1)&
                                           pc1<(1.3))

student_test_7_baseline_pc_med_t <- subset(student_test_7_baseline,
                                           pc1>=(-1)&
                                             pc1<(1.3)&
                                             treatment_7==1)

student_test_7_baseline_pc_med_c <- subset(student_test_7_baseline,
                                           pc1>=(-1)&
                                             pc1<(1.3)&
                                             treatment_7==0)

student_test_7_baseline_pc_hig <- subset(student_test_7_baseline,
                                         pc1>=(1.3))

student_test_7_baseline_pc_hig_t <- subset(student_test_7_baseline,
                                           pc1>=(1.3)&
                                             treatment_7==1)

student_test_7_baseline_pc_hig_c <- subset(student_test_7_baseline,
                                           pc1>=(1.3)&
                                             treatment_7==0)

nrow(student_test_7_baseline_pc_low_t)
nrow(student_test_7_baseline_pc_low_c)

nrow(student_test_7_baseline_pc_med_t)
nrow(student_test_7_baseline_pc_med_c)

nrow(student_test_7_baseline_pc_hig_t)
nrow(student_test_7_baseline_pc_hig_c)

#low
#smartphone

mean(student_test_7_baseline_pc_low_t$Item_House__1*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__1*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_7_baseline_pc_low_t$Item_House__2*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__2*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_7_baseline_pc_low_t$Item_House__3*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__3*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_7_baseline_pc_low_t$Item_House__4*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__4*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_7_baseline_pc_low_t$Item_House__5*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__5*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_7_baseline_pc_low_t$Item_House__6*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__6*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_7_baseline_pc_low_t$Item_House__7*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__7*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_7_baseline_pc_low_t$Item_House__8*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_7_baseline_pc_low_t$Item_House__8*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_low, 
                               clusters = ID_IE, se_type = "stata")

#Not using fire for cooking

mean(student_test_7_baseline_pc_low_t$Cooking_Equipment__1_r*100, na.rm = T)
mean(student_test_7_baseline_pc_low_c$Cooking_Equipment__1_r*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_7_baseline_pc_low_t$Cooking_Equipment__1_r*100, na.rm = T)-
  mean(student_test_7_baseline_pc_low_c$Cooking_Equipment__1_r*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1_r~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline_pc_low, 
                                      clusters = ID_IE, se_type = "stata")

#medium
#smartphone

mean(student_test_7_baseline_pc_med_t$Item_House__1*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__1*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_7_baseline_pc_med_t$Item_House__2*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__2*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_7_baseline_pc_med_t$Item_House__3*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__3*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_7_baseline_pc_med_t$Item_House__4*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__4*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_7_baseline_pc_med_t$Item_House__5*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__5*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_7_baseline_pc_med_t$Item_House__6*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__6*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_7_baseline_pc_med_t$Item_House__7*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__7*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_7_baseline_pc_med_t$Item_House__8*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_7_baseline_pc_med_t$Item_House__8*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_med, 
                               clusters = ID_IE, se_type = "stata")

#Not using fire for cooking

mean(student_test_7_baseline_pc_med_t$Cooking_Equipment__1_r*100, na.rm = T)
mean(student_test_7_baseline_pc_med_c$Cooking_Equipment__1_r*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_7_baseline_pc_med_t$Cooking_Equipment__1_r*100, na.rm = T)-
  mean(student_test_7_baseline_pc_med_c$Cooking_Equipment__1_r*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1_r~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline_pc_med, 
                                      clusters = ID_IE, se_type = "stata")

#high
#smartphone

mean(student_test_7_baseline_pc_hig_t$Item_House__1*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__1*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_7_baseline_pc_hig_t$Item_House__2*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__2*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_7_baseline_pc_hig_t$Item_House__3*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__3*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_7_baseline_pc_hig_t$Item_House__4*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__4*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_7_baseline_pc_hig_t$Item_House__5*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__5*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_7_baseline_pc_hig_t$Item_House__6*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__6*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_7_baseline_pc_hig_t$Item_House__7*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__7*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_7_baseline_pc_hig_t$Item_House__8*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_7_baseline_pc_hig_t$Item_House__8*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_7+
                                 department*urban.rural, 
                               
                               data=student_test_7_baseline_pc_hig, 
                               clusters = ID_IE, se_type = "stata")

#Not using fire for cooking

mean(student_test_7_baseline_pc_hig_t$Cooking_Equipment__1_r*100, na.rm = T)
mean(student_test_7_baseline_pc_hig_c$Cooking_Equipment__1_r*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_7_baseline_pc_hig_t$Cooking_Equipment__1_r*100, na.rm = T)-
  mean(student_test_7_baseline_pc_hig_c$Cooking_Equipment__1_r*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1_r~
                                        treatment_7+
                                        department*urban.rural, 
                                      
                                      data=student_test_7_baseline_pc_hig, 
                                      clusters = ID_IE, se_type = "stata")

###Table E.1 in Appendix E#### 

#prepare dataframe for checking attrition at the endline

rm (list = ls(all=TRUE))

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

student_test_7_remaining_e <- subset(student_test_7_end,
                                     select=c("ID_IE_test"))

student_test_7_remaining_e$attrition_2018 <- c(0)

student_test_7_baseline_id <- subset(student_test_7_baseline,
                                     select=c("ID_IE_test"))

student_test_7_remaining_e_r <- 
  dplyr::left_join(student_test_7_baseline_id, 
                   student_test_7_remaining_e,
                   by=c("ID_IE_test"))

student_test_7_remaining_e_r$attrition_2018[
  is.na(student_test_7_remaining_e_r$attrition_2018)] <- 1

student_test_7_baseline <- merge(student_test_7_remaining_e_r, student_test_7_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

#Column: End-line, OLS

student_test_7_baseline$total_asset_num_baseline

reg_d1_end_ols <- lm_robust(attrition_2018~
                              
                              treatment_7+
                              ztest_score_baseline+
                              Age+
                              Sex+
                              Repeated+
                              Shift+
                              no_elder+
                              no_younger+
                              total_asset_num_baseline+
                              department*urban.rural,
                            
                            data=student_test_7_baseline, 
                            clusters = ID_IE, se_type = "stata")

summary(reg_d1_end_ols)

#Column: Endline, logit

formula_logit <-  attrition_2018 ~ 
  treatment_7+
  ztest_score_baseline+
  Age+
  Sex+
  Repeated+
  Shift+
  no_elder+
  no_younger+
  total_asset_num_baseline+
  department*urban.rural

reg_d1_end_logit <- logitmfx(formula_logit, data=student_test_7_baseline, 
                             atmean = TRUE, robust = TRUE, clustervar1 = "ID_IE", 
                             clustervar2 = NULL, start = NULL, control = list())

extract(reg_d1_end_logit)

#prepare dataframe for checking attrition at the follow-up

rm (list = ls(all=TRUE))

student_test_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

student_test_7_baseline <- read.csv("student_test_7_baseline.csv", 
                                    header=T, stringsAsFactors = F)

student_test_7_remaining_f <- subset(student_test_fus,
                                   select=c("ID_IE_test"))

student_test_7_remaining_f$attrition_2019 <- c(0)

student_test_7_baseline_id <- subset(student_test_7_baseline,
                                     select=c("ID_IE_test"))

student_test_7_remaining_f_r <- 
  dplyr::left_join(student_test_7_baseline_id, 
                   student_test_7_remaining_f,
                   by=c("ID_IE_test"))

student_test_7_remaining_f_r$attrition_2019[
  is.na(student_test_7_remaining_f_r$attrition_2019)] <- 1

student_test_7_baseline <- merge(student_test_7_remaining_f_r, 
                                 student_test_7_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

#Column: Follow-up, OLS
reg_d1_fus_ols <- lm_robust(attrition_2019~
                           
                              treatment_7+
                              ztest_score_baseline+
                              Age+
                              Sex+
                              Repeated+
                              Shift+
                              no_elder+
                              no_younger+
                              total_asset_num_baseline+
                              department*urban.rural, 
                         
                         data=student_test_7_baseline, 
                         clusters = ID_IE, se_type = "stata")

summary(reg_d1_fus_ols)

#Column: follow-up, logit

formula_logit <-  attrition_2019 ~ 
  treatment_7+
  ztest_score_baseline+
  Age+
  Sex+
  Repeated+
  Shift+
  no_elder+
  no_younger+
  total_asset_num_baseline+
  department*urban.rural

reg_d1_fus_logit <- logitmfx(formula_logit, data=student_test_7_baseline, 
                           atmean = TRUE, robust = TRUE, clustervar1 = "ID_IE", 
                           clustervar2 = NULL, start = NULL, control = list())

extract(reg_d1_fus_logit)

###Prepare dataframe to estimate impacts (Tables 5, 6, 7, F.3, G.1, G.2, Figure F.1 and F.2)#####

rm (list = ls(all=TRUE))

###store the csv files in your working directory.
###read csv files

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

student_test_7_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

#please refer to the section of "compute composite index of household economic status" 
#and implement the codes

student_test_7_p <- 
  read.csv("student_test_7_p.csv", header=T, stringsAsFactors = F)

#merge data frames

student_test_7_end <- merge(student_test_7_end, student_test_7_p,
                            by.x=c("ID_IE_test"), 
                            by.y=c("ID_IE_test"), all=F)

student_test_7_fus <- merge(student_test_7_fus, student_test_7_p,
                            by.x=c("ID_IE_test"), 
                            by.y=c("ID_IE_test"), all=F)

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q1_Correct_or_Wrong_G7" ) ] <- "Q1_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q2_Correct_or_Wrong_G7" ) ] <- "Q2_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q3_Correct_or_Wrong_G7" ) ] <- "Q3_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q4_Correct_or_Wrong_G7" ) ] <- "Q4_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q5_Correct_or_Wrong_G7" ) ] <- "Q5_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q6_Correct_or_Wrong_G7" ) ] <- "Q6_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q7_Correct_or_Wrong_G7" ) ] <- "Q7_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q8_Correct_or_Wrong_G7" ) ] <- "Q8_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q9_Correct_or_Wrong_G7" ) ] <- "Q9_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q10_Correct_or_Wrong_G7" ) ] <- "Q10_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q11_Correct_or_Wrong_G7" ) ] <- "Q11_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q12_Correct_or_Wrong_G7" ) ] <- "Q12_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q13_Correct_or_Wrong_G7" ) ] <- "Q13_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q14_Correct_or_Wrong_G7" ) ] <- "Q14_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q15_Correct_or_Wrong_G7" ) ] <- "Q15_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q16_Correct_or_Wrong_G7" ) ] <- "Q16_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q17_Correct_or_Wrong_G7" ) ] <- "Q17_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q18_Correct_or_Wrong_G7" ) ] <- "Q18_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q19_Correct_or_Wrong_G7" ) ] <- "Q19_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q20_Correct_or_Wrong_G7" ) ] <- "Q20_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q21_Correct_or_Wrong_G7" ) ] <- "Q21_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q22_Correct_or_Wrong_G7" ) ] <- "Q22_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q23_Correct_or_Wrong_G7" ) ] <- "Q23_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q24_Correct_or_Wrong_G7" ) ] <- "Q24_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q25_Correct_or_Wrong_G7" ) ] <- "Q25_Correct_or_Wrong_G7_fus"

#extracting student test data from the original data frame

test_data_end <- cbind.data.frame(student_test_7_end$Q1_Correct_or_Wrong_G7,
                                  student_test_7_end$Q2_Correct_or_Wrong_G7,
                                  student_test_7_end$Q3_Correct_or_Wrong_G7,
                                  student_test_7_end$Q4_Correct_or_Wrong_G7,
                                  student_test_7_end$Q5_Correct_or_Wrong_G7,
                                  student_test_7_end$Q6_Correct_or_Wrong_G7,
                                  student_test_7_end$Q7_Correct_or_Wrong_G7,
                                  student_test_7_end$Q8_Correct_or_Wrong_G7,
                                  student_test_7_end$Q9_Correct_or_Wrong_G7,
                                  student_test_7_end$Q10_Correct_or_Wrong_G7,
                                  student_test_7_end$Q11_Correct_or_Wrong_G7,
                                  student_test_7_end$Q12_Correct_or_Wrong_G7,
                                  student_test_7_end$Q13_Correct_or_Wrong_G7,
                                  student_test_7_end$Q14_Correct_or_Wrong_G7,
                                  student_test_7_end$Q15_Correct_or_Wrong_G7,
                                  student_test_7_end$Q16_Correct_or_Wrong_G7,
                                  student_test_7_end$Q17_Correct_or_Wrong_G7,
                                  student_test_7_end$Q18_Correct_or_Wrong_G7,
                                  student_test_7_end$Q19_Correct_or_Wrong_G7,
                                  student_test_7_end$Q20_Correct_or_Wrong_G7)

test_data_fus <- cbind.data.frame(student_test_7_fus$Q1_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q2_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q3_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q4_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q5_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q6_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q7_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q8_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q9_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q10_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q11_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q12_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q13_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q14_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q15_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q16_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q17_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q18_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q19_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q20_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q21_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q22_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q23_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q24_Correct_or_Wrong_G7_fus,
                                  student_test_7_fus$Q25_Correct_or_Wrong_G7_fus)

###read csv file of the parameters for IRT estimates

item_analysis_el_salvador <- 
  read.csv("item_analysis_el_salvador_G7.csv", header=T, stringsAsFactors = F)

para.old <- item_analysis_el_salvador[1:20, 2:3]
para.new <- item_analysis_el_salvador[1:25, 4:5]
para.on <- list(para.old, para.new)

cat.old <- rep(x = 2, times = 20)
cat.new <- rep(x = 2, times = 25)
cat.on <- list(cat.old, cat.new)

pm.old <- as.poly.mod(n = 20)
pm.new <- as.poly.mod(n = 25)
pm.on <- list(pm.old, pm.new)

common.old<-c(3, 8, 9, 12, 16)
common.new<-c(1,2,3,4,5)
common.on<-cbind(common.old, common.new)

pars.on <- as.irt.pars(x = para.on, 
                       common = common.on, cat = cat.on, poly.mod = pm.on)

out.on <- plink(x = pars.on, rescale = "MS", symmetric = TRUE)

para.equ <- link.pars(x = out.on)

#

para.old$c_3 <- rep(0, 20)
para.end <- as.matrix(para.old)

#estimate IRT scores

theta.est_end <-mlebme(resp=test_data_end,ip=para.end,method="BM")
theta.est<-mlebme(resp=test_data_fus,ip=para.equ$group2,method="BM")

#standardized the IRT scores by the mean and sd of the IRT scores of 
#the control group at the end-line survey

student_test_7_end$irt_score_end <- theta.est_end[,1]
student_test_7_fus$irt_score <- theta.est[,1]

student_test_7_end_c <- subset(student_test_7_end,
                               treatment_7==0)

student_test_7_fus$irt_score_s <- c((student_test_7_fus$irt_score-
                                       mean(student_test_7_end_c$irt_score_end))/
                                      sd(student_test_7_end_c$irt_score_end))

student_test_7_end$irt_score_end_r <- c((student_test_7_end$irt_score_end-
                                           mean(student_test_7_end_c$irt_score_end))/
                                          sd(student_test_7_end_c$irt_score_end))

#r-binding the dataframes of end-line and follow-up data

student_test_7_end_r <- subset(student_test_7_end,
                               select=c("irt_score_end_r",
                                        "treatment_7",
                                        "department",
                                        "urban.rural",
                                        "ztest_score_baseline",
                                        "Age",
                                        "Age_b",
                                        "Sex",
                                        "Repeated",
                                        "Shift_baseline",
                                        "no_elder",
                                        "no_younger",
                                        "Student_desk_home",
                                        "Math_textbook_LY_e",
                                        "Math_Notebook_LY_e",
                                        "Item_House__1",
                                        "Item_House__2",
                                        "Item_House__3",
                                        "Item_House__4",
                                        "Item_House__5",
                                        "Item_House__6",
                                        "Item_House__7",
                                        "Item_House__8",
                                        "Cooking_non_fire",
                                        "pc1",
                                        
                                        "Multigrade_G7",
                                        "Number_total_Students",
                                        "Number_G7_Students_AM_r",
                                        "Number_G7_Students_PM_r",
                                        
                                        "Teacher_Sex",
                                        "Teacher_Age",
                                        
                                        "Highest_Degree_professorate",
                                        "Highest_Degree_bachelor",
                                        "Highest_Degree_master",
                                        "Highest_Degree_other",
                                        
                                        "Teachr_Qualification__2",
                                        "Teachr_Qualification__3",
                                        "Teachr_Qualification__4",
                                        "Teachr_Qualification__6",
                                        "Teachr_Qualification__7",
                                        
                                        "Teachr_Apontmt_Post__2",
                                        "Teachr_Apontmt_Post__3",
                                        "Teachr_Apontmt_Post__4",
                                        
                                        "Year_Start_teach_r",
                                        
                                        "HM_Sex",
                                        "HM_Age",
                                        "Year_HM_ttl",
                                        "Year_HM_this_school",
                                        "HM_Highest_Degree_highschool",
                                        "HM_Highest_Degree_master",
                                        "HM_Highest_Degree_professorate",
                                        "HM_Highest_Degree_other",
                                        
                                        "School_Facility__2",
                                        "School_Facility__3",
                                        "School_Facility__4",
                                        "School_Facility__5",
                                        "School_Facility__6",
                                        "School_Facility__7",
                                        "School_Facility__8",
                                        
                                        "Donor_Project__2",
                                        "Donor_Project__3",
                                        "Donor_Project__4",
                                        "Donor_Project__5",
                                        "Donor_Project__6",
                                        "Donor_Project__7",
                                        "Donor_Project__8",
                                        "Donor_Project__9",
                                        "Donor_Project__10",
                                        "Donor_Project__11",
                                        "Donor_Project__12",
                                        "Donor_Project__13",
                                        "Donor_Project__14",
                                        "Donor_Project__15",
                                        "Donor_Project__16",
                                        "Donor_Project__17",
                                        "Donor_Project__18",
                                        "Donor_Project__19",
                                        "Donor_Project__20",
                                        
                                        "ID_IE",
                                        "ID_IE_test",
                                        
                                        "Time_Study_HW_G7",
                                        "Time_Study_not_HW_G7",
                                        
                                        "Method_Home_Study__1",
                                        "Method_Home_Study__2",
                                        "Method_Home_Study__3",
                                        "Method_Home_Study__4",
                                        
                                        "Family_teach_study",
                                        "Family_ask_have_freq_e",
                                        "Family_ask_finished_freq_e",
                                        
                                        "read_textbook_e",
                                        "Homework_Given_G7",
                                        "adjust_delay_class_e__5"))

#Inserting NAs for the variables only used for either end-line or follow-up data analysis
student_test_7_fus$Homework_Given_G7 <- NA
student_test_7_fus$adjust_delay_class_e__5 <- NA

student_test_7_fus_r <- subset(student_test_7_fus,
                               select=c("irt_score_s",
                                        "treatment_7",
                                        "department",
                                        "urban.rural",
                                        "ztest_score_baseline",
                                        "Age",
                                        "Age_b",
                                        "Sex",
                                        "repeated_2019",
                                        "Shift_2019",
                                        "no_elder",
                                        "no_younger",
                                        "Student_desk_home",
                                        "Math_textbook_LY_e",
                                        "Math_Notebook_LY_e",
                                        "Item_House__1",
                                        "Item_House__2",
                                        "Item_House__3",
                                        "Item_House__4",
                                        "Item_House__5",
                                        "Item_House__6",
                                        "Item_House__7",
                                        "Item_House__8",
                                        "Cooking_non_fire",
                                        "Math_textbook_e",
                                        "pc1",
                                        
                                        "Multigrade_G7_2019_r",
                                        "Multigrade_G8_2019_r",
                                        "Number_total_Students_2018",
                                        "Number_G7_Students_AM_r_2019",
                                        "Number_G7_Students_PM_r_2019",
                                        
                                        "Teacher_Sex",
                                        "Teacher_Age",
                                        "Highest_Degree_professorate",
                                        "Highest_Degree_bachelor",
                                        "Highest_Degree_master",
                                        "Highest_Degree_other",
                                        
                                        "Teachr_Qualification__2",
                                        "Teachr_Qualification__3",
                                        "Teachr_Qualification__4",
                                        "Teachr_Qualification__6",
                                        "Teachr_Qualification__7",
                                        
                                        "Teachr_Apontmt_Post__2",
                                        "Teachr_Apontmt_Post__3",
                                        "Teachr_Apontmt_Post__4",
                                        
                                        "Year_Start_teach_r",
                                        
                                        "HM_Sex",
                                        "HM_Age",
                                        "Year_HM_ttl",
                                        "Year_HM_this_school",
                                        "HM_Highest_Degree_highschool",
                                        "HM_Highest_Degree_master",
                                        "HM_Highest_Degree_professorate",
                                        "HM_Highest_Degree_other",
                                        
                                        "School_Facility__2",
                                        "School_Facility__3",
                                        "School_Facility__4",
                                        "School_Facility__5",
                                        "School_Facility__6",
                                        "School_Facility__7",
                                        "School_Facility__8",
                                        
                                        "Donor_Project__2",
                                        "Donor_Project__3",
                                        "Donor_Project__4",
                                        "Donor_Project__5",
                                        "Donor_Project__6",
                                        "Donor_Project__7",
                                        "Donor_Project__8",
                                        "Donor_Project__9",
                                        "Donor_Project__10",
                                        "Donor_Project__11",
                                        "Donor_Project__12",
                                        "Donor_Project__13",
                                        "Donor_Project__14",
                                        "Donor_Project__15",
                                        "Donor_Project__16",
                                        "Donor_Project__17",
                                        "Donor_Project__18",
                                        "Donor_Project__19",
                                        "Donor_Project__20",
                                        
                                        "ID_IE",
                                        "ID_IE_test",
                                        
                                        "Time_Study_HW_G7",
                                        "Time_Study_not_HW_G7",
                                        "Method_Home_Study__1",
                                        "Method_Home_Study__2",
                                        "Method_Home_Study__3",
                                        "Method_Home_Study__4",
                                        
                                        "Family_teach_study",
                                        "Family_ask_have_freq_e",
                                        "Family_ask_finished_freq_e",
                                        
                                        "read_textbook_e",
                                        "Homework_Given_G7",
                                        "adjust_delay_class_e__5")) 

#

names(student_test_7_end_r)[which(names(student_test_7_end_r)==
                                    "irt_score_end_r" ) ] <- "irt_score_rev"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "irt_score_s" ) ] <- "irt_score_rev"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "repeated_2019" ) ] <- "Repeated"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "Number_total_Students_2018" ) ] <- "Number_total_Students"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "Number_G7_Students_AM_r_2019" ) ] <- "Number_G7_Students_AM_r"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "Number_G7_Students_PM_r_2019" ) ] <- "Number_G7_Students_PM_r"

names(student_test_7_end_r)[which(names(student_test_7_end_r)==
                                    "Shift_baseline" ) ] <- "Shift"

names(student_test_7_fus_r)[which(names(student_test_7_fus_r)==
                                    "Shift_2019" ) ] <- "Shift"

#

student_test_7_end_r$timing_fus <- c(0)
student_test_7_fus_r$timing_fus <- c(1)

student_test_7_end_r$timing_end <- c(1)
student_test_7_fus_r$timing_end <- c(0)

#

student_test_7_fus_r$Multigrade_G7 <- c(0)

student_test_7_end_r$Multigrade_G7_2019_r <- c(0)
student_test_7_end_r$Multigrade_G8_2019_r <- c(0)

#

student_test_7_end_r$Math_textbook_e <- c(0)

#

student_test_7_rev <- rbind.data.frame(student_test_7_end_r,
                                       student_test_7_fus_r)

student_test_7_rev$Method_Home_Study__3[
  is.na(student_test_7_rev$Method_Home_Study__3)]<-0

#creating variables for regressions

student_test_7_rev$treatment_7_2018 <- c(
  student_test_7_rev$treatment_7*student_test_7_rev$timing_end)

student_test_7_rev$treatment_7_2019 <- c(
  student_test_7_rev$treatment_7*student_test_7_rev$timing_fus)

#

student_test_7_rev$zscore_baseline_2018 <- c(
  student_test_7_rev$timing_end*student_test_7_rev$ztest_score_baseline)

student_test_7_rev$zscore_baseline_2019 <- c(
  student_test_7_rev$timing_fus*student_test_7_rev$ztest_score_baseline)

#

student_test_7_rev$Repeated_2018 <- c(
  student_test_7_rev$Repeated*student_test_7_rev$timing_end)

student_test_7_rev$Repeated_2019 <- c(
  student_test_7_rev$Repeated*student_test_7_rev$timing_fus)

#

student_test_7_rev$Shift_2018 <- c(
  student_test_7_rev$Shift*student_test_7_rev$timing_end)

student_test_7_rev$Shift_2019 <- c(
  student_test_7_rev$Shift*student_test_7_rev$timing_fus)

###implement the following code for
#balanced samples

student_test_7_rev_end <- subset(student_test_7_rev,
                                  timing_end==1)

student_test_7_rev_fus <- subset(student_test_7_rev,
                                  timing_fus==1)

student_test_7_rev_end_id <- subset(student_test_7_rev_end,
                                     select=c("ID_IE_test"))

student_test_7_rev_fus_id <- subset(student_test_7_rev_fus,
                                     select=c("ID_IE_test"))

student_test_7_rev_id <- merge(student_test_7_rev_end_id, student_test_7_rev_fus_id,
                               by.x=c("ID_IE_test"), 
                               by.y=c("ID_IE_test"), all=F)

student_test_7_rev_end <- merge(student_test_7_rev_id, student_test_7_rev_end,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_7_rev_fus <- merge(student_test_7_rev_id, student_test_7_rev_fus,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_7_rev_panel <- rbind.data.frame(student_test_7_rev_end,
                                             student_test_7_rev_fus)

nrow(student_test_7_rev)
nrow(student_test_7_rev_panel)

#

student_test_7_rev_end <- subset(student_test_7_rev,
                                 timing_end==1)

student_test_7_rev_fus <- subset(student_test_7_rev,
                                 timing_fus==1)

###Table 5####
#column 1-1

reg5_1_1 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_1)

#column 1-2

reg5_1_2 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_2)

#column 1-3

reg5_1_3 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        Age_b+
                        Sex+
                        Repeated_2018+
                        Repeated_2019+
                        Shift_2018+
                        Shift_2019+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Cooking_non_fire+
                        Math_textbook_e+
                        
                        Multigrade_G7+
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_3)

#column 2-1

reg5_2_1 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural, 
                      
                      data=student_test_7_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_1)

#column 2-2

reg5_2_2 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019, 
                      
                      data=student_test_7_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_2)

#column 2-3

reg5_2_3 <- lm_robust(irt_score_rev~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        Age_b+
                        Sex+
                        Repeated_2018+
                        Repeated_2019+
                        Shift_2018+
                        Shift_2019+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Cooking_non_fire+
                        Math_textbook_e+
                        
                        Multigrade_G7+
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_3)

unique(student_test_2_rev_panel$repeated_2019)

###Table 6####

#creating dummy variables for the heterogeneity analysis

student_test_7_rev$treatment_7_2018_pc1 <-
  c(student_test_7_rev$treatment_7_2018*
      student_test_7_rev$pc1)

student_test_7_rev$treatment_7_2019_pc1 <-
  c(student_test_7_rev$treatment_7_2019*
      student_test_7_rev$pc1)

#

student_test_7_rev$treatment_7_2018_pc1_zs <-
  c(student_test_7_rev$treatment_7_2018*
      student_test_7_rev$pc1*
      student_test_7_rev$ztest_score_baseline)

student_test_7_rev$treatment_7_2019_pc1_zs <-
  c(student_test_7_rev$treatment_7_2019*
      student_test_7_rev$pc1*
      student_test_7_rev$ztest_score_baseline)

#

student_test_7_rev$treatment_7_2018_zs <-
  c(student_test_7_rev$treatment_7_2018*
      student_test_7_rev$ztest_score_baseline)

student_test_7_rev$treatment_7_2019_zs <-
  c(student_test_7_rev$treatment_7_2019*
      student_test_7_rev$ztest_score_baseline)

#

student_test_7_rev$pc1_2018 <-
  c(student_test_7_rev$timing_end*
      student_test_7_rev$pc1)

student_test_7_rev$pc1_2019 <-
  c(student_test_7_rev$timing_fus*
      student_test_7_rev$pc1)

#

student_test_7_rev$pc1_zs_2018 <-
  c(student_test_7_rev$timing_end*
      student_test_7_rev$pc1*
      student_test_7_rev$ztest_score_baseline)

student_test_7_rev$pc1_zs_2019 <-
  c(student_test_7_rev$timing_fus*
      student_test_7_rev$pc1*
      student_test_7_rev$ztest_score_baseline)

#Column 1-1

reg6_1 <- lm_robust(irt_score_rev~
                      treatment_7_2018+
                      treatment_7_2019+
                      treatment_7_2018_pc1+
                      treatment_7_2019_pc1+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018+
                      zscore_baseline_2019+
                      Age_b+
                      Sex+
                      Repeated_2018+
                      Repeated_2019+
                      Shift_2018+
                      Shift_2019+
                      no_elder+
                      no_younger+
                      Student_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      Math_textbook_e+
                      
                      Multigrade_G7+
                      Multigrade_G7_2019_r+
                      Multigrade_G8_2019_r+
                      Number_total_Students+
                      Number_G7_Students_AM_r+
                      Number_G7_Students_PM_r+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      
                      Highest_Degree_professorate+
                      Highest_Degree_bachelor+
                      Highest_Degree_master+
                      Highest_Degree_other+
                      
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+
                      School_Facility__4+
                      School_Facility__5+
                      School_Facility__6+
                      School_Facility__7+
                      School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_7_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg6_1)

#column 2

reg6_2 <- lm_robust(irt_score_rev~
                      treatment_7_2018+
                      treatment_7_2019+
                      treatment_7_2018_zs+
                      treatment_7_2019_zs+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018+
                      zscore_baseline_2019+
                      Age_b+
                      Sex+
                      Repeated_2018+
                      Repeated_2019+
                      Shift_2018+
                      Shift_2019+
                      no_elder+
                      no_younger+
                      Student_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      Math_textbook_e+
                      
                      Multigrade_G7+
                      Multigrade_G7_2019_r+
                      Multigrade_G8_2019_r+
                      Number_total_Students+
                      Number_G7_Students_AM_r+
                      Number_G7_Students_PM_r+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      
                      Highest_Degree_professorate+
                      Highest_Degree_bachelor+
                      Highest_Degree_master+
                      Highest_Degree_other+
                      
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+
                      School_Facility__4+
                      School_Facility__5+
                      School_Facility__6+
                      School_Facility__7+
                      School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_7_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg6_2)

#Column 3

reg6_3 <- lm_robust(irt_score_rev~
                      treatment_7_2018+
                      treatment_7_2019+
                      treatment_7_2018_pc1+
                      treatment_7_2019_pc1+
                      treatment_7_2018_zs+
                      treatment_7_2019_zs+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018+
                      zscore_baseline_2019+
                      Age_b+
                      Sex+
                      Repeated_2018+
                      Repeated_2019+
                      Shift_2018+
                      Shift_2019+
                      no_elder+
                      no_younger+
                      Student_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      Math_textbook_e+
                      
                      Multigrade_G7+
                      Multigrade_G7_2019_r+
                      Multigrade_G8_2019_r+
                      Number_total_Students+
                      Number_G7_Students_AM_r+
                      Number_G7_Students_PM_r+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      
                      Highest_Degree_professorate+
                      Highest_Degree_bachelor+
                      Highest_Degree_master+
                      Highest_Degree_other+
                      
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+
                      School_Facility__4+
                      School_Facility__5+
                      School_Facility__6+
                      School_Facility__7+
                      School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_7_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg6_3)

#Column 4

reg6_4 <- lm_robust(irt_score_rev~
                      treatment_7_2018+
                      treatment_7_2019+
                      treatment_7_2018_pc1+
                      treatment_7_2019_pc1+
                      treatment_7_2018_zs+
                      treatment_7_2019_zs+
                      treatment_7_2018_pc1_zs+
                      treatment_7_2019_pc1_zs+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018+
                      zscore_baseline_2019+
                      Age_b+
                      Sex+
                      Repeated_2018+
                      Repeated_2019+
                      Shift_2018+
                      Shift_2019+
                      no_elder+
                      no_younger+
                      Student_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      pc1_zs_2018+
                      pc1_zs_2019+
                      
                      Math_textbook_e+
                      
                      Multigrade_G7+
                      Multigrade_G7_2019_r+
                      Multigrade_G8_2019_r+
                      Number_total_Students+
                      Number_G7_Students_AM_r+
                      Number_G7_Students_PM_r+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      
                      Highest_Degree_professorate+
                      Highest_Degree_bachelor+
                      Highest_Degree_master+
                      Highest_Degree_other+
                      
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+
                      School_Facility__4+
                      School_Facility__5+
                      School_Facility__6+
                      School_Facility__7+
                      School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_7_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg6_4)

###Table 7####

#column 3-2

student_test_7_rev$Time_Study_HW_G7[
  is.na(student_test_7_rev$Time_Study_HW_G7)]<-1

unique(student_test_7_rev$Time_Study_HW_G7)

reg7_3_2 <- lm_robust(Time_Study_HW_G7~
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        Age_b+
                        Sex+
                        Repeated_2018+
                        Repeated_2019+
                        Shift_2018+
                        Shift_2019+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Cooking_non_fire+
                        Math_textbook_e+
                        
                        Multigrade_G7+
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_3_2)

student_test_7_rev_end_c  <- subset(student_test_7_rev,
                                    timing_end==1&
                                      treatment_7_2018==0)

mean(student_test_7_rev_end_c$Time_Study_HW_G7, na.rm=T)

###Figure 1####

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_7_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_7_p <- cbind(student_test_7_baseline$Item_House__1,
                          student_test_7_baseline$Item_House__2,
                          student_test_7_baseline$Item_House__3,
                          student_test_7_baseline$Item_House__4,
                          student_test_7_baseline$Item_House__5,
                          student_test_7_baseline$Item_House__6,
                          student_test_7_baseline$Item_House__7,
                          student_test_7_baseline$Item_House__8,
                          student_test_7_baseline$Cooking_Equipment__1_r)

row.names(student_test_7_p) <- c(student_test_7_baseline$ID_IE_test)

student_test_7_p <- na.omit(student_test_7_p)

result <- prcomp(student_test_7_p, scale=T, na.rm=T, center=T)

summary(result)

student_test_7_p <- data.frame(student_test_7_p)

result2 <- round(result$rotation, 3)
result3 <- round(result$x, digits=3)

pc1 <- result$x[,1] # first principal component
pc2 <- result$x[,2] # second principal component
pc3 <- result$x[,3] # third principal component

student_id <- rownames(student_test_7_p)

student_test_7_p <- cbind(student_id, student_test_7_p, pc1, pc2, pc3)

row_name <- rbind("smartphone", "Computer", "Refrig", "Car", "TV", "Water", "Electricity",
                  "Toilet", "Non_Wood")

cor_pc1 <- rbind(cor(student_test_7_p$pc1, student_test_7_p$X1),
                 cor(student_test_7_p$pc1, student_test_7_p$X2),
                 cor(student_test_7_p$pc1, student_test_7_p$X3),
                 cor(student_test_7_p$pc1, student_test_7_p$X4),
                 cor(student_test_7_p$pc1, student_test_7_p$X5),
                 cor(student_test_7_p$pc1, student_test_7_p$X6),
                 cor(student_test_7_p$pc1, student_test_7_p$X7),
                 cor(student_test_7_p$pc1, student_test_7_p$X8),
                 cor(student_test_7_p$pc1, student_test_7_p$X9))

cor_table <- cbind.data.frame(row_name, cor_pc1)

student_test_7_p <- subset(student_test_7_p,
                           select=c("student_id",
                                    "pc1"))

write.csv(student_test_7_p,
          "student_test_7_p.csv",
          row.names = F)

student_test_7_baseline <- merge(student_test_7_baseline, student_test_7_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("student_id"), all=F)

#

student_test_7_baseline$level_pc <- c(0)

student_test_7_baseline$level_pc <- ifelse(student_test_7_baseline$pc1<(-1),
                                  "1. Low",student_test_7_baseline$level_pc)

student_test_7_baseline$level_pc <- ifelse(student_test_7_baseline$pc1>=(-1)&
                                             student_test_7_baseline$pc1<(1.3),
                                  "2. Medium",student_test_7_baseline$level_pc)

student_test_7_baseline$level_pc <- ifelse(student_test_7_baseline$pc1>=(1.3),
                                  "3. High",student_test_7_baseline$level_pc)

#

g <- ggplot(aes(x=level_pc, y = total_score_7), 
            data=student_test_7_baseline)

g <- g + xlab("Student household economic status") 
g <- g + ylab("Total baseline score") 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(axis.title.x = element_text(size = 9))

g <- g + geom_boxplot()

plot(g)

ggsave(file = "figure_boxplot_es_g7.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

###Figure 2####

student_test_7_baseline$treatment_7r <- student_test_7_baseline$treatment_7

student_test_7_baseline$treatment_7r <- 
  replace(student_test_7_baseline$treatment_7r, 
          which(student_test_7_baseline$treatment_7r==0), "Control")

student_test_7_baseline$treatment_7r <- 
  replace(student_test_7_baseline$treatment_7r, 
          which(student_test_7_baseline$treatment_7r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_test_7_baseline)
g <- g + geom_line(aes(ztest_score_baseline, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"),
                   adjust=1.5)
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Z Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Z_score_dens_baseline_g7.png", plot = g,
       dpi = 1000, width = 6, height = 4)

###Figure 3 and 4####

#Draw Figure 3

student_test_7_end <- subset(student_test_7_rev,
                             timing_end==1)

student_test_7_end$treatment_7r <- student_test_7_end$treatment_7

student_test_7_end$treatment_7r <- replace(student_test_7_end$treatment_7r, 
                                           which(student_test_7_end$treatment_7r==0), "Control")

student_test_7_end$treatment_7r <- replace(student_test_7_end$treatment_7r, 
                                           which(student_test_7_end$treatment_7r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

max(student_test_7_end$irt_score_rev)
min(student_test_7_end$irt_score_rev)

g <- ggplot(student_test_7_end)
g <- g + geom_line(aes(irt_score_rev, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-1.7,5))
g <- g + xlab("IRT Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_plot_end_g7.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#Draw Figure 4

student_test_7_fus <- subset(student_test_7_rev,
                             timing_fus==1)

student_test_7_fus$treatment_7r <- student_test_7_fus$treatment_7

student_test_7_fus$treatment_7r <- replace(student_test_7_fus$treatment_7r, 
                                           which(student_test_7_fus$treatment_7r==0), "Control")

student_test_7_fus$treatment_7r <- replace(student_test_7_fus$treatment_7r, 
                                           which(student_test_7_fus$treatment_7r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

max(student_test_7_fus$irt_score_rev)
min(student_test_7_fus$irt_score_rev)

g <- ggplot(student_test_7_fus)
g <- g + geom_line(aes(irt_score_rev, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-1.2,5))
g <- g + scale_y_continuous(breaks=seq(0,0.4,by=0.1),limits=c(0,0.4))
g <- g + xlab("IRT Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_plot_fus_g7.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

###Figure C.1 in Appendix C####
###compute composite index of household economic status

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_7_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_7_p <- cbind(student_test_7_baseline$Item_House__1,
                          student_test_7_baseline$Item_House__2,
                          student_test_7_baseline$Item_House__3,
                          student_test_7_baseline$Item_House__4,
                          student_test_7_baseline$Item_House__5,
                          student_test_7_baseline$Item_House__6,
                          student_test_7_baseline$Item_House__7,
                          student_test_7_baseline$Item_House__8,
                          student_test_7_baseline$Cooking_Equipment__1_r)

row.names(student_test_7_p) <- c(student_test_7_baseline$ID_IE_test)

student_test_7_p <- na.omit(student_test_7_p)

result <- prcomp(student_test_7_p, scale=T, na.rm=T, center=T)

summary(result)

student_test_7_p <- data.frame(student_test_7_p)

result2 <- round(result$rotation, 3)
result3 <- round(result$x, digits=3)

pc1 <- result$x[,1] # first principal component
pc2 <- result$x[,2] # second principal component
pc3 <- result$x[,3] # third principal component

student_id <- rownames(student_test_7_p)

student_test_7_p <- cbind(student_id, student_test_7_p, pc1, pc2, pc3)

row_name <- rbind("smartphone", "Computer", "Refrig", "Car", "TV", "Water", "Electricity",
                  "Toilet", "Non_Wood")

cor_pc1 <- rbind(cor(student_test_7_p$pc1, student_test_7_p$X1),
                 cor(student_test_7_p$pc1, student_test_7_p$X2),
                 cor(student_test_7_p$pc1, student_test_7_p$X3),
                 cor(student_test_7_p$pc1, student_test_7_p$X4),
                 cor(student_test_7_p$pc1, student_test_7_p$X5),
                 cor(student_test_7_p$pc1, student_test_7_p$X6),
                 cor(student_test_7_p$pc1, student_test_7_p$X7),
                 cor(student_test_7_p$pc1, student_test_7_p$X8),
                 cor(student_test_7_p$pc1, student_test_7_p$X9))

cor_table <- cbind.data.frame(row_name, cor_pc1)

student_test_7_p <- subset(student_test_7_p,
                           select=c("student_id",
                                    "pc1"))

student_test_7_baseline <- merge(student_test_7_baseline, student_test_7_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("student_id"), all=F)

#Draw Figure C.1 in Appendix C

student_test_7_baseline$treatment_7r <- student_test_7_baseline$treatment_7

student_test_7_baseline$treatment_7r <- 
  replace(student_test_7_baseline$treatment_7r, 
          which(student_test_7_baseline$treatment_7r==0), "Control")

student_test_7_baseline$treatment_7r <- 
  replace(student_test_7_baseline$treatment_7r, 
          which(student_test_7_baseline$treatment_7r=="1"), "Treatment")

g <- ggplot(student_test_7_baseline)
g <- g + geom_line(aes(pc1, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat="density")
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("First Principal Component") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + geom_vline(xintercept = (-1.00), size = 0.5, linetype = "dashed")
g <- g + geom_vline(xintercept = 1.3, size = 0.5, linetype = "dashed")
g <- g + theme(legend.position=c("bottom"))
g <- g + guides(color = guide_legend(override.aes = list(size = 0.5)))

plot(g)
ggsave(file = "pca_asset_g7.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

###Tables F.1 and F.3, Figure 1 and 2 in Appendix F#####

#Table F.1

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

#column 1-1

reg_f1_1 <- lm_robust(ztest_score_7~
                        
                        treatment_7+ztest_score_baseline+
                        department*urban.rural+
                        Age+Sex+no_elder+no_younger+
                        Shift+Repeated+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Number_G7_Students_b+
                        Multigrade_G7+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        Work_Shift_G7_both+
                        Teaching_other_subject_G7+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        Internet_Place__3+Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      data=student_test_7_end, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f1_1)

#column 2-1

reg_f2_1 <- lm_robust(ztest_score_number_7~
                        
                        treatment_7+ztest_score_baseline+
                        department*urban.rural+
                        Age+Sex+no_elder+no_younger+
                        Shift+Repeated+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Number_G7_Students_b+
                        Multigrade_G7+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        Work_Shift_G7_both+
                        Teaching_other_subject_G7+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        Internet_Place__3+Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      data=student_test_7_end, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f2_1)

#column 3-1

reg_f3_1 <- lm_robust(ztest_score_function_7~
                        
                        treatment_7+ztest_score_baseline+
                        department*urban.rural+
                        Age+Sex+no_elder+no_younger+
                        Shift+Repeated+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Number_G7_Students_b+
                        Multigrade_G7+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        Work_Shift_G7_both+
                        Teaching_other_subject_G7+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        Internet_Place__3+Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      data=student_test_7_end, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f3_1)

#column 4-1

reg_f4_1 <- lm_robust(ztest_score_geometry_7~
                        
                        treatment_7+ztest_score_baseline+
                        department*urban.rural+
                        Age+Sex+no_elder+no_younger+
                        Shift+Repeated+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Number_G7_Students_b+
                        Multigrade_G7+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        Work_Shift_G7_both+
                        Teaching_other_subject_G7+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        Internet_Place__3+Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      data=student_test_7_end, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f4_1)

#column 5-1

reg_f5_1_k <- lm_robust(ztest_score_knowledge_7~
                          
                          treatment_7+ztest_score_baseline+
                          department*urban.rural+
                          Age+Sex+no_elder+no_younger+
                          Shift+Repeated+
                          Student_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          
                          Number_total_Students+
                          Number_G7_Students_b+
                          Multigrade_G7+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_professorate+
                          Highest_Degree_bachelor+
                          Highest_Degree_master+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          
                          
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          
                          Year_Start_teach_r+
                          Work_Shift_G7_both+
                          Teaching_other_subject_G7+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+School_Facility__4+
                          School_Facility__5+School_Facility__6+
                          School_Facility__7+School_Facility__8+
                          Internet_Place__3+Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20, 
                        data=student_test_7_end, 
                        clusters = ID_IE, se_type = "stata")

summary(reg_f5_1_k)

#column 6-1

reg_f6_1_A <- lm_robust(ztest_score_application_7~
                          
                          treatment_7+ztest_score_baseline+
                          department*urban.rural+
                          Age+Sex+no_elder+no_younger+
                          Shift+Repeated+
                          Student_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          
                          Number_total_Students+
                          Number_G7_Students_b+
                          Multigrade_G7+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_professorate+
                          Highest_Degree_bachelor+
                          Highest_Degree_master+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          
                          
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          
                          Year_Start_teach_r+
                          Work_Shift_G7_both+
                          Teaching_other_subject_G7+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+School_Facility__4+
                          School_Facility__5+School_Facility__6+
                          School_Facility__7+School_Facility__8+
                          Internet_Place__3+Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20, 
                        data=student_test_7_end, 
                        clusters = ID_IE, se_type = "stata")

summary(reg_f6_1_A)

#

student_test_7_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

student_test_7_fus_t <- 
  subset(student_test_7_fus, treatment_7==1)

student_test_7_fus_c <- 
  subset(student_test_7_fus, treatment_7==0)

nrow(student_test_7_fus_t)
nrow(student_test_7_fus_c)

#column 2-1

reg_f2_1 <- lm_robust(ztest_score_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r, 
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f2_1)

#column 2-2

reg_f2_2 <- lm_robust(ztest_score_num_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r, 
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f2_2)

#column 3-2

reg_f3_2 <- lm_robust(ztest_score_fun_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r, 
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f3_2)

#column 4-2

reg_f4_2 <- lm_robust(ztest_score_geo_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4, 
                      
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f4_2)

#column 5-2

reg_f5_2 <- lm_robust(ztest_score_knowledge_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4, 
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f5_2)

#column 6-2

reg_f6_2 <- lm_robust(ztest_score_application_7~
                        
                        treatment_7+
                        ztest_score_baseline+
                        department*urban.rural+
                        Age_b+Sex+
                        no_elder+
                        no_younger+
                        Shift_2018+
                        Shift_2019+
                        Repeated_b+
                        repeated_2019+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Math_textbook_e+
                        
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        
                        Number_total_Students_2018+
                        No_Students_Ttl_G7_b+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r, 
                      data=student_test_7_fus, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_f6_2)

#Table F.2 

#Column 3

student_test_7_rev$Family_teach_study[
  is.na(student_test_7_rev$Family_teach_study)] <- 1

reg_f2_3 <- lm_robust(Family_teach_study~
                       
                       treatment_7_2018+
                       treatment_7_2019+
                       timing_fus+
                       department*urban.rural+
                       zscore_baseline_2018+
                       zscore_baseline_2019+
                       
                       Age_b+
                       Sex+
                       Repeated+
                       Shift+
                       no_elder+
                       no_younger+
                       Student_desk_home+
                       Math_textbook_LY_e+
                       Math_Notebook_LY_e+
                       Item_House__1+
                       Item_House__2+
                       Item_House__3+
                       Item_House__4+
                       Item_House__5+
                       Item_House__6+
                       Item_House__7+
                       Item_House__8+
                       Cooking_non_fire+
                       Math_textbook_e+
                       
                       Multigrade_G7+
                       Multigrade_G7_2019_r+
                       Multigrade_G8_2019_r+
                       Number_total_Students+
                       Number_G7_Students_AM_r+
                       Number_G7_Students_PM_r+
                       
                       Teacher_Sex+
                       Teacher_Age+
                       
                       Highest_Degree_professorate+
                       Highest_Degree_bachelor+
                       Highest_Degree_master+
                       Highest_Degree_other+
                       
                       Teachr_Qualification__2+
                       Teachr_Qualification__3+
                       Teachr_Qualification__4+
                       Teachr_Qualification__6+
                       Teachr_Qualification__7+
                       
                       Teachr_Apontmt_Post__2+
                       Teachr_Apontmt_Post__3+
                       Teachr_Apontmt_Post__4+
                       
                       Year_Start_teach_r+
                       
                       HM_Sex+
                       HM_Age+
                       Year_HM_ttl+
                       Year_HM_this_school+
                       
                       HM_Highest_Degree_highschool+
                       HM_Highest_Degree_master+
                       HM_Highest_Degree_professorate+
                       HM_Highest_Degree_other+
                       
                       School_Facility__2+
                       School_Facility__3+
                       School_Facility__4+
                       School_Facility__5+
                       School_Facility__6+
                       School_Facility__7+
                       School_Facility__8+
                       
                       Donor_Project__2+
                       Donor_Project__3+
                       Donor_Project__4+
                       Donor_Project__5+
                       Donor_Project__6+
                       Donor_Project__7+
                       Donor_Project__8+
                       Donor_Project__9+
                       Donor_Project__10+
                       Donor_Project__11+
                       Donor_Project__12+
                       Donor_Project__13+
                       Donor_Project__14+
                       Donor_Project__15+
                       Donor_Project__16+
                       Donor_Project__17+
                       Donor_Project__18+
                       Donor_Project__19+
                       Donor_Project__20, 
                     
                     data=student_test_7_rev,
                     clusters = ID_IE, se_type = "stata")

summary(reg_f2_3)

student_test_7_rev_end_c <- subset(student_test_7_rev,
                                   timing_end==1&
                                     treatment_7_2018==0)

mean(student_test_7_rev_end_c$Family_teach_study)

#Figure F.1

student_test_7_rev_end <- subset(student_test_7_rev,
                                 timing_end==1)

student_test_7_rev_end$quantile_reg <-
  student_test_7_rev_end$treatment_7_2018

reg_f_1 <- rq(irt_score_rev~quantile_reg, 
              tau=seq(0.1, 0.9, by=0.1),
              data=student_test_7_rev_end)

summary(reg_f_1)

plot(summary(reg_f_1), parm="quantile_reg")

#Figure F.2

student_test_7_rev_fus <- subset(student_test_7_rev,
                                 timing_fus==1)

student_test_7_rev_fus$quantile_reg <-
  student_test_7_rev_fus$treatment_7_2019

reg_f_2 <- rq(irt_score_rev~quantile_reg, 
              tau=seq(0.1, 0.9, by=0.1),
              data=student_test_7_rev_fus)

summary(reg_f_2)

plot(summary(reg_f_2), parm="quantile_reg")

###Figure G.1 and G.3, Tables G.1 and G.2 in Appendix G#####

student_test_7_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

student_test_7_p <- 
  read.csv("student_test_7_p.csv", header=T, stringsAsFactors = F)

#merge data frames

student_test_7_fus <- merge(student_test_7_fus, student_test_7_p,
                            by.x=c("ID_IE_test"), 
                            by.y=c("student_id"), all=F)

#coding mother's education variable

student_test_7_fus$Ed_Mthr_Uni_f_r <- 
  ifelse(student_test_7_fus$Ed_Mthr_Uni_f==1,1,0)

student_test_7_fus$Ed_Mthr_Uni_f_r <- 
  ifelse(student_test_7_fus$Ed_Mthr_High_f==2&
           is.na(student_test_7_fus$Ed_Mthr_Uni_f==TRUE),0,
         student_test_7_fus$Ed_Mthr_Uni_f_r)

student_test_7_fus$Ed_Mthr_Uni_f_r <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f==1|
           student_test_7_fus$Ed_Mthr_G1to9_f==2|
           student_test_7_fus$Ed_Mthr_G1to9_f==3|
           student_test_7_fus$Ed_Mthr_G1to9_f==4|
           student_test_7_fus$Ed_Mthr_G1to9_f==5|
           student_test_7_fus$Ed_Mthr_G1to9_f==6|
           student_test_7_fus$Ed_Mthr_G1to9_f==7|
           student_test_7_fus$Ed_Mthr_G1to9_f==8|
           student_test_7_fus$Ed_Mthr_G1to9_f==9|
           (student_test_7_fus$Ed_Mthr_G1to9_f==10&
              (student_test_7_fus$Ed_Mthr_High_f==2|
                 student_test_7_fus$Ed_Mthr_High_f==3)),0,
         student_test_7_fus$Ed_Mthr_Uni_f_r)

student_test_7_fus$Ed_Mthr_High_f_r <- 
  ifelse(student_test_7_fus$Ed_Mthr_High_f==1,1,0)

student_test_7_fus$Ed_Mthr_High_f_r <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f==1|
           student_test_7_fus$Ed_Mthr_G1to9_f==2|
           student_test_7_fus$Ed_Mthr_G1to9_f==3|
           student_test_7_fus$Ed_Mthr_G1to9_f==4|
           student_test_7_fus$Ed_Mthr_G1to9_f==5|
           student_test_7_fus$Ed_Mthr_G1to9_f==6|
           student_test_7_fus$Ed_Mthr_G1to9_f==7|
           student_test_7_fus$Ed_Mthr_G1to9_f==8|
           student_test_7_fus$Ed_Mthr_G1to9_f==9,0,
         student_test_7_fus$Ed_Mthr_High_f_r)

student_test_7_fus$Mother_non_ed <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f==1,1,0)

student_test_7_fus$Mother_primary_drop_out <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f>=2&
           student_test_7_fus$Ed_Mthr_G1to9_f<=6,1,0)

student_test_7_fus$Mother_primary_reach_last_grade <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f==7,1,0)

student_test_7_fus$Mother_lower_secondary <- 
  ifelse(student_test_7_fus$Ed_Mthr_G1to9_f>=8&
           student_test_7_fus$Ed_Mthr_G1to9_f<=10&
           student_test_7_fus$Ed_Mthr_High_f_r==0&
           student_test_7_fus$Ed_Mthr_Uni_f_r==0,1,0)

student_test_7_fus$Mother_non_ed[is.na(student_test_7_fus$Mother_non_ed)]<-0
student_test_7_fus$Mother_primary_drop_out[is.na(student_test_7_fus$Mother_primary_drop_out)]<-0
student_test_7_fus$Mother_primary_reach_last_grade[
  is.na(student_test_7_fus$Mother_primary_reach_last_grade)]<-0
student_test_7_fus$Mother_lower_secondary[
  is.na(student_test_7_fus$Mother_lower_secondary)]<-0

student_test_7_fus$Ed_Mthr_Uni_f_r[is.na(student_test_7_fus$Ed_Mthr_Uni_f_r)]<-0
student_test_7_fus$Ed_Mthr_High_f_r[is.na(student_test_7_fus$Ed_Mthr_High_f_r)]<-0

#creating variables for mothers' education

student_test_7_fus$ed_level_mother <- c(0)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Mother_non_ed==1,
         1, student_test_7_fus$ed_level_mother)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Mother_primary_drop_out==1,
         2, student_test_7_fus$ed_level_mother)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Mother_primary_reach_last_grade==1,
         3, student_test_7_fus$ed_level_mother)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Mother_lower_secondary==1,
         4, student_test_7_fus$ed_level_mother)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Ed_Mthr_High_f_r==1,
         5, student_test_7_fus$ed_level_mother)

student_test_7_fus$ed_level_mother <- 
  ifelse(student_test_7_fus$Ed_Mthr_Uni_f_r==1,
         6, student_test_7_fus$ed_level_mother)

unique(student_test_7_fus$ed_level_mother)

#

student_test_7_fus_characteristics <- 
  subset(student_test_7_fus,
         select = c("ID_IE_test",
                    "ed_level_mother",
                    "pc1"))

student_test_7_baseline <- 
  read.csv("student_test_7_baseline.csv", header=T, stringsAsFactors = F)

student_test_7_fus_characteristics <- merge(student_test_7_fus_characteristics, 
                                            student_test_7_baseline,
                                            by.x=c("ID_IE_test"), 
                                            by.y=c("ID_IE_test"), all=F)

student_test_7_fus_characteristics <- subset(student_test_7_fus_characteristics,
                                             !(ed_level_mother==0))

#Draw Figure G.2

student_test_7_fus_characteristics$ed_level_mother_r <- c(0)

student_test_7_fus_characteristics$ed_level_mother_r <- 
  replace(student_test_7_fus_characteristics$ed_level_mother_r, 
          which(student_test_7_fus_characteristics$ed_level_mother==1|
                  student_test_7_fus_characteristics$ed_level_mother==2), 
          "(A)")

student_test_7_fus_characteristics$ed_level_mother_r <- 
  replace(student_test_7_fus_characteristics$ed_level_mother_r, 
          which(student_test_7_fus_characteristics$ed_level_mother==3), 
          "(B)")

student_test_7_fus_characteristics$ed_level_mother_r <- 
  replace(student_test_7_fus_characteristics$ed_level_mother_r, 
          which(student_test_7_fus_characteristics$ed_level_mother==4), 
          "(C)")

student_test_7_fus_characteristics$ed_level_mother_r <- 
  replace(student_test_7_fus_characteristics$ed_level_mother_r, 
          which(student_test_7_fus_characteristics$ed_level_mother==5|
                  student_test_7_fus_characteristics$ed_level_mother==6), 
          "(D)")

#

freq.table <- prop.table((xtabs(~Family_teach_study+ed_level_mother_r, 
                                data=student_test_7_fus_characteristics)), 2)
freq.table.df <- as.data.frame(freq.table)
freq.table.df$Freq <- round(freq.table.df$Freq,3)

percent_r <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

percent_r(freq.table.df$Freq)

g <- ggplot(freq.table.df, aes(x = ed_level_mother_r, 
                               y = Freq, fill=Family_teach_study))

g <- g + geom_bar(stat = "identity", width=0.5, position = "fill")
g <- g + geom_text(aes(label = percent_r(Freq)), size = 3, 
                   hjust = 0.5, position = position_stack(vjust=0.5)) 
g <- g + theme(axis.title.x = element_text(size=11))
g <- g + theme(axis.title.y = element_blank())
g <- g + xlab("Educational level of mother")         
g <- g + theme(axis.text.x = element_text(size=9))
g <- g + theme(axis.text.y = element_text(size=9))

g <- g + theme(legend.title = element_text(size=9),
               legend.text = element_text(size=9)) 

g <- g + scale_fill_manual(
  name="Frequency of support",
  breaks=c(1, 2, 3, 4),
  labels=c("Never", 
           "Once", 
           "Sometimes",
           "Everyday"),
  values=c("1"="gray40", 
           "2"="gray50", 
           "3"="gray65",
           "4"="gray75"))

plot(g)

ggsave(file = "figure_support_mother_ed.png", plot = g,
       dpi = 1000, width = 6, height = 4) 

student_test_7_fus_characteristics_A <- 
  subset(student_test_7_fus_characteristics,
         ed_level_mother_r=="(A)") 

student_test_7_fus_characteristics_B <- 
  subset(student_test_7_fus_characteristics,
         ed_level_mother_r=="(B)") 

student_test_7_fus_characteristics_C <- 
  subset(student_test_7_fus_characteristics,
         ed_level_mother_r=="(C)") 

student_test_7_fus_characteristics_D <- 
  subset(student_test_7_fus_characteristics,
         ed_level_mother_r=="(D)") 

nrow(student_test_7_fus_characteristics_A)
nrow(student_test_7_fus_characteristics_B)
nrow(student_test_7_fus_characteristics_C)
nrow(student_test_7_fus_characteristics_D)

#Figure G.1

student_test_7_fus_characteristics$level_pc <- c(0)

student_test_7_fus_characteristics$level_pc <- 
  ifelse(student_test_7_fus_characteristics$pc1<(-1),
         "1. Low",student_test_7_fus_characteristics$level_pc)

student_test_7_fus_characteristics$level_pc <- 
  ifelse(student_test_7_fus_characteristics$pc1>=(-1)&
           student_test_7_fus_characteristics$pc1<(1.3),
         "2. Medium",student_test_7_fus_characteristics$level_pc)

student_test_7_fus_characteristics$level_pc <- 
  ifelse(student_test_7_fus_characteristics$pc1>=(1.3),
         "3. High",student_test_7_fus_characteristics$level_pc)
#


g <- g + geom_bar(stat = "identity", width=0.5, position = "fill")
g <- g + geom_text(aes(label = percent_r(Freq)), size = 3, 
                   hjust = 0.5, position = position_stack(vjust=0.5)) 
g <- g + theme(axis.title.x = element_text(size=11))
g <- g + theme(axis.title.y = element_blank())
g <- g + xlab("Educational level of mother")         
g <- g + theme(axis.text.x = element_text(size=9))
g <- g + theme(axis.text.y = element_text(size=9))

g <- g + theme(legend.title = element_text(size=9),
               legend.text = element_text(size=9)) 

g <- g + scale_fill_manual(
  values=c("1. Low"="gray30", "2. Medium"="gray50", 
           "3. High"="gray75"), 
  name = "Student household economic status")


freq.table <- prop.table((xtabs(~level_pc+ed_level_mother_r, 
                                data=student_test_7_fus_characteristics)), 2)
freq.table.df <- as.data.frame(freq.table)
freq.table.df$Freq <- round(freq.table.df$Freq,3)


g <- ggplot(freq.table.df, aes(x = ed_level_mother_r, 
                               y = Freq, fill=level_pc))
g <- g + geom_bar(stat = "identity", width=0.5, position = "fill")
g <- g + geom_text(aes(label = percent(Freq)), size = 3, 
                   hjust = 0.5, position = position_stack(vjust=0.5)) 
g <- g + scale_fill_manual(
  values=c("1. Low"="gray40", "2. Medium"="gray60", 
           "3. High"="gray75"), 
  name = "Student household economic status")
g <- g + theme(axis.title.x = element_text(size=11))
g <- g + theme(axis.title.y = element_blank())
g <- g + xlab("Educational level of mother")         
g <- g + theme(axis.text.x = element_text(size=9))
g <- g + theme(axis.text.y = element_text(size=9))
g <- g + theme(plot.caption=element_text(size=9, 
                                         hjust=0, face="italic", color="black"))
g <- g + theme(legend.title = element_text(size=9)) 

plot(g)

ggsave(file = "econ_status_mother_ed.png", plot = g,
       dpi = 1000, width = 6, height = 4) 

#Figure G.3
student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

student_test_7_p <- 
  read.csv("student_test_7_p.csv", header=T, stringsAsFactors = F)

student_test_7_end <- merge(student_test_7_end, student_test_7_p,
                            by.x=c("ID_IE_test"), 
                            by.y=c("student_id"), all=F)

student_test_7_end$treatment_7r <- student_test_7_end$treatment_7

student_test_7_end$treatment_7r <- replace(student_test_7_end$treatment_7r, 
                                           which(student_test_7_end$treatment_7r==0), "Control")

student_test_7_end$treatment_7r <- replace(student_test_7_end$treatment_7r, 
                                           which(student_test_7_end$treatment_7r=="1"), "Treatment")

#Low status

student_test_7_low <- subset(student_test_7_end,
                             pc1<=(-1))

cols <- c("Treatment"="red", "Control"="blue") 

min(student_test_7_low$ztest_score_7)
min(student_test_7_low$ztest_score_baseline)

#baseline

g <- ggplot(student_test_7_low)
g <- g + geom_line(aes(ztest_score_baseline, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-2,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_base_g7_low.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#end-line

g <- ggplot(student_test_7_low)
g <- g + geom_line(aes(ztest_score_7, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-1.5,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_end_g7_low.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#medium_status

student_test_7_med <- subset(student_test_7_end,
                             student_test_7_end$pc1>-1&
                               student_test_7_end$pc1<=1.3)

cols <- c("Treatment"="red", "Control"="blue") 

#baseline

g <- ggplot(student_test_7_med)
g <- g + geom_line(aes(ztest_score_baseline, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-2,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_base_g7_med.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#end-line

g <- ggplot(student_test_7_med)
g <- g + geom_line(aes(ztest_score_7, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-1.5,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_end_g7_med.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#High_status

student_test_7_hig <- subset(student_test_7_end,
                             pc1>(1.3))

#baseline

g <- ggplot(student_test_7_hig)
g <- g + geom_line(aes(ztest_score_baseline, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-2,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_base_g7_hig.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#end-line

g <- ggplot(student_test_7_hig)
g <- g + geom_line(aes(ztest_score_7, 
                       group=treatment_7r, color=treatment_7r,
                       linetype=treatment_7r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-1,5,by=1),limits=c(-1.5,5))
g <- g + scale_y_continuous(breaks=seq(0,0.45,by=0.1),limits=c(0,0.48))
g <- g + xlab("Standardized Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_point_end_g7_hig.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

#Table G.1

student_test_7_rev$l_pc_low <- ifelse(student_test_7_rev$pc1<(-1),
                                      1,0)

student_test_7_rev$l_pc_high <- ifelse(student_test_7_rev$pc1>=(1.3),
                                       1,0)

#

student_test_7_rev$l_pc_low_end <- 
  student_test_7_rev$l_pc_low*student_test_7_rev$timing_end

student_test_7_rev$l_pc_low_fus <- 
  student_test_7_rev$l_pc_low*student_test_7_rev$timing_fus

#

student_test_7_rev$l_pc_high_end <- 
  student_test_7_rev$l_pc_high*student_test_7_rev$timing_end

student_test_7_rev$l_pc_high_fus <- 
  student_test_7_rev$l_pc_high*student_test_7_rev$timing_fus

#

student_test_7_rev$l_pc_low_end_t <- 
  student_test_7_rev$l_pc_low_end*
  student_test_7_rev$treatment_7

student_test_7_rev$l_pc_low_fus_t <- 
  student_test_7_rev$l_pc_low_fus*
  student_test_7_rev$treatment_7

#

student_test_7_rev$l_pc_high_end_t <- 
  student_test_7_rev$l_pc_high_end*
  student_test_7_rev$treatment_7

student_test_7_rev$l_pc_high_fus_t <- 
  student_test_7_rev$l_pc_high_fus*
  student_test_7_rev$treatment_7

#

student_test_7_rev$pc_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end

student_test_7_rev$pc_fus <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_fus

#

student_test_7_rev$I_zs_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_end

student_test_7_rev$I_zs_fus <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_fus

#

student_test_7_rev$I_zs_low_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_low_end

student_test_7_rev$I_zs_high_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_high_end

#

student_test_7_rev$I_zs_low_fus <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_low_fus

student_test_7_rev$I_zs_high_fus <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_high_fus

#

student_test_7_rev$I_zs_low_end_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_low_end

student_test_7_rev$I_zs_high_end_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_high_end

#

student_test_7_rev$I_zs_low_fus_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_low_fus

student_test_7_rev$I_zs_high_fus_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_high_fus

#

student_test_7_rev$I_pc_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_end

student_test_7_rev$I_pc_fus <- 
  student_test_7_rev$pc1*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_fus

#

student_test_7_rev$pc_zs_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end*
  student_test_7_rev$ztest_score_baseline

student_test_7_rev$pc_zs_fus <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_fus*
  student_test_7_rev$ztest_score_baseline

#

student_test_7_rev$pc_zs_end_t <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end*
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7

student_test_7_rev$pc_zs_fus_t <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_fus*
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7

#Column 1

reg_g1_1 <- lm_robust(irt_score_rev~
                       
                       treatment_7_2018+
                       treatment_7_2019+
                       timing_fus+
                       department*urban.rural+
                       
                       l_pc_low_end_t+
                       l_pc_high_end_t+
                       
                       l_pc_low_fus_t+
                       l_pc_high_fus_t+
                       
                       l_pc_low_end+
                       l_pc_high_end+
                       
                       l_pc_low_fus+
                       l_pc_high_fus+
                       
                       zscore_baseline_2018+
                       zscore_baseline_2019+
                       
                       Age+
                       Sex+
                       Repeated_2018+
                       Repeated_2019+
                       Shift_2018+
                       Shift_2019+
                       no_elder+
                       no_younger+
                       Student_desk_home+
                       Math_textbook_LY_e+
                       Math_Notebook_LY_e+
                       Math_textbook_e+
                       
                       Multigrade_G7+
                       Multigrade_G7_2019_r+
                       Multigrade_G8_2019_r+
                       Number_total_Students+
                       Number_G7_Students_AM_r+
                       Number_G7_Students_PM_r+
                       
                       Teacher_Sex+
                       Teacher_Age+
                       
                       Highest_Degree_professorate+
                       Highest_Degree_bachelor+
                       Highest_Degree_master+
                       Highest_Degree_other+
                       
                       Teachr_Qualification__2+
                       Teachr_Qualification__3+
                       Teachr_Qualification__4+
                       Teachr_Qualification__6+
                       Teachr_Qualification__7+
                       
                       Teachr_Apontmt_Post__2+
                       Teachr_Apontmt_Post__3+
                       Teachr_Apontmt_Post__4+
                       
                       Year_Start_teach_r+
                       
                       HM_Sex+
                       HM_Age+
                       Year_HM_ttl+
                       Year_HM_this_school+
                       
                       HM_Highest_Degree_highschool+
                       HM_Highest_Degree_master+
                       HM_Highest_Degree_professorate+
                       HM_Highest_Degree_other+
                       
                       School_Facility__2+
                       School_Facility__3+
                       School_Facility__4+
                       School_Facility__5+
                       School_Facility__6+
                       School_Facility__7+
                       School_Facility__8+
                       
                       Donor_Project__2+
                       Donor_Project__3+
                       Donor_Project__4+
                       Donor_Project__5+
                       Donor_Project__6+
                       Donor_Project__7+
                       Donor_Project__8+
                       Donor_Project__9+
                       Donor_Project__10+
                       Donor_Project__11+
                       Donor_Project__12+
                       Donor_Project__13+
                       Donor_Project__14+
                       Donor_Project__15+
                       Donor_Project__16+
                       Donor_Project__17+
                       Donor_Project__18+
                       Donor_Project__19+
                       Donor_Project__20, 
                     
                     data=student_test_7_rev,
                     clusters = ID_IE, se_type = "stata")

summary(reg_g1_1)

#Column 2

reg_g1_2 <- lm_robust(irt_score_rev~
                        
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        
                        l_pc_low_end_t+
                        l_pc_high_end_t+
                        
                        l_pc_low_fus_t+
                        l_pc_high_fus_t+
                        
                        I_zs_end+
                        I_zs_fus+
                        
                        l_pc_low_end+
                        l_pc_high_end+
                        
                        l_pc_low_fus+
                        l_pc_high_fus+
                        
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        
                        Age+
                        Sex+
                        Repeated_2018+
                        Repeated_2019+
                        Shift_2018+
                        Shift_2019+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Math_textbook_e+
                        
                        Multigrade_G7+
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg_g1_2)

#Column 3

reg_g1_3 <- lm_robust(irt_score_rev~
                        
                        treatment_7_2018+
                        treatment_7_2019+
                        timing_fus+
                        department*urban.rural+
                        
                        I_zs_end+
                        I_zs_low_end_t+
                        I_zs_high_end_t+
                        
                        I_zs_fus+
                        I_zs_low_fus_t+
                        I_zs_high_fus_t+
                        
                        l_pc_low_end_t+
                        l_pc_high_end_t+
                        
                        l_pc_low_fus_t+
                        l_pc_high_fus_t+
                        
                        I_zs_low_end+
                        I_zs_high_end+
                        
                        I_zs_low_fus+
                        I_zs_high_fus+
                        
                        l_pc_low_end+
                        l_pc_high_end+
                        
                        l_pc_low_fus+
                        l_pc_high_fus+
                        
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        
                        Age+
                        Sex+
                        Repeated_2018+
                        Repeated_2019+
                        Shift_2018+
                        Shift_2019+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Math_textbook_e+
                        
                        Multigrade_G7+
                        Multigrade_G7_2019_r+
                        Multigrade_G8_2019_r+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg_g1_3)

#Table G.2

student_test_7_rev$l_pc_low <- ifelse(student_test_7_rev$pc1<(-1),
                                      1,0)

student_test_7_rev$l_pc_high <- ifelse(student_test_7_rev$pc1>=(1.3),
                                       1,0)

#

student_test_7_rev$l_pc_low_end <- 
  student_test_7_rev$l_pc_low*student_test_7_rev$timing_end

#

student_test_7_rev$l_pc_high_end <- 
  student_test_7_rev$l_pc_high*student_test_7_rev$timing_end

#

student_test_7_rev$l_pc_low_end_t <- 
  student_test_7_rev$l_pc_low_end*
  student_test_7_rev$treatment_7

#

student_test_7_rev$l_pc_high_end_t <- 
  student_test_7_rev$l_pc_high_end*
  student_test_7_rev$treatment_7

#

student_test_7_rev$pc_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end

#

student_test_7_rev$I_zs_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_end

#

student_test_7_rev$I_zs_low_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_low_end

student_test_7_rev$I_zs_high_end <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$l_pc_high_end

#

student_test_7_rev$I_zs_low_end_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_low_end

student_test_7_rev$I_zs_high_end_t <- 
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7*
  student_test_7_rev$l_pc_high_end

#

student_test_7_rev$I_pc_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$treatment_7*
  student_test_7_rev$timing_end

#

student_test_7_rev$pc_zs_end <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end*
  student_test_7_rev$ztest_score_baseline

#

student_test_7_rev$pc_zs_end_t <- 
  student_test_7_rev$pc1*
  student_test_7_rev$timing_end*
  student_test_7_rev$ztest_score_baseline*
  student_test_7_rev$treatment_7

#

student_test_7_rev_end <- subset(student_test_7_rev,
                                 timing_end==1)

#Column 1

reg_g2_1 <- lm_robust(Homework_Given_G7~
                        
                        treatment_7_2018+
                        department*urban.rural+
                        
                        l_pc_low_end_t+
                        l_pc_high_end_t+
                        
                        l_pc_low_end+
                        l_pc_high_end+
                        
                        zscore_baseline_2018+
                        
                        Age+
                        Sex+
                        Repeated_2018+
                        Shift_2018+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        
                        Multigrade_G7+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg_g2_1)

#Column 2

reg_g2_2 <- lm_robust(Homework_Given_G7~
                        
                        treatment_7_2018+
                        department*urban.rural+
                        
                        l_pc_low_end_t+
                        l_pc_high_end_t+
                        
                        l_pc_low_end+
                        l_pc_high_end+
                        
                        I_zs_end+
                        zscore_baseline_2018+
                        
                        Age+
                        Sex+
                        Repeated_2018+
                        Shift_2018+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        
                        Multigrade_G7+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg_g2_2)

#Column 3

reg_g2_3 <- lm_robust(Homework_Given_G7~
                        
                        treatment_7_2018+
                        department*urban.rural+
                        
                        l_pc_low_end_t+
                        l_pc_high_end_t+
                        
                        l_pc_low_end+
                        l_pc_high_end+
                        
                        I_zs_end+
                        zscore_baseline_2018+
                        
                        I_zs_low_end_t+
                        I_zs_high_end_t+
                        
                        I_zs_low_end+
                        I_zs_high_end+
                        
                        Age+
                        Sex+
                        Repeated_2018+
                        Shift_2018+
                        no_elder+
                        no_younger+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        
                        Multigrade_G7+
                        Number_total_Students+
                        Number_G7_Students_AM_r+
                        Number_G7_Students_PM_r+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_7_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg_g2_3)

###Appendix B#####

#Estimate item parameters

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

student_test_7_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q1_Correct_or_Wrong_G7" ) ] <- "Q1_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q2_Correct_or_Wrong_G7" ) ] <- "Q2_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q3_Correct_or_Wrong_G7" ) ] <- "Q3_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q4_Correct_or_Wrong_G7" ) ] <- "Q4_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q5_Correct_or_Wrong_G7" ) ] <- "Q5_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q6_Correct_or_Wrong_G7" ) ] <- "Q6_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q7_Correct_or_Wrong_G7" ) ] <- "Q7_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q8_Correct_or_Wrong_G7" ) ] <- "Q8_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q9_Correct_or_Wrong_G7" ) ] <- "Q9_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q10_Correct_or_Wrong_G7" ) ] <- "Q10_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q11_Correct_or_Wrong_G7" ) ] <- "Q11_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q12_Correct_or_Wrong_G7" ) ] <- "Q12_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q13_Correct_or_Wrong_G7" ) ] <- "Q13_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q14_Correct_or_Wrong_G7" ) ] <- "Q14_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q15_Correct_or_Wrong_G7" ) ] <- "Q15_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q16_Correct_or_Wrong_G7" ) ] <- "Q16_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q17_Correct_or_Wrong_G7" ) ] <- "Q17_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q18_Correct_or_Wrong_G7" ) ] <- "Q18_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q19_Correct_or_Wrong_G7" ) ] <- "Q19_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q20_Correct_or_Wrong_G7" ) ] <- "Q20_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q21_Correct_or_Wrong_G7" ) ] <- "Q21_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q22_Correct_or_Wrong_G7" ) ] <- "Q22_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q23_Correct_or_Wrong_G7" ) ] <- "Q23_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q24_Correct_or_Wrong_G7" ) ] <- "Q24_Correct_or_Wrong_G7_fus"

names(student_test_7_fus)[which(names(student_test_7_fus)==
                                  "Q25_Correct_or_Wrong_G7" ) ] <- "Q25_Correct_or_Wrong_G7_fus"

student_test_7_r <- merge(student_test_7_end, student_test_7_fus,
                          by.x=c("ID_IE_test"), 
                          by.y=c("ID_IE_test"), all=F)

test_data_end <- cbind.data.frame(student_test_7_r$Q1_Correct_or_Wrong_G7,
                                  student_test_7_r$Q2_Correct_or_Wrong_G7,
                                  student_test_7_r$Q3_Correct_or_Wrong_G7,
                                  student_test_7_r$Q4_Correct_or_Wrong_G7,
                                  student_test_7_r$Q5_Correct_or_Wrong_G7,
                                  student_test_7_r$Q6_Correct_or_Wrong_G7,
                                  student_test_7_r$Q7_Correct_or_Wrong_G7,
                                  student_test_7_r$Q8_Correct_or_Wrong_G7,
                                  student_test_7_r$Q9_Correct_or_Wrong_G7,
                                  student_test_7_r$Q10_Correct_or_Wrong_G7,
                                  student_test_7_r$Q11_Correct_or_Wrong_G7,
                                  student_test_7_r$Q12_Correct_or_Wrong_G7,
                                  student_test_7_r$Q13_Correct_or_Wrong_G7,
                                  student_test_7_r$Q14_Correct_or_Wrong_G7,
                                  student_test_7_r$Q15_Correct_or_Wrong_G7,
                                  student_test_7_r$Q16_Correct_or_Wrong_G7,
                                  student_test_7_r$Q17_Correct_or_Wrong_G7,
                                  student_test_7_r$Q18_Correct_or_Wrong_G7,
                                  student_test_7_r$Q19_Correct_or_Wrong_G7,
                                  student_test_7_r$Q20_Correct_or_Wrong_G7)

test_data_fus <- cbind.data.frame(student_test_7_r$Q1_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q2_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q3_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q4_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q5_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q6_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q7_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q8_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q9_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q10_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q11_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q12_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q13_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q14_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q15_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q16_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q17_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q18_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q19_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q20_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q21_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q22_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q23_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q24_Correct_or_Wrong_G7_fus,
                                  student_test_7_r$Q25_Correct_or_Wrong_G7_fus)

para.2PL_end <-est(resp=test_data_end,model="2PL",engine="ltm")
para.2PL_fus <-est(resp=test_data_fus,model="2PL",engine="ltm")

###end-line test items

#item discrimination
item_discrimination_end <- as.data.frame(para.2PL_end$est[,1])
write.csv(item_discrimination_end,
          "item_discrimination_end_g7.csv",
          row.names = F)

#item difficulty
item_difficulty_end <- as.data.frame(para.2PL_end$est[,2])
write.csv(item_difficulty_end,
          "item_difficulty_end_g7.csv",
          row.names = F)

###follow-up test items

#item discrimination
item_discrimination_fus <-as.data.frame(para.2PL_fus$est[,1])
write.csv(item_discrimination_fus,
          "item_discrimination_fus_g7.csv",
          row.names = F)

#item difficulty
item_difficulty_fus <- as.data.frame(para.2PL_fus$est[,2])
write.csv(item_difficulty_fus,
          "item_difficulty_fus_g7.csv",
          row.names = F)

###Otheres####

#footnote 12
#implement the following code after implementing codes for column 1-3 in table 5.

linearHypothesis(reg5_1_3, "treatment_7_2018 - treatment_7_2019 = 0")

#estimate of impact only on the units 1 through 4 in math curricula (page 21 to 22)

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

reg_1 <- lm_robust(ztest_score_7_by_u4~
                        
                        treatment_7+ztest_score_baseline+
                        department*urban.rural+
                        Age+Sex+no_elder+no_younger+
                        Shift+Repeated+
                        Student_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Number_G7_Students_b+
                        Multigrade_G7+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_professorate+
                        Highest_Degree_bachelor+
                        Highest_Degree_master+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        Work_Shift_G7_both+
                        Teaching_other_subject_G7+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        Internet_Place__3+Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      data=student_test_7_end, 
                      clusters = ID_IE, se_type = "stata")

summary(reg_1)

#status of workbook (in discussion section)

student_test_7_fus <- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

unite_one_wb <-
  cbind(student_test_7_fus$WB_G7_U1_f2,
        student_test_7_fus$WB_G7_U1_f3,
        student_test_7_fus$WB_G7_U1_f4,
        student_test_7_fus$WB_G7_U1_f5,
        student_test_7_fus$WB_G7_U1_f6,
        student_test_7_fus$WB_G7_U1_f7,
        student_test_7_fus$WB_G7_U1_f8,
        student_test_7_fus$WB_G7_U1_f9)

#student solved at least one problem in each page

unite_one_wb_1_3 <- ifelse(unite_one_wb==1|
                             unite_one_wb==2|
                             unite_one_wb==3,
                           1,0)

unite_one_wb_1_3 <- as.data.frame(unite_one_wb_1_3)

unite_one_wb_1_3_r <- apply(unite_one_wb_1_3, 1, sum)

unite_one_wb_1_3_r2 <- c(unite_one_wb_1_3_r/8)

mean(unite_one_wb_1_3_r2, na.rm=T)


#student checked his/her answer 

unite_one_wb_2_3 <- ifelse(unite_one_wb==2|
                             unite_one_wb==3,
                           1,0)

unite_one_wb_2_3 <- as.data.frame(unite_one_wb_2_3)

unite_one_wb_2_3_r <- apply(unite_one_wb_2_3, 1, sum)

unite_one_wb_2_3_r2 <- c(unite_one_wb_2_3_r/8)

mean(unite_one_wb_2_3_r2, na.rm=T)

#teacher checked the homework self-marked by student 
unite_one_wb_3 <- ifelse(unite_one_wb==3,
                         1,0)

unite_one_wb_3 <- as.data.frame(unite_one_wb_3)

unite_one_wb_3_r <- apply(unite_one_wb_3, 1, sum)

unite_one_wb_3_r2 <- c(unite_one_wb_3_r/8)

mean(unite_one_wb_3_r2, na.rm=T)

