
rm (list = ls(all=TRUE))

#set your working directory

setwd(" ")

#install necessary packages

install.packages("estimatr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("survey")
install.packages("dplyr")
install.packages("haven")
install.packages("survey")

library(estimatr)
library(ggplot2)
library(GGally)
library(survey)
library(dplyr)
library(haven)

###Table 4####

teacher_baseline <- 
  read.csv("teacher_7_baseline.csv", header=T, stringsAsFactors = F)

#arrange dataframe for statistical tests

teacher_baseline_r <- svydesign(id = ~0, 
                                strata=~
                                  department*urban.rural,
                                data = teacher_baseline, nest=T)

#

school_baseline <- 
  read.csv("school_principal_7_baseline.csv", header=T, stringsAsFactors = F)

#arrange dataframe for statistical tests

school_baseline_r <- svydesign(id = ~0, 
                                strata=~
                                  department*urban.rural,
                                data = school_baseline, nest=T)

###teacher section

teacher_baseline_t <- subset(teacher_baseline,
                             treatment_d==1)

teacher_baseline_c <- subset(teacher_baseline,
                             treatment_d==0)

#teacher sex

mean(teacher_baseline_t$Teacher_Sex, na.rm=T)*100
mean(teacher_baseline_c$Teacher_Sex, na.rm=T)*100

Diff_sex <- mean(teacher_baseline_t$Teacher_Sex, na.rm=T)*100-
  mean(teacher_baseline_c$Teacher_Sex, na.rm=T)*100

p_Sex <- svychisq(~Teacher_Sex+treatment_d, teacher_baseline_r,
                  statistic = c("Chisq"))

#teacher age

mean(teacher_baseline_t$Teacher_Age, na.rm=T)
mean(teacher_baseline_c$Teacher_Age, na.rm=T)

sd(teacher_baseline_t$Teacher_Age, na.rm=T)
sd(teacher_baseline_c$Teacher_Age, na.rm=T)

Diff_age <- mean(teacher_baseline_t$Teacher_Age, na.rm=T)-
  mean(teacher_baseline_c$Teacher_Age, na.rm=T)

p_Teacher_Age <- svyranktest(Teacher_Age~treatment_d, teacher_baseline_r,
                             test = c("wilcoxon"))

#teaching period

mean(teacher_baseline_t$Year_Start_teach_r, na.rm=T)
mean(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

sd(teacher_baseline_t$Year_Start_teach_r, na.rm=T)
sd(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

Diff_Year_Start_teach_r <- mean(teacher_baseline_t$Year_Start_teach_r, na.rm=T)-
  mean(teacher_baseline_c$Year_Start_teach_r, na.rm=T)

p_Year_Start_teach_r <- svyranktest(Year_Start_teach_r~treatment_d, teacher_baseline_r,
                                    test = c("wilcoxon"))

#highest degree: high school

mean(teacher_baseline_t$Highest_Degree_highschool, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_highschool, na.rm=T)*100

Diff_Highest_Degree_highschool <- 
  mean(teacher_baseline_t$Highest_Degree_highschool, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_highschool, na.rm=T)*100

p_Highest_Degree_highschool <- 
  svychisq(~Highest_Degree_highschool+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: professorate

mean(teacher_baseline_t$Highest_Degree_professorate, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_professorate, na.rm=T)*100

Diff_Highest_Degree_professorate <- 
  mean(teacher_baseline_t$Highest_Degree_professorate, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_professorate, na.rm=T)*100

p_Highest_Degree_professorate <- 
  svychisq(~Highest_Degree_professorate+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: bachelor

mean(teacher_baseline_t$Highest_Degree_bachelor, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_bachelor, na.rm=T)*100

Diff_Highest_Degree_bachelor <- 
  mean(teacher_baseline_t$Highest_Degree_bachelor, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_bachelor, na.rm=T)*100

p_Highest_Degree_bachelor <- 
  svychisq(~Highest_Degree_bachelor+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: master

mean(teacher_baseline_t$Highest_Degree_master, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_master, na.rm=T)*100

Diff_Highest_Degree_master <- 
  mean(teacher_baseline_t$Highest_Degree_master, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_master, na.rm=T)*100

p_Highest_Degree_master <- 
  svychisq(~Highest_Degree_master+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#highest degree: doctor

mean(teacher_baseline_t$Highest_Degree_doctor, na.rm=T)*100
mean(teacher_baseline_c$Highest_Degree_doctor, na.rm=T)*100

Diff_Highest_Degree_master <- 
  mean(teacher_baseline_t$Highest_Degree_doctor, na.rm=T)*100-
  mean(teacher_baseline_c$Highest_Degree_doctor, na.rm=T)*100

p_Highest_Degree_master <- 
  svychisq(~Highest_Degree_doctor+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Pedagogical Bachelor

mean(teacher_baseline_t$Teachr_Qualification__1, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__1, na.rm=T)*100

Diff_Teachr_Qualification__1 <- 
  mean(teacher_baseline_t$Teachr_Qualification__1, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__1, na.rm=T)*100

p_Teachr_Qualification__1 <- 
  svychisq(~Teachr_Qualification__1+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Professorate

mean(teacher_baseline_t$Teachr_Qualification__2, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__2, na.rm=T)*100

Diff_Teachr_Qualification__2 <- 
  mean(teacher_baseline_t$Teachr_Qualification__2, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__2, na.rm=T)*100

p_Teachr_Qualification__2 <- 
  svychisq(~Teachr_Qualification__2+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Bachelor in Education

mean(teacher_baseline_t$Teachr_Qualification__3, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__3, na.rm=T)*100

Diff_Teachr_Qualification__3 <- 
  mean(teacher_baseline_t$Teachr_Qualification__3, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__3, na.rm=T)*100

p_Teachr_Qualification__3 <- 
  svychisq(~Teachr_Qualification__3+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (1): Master in Education

mean(teacher_baseline_t$Teachr_Qualification__4, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__4, na.rm=T)*100

Diff_Teachr_Qualification__4 <- 
  mean(teacher_baseline_t$Teachr_Qualification__4, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__4, na.rm=T)*100

p_Teachr_Qualification__4 <- 
  svychisq(~Teachr_Qualification__4+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

mean(teacher_baseline$Teachr_Qualification__5, na.rm=T)*100

#teacher qualification (1): Pedagogical Training Course

mean(teacher_baseline_t$Teachr_Qualification__6, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Qualification__6, na.rm=T)*100

Diff_Teachr_Qualification__6 <- 
  mean(teacher_baseline_t$Teachr_Qualification__6, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Qualification__6, na.rm=T)*100

p_Teachr_Qualification__6 <- 
  svychisq(~Teachr_Qualification__6+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Basic Education Teacher (cycle 1 and 2) 

mean(teacher_baseline_t$Teachr_Apontmt_Post__1, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__1, na.rm=T)*100

Diff_Teachr_Apontmt_Post__1 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__1, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__1, na.rm=T)*100

p_Teachr_Apontmt_Post__1 <- 
  svychisq(~Teachr_Apontmt_Post__1+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Mathematics Specialty Teacher (cycle 3)

mean(teacher_baseline_t$Teachr_Apontmt_Post__2, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__2, na.rm=T)*100

Diff_Teachr_Apontmt_Post__2 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__2, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__2, na.rm=T)*100

p_Teachr_Apontmt_Post__2 <- 
  svychisq(~Teachr_Apontmt_Post__2+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): Teacher specialized in other than math (cycle 3) 

mean(teacher_baseline_t$Teachr_Apontmt_Post__3, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__3, na.rm=T)*100

Diff_Teachr_Apontmt_Post__3 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__3, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__3, na.rm=T)*100

p_Teachr_Apontmt_Post__3 <- 
  svychisq(~Teachr_Apontmt_Post__3+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

#teacher qualification (2): others 

mean(teacher_baseline_t$Teachr_Apontmt_Post__4, na.rm=T)*100
mean(teacher_baseline_c$Teachr_Apontmt_Post__4, na.rm=T)*100

Diff_Teachr_Apontmt_Post__4 <- 
  mean(teacher_baseline_t$Teachr_Apontmt_Post__4, na.rm=T)*100-
  mean(teacher_baseline_c$Teachr_Apontmt_Post__4, na.rm=T)*100

p_Teachr_Apontmt_Post__4 <- 
  svychisq(~Teachr_Apontmt_Post__4+treatment_d, teacher_baseline_r,
           statistic = c("Chisq"))

###school section in table 4

school_baseline_t <- subset(school_baseline,
                             treatment_d==1)

school_baseline_c <- subset(school_baseline,
                             treatment_d==0)

#total number of 7th grade students AM shift

mean(school_baseline_t$Number_G7_Students_AM, na.rm=T)
mean(school_baseline_c$Number_G7_Students_AM, na.rm=T)

sd(school_baseline_t$Number_G7_Students_AM, na.rm=T)
sd(school_baseline_c$Number_G7_Students_AM, na.rm=T)

Diff_Number_G7_Students_AM <- mean(school_baseline_t$Number_G7_Students_AM, na.rm=T)-
  mean(school_baseline_c$Number_G7_Students_AM, na.rm=T)

p_Number_G7_Students_AM <- svyranktest(Number_G7_Students_AM~treatment_d, school_baseline_r,
                                    test = c("wilcoxon"))

#total number of 7th grade students PM shift

mean(school_baseline_t$Number_G7_Students_PM, na.rm=T)
mean(school_baseline_c$Number_G7_Students_PM, na.rm=T)

sd(school_baseline_t$Number_G7_Students_PM, na.rm=T)
sd(school_baseline_c$Number_G7_Students_PM, na.rm=T)

Diff_Number_G7_Students_PM <- mean(school_baseline_t$Number_G7_Students_PM, na.rm=T)-
  mean(school_baseline_c$Number_G7_Students_PM, na.rm=T)

p_Number_G7_Students_PM <- svyranktest(Number_G7_Students_PM~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#total number of students

mean(school_baseline_t$Number_total_Students, na.rm=T)
mean(school_baseline_c$Number_total_Students, na.rm=T)

sd(school_baseline_t$Number_total_Students, na.rm=T)
sd(school_baseline_c$Number_total_Students, na.rm=T)

Diff_Number_total_Students <- mean(school_baseline_t$Number_total_Students, na.rm=T)-
  mean(school_baseline_c$Number_total_Students, na.rm=T)

p_Number_total_Students <- svyranktest(Number_total_Students~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#repetition rate grade 7 AM shift

mean(school_baseline_t$repeat_rate_g7_am, na.rm=T)*100
mean(school_baseline_c$repeat_rate_g7_am, na.rm=T)*100

Diff_repeat_rate_g7_am <- mean(school_baseline_t$repeat_rate_g7_am, na.rm=T)*100-
  mean(school_baseline_c$repeat_rate_g7_am, na.rm=T)*100

p_repeat_rate_g7_am <- svyranktest(repeat_rate_g7_am~treatment_d, school_baseline_r,
                                       test = c("wilcoxon"))

#repetition rate grade 7 PM shift

mean(school_baseline_t$repeat_rate_g7_pm, na.rm=T)*100
mean(school_baseline_c$repeat_rate_g7_pm, na.rm=T)*100

Diff_repeat_rate_g7_pm <- mean(school_baseline_t$repeat_rate_g7_pm, na.rm=T)*100-
  mean(school_baseline_c$repeat_rate_g7_pm, na.rm=T)*100

p_repeat_rate_g7_pm <- svyranktest(repeat_rate_g7_pm~treatment_d, school_baseline_r,
                                   test = c("wilcoxon"))

#dropout rate grade 7 AM shift

mean(school_baseline_t$drop_out_rate_g7_am, na.rm=T)*100
mean(school_baseline_c$drop_out_rate_g7_am, na.rm=T)*100

Diff_drop_out_rate_g7_am <- mean(school_baseline_t$drop_out_rate_g7_am, na.rm=T)*100-
  mean(school_baseline_c$drop_out_rate_g7_am, na.rm=T)*100

p_drop_out_rate_g7_am <- svyranktest(drop_out_rate_g7_am~treatment_d, school_baseline_r,
                                     test = c("wilcoxon"))

#dropout rate grade 7 PM shift

mean(school_baseline_t$drop_out_rate_g7_pm, na.rm=T)*100
mean(school_baseline_c$drop_out_rate_g7_pm, na.rm=T)*100

Diff_drop_out_rate_g7_pm <- mean(school_baseline_t$drop_out_rate_g7_pm, na.rm=T)*100-
  mean(school_baseline_c$drop_out_rate_g7_pm, na.rm=T)*100

p_drop_out_rate_g7_pm <- svyranktest(drop_out_rate_g7_pm~treatment_d, school_baseline_r,
                                   test = c("wilcoxon"))

#number of teachers

mean(school_baseline_t$Number_of_Teachers, na.rm=T)
mean(school_baseline_c$Number_of_Teachers, na.rm=T)

sd(school_baseline_t$Number_of_Teachers, na.rm=T)
sd(school_baseline_c$Number_of_Teachers, na.rm=T)

Diff_Number_of_Teachers <- mean(school_baseline_t$Number_of_Teachers, na.rm=T)-
  mean(school_baseline_c$Number_of_Teachers, na.rm=T)

p_Number_of_Teachers <- svyranktest(Number_of_Teachers~treatment_d, school_baseline_r,
                                     test = c("wilcoxon"))

#school facility: Electricity

mean(school_baseline_t$School_Facility__1, na.rm=T)*100
mean(school_baseline_c$School_Facility__1, na.rm=T)*100

Diff_School_Facility__1 <- 
  mean(school_baseline_t$School_Facility__1, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__1, na.rm=T)*100

p_School_Facility__1 <- 
  svychisq(~School_Facility__1+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: drinking water

mean(school_baseline_t$School_Facility__2, na.rm=T)*100
mean(school_baseline_c$School_Facility__2, na.rm=T)*100

Diff_School_Facility__2 <- 
  mean(school_baseline_t$School_Facility__2, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__2, na.rm=T)*100

p_School_Facility__2 <- 
  svychisq(~School_Facility__2+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: computer

mean(school_baseline_t$School_Facility__3, na.rm=T)*100
mean(school_baseline_c$School_Facility__3, na.rm=T)*100

Diff_School_Facility__3 <- 
  mean(school_baseline_t$School_Facility__3, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__3, na.rm=T)*100

p_School_Facility__3 <- 
  svychisq(~School_Facility__3+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: library

mean(school_baseline_t$School_Facility__5, na.rm=T)*100
mean(school_baseline_c$School_Facility__5, na.rm=T)*100

Diff_School_Facility__5 <- 
  mean(school_baseline_t$School_Facility__5, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__5, na.rm=T)*100

p_School_Facility__5 <- 
  svychisq(~School_Facility__5+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#school facility: laboratory

mean(school_baseline_t$School_Facility__6, na.rm=T)*100
mean(school_baseline_c$School_Facility__6, na.rm=T)*100

Diff_School_Facility__6 <- 
  mean(school_baseline_t$School_Facility__6, na.rm=T)*100-
  mean(school_baseline_c$School_Facility__6, na.rm=T)*100

p_School_Facility__6 <- 
  svychisq(~School_Facility__6+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

#donor support

mean(school_baseline_t$Donor_Project, na.rm=T)*100
mean(school_baseline_c$Donor_Project, na.rm=T)*100

Diff_Donor_Project <- 
  mean(school_baseline_t$Donor_Project, na.rm=T)*100-
  mean(school_baseline_c$Donor_Project, na.rm=T)*100

p_Donor_Project <- 
  svychisq(~Donor_Project+treatment_d, school_baseline_r,
           statistic = c("Chisq"))

###Prepare dataframe to estimate the impact for teachers (Table 6, F-2, Figures F.1 to F.4)#####
###store the csv files in your working directory.

###read csv files 

teacher_pool <- 
  read.csv("teacher_fus_pool.csv", header=T, stringsAsFactors = F)

teacher_pool$treatment_2018 <- c(teacher_pool$treatment_d*
                                   teacher_pool$timing_end)

teacher_pool$treatment_2019 <- c(teacher_pool$treatment_d*
                                   teacher_pool$timing_fus)

#coding variables for regression analysis

teacher_pool$Percentage_Time_Active_Learn_Students_min <-
  c((teacher_pool$Time_Acrtive_Learn_Students_min/teacher_pool$Duration_Lesson_min)*
      100)

teacher_pool$Percentage_Time_Active_Learn_Students_min <- 
  round(teacher_pool$Percentage_Time_Active_Learn_Students_min, digits=2)

teacher_pool <- subset(teacher_pool,
                       teacher_pool$Duration_Lesson_min>0)

#

teacher_pool$Giving_Excercise <-
  ifelse(teacher_pool$Giving_Excercise==1,1,0)

teacher_pool$Frequency_Suggestion_Clues <-
  ifelse(teacher_pool$Frequency_Suggestion_Clues==1,1,0)

teacher_pool$Advice_Consultation_Students <-
  ifelse(teacher_pool$Advice_Consultation_Students==1,1,0)

teacher_pool$Instruction_Try_Wrong_Solutions <-
  ifelse(teacher_pool$Instruction_Try_Wrong_Solutions==1,1,0)

teacher_pool$Walk_through_Desks <-
  ifelse(teacher_pool$Walk_through_Desks==1,1,0)

teacher_pool$Giving_HW <-
  ifelse(teacher_pool$Giving_HW==1,1,0)

###Table 7#####

teacher_pool_end <- subset(teacher_pool,
                           timing_end==1)

teacher_pool_fus <- subset(teacher_pool,
                           timing_fus==1)

#exclude teacheres who were in charge of grade 7 in year 2 of the research

teacher_pool_fus <- subset(teacher_pool,
                           Grade_Observed_f_g8==1)

teacher_pool <- rbind.data.frame(teacher_pool_end,
                                 teacher_pool_fus)

#percentage of student engaged time on learning math in a lesson
#column 1-1

reg_t7_1_1 <- lm_robust(Percentage_Time_Active_Learn_Students_min~
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students_2018+
                          Total_No_Students_2019+
                          Multigrade_G7_2018+
                          Multigrade_G7_2019+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
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
                        data=teacher_pool, se_type = "stata")

summary(reg_t7_1_1)

teacher_pool_c <- subset(teacher_pool,
                         treatment_2018==0&
                           timing_end==1)

mean(teacher_pool_c$Percentage_Time_Active_Learn_Students_min)

teacher_pool$Percentage_Time_Active_Learn_Students_min

#column 1-2

teacher_pool$Instruction_Try_Wrong_Solutions_r <-
  teacher_pool$Instruction_Try_Wrong_Solutions*100

reg_t7_1_2 <- lm_robust(Instruction_Try_Wrong_Solutions_r ~
                          
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students_2018+
                          Total_No_Students_2019+
                          Multigrade_G7_2018+
                          Multigrade_G7_2019+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
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
                        data=teacher_pool, se_type = "stata")

summary(reg_t7_1_2)

teacher_pool_c <- subset(teacher_pool,
                         treatment_2018==0&
                           timing_end==1)

mean(teacher_pool_c$Instruction_Try_Wrong_Solutions_r)

#mutual review meeting
#column 2-1

school_fus <- read.csv("school_principal_7_fus.csv", header=T, stringsAsFactors = F)

school_fus$RP_Theme_test_e <- ifelse(school_fus$RP_Theme_test_e==1,1,0)
school_fus$RP_Theme_test_e[is.na(school_fus$RP_Theme_test_e)]<-0
school_fus$RP_Theme_test_e_r <- school_fus$RP_Theme_test_e*100

reg_t7_2_1 <- lm_robust(RP_Theme_test_e_r~
                          
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_other+
                          
                          Number_of_Teachers+
                          Number_total_Students+
                          Vice_Headmaster+
                          
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
                        data=school_fus, se_type = "stata")

summary(reg_t7_2_1)

school_fus_c <- subset(school_fus,
                       timing_end==1&
                         treatment_2018==0)

mean(school_fus_c$RP_Theme_test_e_r)

#Column 2-2

school_fus$RP_Theme_test_cmpr_e <- ifelse(school_fus$RP_Theme_test_cmpr_e==1,1,0)
school_fus$RP_Theme_test_cmpr_e[is.na(school_fus$RP_Theme_test_cmpr_e)]<-0
school_fus$RP_Theme_test_cmpr_e_r <- school_fus$RP_Theme_test_cmpr_e*100

reg_t7_2_2 <- lm_robust(RP_Theme_test_cmpr_e_r~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_t7_2_2)

school_fus_c <- subset(school_fus,
                          timing_end==1&
                            treatment_2018==0)

mean(school_fus_c$RP_Theme_test_cmpr_e)

#frequency of homework assignment in a week
#column 3-1

reg_t7_3_1 <- lm_robust(Homework_Given_G7~
                          
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students_2018+
                          Total_No_Students_2019+
                          Multigrade_G7_2018+
                          Multigrade_G7_2019+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
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
                        data=teacher_pool, se_type = "stata")

summary(reg_t7_3_1)

teacher_pool$Homework_Given_G7

teacher_pool_c <- subset(teacher_pool,
                               treatment_2018==0&
                                 timing_end==1)

nrow(teacher_pool)

teacher_pool$Homework_Given_G2
mean(teacher_pool_c$Homework_Given_G7)

###Figures F3, F4, F5 and F6 in Appendix F#### 

###Figure F3 and F4
###impacts on instructional routines in a math lesson

#give exercise 

reg_f1_A <- lm_robust(Giving_Excercise~
                        
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        Teacher_Age+
                        Teacher_Sex+
                        Year_Start_teach_r+
                        Highest_Degree_highschool+
                        Highest_Degree_bachelor+
                        Teachr_Qualification__1+
                        Teachr_Qualification__3+
                        Teachr_Qualification__4+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Total_No_Students_2018+
                        Total_No_Students_2019+
                        Multigrade_G7_2018+
                        Multigrade_G7_2019+
                        Year_HM_ttl_e2+
                        Year_HM_this_school_e2+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
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
                      data=teacher_pool, se_type = "stata")

summary(reg_f1_A)

#check notebooks

reg_f1_B <- lm_robust(How_to_check_solutn__3~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students_2018+
                      Total_No_Students_2019+
                      Multigrade_G7_2018+
                      Multigrade_G7_2019+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
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
                    data=teacher_pool, se_type = "stata")

summary(reg_f1_B)

#walk in classroom to check notebook

reg_f1_C <- lm_robust(Walk_through_Desks~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students_2018+
                      Total_No_Students_2019+
                      Multigrade_G7_2018+
                      Multigrade_G7_2019+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
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
                    data=teacher_pool, se_type = "stata")

summary(reg_f1_C)

#advice students to consult with each other

reg_f1_D <- lm_robust(Advice_Consultation_Students~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students_2018+
                      Total_No_Students_2019+
                      Multigrade_G7_2018+
                      Multigrade_G7_2019+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
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
                    data=teacher_pool, se_type = "stata")

summary(reg_f1_D)

#tell students to check their own answers

reg_f1_E <- lm_robust(How_to_check_solutn__2~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students_2018+
                      Total_No_Students_2019+
                      Multigrade_G7_2018+
                      Multigrade_G7_2019+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
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
                    data=teacher_pool, se_type = "stata")

summary(reg_f1_E)

#instruct students try again wrong answer

reg_f1_F <- lm_robust(Instruction_Try_Wrong_Solutions~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural+department+
                      Teacher_Age+
                      Teacher_Sex+
                      Year_Start_teach_r+
                      Highest_Degree_highschool+
                      Highest_Degree_bachelor+
                      Teachr_Qualification__1+
                      Teachr_Qualification__3+
                      Teachr_Qualification__4+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Total_No_Students_2018+
                      Total_No_Students_2019+
                      Multigrade_G7_2018+
                      Multigrade_G7_2019+
                      Year_HM_ttl_e2+
                      Year_HM_this_school_e2+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
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
                    data=teacher_pool, se_type = "stata")

summary(reg_f1_F)

#Draw Figure F3

reg_f1_A_r <- tidy(reg_f1_A, conf.int = TRUE)
reg_f1_A_r2 <- reg_f1_A_r[2,]
reg_f1_A_r2$term <- c("(A)")

reg_f1_B_r <- tidy(reg_f1_B, conf.int = TRUE)
reg_f1_B_r2 <- reg_f1_B_r[2,]
reg_f1_B_r2$term <- c("(B)")

reg_f1_C_r <- tidy(reg_f1_C, conf.int = TRUE)
reg_f1_C_r2 <- reg_f1_C_r[2,]
reg_f1_C_r2$term <- c("(C)")

reg_f1_D_r <- tidy(reg_f1_D, conf.int = TRUE)
reg_f1_D_r2 <- reg_f1_D_r[2,]
reg_f1_D_r2$term <- c("(D)")

reg_f1_E_r <- tidy(reg_f1_E, conf.int = TRUE)
reg_f1_E_r2 <- reg_f1_E_r[2,]
reg_f1_E_r2$term <- c("(E)")

reg_f1_F_r <- tidy(reg_f1_F, conf.int = TRUE)
reg_f1_F_r2 <- reg_f1_F_r[2,]
reg_f1_F_r2$term <- c("(F)")

#
reg_f1 <- rbind(reg_f1_A_r2,
                reg_f1_B_r2,
                reg_f1_C_r2,
                reg_f1_D_r2,
                reg_f1_E_r2,
                reg_f1_F_r2)

#

g <- ggcoef(reg_f1)
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_e_g7.png", dpi = 1000, 
       width = 8, height = 4, plot = g)

#Draw Figure F4

reg_f2_A_r2 <- reg_f1_A_r[3,]
reg_f2_A_r2$term <- c("(A)")

reg_f2_B_r2 <- reg_f1_B_r[3,]
reg_f2_B_r2$term <- c("(B)")

reg_f2_C_r2 <- reg_f1_C_r[3,]
reg_f2_C_r2$term <- c("(C)")

reg_f2_D_r2 <- reg_f1_D_r[3,]
reg_f2_D_r2$term <- c("(D)")

reg_f2_E_r2 <- reg_f1_E_r[3,]
reg_f2_E_r2$term <- c("(E)")

reg_f2_F_r2 <- reg_f1_F_r[3,]
reg_f2_F_r2$term <- c("(F)")

#
reg_f2 <- rbind(reg_f2_A_r2,
                reg_f2_B_r2,
                reg_f2_C_r2,
                reg_f2_D_r2,
                reg_f2_E_r2,
                reg_f2_F_r2)

#

g <- ggcoef(reg_f2)
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_f_g7.png", dpi = 1000, 
       width = 8, height = 4, plot = g)

###Figure F5 and F6

school_fus <- read.csv("school_principal_7_fus.csv", header=T, stringsAsFactors = F)

#Orderly use of chalkboard

reg_f3_A <- lm_robust(Suggestion_to_Teachr__1~
                        
                        treatment_2018+
                        treatment_2019+
                        timing_fus+
                        urban.rural*department+
                        
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_bachelor+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_other+
                        
                        Number_of_Teachers+
                        Number_total_Students+
                        Vice_Headmaster+
                        
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
                      data=school_fus, se_type = "stata")

summary(reg_f3_A)

#Intensive intervention of teacher

reg_f3_B <- lm_robust(Suggestion_to_Teachr__2~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_f3_B)

#To adjust the development of the class according to students' understanding

reg_f3_C <- lm_robust(Suggestion_to_Teachr__3~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_f3_C)

#Guidance about prerequisite knowledge

reg_f3_D <- lm_robust(Suggestion_to_Teachr__4~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_f3_D)

#To make students solve again the problems with wrong solutions

reg_f3_E <- lm_robust(Suggestion_to_Teachr__5~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_f3_E)

#Concise explanation by the teacher

reg_f3_F <- lm_robust(Suggestion_to_Teachr__6~
                      
                      treatment_2018+
                      treatment_2019+
                      timing_fus+
                      urban.rural*department+
                      
                      Year_HM_ttl+
                      Year_HM_this_school+
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_bachelor+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_other+
                      
                      Number_of_Teachers+
                      Number_total_Students+
                      Vice_Headmaster+
                      
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
                    data=school_fus, se_type = "stata")

summary(reg_f3_F)

#Draw Figure F3

reg_f3_Ar <- tidy(reg_f3_A, conf.int = TRUE)
reg_f3_Ar2 <- reg_f3_Ar[2,]
reg_f3_Ar2$term <- c("(A)")

reg_f3_Br <- tidy(reg_f3_B, conf.int = TRUE)
reg_f3_Br2 <- reg_f3_Br[2,]
reg_f3_Br2$term <- c("(B)")

reg_f3_Cr <- tidy(reg_f3_C, conf.int = TRUE)
reg_f3_Cr2 <- reg_f3_Cr[2,]
reg_f3_Cr2$term <- c("(C)")

reg_f3_Dr <- tidy(reg_f3_D, conf.int = TRUE)
reg_f3_Dr2 <- reg_f3_Dr[2,]
reg_f3_Dr2$term <- c("(D)")

reg_f3_Er <- tidy(reg_f3_E, conf.int = TRUE)
reg_f3_Er2 <- reg_f3_Er[2,]
reg_f3_Er2$term <- c("(E)")

reg_f3_Fr <- tidy(reg_f3_F, conf.int = TRUE)
reg_f3_Fr2 <- reg_f3_Fr[2,]
reg_f3_Fr2$term <- c("(F)")

reg_f3 <- rbind(reg_f3_Ar2,
               reg_f3_Br2,
               reg_f3_Cr2,
               reg_f3_Dr2,
               reg_f3_Er2,
               reg_f3_Fr2)

g <- ggcoef(reg_f3)
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_support_e_g7.png", dpi = 1000, 
       width = 8, height = 4, plot = g)

#Draw Figure F4

reg_f4_Ar2 <- reg_f3_Ar[3,]
reg_f4_Ar2$term <- c("(A)")

reg_f4_Br2 <- reg_f3_Br[3,]
reg_f4_Br2$term <- c("(B)")

reg_f4_Cr2 <- reg_f3_Cr[3,]
reg_f4_Cr2$term <- c("(C)")

reg_f4_Dr2 <- reg_f3_Dr[3,]
reg_f4_Dr2$term <- c("(D)")

reg_f4_Er2 <- reg_f3_Er[3,]
reg_f4_Er2$term <- c("(E)")

reg_f4_Fr2 <- reg_f3_Fr[3,]
reg_f4_Fr2$term <- c("(F)")

#

reg_f4 <- rbind(reg_f4_Ar2,
                reg_f4_Br2,
                reg_f4_Cr2,
                reg_f4_Dr2,
                reg_f4_Er2,
                reg_f4_Fr2)

g <- ggcoef(reg_f4)
g <- g + ylab("") 
g <- g + coord_flip() 
plot(g)

ggsave(file = "estimate_instruction_support_f_g7.png", dpi = 1000, 
       width = 8, height = 4, plot = g)


###Tables F-2 and F-3 in Appendix F####

#Table F-2
#Frequency of lesson observation of school principal
#column 1

reg_tf_2_1 <- lm_robust(HM_Observe~
                          
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students_2018+
                          Total_No_Students_2019+
                          Multigrade_G7_2018+
                          Multigrade_G7_2019+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
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
                        
                        data=teacher_pool, se_type = "stata")

summary(reg_tf_2_1)

teacher_pool_c <- subset(teacher_pool,
                         treatment_2018==0&
                           timing_end==1)

mean(teacher_pool_c$HM_Observe)

#Frequency of suggestions in lesson observation of school principal
#column 2

reg_tf_2_2 <- lm_robust(HM_Suggestion~
                          
                          treatment_2018+
                          treatment_2019+
                          timing_fus+
                          urban.rural*department+
                          Teacher_Age+
                          Teacher_Sex+
                          Year_Start_teach_r+
                          Highest_Degree_highschool+
                          Highest_Degree_bachelor+
                          Teachr_Qualification__1+
                          Teachr_Qualification__3+
                          Teachr_Qualification__4+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Total_No_Students_2018+
                          Total_No_Students_2019+
                          Multigrade_G7_2018+
                          Multigrade_G7_2019+
                          Year_HM_ttl_e2+
                          Year_HM_this_school_e2+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_bachelor+
                          HM_Highest_Degree_master+
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
                        
                        data=teacher_pool, se_type = "stata")

summary(reg_tf_2_2)

teacher_pool_end_c <- subset(teacher_pool,
                             treatment_2018==0&
                               timing_end==1)

mean(teacher_pool_end_c$HM_Suggestion)


###Others####

#footnote 20

teacher_pool_end <-subset(teacher_pool,
                          timing_end==1)

teacher_pool_end_t <- subset(teacher_pool,
                             timing_end==1&
                               treatment_d==1)

teacher_pool_end_c <- subset(teacher_pool,
                             timing_end==1&
                               treatment_d==0)

teacher_pool_end_r <- svydesign(id = ~0, 
                                strata=~
                                  department*urban.rural,
                                data = teacher_pool_end, nest=T)

mean(teacher_pool_end_t$lesson_text_G7_e__5)
mean(teacher_pool_end_c$lesson_text_G7_e__5)

p_lesson_text_G7_e__5 <- svychisq(~lesson_text_G7_e__5+treatment_d, teacher_pool_end_r,
                                  statistic = c("Chisq"))

#footnote 23

teacher_pool_end_rural <-subset(teacher_pool,
                                timing_end==1&
                                  urban.rural=="Rural")

teacher_pool_end_rural_t <- subset(teacher_pool,
                                   timing_end==1&
                                     treatment_d==1&
                                     urban.rural=="Rural")

teacher_pool_end_rural_c <- subset(teacher_pool,
                                   timing_end==1&
                                     treatment_d==0&
                                     urban.rural=="Rural")

teacher_pool_end_rural_r <- svydesign(id = ~0, 
                                      strata=~
                                        department*urban.rural,
                                      data = teacher_pool_end_rural, nest=T)

mean(teacher_pool_end_rural_t$Homework_Given_G7)
mean(teacher_pool_end_rural_c$Homework_Given_G7)

p_Homework_Given_G7 <- svyranktest(Homework_Given_G7~treatment_d, teacher_pool_end_rural_r,
                                    test = c("wilcoxon"))

#

unique(teacher_pool$urban.rural)

teacher_pool_end_urban <-subset(teacher_pool,
                                timing_end==1&
                                  urban.rural=="Urbana")

teacher_pool_end_urban_t <- subset(teacher_pool,
                                   timing_end==1&
                                     treatment_d==1&
                                     urban.rural=="Urbana")

teacher_pool_end_urban_c <- subset(teacher_pool,
                                   timing_end==1&
                                     treatment_d==0&
                                     urban.rural=="Urbana")

teacher_pool_end_urban_r <- svydesign(id = ~0, 
                                      strata=~
                                        department*urban.rural,
                                      data = teacher_pool_end_urban, nest=T)

mean(teacher_pool_end_urban_t$Homework_Given_G7)
mean(teacher_pool_end_urban_c$Homework_Given_G7)

p_Homework_Given_G7 <- svyranktest(Homework_Given_G7~treatment_d, teacher_pool_end_urban_r,
                                   test = c("wilcoxon"))

#timing of checking homework 
#in section 4.2

teacher_pool_end <-subset(teacher_pool,
                          timing_end==1)

teacher_pool_end$timing_check_HW_G7_e__2_or__3 <-
  ifelse(teacher_pool_end$timing_check_HW_G7_e__2==1|
           teacher_pool_end$timing_check_HW_G7_e__3==1|
           teacher_pool_end$timing_check_HW_G7_e__4==1,
         1,0)

teacher_pool_end_t <-subset(teacher_pool_end,
                          timing_end==1&
                            treatment_2018==1)

teacher_pool_end_c <-subset(teacher_pool_end,
                          timing_end==1&
                            treatment_2018==0)

teacher_pool_end <- svydesign(id = ~0, 
                              strata=~
                                department*urban.rural,
                              data = teacher_pool_end, nest=T)

teacher_pool_end$timing_check_HW_G7_e__2 

mean(teacher_pool_end_t$timing_check_HW_G7_e__2_or__3, na.rm=T)
mean(teacher_pool_end_c$timing_check_HW_G7_e__2_or__3, na.rm=T)

p_timing_check_HW_G7_e__2_or__3 <- svychisq(~timing_check_HW_G7_e__2_or__3+treatment_d, teacher_pool_end,
                                  statistic = c("Chisq"))

#correlation between the number of experience years and frequency of suggestions
#in discussion section

teacher_pool_end <-subset(teacher_pool,
                          timing_end==1)

teacher_pool_end_t <-subset(teacher_pool_end,
                            timing_end==1&
                              treatment_2018==1)

teacher_pool_end_t$HM_Suggestion_yes <-
  ifelse(teacher_pool_end_t$HM_Suggestion==1, 0, 1)

teacher_pool_end_t$Year_HM_ttl_e2_less_than_ten <-
  ifelse(teacher_pool_end_t$Year_HM_ttl_e2<=10,
         1,0)

table(teacher_pool_end_t$Year_HM_ttl_e2_less_than_ten,
      teacher_pool_end_t$HM_Suggestion_yes)

#

school_baseline_ext <- subset(school_baseline,
                              select=c(ID_IE,
                                       Number_of_Teachers))

teacher_pool_end <-subset(teacher_pool,
                          timing_end==1)

teacher_pool_end <- merge(teacher_pool_end, school_baseline_ext,
                            by.x=c("ID_IE"), 
                            by.y=c("ID_IE"), all=F)

teacher_pool_end_rural_t <- subset(teacher_pool_end,
                                   timing_end==1&
                                     treatment_d==1&
                                     urban.rural=="Rural")

teacher_pool_end_urban_t <- subset(teacher_pool_end,
                                   timing_end==1&
                                     treatment_d==1&
                                     urban.rural=="Urbana")

teacher_pool_end_rural_c <- subset(teacher_pool_end,
                                   timing_end==1&
                                     treatment_d==0&
                                     urban.rural=="Rural")

teacher_pool_end_urban_c <- subset(teacher_pool_end,
                                   timing_end==1&
                                     treatment_d==0&
                                     urban.rural=="Urbana")

mean(teacher_pool_end_rural_t$Number_of_Teachers)
mean(teacher_pool_end_urban_t$Number_of_Teachers)

mean(teacher_pool_end_rural_c$Number_of_Teachers)
mean(teacher_pool_end_urban_c$Number_of_Teachers)

table(teacher_pool_end_rural_c$Homework_Given_G7,
      teacher_pool_end_rural_c$Number_of_Teachers)

table(teacher_pool_end_rural_t$Homework_Given_G7,
      teacher_pool_end_rural_t$Number_of_Teachers)
