install.packages("GDINA", dependencies = T)

library(GDINA)

# set your working directory
# store the following q matrix csv in your working directory

#setwd("")

# CHANGED: Grade 8 Q-matrix file
q_mat_8 <- read.csv("Revised Q-matrix2025_09_30_g8.csv", 
                      header=T, stringsAsFactors = F)

# CHANGED: Check if "Item" column exists before removing it
if("Item" %in% colnames(q_mat_8)) {
  q_mat_8 <- subset(q_mat_8, select=-c(Item))
}

#q_mat_end_filtered <- q_mat_end[-13, ]

test <- cdmTools::is.Qid(q_mat_8, model = "others")

print(test)

# CHANGED: Grade 8 test data file
student_test_8<- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

# CHANGED: Grade 8 column names (G8 instead of G7)
test_data_8 <- cbind.data.frame(student_test_8$Q1_Correct_or_Wrong_G7,
                                  student_test_8$Q2_Correct_or_Wrong_G7,
                                  student_test_8$Q3_Correct_or_Wrong_G7,
                                  student_test_8$Q4_Correct_or_Wrong_G7,
                                  student_test_8$Q5_Correct_or_Wrong_G7,
                                  student_test_8$Q6_Correct_or_Wrong_G7,
                                  student_test_8$Q7_Correct_or_Wrong_G7,
                                  student_test_8$Q8_Correct_or_Wrong_G7,
                                  student_test_8$Q9_Correct_or_Wrong_G7,
                                  student_test_8$Q10_Correct_or_Wrong_G7,
                                  student_test_8$Q11_Correct_or_Wrong_G7,
                                  student_test_8$Q12_Correct_or_Wrong_G7,
                                  student_test_8$Q13_Correct_or_Wrong_G7,
                                  student_test_8$Q14_Correct_or_Wrong_G7,
                                  student_test_8$Q15_Correct_or_Wrong_G7,
                                  student_test_8$Q16_Correct_or_Wrong_G7,
                                  student_test_8$Q17_Correct_or_Wrong_G7,
                                  student_test_8$Q18_Correct_or_Wrong_G7,
                                  student_test_8$Q19_Correct_or_Wrong_G7,
                                  student_test_8$Q20_Correct_or_Wrong_G7,
                                  student_test_8$Q21_Correct_or_Wrong_G7,
                                  student_test_8$Q22_Correct_or_Wrong_G7,
                                  student_test_8$Q23_Correct_or_Wrong_G7,
                                  student_test_8$Q24_Correct_or_Wrong_G7,
                                  student_test_8$Q25_Correct_or_Wrong_G7)

# model comparison: end-line except No.13

est_gdina <- GDINA(dat = test_data_8, Q = q_mat_8, model = "GDINA",
                   mono.constraint = T,
                   control=list(conv.crit=0.000001))

est_dina <- GDINA(dat = test_data_8, Q = q_mat_8, model = "DINA",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_dino <- GDINA(dat = test_data_8, Q = q_mat_8, model = "DINO",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_rrum <- GDINA(dat = test_data_8, Q = q_mat_8, model = "RRUM",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_llm <- GDINA(dat = test_data_8, Q = q_mat_8, model = "LLM",
                 mono.constraint = T,
                 control=list(conv.crit=0.000001))

est_acdm <- GDINA(dat = test_data_8, Q = q_mat_8, model = "ACDM",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

#

modelfit(est_gdina)
modelfit(est_dina)
modelfit(est_dino)
modelfit(est_rrum)
modelfit(est_llm)
modelfit(est_acdm)

Qv <- Qval(est_rrum)
plot(Qv, item=c(1:25), eps=0.95, data.label = TRUE)