
install.packages("GDINA", dependencies = T)

library(GDINA)

# set your working directory
# store the following q matrix csv in your working directory

#setwd("")

q_mat_end <- read.csv("Revised Q-matrix2025_09_30_g7.csv", 
                      header=T, stringsAsFactors = F)

q_mat_end <- subset(q_mat_end,
                    select=-c(Item))

q_mat_end_filtered <- q_mat_end[-13, ]

test <- cdmTools::is.Qid(q_mat_end_filtered, model = "others")

print(test)

# download and read the csv data file for the end-line of grade 7
# student_test_7_endline.csv
# from open data source URL
# https://data.mendeley.com/datasets/vmf6fbfm2n/1
# store the file in your working directory

student_test_7_end <- 
  read.csv("student_test_7_endline.csv", header=T, stringsAsFactors = F)

# compose matrix of end-line response data except No.13

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
                                  student_test_7_end$Q14_Correct_or_Wrong_G7,
                                  student_test_7_end$Q15_Correct_or_Wrong_G7,
                                  student_test_7_end$Q16_Correct_or_Wrong_G7,
                                  student_test_7_end$Q17_Correct_or_Wrong_G7,
                                  student_test_7_end$Q18_Correct_or_Wrong_G7,
                                  student_test_7_end$Q19_Correct_or_Wrong_G7,
                                  student_test_7_end$Q20_Correct_or_Wrong_G7)

# model comparison: end-line except No.13

est_gdina <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "GDINA",
                   mono.constraint = T,
                   control=list(conv.crit=0.000001))

est_dina <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "DINA",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_dino <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "DINO",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_rrum <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "RRUM",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_llm <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "LLM",
                 mono.constraint = T,
                 control=list(conv.crit=0.000001))

est_acdm <- GDINA(dat = test_data_end, Q = q_mat_end_filtered, model = "ACDM",
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
plot(Qv, item=c(1:19), eps=0.95, data.label = TRUE)

