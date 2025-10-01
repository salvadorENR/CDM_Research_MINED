# ESMATE Project - Grade 8 Analysis
# Revised R script based on Grade 7 code

install.packages("GDINA", dependencies = TRUE)
install.packages("cdmTools", dependencies = TRUE)

library(GDINA)
library(cdmTools)

# Set your working directory
setwd("C:/Your/Directory/Path/Here")  # Modify this path

# Read the Q-matrix for Grade 8
q_mat_grade8 <- read.csv("Revised Q-matrix2025_09_30_g8.csv",  # Assuming similar naming convention
                         header = TRUE, stringsAsFactors = FALSE)

# Remove Item column if exists (similar to Grade 7 approach)
if("Item" %in% colnames(q_mat_grade8)) {
  q_mat_grade8 <- subset(q_mat_grade8, select = -c(Item))
}

# Check if any items need to be filtered (similar to Grade 7 removing item 13)
# Adjust this based on your Grade 8 Q-matrix structure
q_mat_grade8_filtered <- q_mat_grade8  # Modify if specific items need removal

# Validate Q-matrix
test_q <- cdmTools::is.Qid(q_mat_grade8_filtered, model = "others")
print(test_q)

# Read Grade 8 test data
student_test_grade8 <- read.csv("student_test_7_fus.csv",  # Using the file mentioned in email
                                header = TRUE, stringsAsFactors = FALSE)

# Compose response matrix for Grade 8
# Adjust column names based on actual Grade 8 data structure
test_data_grade8 <- data.frame(
  student_test_grade8$Q1_Correct_or_Wrong_G8,   # Adjust column names as needed
  student_test_grade8$Q2_Correct_or_Wrong_G8,
  student_test_grade8$Q3_Correct_or_Wrong_G8,
  student_test_grade8$Q4_Correct_or_Wrong_G8,
  student_test_grade8$Q5_Correct_or_Wrong_G8,
  student_test_grade8$Q6_Correct_or_Wrong_G8,
  student_test_grade8$Q7_Correct_or_Wrong_G8,
  student_test_grade8$Q8_Correct_or_Wrong_G8,
  student_test_grade8$Q9_Correct_or_Wrong_G8,
  student_test_grade8$Q10_Correct_or_Wrong_G8,
  student_test_grade8$Q11_Correct_or_Wrong_G8,
  student_test_grade8$Q12_Correct_or_Wrong_G8,
  student_test_grade8$Q13_Correct_or_Wrong_G8,  # Include if applicable
  student_test_grade8$Q14_Correct_or_Wrong_G8,
  student_test_grade8$Q15_Correct_or_Wrong_G8,
  student_test_grade8$Q16_Correct_or_Wrong_G8,
  student_test_grade8$Q17_Correct_or_Wrong_G8,
  student_test_grade8$Q18_Correct_or_Wrong_G8,
  student_test_grade8$Q19_Correct_or_Wrong_G8,
  student_test_grade8$Q20_Correct_or_Wrong_G8
)

# Remove any NA columns (if Grade 8 has different number of items)
test_data_grade8 <- test_data_grade8[, colSums(is.na(test_data_grade8)) < nrow(test_data_grade8)]

# Model estimation for Grade 8
est_gdina_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "GDINA",
                      mono.constraint = TRUE,
                      control = list(conv.crit = 0.000001))

est_dina_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "DINA",
                     mono.constraint = TRUE,
                     control = list(conv.crit = 0.000001))

est_dino_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "DINO",
                     mono.constraint = TRUE,
                     control = list(conv.crit = 0.000001))

est_rrum_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "RRUM",
                     mono.constraint = TRUE,
                     control = list(conv.crit = 0.000001))

est_llm_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "LLM",
                    mono.constraint = TRUE,
                    control = list(conv.crit = 0.000001))

est_acdm_g8 <- GDINA(dat = test_data_grade8, Q = q_mat_grade8_filtered, model = "ACDM",
                     mono.constraint = TRUE,
                     control = list(conv.crit = 0.000001))

# Model fit comparison for Grade 8
cat("=== GRADE 8 MODEL FIT COMPARISON ===\n")
cat("\nGDINA Model Fit:\n")
print(modelfit(est_gdina_g8))

cat("\nDINA Model Fit:\n")
print(modelfit(est_dina_g8))

cat("\nDINO Model Fit:\n")
print(modelfit(est_dino_g8))

cat("\nRRUM Model Fit:\n")
print(modelfit(est_rrum_g8))

cat("\nLLM Model Fit:\n")
print(modelfit(est_llm_g8))

cat("\nACDM Model Fit:\n")
print(modelfit(est_acdm_g8))

# Q-matrix validation for the best fitting model
# Using RRUM as example (adjust based on your best model)
Qv_g8 <- Qval(est_rrum_g8)
plot(Qv_g8, item = c(1:ncol(test_data_grade8)), eps = 0.95, data.label = TRUE, 
     main = "Grade 8 Q-matrix Validation")

# Additional useful outputs for reporting
cat("\n=== GRADE 8 SUMMARY STATISTICS ===\n")
cat("Number of students:", nrow(test_data_grade8), "\n")
cat("Number of items:", ncol(test_data_grade8), "\n")
cat("Average score:", mean(rowSums(test_data_grade8)), "\n")

# Item difficulty
item_difficulty <- colMeans(test_data_grade8)
cat("\nItem Difficulty (proportion correct):\n")
print(round(item_difficulty, 3))

# Save workspace for further analysis
save.image("ESMATE_Grade8_Analysis.RData")