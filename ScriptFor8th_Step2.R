install.packages("GDINA", dependencies = T)

library(GDINA)

# Loading the updated Q-matrix for 8th grade
q_mat_8 <- read.csv("Revised Q-matrix2025_09_30_g8.csv", 
                    header=T, stringsAsFactors = F)

if("Item" %in% colnames(q_mat_8)) {
  q_mat_8 <- subset(q_mat_8, select=-c(Item))
}

student_test_8 <- read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

# Create test data and immediately remove problematic items
test_data_8 <- cbind.data.frame(
  student_test_8$Q1_Correct_or_Wrong_G7,
  # student_test_8$Q2_Correct_or_Wrong_G7,  # ELIMINADO - Ítem 2
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
  # student_test_8$Q21_Correct_or_Wrong_G7, # ELIMINADO - Ítem 21
  student_test_8$Q22_Correct_or_Wrong_G7, 
  # student_test_8$Q23_Correct_or_Wrong_G7, # ELIMINADO - Ítem 23
  # student_test_8$Q24_Correct_or_Wrong_G7  # ELIMINADO - Ítem 24
  student_test_8$Q25_Correct_or_Wrong_G7
)

# Also remove the corresponding rows from Q-matrix
items_to_remove <- c(2, 21, 23, 24)  # Ítems a eliminar inmediatamente
# items_to_consider <- c(11, 22)      # Ítems a considerar eliminar

q_mat_8_cleaned <- q_mat_8[-items_to_remove, ]

# Test identifiability with cleaned Q-matrix
test <- cdmTools::is.Qid(q_mat_8_cleaned, model = "others")
print(test)

# Fit models with cleaned data
est_gdina <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "GDINA",
                   mono.constraint = T,
                   control=list(conv.crit=0.000001))

est_dina <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "DINA",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_dino <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "DINO",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_rrum <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "RRUM",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

est_llm <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "LLM",
                 mono.constraint = T,
                 control=list(conv.crit=0.000001))

est_acdm <- GDINA(dat = test_data_8, Q = q_mat_8_cleaned, model = "ACDM",
                  mono.constraint = T,
                  control=list(conv.crit=0.000001))

# Compare model fit
modelfit(est_gdina)
modelfit(est_dina)
modelfit(est_dino)
modelfit(est_rrum)
modelfit(est_llm)
modelfit(est_acdm)

# Q-matrix validation with cleaned data
Qv <- Qval(est_gdina)  # Usar G-DINA para validación como recomienda el documento
plot(Qv, item = c(1:nrow(q_mat_8_cleaned)), eps = 0.95, data.label = TRUE)

# Additional analysis with items to consider removed (opcional)
# Si quieres probar también eliminando los ítems 11 y 22:
items_to_remove_extended <- c(2, 11, 21, 22, 23, 24)
q_mat_8_extended_cleaned <- q_mat_8[-items_to_remove_extended, ]
test_data_8_extended <- test_data_8[, -c(2, 10, 20, 21, 22, 23)] # Ajustar índices según lo que quedó
