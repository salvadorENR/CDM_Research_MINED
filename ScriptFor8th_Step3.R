install.packages("GDINA", dependencies = T)

library(GDINA)

# Loading the updated Q-matrix for 8th grade
q_mat_8 <- read.csv("Revised Q-matrix2025_09_30_g8.csv", 
                    header=T, stringsAsFactors = F)

if("Item" %in% colnames(q_mat_8)) {
  q_mat_8 <- subset(q_mat_8, select=-c(Item))
}

student_test_8 <- read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

# Create test data and remove ALL problematic items (2, 11, 21, 22, 23, 24)
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
  # student_test_8$Q11_Correct_or_Wrong_G7, # ELIMINADO - Ítem 11
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
  # student_test_8$Q22_Correct_or_Wrong_G7, # ELIMINADO - Ítem 22
  # student_test_8$Q23_Correct_or_Wrong_G7, # ELIMINADO - Ítem 23
  # student_test_8$Q24_Correct_or_Wrong_G7, # ELIMINADO - Ítem 24
  student_test_8$Q25_Correct_or_Wrong_G7
)

# Remove corresponding rows from Q-matrix for ALL problematic items
items_to_remove <- c(2, 11, 21, 22, 23, 24)  # Todos los ítems problemáticos
q_mat_8_cleaned <- q_mat_8[-items_to_remove, ]

# Verify the dimensions
cat("Dimensiones originales de Q-matrix:", dim(q_mat_8), "\n")
cat("Dimensiones de Q-matrix limpia:", dim(q_mat_8_cleaned), "\n")
cat("Dimensiones de datos de prueba:", dim(test_data_8), "\n")

# Test identifiability with cleaned Q-matrix
test <- cdmTools::is.Qid(q_mat_8_cleaned, model = "others")
print(test)

# Fit models with fully cleaned data
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
cat("=== COMPARACIÓN DE AJUSTE DEL MODELO ===\n")
cat("\nG-DINA Model:\n")
print(modelfit(est_gdina))

cat("\nDINA Model:\n")
print(modelfit(est_dina))

cat("\nDINO Model:\n")
print(modelfit(est_dino))

cat("\nR-RUM Model:\n")
print(modelfit(est_rrum))

cat("\nLLM Model:\n")
print(modelfit(est_llm))

cat("\nA-CDM Model:\n")
print(modelfit(est_acdm))

# Q-matrix validation with fully cleaned data
cat("\n=== VALIDACIÓN DE Q-MATRIZ LIMPIA ===\n")
Qv <- Qval(est_gdina)  # Usar G-DINA para validación como recomienda el documento
print(Qv)

# Plot Mesa plots for all remaining items
cat("\n=== GRÁFICOS MESA PARA ÍTEMS RESTANTES ===\n")
plot(Qv, item = c(1:nrow(q_mat_8_cleaned)), eps = 0.95, data.label = TRUE)

# Additional diagnostic information
cat("\n=== INFORMACIÓN DIAGNÓSTICA ADICIONAL ===\n")
cat("Ítems restantes en el análisis:", nrow(q_mat_8_cleaned), "\n")
cat("Ítems eliminados:", paste(items_to_remove, collapse = ", "), "\n")
cat("Tasa de eliminación:", round(length(items_to_remove)/25*100, 1), "%\n")

# Check item diagnosticity for remaining items
cat("\n=== DIAGNOSTICIDAD DE ÍTEMS RESTANTES ===\n")
item_discrim <- extract(est_gdina, what = "discrim")
print(item_discrim)






