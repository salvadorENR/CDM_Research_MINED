install.packages("GDINA", dependencies = T)

library(GDINA)

#Loading the updated Q-matrix for 8th grade
q_mat_8 <- read.csv("Revised Q-matrix2025_09_30_g8.csv", 
                      header=T, stringsAsFactors = F)

if("Item" %in% colnames(q_mat_8)) {
  q_mat_8 <- subset(q_mat_8, select=-c(Item))
}

test <- cdmTools::is.Qid(q_mat_8, model = "others")

print(test)

student_test_8<- 
  read.csv("student_test_7_fus.csv", header=T, stringsAsFactors = F)

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


modelfit(est_gdina)
modelfit(est_dina)
modelfit(est_dino)
modelfit(est_rrum)
modelfit(est_llm)
modelfit(est_acdm)

#For RRUM
Qv <- Qval(est_rrum)
plot(Qv, item=c(1:25), eps=0.95, data.label = TRUE)

#For G-Dina
Qv1 <- Qval(est_gdina)
plot(Qv1, item=c(1:25), eps=0.95, data.label = TRUE)

# ============================================================================
# CREATE PROPOSED Q-MATRICES BASED ON RRUM AND GDINA MODELS
# ============================================================================

# Extract suggested Q-matrices from Qval results
proposed_q_matrix_rrum <- Qv$sug.Q
proposed_q_matrix_gdina <- Qv1$sug.Q

# Display ORIGINAL Q-matrix
print("ORIGINAL Q-MATRIX:")
original_display <- q_mat_8
rownames(original_display) <- paste0("Item", 1:nrow(original_display))
colnames(original_display) <- c("Attr1", "Attr2", "Attr3", "Attr4")
print(original_display)

# Display PROPOSED Q-MATRIX FROM RRUM MODEL with asterisks for changes
print("PROPOSED Q-MATRIX FROM RRUM MODEL (asterisks * indicate changed entries):")

proposed_with_asterisks_rrum <- matrix(as.character(proposed_q_matrix_rrum), 
                                       nrow = nrow(proposed_q_matrix_rrum))

for(i in 1:nrow(q_mat_8)) {
  for(j in 1:ncol(q_mat_8)) {
    if(q_mat_8[i, j] != proposed_q_matrix_rrum[i, j]) {
      proposed_with_asterisks_rrum[i, j] <- paste0(proposed_q_matrix_rrum[i, j], "*")
    }
  }
}

rownames(proposed_with_asterisks_rrum) <- paste0("Item", 1:nrow(proposed_with_asterisks_rrum))
colnames(proposed_with_asterisks_rrum) <- c("Attr1", "Attr2", "Attr3", "Attr4")
print(proposed_with_asterisks_rrum, quote = FALSE)

# Display PROPOSED Q-MATRIX FROM GDINA MODEL with asterisks for changes
print("PROPOSED Q-MATRIX FROM GDINA MODEL (asterisks * indicate changed entries):")

proposed_with_asterisks_gdina <- matrix(as.character(proposed_q_matrix_gdina), 
                                        nrow = nrow(proposed_q_matrix_gdina))

for(i in 1:nrow(q_mat_8)) {
  for(j in 1:ncol(q_mat_8)) {
    if(q_mat_8[i, j] != proposed_q_matrix_gdina[i, j]) {
      proposed_with_asterisks_gdina[i, j] <- paste0(proposed_q_matrix_gdina[i, j], "*")
    }
  }
}

rownames(proposed_with_asterisks_gdina) <- paste0("Item", 1:nrow(proposed_with_asterisks_gdina))
colnames(proposed_with_asterisks_gdina) <- c("Attr1", "Attr2", "Attr3", "Attr4")
print(proposed_with_asterisks_gdina, quote = FALSE)

# Save the proposed Q-matrices to CSV
write.csv(proposed_q_matrix_rrum, "proposed_q_matrix_rrum.csv", row.names = FALSE)
write.csv(proposed_q_matrix_gdina, "proposed_q_matrix_gdina.csv", row.names = FALSE)

# Show summary of changes for both models
modified_items_rrum <- which(apply(q_mat_8 != proposed_q_matrix_rrum, 1, any))
modified_items_gdina <- which(apply(q_mat_8 != proposed_q_matrix_gdina, 1, any))

print(paste("RRUM MODEL - Items with modified Q-entries:", paste(modified_items_rrum, collapse = ", ")))
print(paste("GDINA MODEL - Items with modified Q-entries:", paste(modified_items_gdina, collapse = ", ")))

# Show detailed changes for RRUM model
if(length(modified_items_rrum) > 0) {
  print("RRUM MODEL - DETAILED CHANGES BY ITEM:")
  for(item in modified_items_rrum) {
    original_pattern <- paste(q_mat_8[item,], collapse = "")
    proposed_pattern <- paste(proposed_q_matrix_rrum[item,], collapse = "")
    cat(sprintf("Item %d: %s → %s\n", item, original_pattern, proposed_pattern))
    
    changes <- which(q_mat_8[item,] != proposed_q_matrix_rrum[item,])
    if(length(changes) > 0) {
      cat(sprintf("       Changed attributes: %s\n", paste(paste0("Attr", changes), collapse = ", ")))
    }
    cat("\n")
  }
} else {
  print("RRUM MODEL - No changes were made to the Q-matrix.")
}

# Show detailed changes for GDINA model
if(length(modified_items_gdina) > 0) {
  print("GDINA MODEL - DETAILED CHANGES BY ITEM:")
  for(item in modified_items_gdina) {
    original_pattern <- paste(q_mat_8[item,], collapse = "")
    proposed_pattern <- paste(proposed_q_matrix_gdina[item,], collapse = "")
    cat(sprintf("Item %d: %s → %s\n", item, original_pattern, proposed_pattern))
    
    changes <- which(q_mat_8[item,] != proposed_q_matrix_gdina[item,])
    if(length(changes) > 0) {
      cat(sprintf("       Changed attributes: %s\n", paste(paste0("Attr", changes), collapse = ", ")))
    }
    cat("\n")
  }
} else {
  print("GDINA MODEL - No changes were made to the Q-matrix.")
}