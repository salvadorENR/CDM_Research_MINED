# Load required libraries
library(dplyr)

# Read the data
student_test_7 <- read.csv("student_test_7_fus.csv", header = TRUE, stringsAsFactors = FALSE)

# Create test data frame with all question correctness columns
test_data_7 <- data.frame(
  Q1 = student_test_7$Q1_Correct_or_Wrong_G7,
  Q2 = student_test_7$Q2_Correct_or_Wrong_G7,
  Q3 = student_test_7$Q3_Correct_or_Wrong_G7,
  Q4 = student_test_7$Q4_Correct_or_Wrong_G7,
  Q5 = student_test_7$Q5_Correct_or_Wrong_G7,
  Q6 = student_test_7$Q6_Correct_or_Wrong_G7,
  Q7 = student_test_7$Q7_Correct_or_Wrong_G7,
  Q8 = student_test_7$Q8_Correct_or_Wrong_G7,
  Q9 = student_test_7$Q9_Correct_or_Wrong_G7,
  Q10 = student_test_7$Q10_Correct_or_Wrong_G7,
  Q11 = student_test_7$Q11_Correct_or_Wrong_G7,
  Q12 = student_test_7$Q12_Correct_or_Wrong_G7,
  Q13 = student_test_7$Q13_Correct_or_Wrong_G7,
  Q14 = student_test_7$Q14_Correct_or_Wrong_G7,
  Q15 = student_test_7$Q15_Correct_or_Wrong_G7,
  Q16 = student_test_7$Q16_Correct_or_Wrong_G7,
  Q17 = student_test_7$Q17_Correct_or_Wrong_G7,
  Q18 = student_test_7$Q18_Correct_or_Wrong_G7,
  Q19 = student_test_7$Q19_Correct_or_Wrong_G7,
  Q20 = student_test_7$Q20_Correct_or_Wrong_G7,
  Q21 = student_test_7$Q21_Correct_or_Wrong_G7,
  Q22 = student_test_7$Q22_Correct_or_Wrong_G7,
  Q23 = student_test_7$Q23_Correct_or_Wrong_G7,
  Q24 = student_test_7$Q24_Correct_or_Wrong_G7,
  Q25 = student_test_7$Q25_Correct_or_Wrong_G7
)

# Calculate correct answer rates for each item
correct_rates <- data.frame(
  Item = paste0("Q", 1:25),
  Correct_Rate = sapply(test_data_7, function(x) mean(x == 1, na.rm = TRUE)),
  N_Correct = sapply(test_data_7, function(x) sum(x == 1, na.rm = TRUE)),
  N_Wrong = sapply(test_data_7, function(x) sum(x == 0, na.rm = TRUE)),
  N_Missing = sapply(test_data_7, function(x) sum(is.na(x))),
  Total_Responses = sapply(test_data_7, function(x) sum(!is.na(x)))
)

# Add percentage format for easier reading
correct_rates$Correct_Percentage <- round(correct_rates$Correct_Rate * 100, 1)

# Display the results
print(correct_rates)

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Average correct rate across all items:", round(mean(correct_rates$Correct_Rate), 3), "\n")
cat("Range of correct rates:", round(min(correct_rates$Correct_Rate), 3), "-", round(max(correct_rates$Correct_Rate), 3), "\n")
cat("Most difficult item:", correct_rates$Item[which.min(correct_rates$Correct_Rate)], 
    "(", min(correct_rates$Correct_Rate), ")\n")
cat("Easiest item:", correct_rates$Item[which.max(correct_rates$Correct_Rate)], 
    "(", max(correct_rates$Correct_Rate), ")\n")

# Optional: Save to CSV
write.csv(correct_rates, "item_correct_rates_G7.csv", row.names = FALSE)

# Optional: Create a simple plot
barplot(correct_rates$Correct_Rate, 
        names.arg = correct_rates$Item,
        main = "Correct Answer Rates by Item (Grade 7)",
        ylab = "Correct Rate",
        xlab = "Items",
        ylim = c(0, 1),
        col = "lightblue")
abline(h = mean(correct_rates$Correct_Rate), col = "red", lwd = 2)
legend("topright", legend = paste("Mean =", round(mean(correct_rates$Correct_Rate), 3)), 
       col = "red", lwd = 2)