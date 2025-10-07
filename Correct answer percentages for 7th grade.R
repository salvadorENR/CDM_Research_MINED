# Install and load required packages
install.packages("GDINA", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)

library(GDINA)
library(ggplot2)

# Load the student test data
student_test_7_end <- read.csv("student_test_7_endline.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate correct answer percentage for each question
correct_percentages <- data.frame(
  Question = paste0("Q", c(1:12, 14:20)),
  Correct_Percentage = NA
)

# List of question columns (excluding Q13 as in your original code)
question_cols <- c(
  "Q1_Correct_or_Wrong_G7", "Q2_Correct_or_Wrong_G7", "Q3_Correct_or_Wrong_G7",
  "Q4_Correct_or_Wrong_G7", "Q5_Correct_or_Wrong_G7", "Q6_Correct_or_Wrong_G7",
  "Q7_Correct_or_Wrong_G7", "Q8_Correct_or_Wrong_G7", "Q9_Correct_or_Wrong_G7",
  "Q10_Correct_or_Wrong_G7", "Q11_Correct_or_Wrong_G7", "Q12_Correct_or_Wrong_G7",
  "Q14_Correct_or_Wrong_G7", "Q15_Correct_or_Wrong_G7", "Q16_Correct_or_Wrong_G7",
  "Q17_Correct_or_Wrong_G7", "Q18_Correct_or_Wrong_G7", "Q19_Correct_or_Wrong_G7",
  "Q20_Correct_or_Wrong_G7"
)

# Calculate percentage correct for each question
for(i in 1:length(question_cols)) {
  col_name <- question_cols[i]
  correct_count <- sum(student_test_7_end[[col_name]] == 1, na.rm = TRUE)
  total_count <- sum(!is.na(student_test_7_end[[col_name]]))
  correct_percentages$Correct_Percentage[i] <- round((correct_count / total_count) * 100, 2)
}

# Display the results
print("Correct Answer Percentages by Question:")
print(correct_percentages)

# Calculate overall average correct percentage
overall_avg <- mean(correct_percentages$Correct_Percentage)
cat("\nOverall Average Correct Percentage:", round(overall_avg, 2), "%\n")

# Create a bar plot to visualize the results
ggplot(correct_percentages, aes(x = Question, y = Correct_Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = paste0(Correct_Percentage, "%")), vjust = -0.5, size = 3) +
  labs(title = "Correct Answer Percentage by Question (Endline Test)",
       y = "Correct Percentage (%)",
       x = "Question") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary statistics
cat("\n--- Summary Statistics ---\n")
cat("Minimum correct percentage:", min(correct_percentages$Correct_Percentage), "%\n")
cat("Maximum correct percentage:", max(correct_percentages$Correct_Percentage), "%\n")
cat("Median correct percentage:", median(correct_percentages$Correct_Percentage), "%\n")

# Questions with highest and lowest performance
easiest_questions <- correct_percentages[which.max(correct_percentages$Correct_Percentage), ]
hardest_questions <- correct_percentages[which.min(correct_percentages$Correct_Percentage), ]

cat("\nEasiest question:", easiest_questions$Question, "(", easiest_questions$Correct_Percentage, "% correct)\n")
cat("Hardest question:", hardest_questions$Question, "(", hardest_questions$Correct_Percentage, "% correct)\n")

# Additional analysis: Count of students who answered each question
cat("\n--- Response Counts by Question ---\n")
response_counts <- data.frame(
  Question = correct_percentages$Question,
  Responses = sapply(question_cols, function(col) {
    sum(!is.na(student_test_7_end[[col]]))
  })
)
print(response_counts)

# Performance by question difficulty categories (if you want to categorize)
cat("\n--- Performance by Quartiles ---\n")
quartiles <- quantile(correct_percentages$Correct_Percentage)
cat("25th percentile:", quartiles[2], "%\n")
cat("50th percentile (median):", quartiles[3], "%\n") 
cat("75th percentile:", quartiles[4], "%\n")

# Questions below 50% correct
difficult_questions <- correct_percentages[correct_percentages$Correct_Percentage < 50, ]
cat("\nQuestions with less than 50% correct:\n")
print(difficult_questions)