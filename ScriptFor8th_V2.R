# ESMATE Project - Grade 8 Analysis
# Enhanced and Improved Version with Robust File Handling
# Date: October 2024

# Install and load required packages
required_packages <- c("GDINA", "cdmTools", "ggplot2", "dplyr", "reshape2", "officer", "flextable")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(GDINA)
library(cdmTools)
library(ggplot2)
library(dplyr)
library(reshape2)
library(officer)
library(flextable)


# =============================================================================
# DATA LOADING AND PREPROCESSING FUNCTIONS
# =============================================================================

load_and_validate_data <- function(response_file, qmatrix_file) {
  # Robust Q-matrix file handling
  cat("Loading Q-matrix...\n")
  if(!file.exists(qmatrix_file)) {
    # Try alternative filename variations
    alternative_names <- c(
      qmatrix_file,
      "Revised_Q-matrix2025_09_30_g8.csv",
      "Revised Q-matrix2025_09_30_g8.csv", 
      "Revised_Q_matrix2025_09_30_g8.csv",
      "Qmatrix_grade8.csv",
      "Q_matrix_grade8.csv",
      "qmatrix_g8.csv"
    )
    
    found_file <- NULL
    for(fname in alternative_names) {
      if(file.exists(fname)) {
        found_file <- fname
        cat("Found Q-matrix file:", fname, "\n")
        break
      }
    }
    
    if(is.null(found_file)) {
      stop("Q-matrix file not found. Tried:\n", paste(alternative_names, collapse = "\n"))
    } else {
      qmatrix_file <- found_file
    }
  }
  
  q_mat <- read.csv(qmatrix_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Remove Item column if exists
  if("Item" %in% colnames(q_mat)) {
    q_mat <- subset(q_mat, select = -c(Item))
  }
  
  # Robust response data file handling
  cat("Loading response data...\n")
  if(!file.exists(response_file)) {
    # Try alternative response file names
    alternative_response_names <- c(
      response_file,
      "student_test_7_fus.csv",
      "student_test_grade8.csv",
      "grade8_test_data.csv",
      "test_data_g8.csv"
    )
    
    found_response_file <- NULL
    for(fname in alternative_response_names) {
      if(file.exists(fname)) {
        found_response_file <- fname
        cat("Found response data file:", fname, "\n")
        break
      }
    }
    
    if(is.null(found_response_file)) {
      stop("Response data file not found. Tried:\n", paste(alternative_response_names, collapse = "\n"))
    } else {
      response_file <- found_response_file
    }
  }
  
  response_data <- read.csv(response_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Dynamically identify response columns (handles different naming conventions)
  response_patterns <- c("Correct_or_Wrong", "Correct", "Score", "Q[0-9]+", "Item[0-9]+", "R[0-9]+")
  response_columns <- NULL
  
  for(pattern in response_patterns) {
    response_columns <- grep(pattern, names(response_data), value = TRUE, ignore.case = TRUE)
    if(length(response_columns) > 0) {
      cat("Found", length(response_columns), "response columns using pattern:", pattern, "\n")
      break
    }
  }
  
  if(length(response_columns) == 0) {
    # Last resort: try to identify numeric columns that look like responses
    numeric_cols <- sapply(response_data, is.numeric)
    if(sum(numeric_cols) > 0) {
      response_columns <- names(response_data)[numeric_cols]
      cat("Using numeric columns as responses:", paste(response_columns, collapse = ", "), "\n")
    } else {
      stop("No response columns found in the data. Check column names.")
    }
  }
  
  test_data <- response_data[, response_columns, drop = FALSE]
  
  # Data quality checks
  cat("\n=== DATA QUALITY CHECK ===\n")
  cat("Number of students:", nrow(test_data), "\n")
  cat("Number of items:", ncol(test_data), "\n")
  cat("Missing values:", sum(is.na(test_data)), "\n")
  cat("Data range:", paste(range(test_data, na.rm = TRUE), collapse = " to "), "\n")
  cat("Average score:", round(mean(rowSums(test_data, na.rm = TRUE), 2)), "/", ncol(test_data), "\n")
  cat("Average item difficulty:", round(mean(colMeans(test_data, na.rm = TRUE), 3)), "\n")
  
  # Display first few item difficulties
  item_diffs <- round(colMeans(test_data, na.rm = TRUE), 3)
  cat("Item difficulties (first 10):", paste(item_diffs[1:min(10, length(item_diffs))], collapse = ", "), "\n\n")
  
  # Handle missing values (if any)
  if(any(is.na(test_data))) {
    cat("Warning: Missing values found. Imputing with mode.\n")
    mode_impute <- function(x) {
      x[is.na(x)] <- as.numeric(names(which.max(table(x))))
      return(x)
    }
    test_data <- as.data.frame(lapply(test_data, mode_impute))
  }
  
  # Validate Q-matrix dimensions
  if(ncol(test_data) != nrow(q_mat)) {
    cat("Warning: Q-matrix dimensions (", nrow(q_mat), "rows) don't match response data (", 
        ncol(test_data), "columns)\n")
    cat("Attempting to align Q-matrix...\n")
    
    # Try to align by taking first n items from Q-matrix
    min_items <- min(nrow(q_mat), ncol(test_data))
    q_mat <- q_mat[1:min_items, , drop = FALSE]
    test_data <- test_data[, 1:min_items, drop = FALSE]
    cat("Using first", min_items, "items from both Q-matrix and response data.\n")
  }
  
  # Validate Q-matrix structure
  q_valid <- cdmTools::is.Qid(q_mat, model = "others")
  cat("Q-matrix validation:", ifelse(q_valid, "PASS", "FAIL"), "\n")
  
  return(list(test_data = test_data, q_matrix = q_mat, response_data = response_data))
}

# =============================================================================
# MODEL FITTING AND COMPARISON
# =============================================================================

fit_cdm_models <- function(test_data, q_matrix) {
  models <- c("GDINA", "DINA", "DINO", "RRUM", "LLM", "ACDM")
  model_fits <- list()
  
  cat("\n=== FITTING CDM MODELS ===\n")
  
  for(i in seq_along(models)) {
    cat("Fitting", models[i], "model...")
    tryCatch({
      model_fits[[models[i]]] <- GDINA(
        dat = test_data, 
        Q = q_matrix, 
        model = models[i],
        mono.constraint = TRUE,
        control = list(conv.crit = 0.000001, maxitr = 500)
      )
      cat(" SUCCESS\n")
    }, error = function(e) {
      cat(" FAILED -", e$message, "\n")
      model_fits[[models[i]]] <- NULL
    })
  }
  
  # Check if any models were successfully fitted
  successful_models <- names(model_fits)
  if(length(successful_models) == 0) {
    stop("No CDM models could be fitted. Please check your data and Q-matrix.")
  }
  
  cat("Successfully fitted", length(successful_models), "models:", paste(successful_models, collapse = ", "), "\n")
  return(model_fits)
}

compare_models <- function(model_fits) {
  # Extract fit statistics
  fit_stats <- data.frame(
    Model = character(),
    AIC = numeric(),
    BIC = numeric(),
    logLik = numeric(),
    Deviance = numeric(),
    npar = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(model_name in names(model_fits)) {
    if(!is.null(model_fits[[model_name]])) {
      fit <- modelfit(model_fits[[model_name]])
      fit_stats <- rbind(fit_stats, data.frame(
        Model = model_name,
        AIC = AIC(model_fits[[model_name]]),
        BIC = BIC(model_fits[[model_name]]),
        logLik = logLik(model_fits[[model_name]]),
        Deviance = fit$Deviance,
        npar = fit$npar
      ))
    }
  }
  
  # Identify best model by AIC and BIC
  fit_stats$AIC_rank <- rank(fit_stats$AIC)
  fit_stats$BIC_rank <- rank(fit_stats$BIC)
  fit_stats$Best_AIC <- fit_stats$AIC_rank == 1
  fit_stats$Best_BIC <- fit_stats$BIC_rank == 1
  
  # Sort by AIC
  fit_stats <- fit_stats[order(fit_stats$AIC), ]
  
  return(fit_stats)
}

# =============================================================================
# RESULTS EXTRACTION AND ANALYSIS
# =============================================================================

extract_detailed_results <- function(best_model, test_data, q_matrix) {
  results <- list()
  
  cat("Extracting detailed results from best model...\n")
  
  # Item parameters
  results$item_params <- coef(best_model)
  
  # Attribute mastery probabilities
  results$attribute_mastery <- personparm(best_model)
  
  # Expected vs observed scores
  results$expected_scores <- rowSums(fitted(best_model))
  results$observed_scores <- rowSums(test_data)
  
  # Model fit at item level
  results$item_fit <- itemfit(best_model)
  
  # Q-matrix validation
  results$q_validation <- Qval(best_model)
  
  # Reliability estimates
  results$reliability <- reliability(best_model)
  
  # Additional diagnostics
  results$model_converged <- best_model$converged
  results$n_attributes <- ncol(q_matrix)
  results$n_items <- nrow(q_matrix)
  results$n_students <- nrow(test_data)
  
  return(results)
}

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

create_diagnostic_plots <- function(model_fits, best_model, detailed_results, output_prefix = "Grade8") {
  plots <- list()
  
  cat("Creating diagnostic plots...\n")
  
  # 1. Model comparison plot
  fit_stats <- compare_models(model_fits)
  melt_fit <- melt(fit_stats, id.vars = "Model", 
                   measure.vars = c("AIC", "BIC"))
  
  plots$model_comparison <- ggplot(melt_fit, aes(x = Model, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Model Comparison - AIC and BIC",
         y = "Information Criterion", x = "Model") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 2. Attribute mastery distribution
  attr_mastery <- as.data.frame(detailed_results$attribute_mastery)
  plots$attribute_mastery <- ggplot(melt(attr_mastery), aes(x = value, fill = variable)) +
    geom_histogram(alpha = 0.7, bins = 20) +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Attribute Mastery Distribution",
         x = "Mastery Probability", y = "Frequency") +
    theme_minimal()
  
  # 3. Expected vs observed scores
  score_comparison <- data.frame(
    Expected = detailed_results$expected_scores,
    Observed = detailed_results$observed_scores
  )
  plots$score_comparison <- ggplot(score_comparison, aes(x = Expected, y = Observed)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Expected vs Observed Scores",
         x = "Expected Scores", y = "Observed Scores") +
    theme_minimal()
  
  # 4. Item difficulty
  item_difficulty <- data.frame(
    Item = 1:detailed_results$n_items,
    Difficulty = colMeans(best_model$dat)
  )
  plots$item_difficulty <- ggplot(item_difficulty, aes(x = factor(Item), y = Difficulty)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    labs(title = "Item Difficulty", x = "Item", y = "Proportion Correct") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 5. Q-matrix validation plot
  png(paste0(output_prefix, "_qmatrix_validation.png"), width = 10, height = 8, units = "in", res = 300)
  plot(detailed_results$q_validation, item = 1:detailed_results$n_items, eps = 0.95, data.label = TRUE,
       main = "Q-matrix Validation")
  dev.off()
  
  # Save plots
  for(plot_name in names(plots)) {
    if(plot_name != "qmatrix_validation") {  # Already saved above
      ggsave(paste0(output_prefix, "_", plot_name, ".png"), 
             plots[[plot_name]], width = 8, height = 6, dpi = 300)
    }
  }
  
  return(plots)
}

# =============================================================================
# POWERPOINT REPORT GENERATION
# =============================================================================

generate_powerpoint_report <- function(model_fits, best_model, detailed_results, 
                                       fit_stats, plots, output_file = "Grade8_Analysis_Report.pptx") {
  
  cat("Generating PowerPoint report...\n")
  
  # Create new PowerPoint
  doc <- read_pptx()
  
  # Title slide
  doc <- add_slide(doc, layout = "Title Slide", master = "Office Theme")
  doc <- ph_with(doc, value = "ESMATE Project - Grade 8 CDM Analysis", 
                 location = ph_location_type(type = "ctrTitle"))
  doc <- ph_with(doc, value = paste("Analysis Date:", format(Sys.Date(), "%B %d, %Y")), 
                 location = ph_location_type(type = "subTitle"))
  
  # Overview slide
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = "Analysis Overview", 
                 location = ph_location_type(type = "title"))
  
  best_model_name <- fit_stats$Model[fit_stats$Best_AIC]
  overview_text <- paste(
    "• Number of Students: ", detailed_results$n_students, "\n",
    "• Number of Items: ", detailed_results$n_items, "\n", 
    "• Number of Attributes: ", detailed_results$n_attributes, "\n",
    "• Best Fitting Model: ", best_model_name, "\n",
    "• Average Observed Score: ", round(mean(detailed_results$observed_scores), 2), "/", detailed_results$n_items, "\n",
    "• Model Reliability: ", round(detailed_results$reliability$rho, 3), "\n",
    "• Model Converged: ", ifelse(detailed_results$model_converged, "Yes", "No"), "\n",
    sep = ""
  )
  doc <- ph_with(doc, value = overview_text, 
                 location = ph_location_type(type = "body"))
  
  # Model comparison slide
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = "Model Fit Statistics", 
                 location = ph_location_type(type = "title"))
  
  fit_table <- fit_stats %>% 
    select(Model, AIC, BIC, logLik, npar) %>%
    mutate(across(where(is.numeric), round, 2))
  
  # Highlight best model
  best_ft <- fit_table %>% 
    filter(Model == best_model_name) %>%
    flextable() %>%
    bg(bg = "#E6F3FF", part = "body")
  
  doc <- ph_with(doc, value = best_ft, 
                 location = ph_location_type(type = "body"))
  
  # Add plot slides
  plot_files <- list.files(pattern = "Grade8_.*\\.png$")
  for(plot_file in plot_files) {
    doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
    plot_name <- gsub("Grade8_|.png", "", plot_file)
    plot_name <- gsub("_", " ", plot_name)
    doc <- ph_with(doc, value = tools::toTitleCase(plot_name), 
                   location = ph_location_type(type = "title"))
    doc <- ph_with(doc, value = external_img(plot_file), 
                   location = ph_location_type(type = "body"))
  }
  
  # Item parameters slide
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = "Item Summary Statistics", 
                 location = ph_location_type(type = "title"))
  
  item_summary <- data.frame(
    Item = 1:detailed_results$n_items,
    Difficulty = round(colMeans(best_model$dat), 3),
    Attribute = apply(detailed_results$item_params, 1, 
                      function(x) paste(which(x > 0.1), collapse = ","))
  )
  
  doc <- ph_with(doc, value = flextable(head(item_summary, 15)), 
                 location = ph_location_type(type = "body"))
  
  # Attribute mastery summary slide
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = "Attribute Mastery Summary", 
                 location = ph_location_type(type = "title"))
  
  attr_summary <- data.frame(
    Attribute = colnames(detailed_results$attribute_mastery),
    Mean_Mastery = round(colMeans(detailed_results$attribute_mastery), 3),
    SD_Mastery = round(apply(detailed_results$attribute_mastery, 2, sd), 3)
  )
  
  doc <- ph_with(doc, value = flextable(attr_summary), 
                 location = ph_location_type(type = "body"))
  
  # Conclusion slide
  doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- ph_with(doc, value = "Conclusions and Recommendations", 
                 location = ph_location_type(type = "title"))
  
  conclusions <- paste(
    "• The ", best_model_name, " model provided the best fit to the data.\n",
    "• Average attribute mastery levels range from ", 
    round(min(colMeans(detailed_results$attribute_mastery)), 3), " to ",
    round(max(colMeans(detailed_results$attribute_mastery)), 3), ".\n",
    "• Model shows ", ifelse(detailed_results$reliability$rho > 0.7, "good", "moderate"), 
    " reliability (", round(detailed_results$reliability$rho, 3), ").\n",
    "• The Q-matrix appears to be ", 
    ifelse(mean(detailed_results$q_validation$Qvals) > 0.8, "appropriate", "potentially problematic"), 
    " for this dataset.\n",
    sep = ""
  )
  
  doc <- ph_with(doc, value = conclusions, 
                 location = ph_location_type(type = "body"))
  
  # Save PowerPoint
  print(doc, target = output_file)
  cat("PowerPoint report saved as:", output_file, "\n")
}

# =============================================================================
# MAIN ANALYSIS FUNCTION
# =============================================================================

run_grade8_analysis <- function(response_file = "student_test_7_fus.csv",
                                qmatrix_file = "Revised Q-matrix2025_09_30_g8.csv",  # Your actual filename with spaces
                                generate_report = TRUE) {
  
  cat("=== ESMATE GRADE 8 CDM ANALYSIS ===\n")
  cat("Response file:", response_file, "\n")
  cat("Q-matrix file:", qmatrix_file, "\n\n")
  
  # Step 1: Load and validate data
  data_objects <- load_and_validate_data(response_file, qmatrix_file)
  test_data <- data_objects$test_data
  q_matrix <- data_objects$q_matrix
  
  # Step 2: Fit CDM models
  model_fits <- fit_cdm_models(test_data, q_matrix)
  
  # Step 3: Compare models and select best one
  fit_stats <- compare_models(model_fits)
  cat("\n=== MODEL COMPARISON RESULTS ===\n")
  print(fit_stats)
  
  best_model_name <- fit_stats$Model[fit_stats$Best_AIC]
  best_model <- model_fits[[best_model_name]]
  
  cat("\n=== BEST MODEL SELECTED ===\n")
  cat("Best model by AIC:", best_model_name, "\n")
  cat("AIC:", round(fit_stats$AIC[fit_stats$Model == best_model_name], 2), "\n")
  cat("BIC:", round(fit_stats$BIC[fit_stats$Model == best_model_name], 2), "\n")
  
  # Step 4: Extract detailed results
  detailed_results <- extract_detailed_results(best_model, test_data, q_matrix)
  
  # Step 5: Create diagnostic plots
  plots <- create_diagnostic_plots(model_fits, best_model, detailed_results)
  
  # Step 6: Generate PowerPoint report if requested
  if(generate_report) {
    generate_powerpoint_report(model_fits, best_model, detailed_results, fit_stats, plots)
  }
  
  # Step 7: Save all results
  save(model_fits, best_model, detailed_results, fit_stats, plots,
       file = "Grade8_Complete_Analysis.RData")
  
  cat("\n=== ANALYSIS COMPLETED SUCCESSFULLY ===\n")
  cat("Results saved in:\n")
  cat("- Grade8_Complete_Analysis.RData (R data file)\n")
  cat("- Grade8_Analysis_Report.pptx (PowerPoint report)\n")
  cat("- Various PNG files (plots and diagnostics)\n")
  
  return(list(
    model_fits = model_fits,
    best_model = best_model,
    detailed_results = detailed_results,
    fit_stats = fit_stats,
    plots = plots
  ))
}

# =============================================================================
# EXECUTE ANALYSIS
# =============================================================================

# Run the complete analysis
results <- run_grade8_analysis()

# Print final summary
cat("\n" + rep("=", 50) + "\n")
cat("FINAL ANALYSIS SUMMARY\n")
cat(rep("=", 50) + "\n")
cat("Best model:", results$fit_stats$Model[results$fit_stats$Best_AIC], "\n")
cat("Number of students:", results$detailed_results$n_students, "\n")
cat("Number of items:", results$detailed_results$n_items, "\n")
cat("Number of attributes:", results$detailed_results$n_attributes, "\n")
cat("Average observed score:", round(mean(results$detailed_results$observed_scores), 2), "\n")
cat("Model reliability:", round(results$detailed_results$reliability$rho, 3), "\n")
cat("Analysis completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 50) + "\n")