# Load necessary libraries (commented out installation lines for convenience)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("car")
# install.packages("openxlsx")
# install.packages("rstatix")
# install.packages("ggplot2")
# install.packages("ggsignif")
# install.packages("plotly")

library(readxl)
library(dplyr)
library(car)
library(openxlsx)
library(rstatix)
library(ggplot2)
library(ggsignif)
library(plotly)

# Set file paths (generalized, assumes data in 'data/' and outputs in 'results/')
data_file <- "./data/your_data_file.xlsx"
output_descriptive <- "./results/descriptive_statistics.csv"
output_normality <- "./results/normality_results.csv"
output_levene <- "./results/levene_test_results.xlsx"
output_anova <- "./results/anova_results.xlsx"

# Error handling for reading data
tryCatch({
  data <- read_excel(data_file, sheet = "Sheet1")
}, error = function(e) {
  stop("Error reading data file: ", e$message)
})

# Basic structure
cat("Number of rows (entries):", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")
print(head(data))

# Data validation: check for missing values and outliers
if(anyNA(data)) {
  warning("Data contains missing values. Consider addressing these before analysis.")
}

# Simple outlier detection using IQR
outlier_detection <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  sum(x < lower_bound | x > upper_bound, na.rm = TRUE)
}
outliers <- sapply(data, outlier_detection)
print("Outlier counts per column:")
print(outliers)

# Descriptive statistics for each column
descriptive_stats <- data %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  ))

print("Descriptive Statistics:")
print(descriptive_stats)
write.csv(descriptive_stats, output_descriptive, row.names = FALSE)

# Normality test (Shapiro-Wilk) for each column
normality_results <- data %>%
  summarise_all(~shapiro.test(.)$p.value)

print("Shapiro-Wilk Normality Test Results:")
print(normality_results)
write.csv(normality_results, output_normality, row.names = FALSE)

# Convert data from wide to long format (generalized, modify columns as needed)
data_long <- pivot_longer(data, cols = everything(), names_to = "Group", values_to = "Value")

# Levene's test to compare variances
levene_test_result <- leveneTest(Value ~ Group, data = data_long)
print(levene_test_result)

levene_df <- data.frame(
  Df_Group = levene_test_result$Df[1],
  Df_Residual = levene_test_result$Df[2],
  F_value = levene_test_result$"F value"[1],
  P_value = levene_test_result$"Pr(>F)"[1]
)

write.xlsx(levene_df, output_levene)

# Welch ANOVA test (unequal variances)
welch_anova <- oneway.test(Value ~ Group, data = data_long, var.equal = FALSE)
welch_anova_df <- data.frame(
  F_value = welch_anova$statistic,
  Num_DF = welch_anova$parameter[1],
  Denom_DF = welch_anova$parameter[2],
  P_value = welch_anova$p.value
)

# Games-Howell post-hoc test
games_howell <- data_long %>%
  pairwise_t_test(Value ~ Group, p.adjust.method = "bonferroni")

# Save ANOVA and post-hoc results
wb <- createWorkbook()
addWorksheet(wb, "Welch_ANOVA")
writeData(wb, "Welch_ANOVA", welch_anova_df)
addWorksheet(wb, "Games_Howell")
writeData(wb, "Games_Howell", games_howell)
saveWorkbook(wb, output_anova, overwrite = TRUE)

# Visualization: Boxplot with jittered points and significance (customize as needed)
mean_values <- data_long %>%
  group_by(Group) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))

# Customize significant pairs based on your statistical tests
significant_pairs <- list(c("Group1", "Group2"))  # Modify with actual group names
p_values <- c(0.05)  # Replace with actual p-values

max_y <- max(data_long$Value, na.rm = TRUE)
y_bracket_position <- max_y + (0.10 * max_y)

# Interactive plot using plotly
plot <- ggplot(data_long, aes(x = Group, y = Value, color = Group)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, notch = TRUE) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black") +
  geom_text(data = mean_values, aes(x = Group, y = mean_value, label = round(mean_value, 2)),
            color = "black", size = 5, vjust = -0.5) +
  geom_signif(comparisons = significant_pairs,
              annotations = format(p_values, digits = 2),
              y_position = y_bracket_position,
              textsize = 5) +
  ggtitle("Comparison Across Groups") +
  labs(x = "Group", y = "Measured Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none")

# Convert ggplot to interactive plotly plot
interactive_plot <- ggplotly(plot)
print(interactive_plot)
