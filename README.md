# robust-statistical-analysis
A robust R script template for statistical analysis, visualization, and data validation.

This repository contains a generalized and robust R script designed for statistical analysis of datasets, including descriptive statistics, normality testing, variance comparisons, Welch's ANOVA, post-hoc analysis, outlier detection, and interactive visualization.

## Features:

- **Data Validation:** Checks for missing data and identifies outliers using the Interquartile Range (IQR) method.
- **Descriptive Statistics:** Calculates means, medians, standard deviations, and range for each variable.
- **Normality Testing:** Uses Shapiro-Wilk test to evaluate data normality.
- **Variance Comparison:** Employs Levene's test to assess variance homogeneity.
- **Welch's ANOVA & Post-Hoc Tests:** Conducts robust ANOVA testing and Games-Howell post-hoc comparisons.
- **Visualization:** Creates interactive boxplots using `ggplot2` and `plotly`.

## Getting Started

### **Prerequisites**

Make sure you have R and RStudio installed.  
Install the required packages:
```R
install.packages(c("readxl", "dplyr", "car", "openxlsx", "rstatix", "ggplot2", "ggsignif", "plotly"))
