rm(list = ls())
# 1.Load packages and import data
setwd("~/Desktop/study/year2/stats/assessment")
#1. Load packages
library(psych)
library(tidyverse)
library(corrplot)
library(car)

# 2.Variables and Data Preparation
data <- read_csv("Housing_price_school_quality.csv")
data$DistBostonSq <- data$DistBoston^2
my_vars <- data %>% select(lnHomePrice, EDMathGrade4,lnHHIncome,DistBoston,DistBostonSq,AvgClassSize,ResTaxRate)
# 2.1.1Check missing values
colSums(is.na(my_vars))
nrow(my_vars)
data_clean <- na.omit(my_vars)
# 2.2.2test for skewness
skewness <- function(x) {
  x <- x[!is.na(x)]
  mean((x - mean(x))^3) / sd(x)^3
}
skewness_table <- data.frame(
  Variable = c("HomePrice", "lnHomePrice"),
  Skewness = c(skewness(data$HomePrice), skewness(data$lnHomePrice)) )
skewness_table
  # 2.2 summary
describe(data_clean)

# 3.Visualisations
# Create a categorical variable for boxplot (high/low school quality)
median_math <- median(data_clean$EDMathGrade4)
data_clean$SchoolQualityGroup <- ifelse(data_clean$EDMathGrade4 > median_math, "High", "Low")
data_clean$SchoolQualityGroup <- factor(data_clean$SchoolQualityGroup, levels = c("Low", "High"))
summary(data_clean[, c("lnHomePrice", "EDMathGrade4")])
# Figure 3.1.1: Bivariate Relationship (Scatterplot)
ggplot(data_clean, aes(x = EDMathGrade4, y = lnHomePrice)) +
  geom_point(alpha = 0.6, size = 2, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between 4th Grade Math Scores and Log Housing Prices",
    x = "4th Grade Math Proficiency (%)",
    y = "Log of Median Home Price (in $1000s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 10)
  )
# Figure 3.1.2: Boxplot - Comparing High vs Low School Quality
ggplot(data_clean, aes(x = SchoolQualityGroup, y = lnHomePrice, fill = SchoolQualityGroup)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.size = 2) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("Low" = "salmon", "High" = "steelblue")) +
  labs(
    title = "Housing Prices by School Quality Level",
    x = "School Quality (Above/Below Median Math Proficiency)",
    y = "Log of Median Home Price (in $1000s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 10),
    legend.position = "none"
  )
# Figure 3.2 correlation matrix
install.packages("corrplot")
cor_vars <- data_clean[, c("lnHomePrice", "EDMathGrade4", "lnHHIncome", 
                     "DistBoston", "AvgClassSize", "ResTaxRate")]
cor_matrix <- cor(cor_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", number.cex = 0.8,
         tl.cex = 0.8, tl.col = "black",
         mar = c(0, 0, 2, 0))

# 4.Regression Analysis
# 4.1 Check missing values
colSums(is.na(my_vars))
nrow(my_vars)
data_clean <- na.omit(my_vars)
NROW(data_clean)
# multicollinearity check
library(car)
vif(model3)

#4.2 Regression 1 (only school quality)
model1 <- lm(lnHomePrice ~ EDMathGrade4,
             data = data_clean)
summary(model1)
# Regression 2 (add more control term but without squared distance term)
model2 <- lm(lnHomePrice ~ EDMathGrade4 + lnHHIncome +
               DistBoston + AvgClassSize + ResTaxRate,
             data = data_clean)
summary(model2)
# Regression 3 (Full model with quadratic distance term)
model3 <- lm(lnHomePrice ~ EDMathGrade4 + lnHHIncome + 
              DistBoston + DistBostonSq + AvgClassSize + ResTaxRate, 
            data = data_clean)
summary(model3)
# table
install.packages("stargazer")
library(stargazer)
stargazer(model1, model2, model3,
          type = "html",
          title = "Comparison of Regression Models Explaining Housing Prices",
          dep.var.labels = "ln(HomePrice)",
          column.labels = c("Model1", "Model2", "Model3"),
          digits = 4,
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "regression_table.html") 
# Q-Q plot
qqnorm(resid(model3),
       pch = 19,
       col = "steelblue",
       main = "Q-Q Plot of Residuals (Model 3)")

qqline(resid(model3),
       col = "darkred",
       lwd = 2)