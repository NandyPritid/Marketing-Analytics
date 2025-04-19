# Rewrite and tailor each R code block to match user's four exact use cases:
# 1. NHS Digital – STP using Cluster Analysis
# 2. Relay – AER using Logistic Regression
# 3. Portland Trail Blazers – BOR using Conjoint Analysis
# 4. Belvedere Vodka – RA using Linear Regression


"NHS Digital – STP (Cluster Analysis)": """
# NHS DIGITAL – CLUSTER ANALYSIS FOR PATIENT SEGMENTATION
library(ggplot2)

set.seed(100)
n <- 150
nhs_data <- data.frame(
  Satisfaction = rnorm(n, mean = 6, sd = 1.2),
  DigitalEngagement = rnorm(n, mean = 7, sd = 1.1)
)

# K-Means Clustering
set.seed(100)
nhs_kmeans <- kmeans(nhs_data, centers = 3)
nhs_data$Segment <- as.factor(nhs_kmeans$cluster)

# Cluster Plot
ggplot(nhs_data, aes(x = Satisfaction, y = DigitalEngagement, color = Segment)) +
  geom_point(size = 3) +
  labs(title = "NHS Digital Segmentation by Satisfaction and Engagement",
       x = "Satisfaction Score",
       y = "Digital Engagement Score",
       color = "Segment") +
  theme_minimal()
""",

"Relay – AER (Logistic Regression)": """
# RELAY FOODS – LOGISTIC REGRESSION FOR RETENTION PREDICTION
set.seed(101)
n <- 300
relay_data <- data.frame(
  Retained = rbinom(n, 1, prob = 0.6),
  TotalOrders = rpois(n, lambda = 12),
  AvgBasket = round(runif(n, 25, 80), 1),
  EmailCount = rpois(n, lambda = 10)
)

# Logistic regression model
relay_model <- glm(Retained ~ TotalOrders + AvgBasket + EmailCount, data = relay_data, family = binomial)
summary(relay_model)
""",

"Portland Trail Blazers – BOR (Conjoint Analysis)": """
# PORTLAND TRAIL BLAZERS – CONJOINT ANALYSIS FOR TICKET BUNDLES
library(conjoint)

# Define profiles for conjoint design
trail_profiles <- expand.grid(
  Games = factor(c("3", "6", "10")),
  Seats = factor(c("300_Level", "200_Level")),
  Promo = factor(c("None", "Hotdog_Soda", "Playoff_Priority"))
)

# Simulate preference scores
trail_profiles$true_score <- with(trail_profiles,
  60 +
    ifelse(Games == "6", 5, 0) +
    ifelse(Games == "10", 10, 0) +
    ifelse(Seats == "200_Level", 8, 0) +
    ifelse(Promo == "Hotdog_Soda", 4, 0) +
    ifelse(Promo == "Playoff_Priority", 6, 0)
)

set.seed(202)
n_resp <- 20
sim_data <- trail_profiles[rep(1:nrow(trail_profiles), each = n_resp), ]
sim_data$rating <- sim_data$true_score + rnorm(nrow(sim_data), mean = 0, sd = 5)

# Ratings matrix and attributes
rating_matrix <- matrix(sim_data$rating, nrow = n_resp, byrow = TRUE)
profile_data <- trail_profiles[, c("Games", "Seats", "Promo")]
attr_levels <- data.frame(attr.level = c("3", "6", "10", "300_Level", "200_Level", "None", "Hotdog_Soda", "Playoff_Priority"))

# Estimate part-worth utilities
part_worths <- caPartUtilities(y = rating_matrix, x = profile_data, z = attr_levels)
print("Trail Blazers: Estimated Part-Worth Utilities")
print(part_worths)

# Calculate attribute importance
importance <- caImportance(y = rating_matrix, x = profile_data)
print("Attribute Importance (%)")
print(importance)
""",

"Belvedere Vodka – RA (Linear Regression)": """
# BELVEDERE VODKA – LINEAR REGRESSION FOR PRICE ELASTICITY
vodka_data <- data.frame(
  Year = 2001:2007,
  Sales = c(273, 306, 339, 369, 365, 381, 410),
  Price = c(240.48, 247.55, 241.33, 240.87, 207.45, 211.45, 215.44)
)

vodka_data$logSales <- log(vodka_data$Sales)
vodka_data$logPrice <- log(vodka_data$Price)

# Regression model
vodka_model <- lm(logSales ~ logPrice, data = vodka_data)
summary(vodka_model)
"""
