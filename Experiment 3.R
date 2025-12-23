# Load dataset
data(USArrests)

#1(a) Dataset summary, features, types, records, statistics
# Structure of dataset
str(USArrests)

# Number of features (columns)
ncol(USArrests)

# Number of records (rows)
nrow(USArrests)

# Data types of features
sapply(USArrests, class)

# Statistical summary
summary(USArrests)

#1(b) State with largest number of rape arrests
USArrests[which.max(USArrests$Rape), ]

1(c) States with maximum & minimum murder rates
# Maximum murder rate
USArrests[which.max(USArrests$Murder), ]

# Minimum murder rate
USArrests[which.min(USArrests$Murder), ]

#2(a) Correlation among features
cor(USArrests)

#2(b) States with assault arrests more than country median
median_assault <- median(USArrests$Assault)

USArrests[USArrests$Assault > median_assault, ]

#2(c) States in bottom 25% of murder rate
q1_murder <- quantile(USArrests$Murder, 0.25)

USArrests[USArrests$Murder <= q1_murder, ]

#3(a) Histogram and Density plot of Murder arrests
# Histogram
hist(USArrests$Murder,
     col = "lightblue",
     main = "Histogram of Murder Arrests",
     xlab = "Murder Arrest Rate")

# Density plot
plot(density(USArrests$Murder),
     main = "Density Plot of Murder Arrests",
     xlab = "Murder Arrest Rate")

#3(b) Relationship: Murder vs UrbanPop (colored by Assault)
# Scatter plot
plot(USArrests$UrbanPop, USArrests$Murder,
     col = heat.colors(50)[rank(USArrests$Assault)],
     pch = 19,
     xlab = "Urban Population (%)",
     ylab = "Murder Arrest Rate",
     main = "Murder vs Urban Population (Colored by Assault Rate)")

#3(c) Bar graph of murder rate for each state
barplot(USArrests$Murder,
        names.arg = rownames(USArrests),
        las = 2,
        col = "orange",
        main = "Murder Rate by US State",
        ylab = "Murder Rate")

#4. LINEAR REGRESSION (HEIGHT vs WEIGHT)
#Create input vectors
# Height and Weight vectors
height <- c(151,174,138,186,128,136,179,163,152,131)
weight <- c(63,81,56,91,47,57,76,72,62,48)

df <- data.frame(height, weight)

#Build Linear Regression Model
model <- lm(weight ~ height, data = df)
summary(model)

#Predict weight for height = 170
predict(model, data.frame(height = 170))

#Visualize Regression Graphically
plot(height, weight,
     pch = 19,
     col = "blue",
     xlab = "Height",
     ylab = "Weight",
     main = "Height vs Weight Regression")

abline(model, col = "red", lwd = 2)