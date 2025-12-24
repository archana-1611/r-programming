#=================================================
# 1. Commute Time Analysis
#=================================================

# Given commute times (in minutes)
time <- c(17, 16, 20, 24, 22, 15, 21, 15, 17, 22)

#-------------------------------------------------
# a) Find longest (max) and minimum commute time
#-------------------------------------------------

max_time <- max(time)
min_time <- min(time)

max_time
min_time

#-------------------------------------------------
# b) Correct the mistake: 24 should be 18
#-------------------------------------------------

time[time == 24] <- 18
time

# New average after correction
avg_time <- mean(time)
avg_time

#-------------------------------------------------
# c) Count how many times commute was 20 minutes or more
#-------------------------------------------------

count_20 <- sum(time >= 20)
count_20

#=================================================
# 2. Iris Dataset Analysis
#=================================================

# Load iris dataset
data(iris)

#-------------------------------------------------
# 2(a) Dimension, Structure, Summary & SD
#-------------------------------------------------

# Dimension
dim(iris)

# Structure
str(iris)

# Summary statistics
summary(iris)

# Standard deviation of all numeric features
sapply(iris[, 1:4], sd)

#-------------------------------------------------
# 2(b) Mean & SD grouped by Species
#-------------------------------------------------

aggregate(iris[, 1:4],
          by = list(Species = iris$Species),
          FUN = mean)

aggregate(iris[, 1:4],
          by = list(Species = iris$Species),
          FUN = sd)

#-------------------------------------------------
# 2(c) Quantile values of Sepal Length & Width
#-------------------------------------------------

quantile(iris$Sepal.Length)
quantile(iris$Sepal.Width)

#=================================================
# 3. Data Visualization
#=================================================

#-------------------------------------------------
# 3(a) Box plot of Sepal Length grouped by Species
#-------------------------------------------------

boxplot(Sepal.Length ~ Species,
        data = iris,
        main = "Sepal Length by Species",
        xlab = "Species",
        ylab = "Sepal Length",
        col = c("lightblue", "lightgreen", "lightpink"))

#-------------------------------------------------
# 3(b) Scatter plot of Petal Length vs Width by Species
#-------------------------------------------------

plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species,
     pch = 19,
     xlab = "Petal Length",
     ylab = "Petal Width",
     main = "Petal Length vs Petal Width")

legend("topleft",
       legend = levels(iris$Species),
       col = 1:3,
       pch = 19)

#-------------------------------------------------
# 3(c) Correlation among four features
#-------------------------------------------------

cor(iris[, 1:4])

#=================================================
# 4. Logistic Regression on Iris Dataset
#=================================================

set.seed(123)

#-------------------------------------------------
# 4(a) Split data: 80% Training, 20% Testing
#-------------------------------------------------

index <- sample(1:nrow(iris), 0.8 * nrow(iris))

train_data <- iris[index, ]
test_data <- iris[-index, ]

# Logistic Regression Model
# Target: Species
# Features: Petal.Length & Petal.Width

model <- glm(Species ~ Petal.Length + Petal.Width,
             data = train_data,
             family = "binomial")

summary(model)

#-------------------------------------------------
# 4(b) Predict probabilities using test data
#-------------------------------------------------

probabilities <- predict(model, test_data, type = "response")
head(probabilities)

#-------------------------------------------------
# 4(c) Create Confusion Matrix
#-------------------------------------------------

predicted_class <- ifelse(probabilities > 0.5,
                          levels(train_data$Species)[2],
                          levels(train_data$Species)[1])

table(Predicted = predicted_class,
      Actual = test_data$Species)
