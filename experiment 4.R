# Load iris dataset
data(iris)

# 1(a) Extract last 6 observations of dataset
tail(iris)

# 1(b) Dimension, Structure, Summary statistics, Standard Deviation of all features

# Dimension
dim(iris)

# Structure
str(iris)

# Summary statistics
summary(iris)

# Standard deviation of numeric features
sapply(iris[,1:4], sd)

# 1(c) Mean and Standard deviation of features grouped by species

# Mean by species
aggregate(iris[,1:4], by = list(Species = iris$Species), mean)

# Standard deviation by species
aggregate(iris[,1:4], by = list(Species = iris$Species), sd)

# 2(a) Find quantile value of Sepal.Width
quantile(iris$Sepal.Width)

# 2(b) Create new data frame with new column Sepal.Length.Cate
iris_new <- transform(iris,
                      Sepal.Length.Cate = ifelse(Sepal.Length > mean(Sepal.Length),
                                                 "Long", "Short"))

# View new dataframe
head(iris_new)

# 2(c) Average Sepal.Length by Species and Sepal.Length.Cate
aggregate(Sepal.Length ~ Species + Sepal.Length.Cate,
          data = iris_new,
          mean)

# 3(a) Scatter plot between Sepal.Width and Sepal.Length grouped by Species
plot(iris$Sepal.Width, iris$Sepal.Length,
     col = as.numeric(iris$Species),
     pch = 19,
     xlab = "Sepal Width",
     ylab = "Sepal Length",
     main = "Sepal Width vs Sepal Length")

# 3(b) Scatter plot between Petal.Width and Petal.Length grouped by Species
plot(iris$Petal.Width, iris$Petal.Length,
     col = as.numeric(iris$Species),
     pch = 19,
     xlab = "Petal Width",
     ylab = "Petal Length",
     main = "Petal Width vs Petal Length")

# 3(c) Box plot for Sepal.Length grouped by Species
boxplot(Sepal.Length ~ Species,
        data = iris,
        col = "lightgreen",
        main = "Sepal Length by Species")

# 4(a) Randomly sample 50% data for training and 50% for testing
set.seed(123)
index <- sample(1:nrow(iris), 0.5 * nrow(iris))
train <- iris[index, ]
test <- iris[-index, ]

# 4(b) Logistic regression using species as target and petal & sepal features
model <- glm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
             data = train,
             family = binomial)

summary(model)

# 4(c) Predict probability using test data
pred <- predict(model, test, type = "response")

# 4(d) Create confusion matrix
table(Predicted = ifelse(pred > 0.5,
                         levels(iris$Species)[1],
                         levels(iris$Species)[2]),
      Actual = test$Species)
