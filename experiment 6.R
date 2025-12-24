#=================================================
# 1(a) Fibonacci sequence using recursion
#=================================================

fib <- function(n) {
  if (n <= 1)
    return(n)
  else
    return(fib(n - 1) + fib(n - 2))
}

# Print first 10 Fibonacci numbers
for (i in 0:9) {
  cat(fib(i), " ")
}

cat("\n")

#=================================================
# 1(b) Sum of natural numbers up to 10 using loop
#=================================================

sum <- 0
for (i in 1:10) {
  sum <- sum + i
}
sum

#=================================================
# 2. Load ChickWeight dataset
#=================================================

data(ChickWeight)

# Convert to standard data frame
cw <- as.data.frame(ChickWeight)

#=================================================
# 2(a) Dataset summary, features, types & records
#=================================================

# Structure
str(cw)

# Number of features
ncol(cw)

# Number of records
nrow(cw)

# Data type of each feature
sapply(cw, class)

# Statistical summary
summary(cw)

#=================================================
# 2(b) Number of records for each feature
#=================================================

sapply(cw, length)

#=================================================
# 2(c) Extract last 6 records grouped by Diet
#       sorted by Weight (ascending)
#=================================================

# Sort by weight
sorted_data <- cw[order(cw$weight), ]

# Extract last 6 records for each Diet
result <- do.call(rbind,
                  lapply(split(sorted_data, sorted_data$Diet),
                         function(x) tail(x, 6)))

result

#=================================================
# 3(a) Mean weight grouped by Diet
#=================================================

aggregate(weight ~ Diet, data = cw, mean)

#=================================================
# 3(b) Histogram for Weight (Diet = 2)
#=================================================

hist(cw$weight[cw$Diet == 2],
     main = "Histogram of Weight (Diet 2)",
     xlab = "Weight",
     col = "lightblue",
     breaks = 10)

#=================================================
# 4(a) Multiple Regression Model
# Weight as response, Time and Diet as predictors
#=================================================

model <- lm(weight ~ Time + Diet, data = cw)
summary(model)

#=================================================
# 4(b) Predict weight for Time = 10 and Diet = 1
#=================================================

new_data <- data.frame(
  Time = 10,
  Diet = factor(1, levels = levels(cw$Diet))
)

predict(model, new_data)

#=================================================
# 4(c) Find model error using RMSE
#=================================================

predicted <- predict(model, cw)
error <- cw$weight - predicted
rmse <- sqrt(mean(error^2))
rmse
