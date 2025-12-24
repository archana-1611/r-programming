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
# 2(a) Mean, Median, Mode of given values
#=================================================

x <- c(90, 50, 70, 80, 70, 60, 20, 30, 80, 90, 20)

# Mean
mean(x)

# Median
median(x)

# Mode
mode_value <- function(v) {
  ux <- unique(v)
  ux[which.max(tabulate(match(v, ux)))]
}
mode_value(x)

#=================================================
# 2(b) 2nd highest and 3rd lowest values
#=================================================

sorted_x <- sort(x)

# 2nd highest
sorted_x[length(sorted_x) - 1]

# 3rd lowest
sorted_x[3]

#=================================================
# 3. Load mtcars dataset
#=================================================

data(mtcars)

# Convert vs to factor for better interpretation
mtcars$vs <- factor(mtcars$vs,
                    labels = c("V-shaped", "Straight"))

#=================================================
# 3(a) Weight vs Displacement scatter plot by Engine Shape
#=================================================

plot(mtcars$disp, mtcars$wt,
     col = mtcars$vs,
     pch = 19,
     xlab = "Displacement",
     ylab = "Weight",
     main = "Weight vs Displacement by Engine Shape")

legend("topright",
       legend = levels(mtcars$vs),
       col = 1:2,
       pch = 19)

#=================================================
# 3(b) Horsepower vs Mileage scatter plot by Engine Shape
#=================================================

plot(mtcars$hp, mtcars$mpg,
     col = mtcars$vs,
     pch = 19,
     xlab = "Horsepower",
     ylab = "Mileage (mpg)",
     main = "Mileage vs Horsepower by Engine Shape")

legend("topright",
       legend = levels(mtcars$vs),
       col = 1:2,
       pch = 19)

#=================================================
# 3(c) Separate columns according to cylinder size
#=================================================

split(mtcars, mtcars$cyl)

#=================================================
# 4. Linear Regression: Height vs Weight
#=================================================

# Given height and weight values
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

#-------------------------------------------------
# Create regression model
#-------------------------------------------------

model <- lm(weight ~ height)
summary(model)

#-------------------------------------------------
# Predict weight for height = 170
#-------------------------------------------------

new_data <- data.frame(height = 170)
predict(model, new_data)

#-------------------------------------------------
# Visualize regression graphically
#-------------------------------------------------

plot(height, weight,
     pch = 19,
     col = "blue",
     xlab = "Height",
     ylab = "Weight",
     main = "Height vs Weight Regression")

abline(model, col = "red", lwd = 2)
