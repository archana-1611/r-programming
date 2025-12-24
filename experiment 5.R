# Load mtcars dataset
data(mtcars)

# Convert am and vs to factor for better interpretation
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, labels = c("V-shaped", "Straight"))

#-------------------------------------------------
# 1(a) Dimension and statistical summary
#-------------------------------------------------

# Dimension of dataset
dim(mtcars)

# Structure of dataset
str(mtcars)

# Statistical summary
summary(mtcars)

#-------------------------------------------------
# 1(b) Largest and smallest value of hp
#-------------------------------------------------

# Maximum horsepower
max(mtcars$hp)

# Minimum horsepower
min(mtcars$hp)

#-------------------------------------------------
# 1(c) Mean mileage (mpg) with respect to transmission (am)
#-------------------------------------------------

aggregate(mpg ~ am, data = mtcars, mean)

#-------------------------------------------------
# 2(a) Mean of horsepower (hp) with respect to cylinders (cyl)
#-------------------------------------------------

aggregate(hp ~ cyl, data = mtcars, mean)

#-------------------------------------------------
# 2(b) Mean, Median, Mode of hp and wt
#-------------------------------------------------

# Mean
mean(mtcars$hp)
mean(mtcars$wt)

# Median
median(mtcars$hp)
median(mtcars$wt)

# Mode function
mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_value(mtcars$hp)
mode_value(mtcars$wt)

#-------------------------------------------------
# 2(c) Skewness comment (visual)
#-------------------------------------------------

hist(mtcars$hp, main = "Histogram of Horsepower", xlab = "hp")
hist(mtcars$wt, main = "Histogram of Weight", xlab = "wt")

#-------------------------------------------------
# 3(a) Scatter plot mpg vs hp grouped by transmission (am)
#-------------------------------------------------

plot(mtcars$hp, mtcars$mpg,
     col = mtcars$am,
     pch = 19,
     xlab = "Horsepower",
     ylab = "Mileage (mpg)",
     main = "MPG vs HP grouped by Transmission")

legend("topright",
       legend = levels(mtcars$am),
       col = 1:2,
       pch = 19)

#-------------------------------------------------
# 3(b) Box plot for mpg with respect to transmission (am)
#-------------------------------------------------

boxplot(mpg ~ am,
        data = mtcars,
        col = c("lightblue", "lightgreen"),
        main = "Mileage by Transmission Type",
        xlab = "Transmission",
        ylab = "MPG")

#-------------------------------------------------
# 3(c) Histogram showing distribution of hp
#-------------------------------------------------

hist(mtcars$hp,
     col = "orange",
     breaks = 10,
     main = "Distribution of Horsepower",
     xlab = "Horsepower")

#-------------------------------------------------
# 4(a) Multiple Linear Regression Model
# mpg as response, disp, hp, wt as predictors
#-------------------------------------------------

model <- lm(mpg ~ disp + hp + wt, data = mtcars)
summary(model)

#-------------------------------------------------
# 4(b) Plot regression diagnostics
#-------------------------------------------------

par(mfrow = c(2,2))
plot(model)

#-------------------------------------------------
# 4(c) Predict mileage for:
# disp = 221, hp = 102, wt = 2.91
#-------------------------------------------------

new_car <- data.frame(disp = 221, hp = 102, wt = 2.91)
predict(model, new_car)
