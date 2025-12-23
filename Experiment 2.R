#1 a
set.seed(123)

# Random sample from LETTERS
x <- sample(LETTERS, 20, replace = TRUE)

# Convert to factor
f <- factor(x)

# Extract first five levels
levels(f)[1:5]

#1 b
range_fun <- function(v) {
  max(v) - min(v)
}

# Sample input
C <- c(9,8,7,6,5,4,3,2,1)

# Output
range_fun(C)

#2a
set.seed(100)

mat <- matrix(sample(1:10, 60, replace = TRUE),
              nrow = 6, ncol = 10)

mat

#2b
rowSums(mat > 4)
#2c
which(rowSums(mat == 7) == 2)


#3
data(Titanic)

#3a
barplot(
  margin.table(Titanic, c(2,4)),
  beside = TRUE,
  col = c("red","green"),
  legend = TRUE,
  xlab = "Passenger Class",
  ylab = "Count",
  main = "Survival Based on Passenger Class"
)

#3. b) Modify plot based on Gender
# Extract only survived passengers
survivors <- Titanic[, , , "Yes"]

# Class vs Gender for survivors
class_gender_survival <- margin.table(survivors, c(2,3))

# Bar plot
barplot(
  class_gender_survival,
  beside = TRUE,
  col = c("pink", "lightblue"),
  legend = TRUE,
  args.legend = list(title = "Gender"),
  xlab = "Passenger Class",
  ylab = "Number of Survivors",
  main = "Survivors Based on Class and Gender"
)


#3. c) Histogram for Age distribution
ages <- as.numeric(Titanic[,,"Adult","Yes"])

hist(ages,
     main = "Age Distribution",
     xlab = "Age",
     col = "lightgreen")

#4. a) Create data frame
Month <- 1:12
Spends <- c(1000,4000,5000,4500,3000,4000,9000,11000,15000,12000,7000,3000)
Sales <- c(9914,40487,54324,50044,34719,42551,94871,118914,158484,131348,78504,36284)

df <- data.frame(Month, Spends, Sales)
df

#4. b) Regression model (Sales based on Spends)
model <- lm(Sales ~ Spends, data = df)
summary(model)

#4. c) Predict Sales if Spend = 13500
predict(model, data.frame(Spends = 13500))



