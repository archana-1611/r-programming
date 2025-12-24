#=================================================
# a) Display lower diagonal and upper diagonal matrices
#=================================================

mat <- matrix(1:16, nrow = 4)

# Lower diagonal matrix
lower_diag <- mat
lower_diag[upper.tri(lower_diag)] <- 0
lower_diag

# Upper diagonal matrix
upper_diag <- mat
upper_diag[lower.tri(upper_diag)] <- 0
upper_diag

#=================================================
# b) Count number of 0's and check sparse matrix
#=================================================

mat2 <- matrix(c(0,0,3,0,0,0,5,0,0), nrow = 3)

# Count number of zeros
zero_count <- sum(mat2 == 0)
zero_count

# Check sparse matrix (more than 50% zeros)
total_elements <- length(mat2)

if (zero_count > total_elements / 2) {
  print("Matrix is Sparse")
} else {
  print("Matrix is Not Sparse")
}

#=================================================
# c) Remove outliers (negative numbers) from matrix
#=================================================

mat3 <- matrix(c(5,-2,7,-9,3,6,-1,4), nrow = 4)
mat3

# Remove negative values
mat3[mat3 < 0] <- NA
mat3

#=================================================
# a) Mean, Median, Mode of given values
#=================================================

x <- c(90, 50, 70, 80, 70, 60, 20, 30, 80, 90, 20, 75, 70, 10, 60, 70, 85, 45, 15)

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
# b) Find 2nd highest and 4th lowest value
#=================================================

sorted_x <- sort(x)

# 2nd highest
sorted_x[length(sorted_x) - 1]

# 4th lowest
sorted_x[4]

#=================================================
# c) Pie chart using input vector
#=================================================

values <- c(21, 62, 10, 53)
labels <- c("London", "New York", "Singapore", "Mumbai")

pie(values,
    labels = labels,
    main = "City Pie Chart")

legend("topright", labels, fill = rainbow(4))

#=================================================
# d) Bar chart using given vectors
#=================================================

y <- c(7,12,28,34)
names(y) <- c("Q1", "Q2", "Q3", "Q4")

barplot(y,
        main = "Revenue Chart",
        xlab = "Quarter",
        ylab = "Revenue",
        col = "lightblue")

#=================================================
# e) Histogram plot with suitable bin width
#=================================================

v <- c(19, 25, 11, 5, 16, 21, 32, 14, 19, 27, 39, 120, 40, 70, 90)

hist(v,
     breaks = 6,
     main = "Histogram of Values",
     xlab = "Data Values",
     col = "lightgreen")

#=================================================
# Regression model: Sales based on Expenditure
#=================================================

# Given data
expenditure <- c(1000, 4000, 5000, 4500, 3000, 4000, 9000, 11000, 15000, 12000, 7000, 3000)
sales <- c(9914, 40487, 54321, 50004, 34719, 42351, 94731, 119914, 135844, 131548, 78504, 34534)

# Regression model
model <- lm(sales ~ expenditure)
summary(model)

#=================================================
# Predict sales when expenditure is Rs. 13,500
#=================================================

new_data <- data.frame(expenditure = 13500)
predict(model, new_data)
