#=================================================
# Load Air Quality Dataset
#=================================================

data(airquality)

aq <- airquality   # alias for convenience

#=================================================
# 1(a) Extract first five rows
#=================================================

head(aq, 5)

#=================================================
# 1(b) Compute mean temperature (without built-in mean)
#=================================================

temp_sum <- 0
count <- 0

for (i in aq$Temp) {
  if (!is.na(i)) {
    temp_sum <- temp_sum + i
    count <- count + 1
  }
}

mean_temp <- temp_sum / count
mean_temp

#=================================================
# 1(c) Coldest day in the month of May
#=================================================

may_data <- aq[aq$Month == 5, ]

coldest_day <- may_data[which.min(may_data$Temp), c("Day", "Temp")]
coldest_day

#=================================================
# 2(a) Number of days wind speed > 17 mph
#=================================================

sum(aq$Wind > 17, na.rm = TRUE)

#=================================================
# 2(b) Mean, Median, Mode of Wind Speed
#=================================================

# Mean
mean(aq$Wind, na.rm = TRUE)

# Median
median(aq$Wind, na.rm = TRUE)

# Mode function
mode_value <- function(x) {
  x <- x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_value(aq$Wind)

#=================================================
# 2(c) Comment on skewness of Wind Speed
#=================================================

hist(aq$Wind,
     main = "Histogram of Wind Speed",
     xlab = "Wind Speed",
     col = "lightblue")

# (Right-skewed / Left-skewed based on histogram shape)

#=================================================
# 3(a) Summary statistics of airquality dataset
#=================================================

summary(aq)

#=================================================
# 3(b) Plot distribution of air quality (Ozone)
#=================================================

hist(aq$Ozone,
     main = "Distribution of Ozone Levels",
     xlab = "Ozone (ppb)",
     col = "lightgreen")

#=================================================
# 3(c) Average of Ozone, Solar.R, Wind, Temp per Month
# (Using base R instead of cast for exam safety)
#=================================================

aggregate(cbind(Ozone, Solar.R, Wind, Temp) ~ Month,
          data = aq,
          FUN = mean,
          na.rm = TRUE)

#=================================================
# 4(a) Handle Missing Values
# Rule:
# If NA < 10% → drop rows
# Else → replace with mean
#=================================================

for (col in names(aq)) {
  na_count <- sum(is.na(aq[[col]]))
  total <- length(aq[[col]])
  
  if (na_count / total < 0.10) {
    aq <- aq[!is.na(aq[[col]]), ]
  } else {
    aq[[col]][is.na(aq[[col]])] <- mean(aq[[col]], na.rm = TRUE)
  }
}

summary(aq)

#=================================================
# 4(b) Linear Regression: Ozone vs Solar.R
#=================================================

model <- lm(Ozone ~ Solar.R, data = aq)
summary(model)

#=================================================
# 4(c) Scatter plot with regression line
#=================================================

plot(aq$Solar.R, aq$Ozone,
     pch = 19,
     col = "blue",
     xlab = "Solar Radiation",
     ylab = "Ozone",
     main = "Ozone vs Solar Radiation")

abline(model, col = "red", lwd = 2)
