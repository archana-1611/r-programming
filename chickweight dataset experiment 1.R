# Read your ChickWeight CSV file
chick_data <- read.csv("C:/Users/archana/Downloads/ChickWeight.csv", stringsAsFactors = TRUE)

# Verify structure
str(chick_data)
head(chick_data)


graphics.off()
par(mar = c(4, 4, 2, 1))


plot(chick_data$Time, chick_data$weight,
     main = "Scatter Plot: Weight vs Time",
     xlab = "Time (days)",
     ylab = "Weight (grams)",
     pch = 19)


hist(chick_data$weight,
     main = "Histogram of Chick Weight",
     xlab = "Weight",
     breaks = 15)


boxplot(weight ~ Diet,
        data = chick_data,
        main = "Boxplot of Weight by Diet",
        xlab = "Diet",
        ylab = "Weight")

chick_data$Diet <- as.factor(chick_data$Diet)

plot(weight ~ Time,
     data = chick_data,
     col = Diet,
     pch = 19,
     main = "Weight vs Time Grouped by Diet")

legend("topleft",
       legend = levels(chick_data$Diet),
       col = 1:length(levels(chick_data$Diet)),
       pch = 19)



model <- lm(weight ~ Time + Diet, data = chick_data)
summary(model)



