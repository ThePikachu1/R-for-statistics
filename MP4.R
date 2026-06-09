library(pacman)

pacman::p_load(rio)

heartrate <- import("/Users/fathimafaridamohammad/Downloads/bodytemp-heartrate.csv")

head(heartrate)

hist(heartrate$heart_rate [heartrate$gender == "1"],
     breaks = 15, main = "Histogram for Heart Rate for Male",
     xlab = "", col="lightblue")

hist(heartrate$heart_rate [heartrate$gender == "2"],
     breaks = 15, main = "Histogram for Heart Rate for Female",
     xlab = "", col="pink")

pacman::p_load(dplyr)

boxplot(`heart_rate` ~ gender , data = heartrate, 
        col = c("lightblue" , "lightgreen"),
        main = "Heart Rate comparision by Gender", ylab = "Heart Rate")
## Summary statistics for Male
summary(filter(heartrate, gender == 1)$heart_rate)
sd(filter(heartrate, gender == 1)$heart_rate)
range(filter(heartrate, gender == 1)$heart_rate)
## Summary statistics for Female
summary(filter(heartrate, gender == 2)$heart_rate)
sd(filter(heartrate, gender == 2)$heart_rate)
range(filter(heartrate, gender == 2)$heart_rate)
## QQ Line for Male Heart Rate
qqnorm(heartrate$heart_rate[heartrate$gender == 1], main = "Male")
qqline(heartrate$heart_rate[heartrate$gender == 1] , col = "red")
## QQ line for Female Heart Rate
qqnorm(heartrate$heart_rate[heartrate$gender == 2], main = "Female")
qqline(heartrate$heart_rate[heartrate$gender == 2], col = "red")

male_data <- filter(heartrate, gender == 1)

female_data <- filter(heartrate, gender == 2)

male_hr <- filter(heartrate, gender == 1)$heart_rate

female_hr <- filter(heartrate, gender == 2)$heart_rate
## Two Sample T Test or Welsh Test
t_result <- t.test(male_hr , female_hr,
                   alternative = "two.sided",
                   conf.level = 0.95,
                   var.equal = FALSE)

print(t_result)


# Scatter plot
plot(heartrate$body_temperature,
     heartrate$heart_rate,
     main = "Body Temperature vs Heart Rate",
     xlab = "Body Temperature",
     ylab = "Heart Rate",
     pch = 19)

# Add regression line
abline(lm(heart_rate ~ body_temperature, data = heartrate),
       col = "red",
       lwd = 2)

# Correlation coefficient
cor(heartrate$body_temperature, heartrate$heart_rate)

# Correlation test
cor.test(heartrate$body_temperature, heartrate$heart_rate)