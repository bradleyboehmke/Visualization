

par(bg = "#fdfdfd")
plot(x = mtcars$wt, y = mtcars$mpg, xlab = NA, ylab = NA)

par(bg = "#fdfdfd")
barplot(height = BOD$demand, names.arg = BOD$Time)

par(bg = "#fdfdfd")
plot(x = pressure$temperature, y = pressure$pressure, type = "l", xlab = NA, ylab = NA)

par(bg = "#fdfdfd")
plot(factor(mtcars$cyl), mtcars$mpg)

par(mfrow=c(2,2), mar=c(3,5,2,2), las=1, bty="n")
par(bg = "#fdfdfd")
plot(x = mtcars$wt, y = mtcars$mpg, xlab = NA, ylab = NA)

par(bg = "#fdfdfd")
barplot(height = BOD$demand, names.arg = BOD$Time)

par(bg = "#fdfdfd")
plot(x = pressure$temperature, y = pressure$pressure, type = "l", xlab = NA, ylab = NA)

par(bg = "#fdfdfd")
plot(factor(mtcars$cyl), mtcars$mpg)

