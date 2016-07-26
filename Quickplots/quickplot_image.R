

par(mfrow=c(2,2), mar=c(3,5,2,2), las=1, bty="n")
par(bg = "#EEEEEE")
plot(x = mtcars$wt, y = mtcars$mpg, xlab = NA, ylab = NA)
barplot(height = BOD$demand, names.arg = BOD$Time)
plot(x = pressure$temperature, y = pressure$pressure, type = "l", xlab = NA, ylab = NA)
plot(factor(mtcars$cyl), mtcars$mpg)

