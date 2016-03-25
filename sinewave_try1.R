url <- "http://www.wunderground.com/history/airport/KMSN/2015/8/1/MonthlyHistory.html?format=1"
download.file(url = url, destfile = "./monthly data/aug.csv")
aug <- read.csv("./monthly data/aug.csv", header = TRUE)
head(aug)
max <- aug[,2]
min <- aug[,4]
max_min <- data.frame(max = max, min = min)

y<-c(rbind(min,max))
x<-1:length(y)


time = c(0, .5, 1, 1.5, 2, 2.5)
temp = c(45, 66, 54, 70, 56, 72)
df = data.frame(time, temp)
plot(temp ~ time, ylim = c(0, 70))
time.a = c(.5, 1, 1.5, 2, 2.5)
temp.a = c(66, 54, 70, 56, 72)
plot(temp.a ~ time.a, ylim = c(45, 70))
fit <- lm(temp.a ~ time.a + I(time.a^2) + I(time.a^3) + I(time.a^4))
summary(fit)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)   282.00         NA      NA       NA
#time.a       -812.33         NA      NA       NA
#I(time.a^2)   976.33         NA      NA       NA
#I(time.a^3)  -470.67         NA      NA       NA
#I(time.a^4)    78.67         NA      NA       NA

library(polynom)
p = polynomial(coef(fit))
lines(p)
#b = polynomial(coef = c(0, 4.819444, -0.3263889, 0.005883488))
#lines(b)
abline(h = 50)
integrand <- function(x) {282 -812.33*x + 976.33*x^2 - 470.67*x^3 + 78.67*x^4}
integrand1 <- function(x) {coef(fit)[[1]] + coef(fit)[[2]]*x + coef(fit)[[3]]*x^2 + coef(fit)[[4]]*x^3 + coef(fit)[[5]]*x^4}

auc1 <- integrate(integrand1, lower = 1, upper = 2)
cu <- integrate(integrand, lower = time.a[2], upper = time.a[4])
#64.34983 with absolute error < 7.1e-13
#find way to subtract area under x = TL (50)
dd <- cu$value - 50


time.b = times[300:305]
temp.b = temps[300:305]
fit.b <- lm(temp.b ~ time.b + I(time.b^2) + I(time.b^3) + I(time.b^4))
integrand.b <- function(x) {coef(fit.b)[[1]] + coef(fit.b)[[2]]*x + coef(fit.b)[[3]]*x^2 + coef(fit.b)[[4]]*x^3 + coef(fit.b)[[5]]*x^4}
cu.b <- integrate(integrand, lower = 1, upper = 2)
dd <- cu.b$value - 50


# find area under curve following trapezoid rule
library(flux)
#make one vector of high and low temps
z <- as.vector(rbind(max,min)) 
temp = ts(z, start = 1, end = 3, frequency = 3)
plot(temp)
## Calculate assuming that temperature develop linearly between
## measurements
auc(time(temp), temp)
abline(h = 50)
auc(time(temp), temp, thresh = 50)


