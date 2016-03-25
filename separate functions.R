#example url
#http://www.wunderground.com/history/airport/KWIWISCO3/2016/1/1/MonthlyHistory.html?format=1

#laptop
setwd("~/Google Drive/coursera/degree day data science")
#desktop
setwd("C:/Users/echasen/Google Drive/coursera/degree day data science")

#function 1 reads in the data
DD_data <- function(location,  month, year) {
        #download data from wunderground, create one data frame containing all months with year to date
        mo <- list()
        for(i in 3:month) {
                url <- paste0("http://www.wunderground.com/history/airport/",location, "/", year, "/", i, "/1/MonthlyHistory.html?format=1")
                download.file(url = url, destfile = paste0("./monthly data/mo", i, ".csv"))
                mon <- read.csv(paste0("./monthly data/mo", i, ".csv"), header = TRUE)[ ,c("Max.TemperatureF", "Min.TemperatureF")]
                mon$i <- i
                mo[[i]] <- mon
        }
        deg_data <<- do.call(rbind, mo) #data frame with highs and lows for each month
       
}

DD_data("KISW", 7, 2015)


# Function two calculate degree day accumuulation by either defining the high for each day, 
# the low for each day, running the calculation,
# then creating vector of DD for each day and finally by taking the sum. Or finding the area under
# polynomial curve
Acc_Degday <- function(TH, TL, method = c("simple", "curve")){
        dd <- c()
        if (method == "simple"){ 
        for(i in 1:nrow(deg_data)) {
                # highs or lows used for calculation cannot be outside the temperature thresholds
                High <- if(deg_data[i, 1] > TH) {TH} else {deg_data[i, 1]} #define high temp 
                Low <- if(deg_data[i, 2] < TL) {TL} else {deg_data[i, 2]} # define low temp
                DDtemp <- (High + Low)/2 - TL ##DD calculation
                # degree days cannot be negative
                DDay <- if(DDtemp < 0) {0} else {DDtemp}
                dd <- append(dd, DDay)
                }
        }
# degree day accrual for the month
#print(dd)
        if (method == "curve") {
        require(polynom)
        #make vector of high and low data combined
        temps <-c(rbind(deg_data[,2],deg_data[,1]))
        print(length(temps))
        #times <- seq(0, length(temps), by = 0.5)
        #make vector of time 
        time.a = c(.5, 1, 1.5, 2, 2.5)
        #create loop
        for(i in 1:(length(temps)-5)){
                #time.a = times[(i+1):(i+5)]
                temp.a = temps[(i+1):(i+5)]
                #print(head(time.a))
                fit <- lm(temp.a ~ time.a + I(time.a^2) + I(time.a^3) + I(time.a^4))
                #print(coef(fit)[[1]])
                integrand <- function(x) {coef(fit)[[1]] + coef(fit)[[2]]*x + coef(fit)[[3]]*x^2 + coef(fit)[[4]]*x^3 + coef(fit)[[5]]*x^4}
                #print(integrand)
                #cu <- integrate(integrand, lower = time.a[2], upper = time.a[4])
                cu <- integrate(integrand, lower = 1, upper = 2)
                
                #Error in integrate(integrand, lower = 1, upper = 2) : 
                 #       non-finite function value
                DDtemp <- cu$value - TL
                DDay <- if(DDtemp < 0) {0} else {DDtemp}
                dd <- append(dd, DDay)
                }
        }
GDD <<- sum(dd)
#print(GDD)
print(paste("Total accumulation of GDD is", sum(dd)))
}

Acc_Degday(86, 50, method = "simple")
Acc_Degday(86, 50, method = "curve")
