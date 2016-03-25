#example url
#http://www.wunderground.com/history/airport/KWIWISCO3/2016/1/1/MonthlyHistory.html?format=1

setwd("~/Google Drive/coursera/degree day data science")

if(!file.exists("monthly data")) {
  dir.create("monthly data")
}


degree_day <- function(location,  month, year, TH, TL) {
    #download data from wunderground, create one data frame containing all months with year to date
    mo <- list()
    for(i in 1:month) {
      url <- paste0("http://www.wunderground.com/history/airport/",location, "/", year, "/", i, "/1/MonthlyHistory.html?format=1")
      download.file(url = url, destfile = paste0("./monthly data/mo", i, ".csv"))
      mon <- read.csv(paste0("./monthly data/mo", i, ".csv"), header = TRUE)[ ,c("Max.TemperatureF", "Min.TemperatureF")]
      mon$i <- i
      mo[[i]] <- mon
    }
    all_data <- do.call(rbind, mo) #data frame with highs and lows for each month
    # calculate degree day accumuulation by defining the high for each day, the low for each day, running the calculation,
    # then creating vector of DD for each day and finally by taking the sum.
        dd <- c()
        for(i in 1:nrow(all_data)) {
                # highs or lows used for calculation cannot be outside the temperature thresholds
                High <- if(all_data[i, 1] > TH) {TH} else {all_data[i, 1]} #define high temp 
                Low <- if(all_data[i, 2] < TL) {TL} else {all_data[i, 2]} # define low temp
                DDtemp <- (High + Low)/2 - TL ##DD calculation
                # degree days cannot be negative
                DDay <- if(DDtemp < 0) {0} else {DDtemp}
                dd <- append(dd, DDay)
        }
        # degree day accrual for the month
        #print(dd)
        GDD <<- sum(dd)
        #print(GDD)
        print(paste("Total accumulation of GDD for", location, "is", sum(dd)))
}

degree_day("KRNH", 3, 2016, 85, 41)

#next steps
#1. fill in proper DD calculation
#2-3. learn how to apply this to spatial R






