setwd("~/Google Drive/coursera/degree day data science")

if(!file.exists("monthly data")) {
  dir.create("monthly data")
}

degree_day <- function(location, month, TH, TL) {
        #download data from wunderground
        url <- sprintf(paste0("http://www.wunderground.com/history/airport/",location, "/2016/%s/1/MonthlyHistory.html?format=1"), month)
        download.file(url = url, destfile = "./monthly data/month.csv")
        month_data <- read.csv("./monthly data/month.csv", header = TRUE)
        #create empty vectors for daily highs and lows
        highs <- vector()
        lows <- vector()
        for(i in 1:nrow(month_data)){
                # for each row of data (day), extra the high (dH) and low (dL)
                dH[i] <- month_data[i, 2]
                dL[i] <- month_data[i, 4]
                # add high and low elements to their respective vector
                highs <- append(highs, dH[i])
                lows <- append(lows, dL[i])
        }
        #create dataframe of daily highs and lows ... look into doing this from the original read.csv call
        highlow <- data.frame(highs, lows)
        # create empty vectors and variables to fill in the next iteration
        hi <- vector()
        lo <- vector()
        dd <- vector()
        H <- c()
        L <- c()
        DDay <- c()
        for(i in 1:nrow(highlow)) {
                # highs or lows used for calculation cannot be outside the temperature thresholds
                H[i] <- if(highlow[i, 1] > TH) {TH} else {highlow[i, 1]}
                L[i] <- if(highlow[i, 2] < TL) {TL} else {highlow[i, 2]}
                DDtemp <- (H[i] + L[i])/2 - TL
                # degree days cannot be negative
                DDay[i] <- if(DDtemp < 0) {0} else {DDtemp}
                dd <- append(dd, DDay[i])
        }
        # degree day accrual for the month
        sum(dd)
}

degree_day("KMSN", 2, 86, 30)


# still want to figure out why this didn't work
#my_function1 <- function(location, month) {
#        url <- "www.wunderground.com/history/airport/code/2016/no/1/MonthlyHistory.html?format=1"
#        urll <- gsub("code", "location", url)
#        urlm <- gsub("no", "month", urll)
#        print(urlm)
#}

