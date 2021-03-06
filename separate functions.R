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

DD_data("KRNH", 3, 2016)

#creating the function to read in data from all locations
DD_data_ID_all <- function(ID = 1:46, month, year = 2016) {
        #read in file with ID names
        id_list <- read.csv("StationIDs.csv")
        deg_data <- list()
        for(i in ID){
                location = id_list[i, 3]
                #download data from wunderground, create one data frame containing all months with year to date
                mo <- list()
                for(j in 3:month) {
                        url <- paste0("http://www.wunderground.com/history/airport/",location, "/", year, "/", j, "/1/MonthlyHistory.html?format=1")
                        download.file(url = url, destfile = paste0("./mo", j, ".csv"))
                        mon <- read.csv(paste0("./mo", j, ".csv"), header = TRUE)[ ,c("Max.TemperatureF", "Min.TemperatureF")]
                        mo[[j]] <- mon
                }
                deg_data <- do.call(rbind, mo) #data frame with highs and lows for each month
                #print(deg_data)
                deg_data$date <- seq(as.Date("2016/3/1"), by = "day", length.out = nrow(deg_data))
                #print(deg_data)
                #ddata <<- deg_data
                # create text file
                filename <- paste("./location data/loc", i, ".txt", sep="")
                write.table(deg_data, filename, row.names = FALSE, col.names = FALSE)
        }
}

DD_data_ID_all(ID = 1:5, month = 4)


# Function two calculate degree day accumuulation by either defining the high for each day, 
# the low for each day, running the calculation,
# then creating vector of DD for each day and finally by taking the sum. Or finding the area under
# polynomial curve
Acc_Degday <- function(TH, TL, method = c("simple", "curve")){
        GDD <- c() # empty vector for degree day accumulations for each location
        for(i in 1:5){
                dd <- c() # empty vector for each degree day total
                filename <- paste("./location data/loc", i, ".txt", sep="") #read in data from location data folder
                dat <- read.table(filename, col.names = c("high", "low", "date"))
                if (method == "simple"){ 
                        for(i in 1:nrow(dat)) {
                                # highs or lows used for calculation cannot be outside the temperature thresholds
                                High <- if(dat[i, 1] > TH) {TH} else {dat[i, 1]} #define high temp 
                                Low <- if(dat[i, 2] < TL) {TL} else {dat[i, 2]} # define low temp
                                DDtemp <- (High + Low)/2 - TL ##DD calculation
                                # degree days cannot be negative
                                DDay <- if(DDtemp < 0) {0} else {DDtemp}
                                dd <- append(dd, DDay)
                                #print(dd)
                                }
                        }
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
                                # non-finite function value
                                DDtemp <- cu$value - TL
                                DDay <- if(DDtemp < 0) {0} else {DDtemp}
                                dd <- append(dd, DDay)
                                }
                        }
                #print(paste("Total accumulation of GDD is", sum(dd)))
                DDsum <- sum(dd)
                #print(DDsum)
                GDD <- append(GDD, DDsum) #vector of all GDD accumulations
                }
        #print(GDD)
        #write file that will contain two columns: Location and GDD accumuations
        id_list <- read.csv("StationIDs.csv") #read in file with ID names
        df = data.frame(ID = id_list[,3], deg = GDD)
        write.table(df, "GDDdata.txt", row.names = FALSE)
        }

Acc_Degday(86, 50, method = "simple")
Acc_Degday(86, 50, method = "curve")