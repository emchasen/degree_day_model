setwd("~/Google Drive/coursera/degree day data science")
http://www.wunderground.com/history/airport/KISW/2016/2/1/MonthlyHistory.html?format=1

if(!file.exists("monthly data")) {
  dir.create("monthly data")
}
feb <- "http://www.wunderground.com/history/airport/KISW/2016/2/1/MonthlyHistory.html?format=1"
download.file(url = feb, destfile = "./monthly data/feb.csv")
febdata <- read.csv("./monthly data/feb.csv", header = TRUE)
d1H <- febdata[1, 2]
d1L <- febdata[1, 4]
TL <- 50
TH <- 86
H <- if(d1H > TH) {
  TH} else {
    d1H
  }
L <- if(d1L < TL) {
  TL} else {
    d1L
  }
DD <- ((H + L)/2) - TL
DD1 <- if(DD < 0) {
  0
}
DD1

degree_day <- function(location, month, TL, TH) {
        url <- sprintf(paste0("http://www.wunderground.com/history/airport/",location, "/2016/%s/1/MonthlyHistory.html?format=1"), month)
        #print(url)
        download.file(url = url, destfile = "./monthly data/month.csv")
        month_data <- read.csv("./monthly data/month.csv", header = TRUE)
        #highs <- vector()
        #lows <- vector()
        dd <- vector()
        for(i in 1:nrow(month_data)){
                dH[i] <- month_data[i, 2]
                dL[i] <- month_data[i, 4]
                #highs <- append(highs, dH[i])
                #lows <- append(lows, dL[i])
                H[i] <- if(dH[i] > TH) {
                        TH} else {
                                dH[i]
                        }
                L[i] <- if(dL[i] < TL) {
                        TL} else {
                                dL[i]
                        }
                #DDay[i] <- ((H[i] + L[i])/2) - TL
                #DD1[i] <- if(DDay[i] < 0) {
                 #       0
                #}
                #dd <- append(dd, DD1[i])
        }
        #print(highs)
        #print(lows)
        print(dd)
}

degree_day("KISW", 2, 50, 86)








#my_function1 <- function(location, month) {
#        url <- "www.wunderground.com/history/airport/code/2016/no/1/MonthlyHistory.html?format=1"
#        urll <- gsub("code", "location", url)
#        urlm <- gsub("no", "month", urll)
#        print(urlm)
#}

for(i in 1:nrow(febdata)){
        dH <- febdata[i, 2]
        dL <- febdata[i, 4]
        highs <- append(high, dH)
        #lows <- append(low, dL)
        print(highs)
}
