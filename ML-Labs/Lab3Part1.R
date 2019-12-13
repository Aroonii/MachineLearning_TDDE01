RNGversion('3.5.1')

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding="latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

h_distance <- 140000# These three values are up to the students
  h_date <- 10
  h_time <- "05:00:00"
  a <- 58.4274 # The point to predict (up to the students)
b <- 14.826
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00","18:00:00", "20:00:00","22:00:00", "24:00:00")


temp = vector(length=length(times))
location = data.frame(a,b)

times = c()
for(hour in 1:21){
  adjusted_hour = hour + 3
  #hour = hour-3
  #if(adjusted_hour <10){
  if(adjusted_hour<=9 && adjusted_hour%%2 ==0){
   #if(adjusted_hour%%2 == 0){
    times[hour] = paste('0',adjusted_hour, ':00:00', sep = "")
    #}
  }
  else{
    times[hour] = paste(adjusted_hour, ':00:00', sep = "")
  }
}
View(st)

# Students’ code here
mydate = "2013-01-14"
selected.dates = subset(st, as.Date(st$date) < as.Date(mydate))



plot(temp, type="o")
