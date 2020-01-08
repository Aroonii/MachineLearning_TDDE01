RNGversion('3.5.1')

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding="latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

# The point to predict (up to the students)
a <- 14.826
b <- 58.4274 
location = data.frame(a,b)
  
my_times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00","18:00:00", "20:00:00","22:00:00","00:00:00")
mydate = "2016-01-13"

#Filter dataset to not include dates that are after the time of prediction
selected.dates = subset(st, as.Date(st$date) <= as.Date(mydate))


#med data.frame behöver man inte köra for loopen utan räcker med
#distance2 = kernel_distance(selected.dates, location, h_distance)
#for( i in 1:nrow(selected.dates)){
 # distance[i] = kernel_distance(selected.dates[i,], location, h_distance)
#return(distance)
#}

#gaussan kernel
kernel_distance = function(Xn, x, h){
  u = distHaversine(data.frame(Xn$longitude, Xn$latitude), x)/h
  k = exp(-u^2)
  return (k)
}

h_distance <- 1000*200
distance = kernel_distance(selected.dates, location, h_distance)
plot(distance, xlab = "distance", ylab = "weight")



#Date kernel
date = as.Date(mydate)
kernel_day = function(date, selected.dates, h_date){
 u =  difftime(date, selected.dates$date)
 u = as.numeric(u)
 u = u%%365
 diff = 365-u
 u = ifelse(u>183,diff,u)
 u = u/h_date
 k = exp(-u^2)
 return(k)
}
h_date <- 4
day_kernel = kernel_day(date, selected.dates, h_date)
plot(day_kernel, xlab = "dates", ylab = "weight")



#Time Kernel
kernel_hours = function(selected.dates, times){
  time_diff = difftime(strptime(selected.dates$time, format = "%H:%M:%S"),
                       strptime(times[1], format = "%H:%M:%S"), units = "hours")
  time_abs = abs(time_diff)
  fix_time = 24-time_abs
  fixtime =as.numeric(fix_time)
  time_diff = ifelse(time_abs > 12, fix_time, time_abs)
  u = time_diff/h_time
  k = exp(-u^2)
return(k)
}
h_time <- 5
time_kernel = kernel_hours(selected.dates, times)
plot(time_kernel, xlab = "time", ylab = "weight")

#time_diff = difftime(strptime(selected.dates$time, format = "%H:%M:%S"),
                     #strptime(times, format = "%H:%M:%S"), units = "hours")
#time_abs = abs(time_diff)
#as.numeric(time_abs)
#fix_time = 24-time_abs
#time_diff = ifelse(time_abs > 12, fix_time, time_abs)
#u = time_diff/h_time
#k = exp(-u^2)
#plot(k)

time.vector = c()
temp_sum = c()
temp_mult =c()
for(i in 1:11){
  time.vector = kernel_hours(selected.dates, times[i])
  sum.kernels = time.vector + distance + day_kernel
  product.kernels = time.vector*distance*day_kernel
  temp_sum[i] = sum(sum.kernels*selected.dates$air_temperature)/sum(sum.kernels)
  temp_mult[i] = sum(product.kernels*selected.dates$air_temperature)/sum(product.kernels)
  
}
plot(1:length(time.vector), time.vector)

plot(temp_sum, type ="o", ylim = c(2, 6), xlab = "timeof day", xaxt ='n')
axis(1, at=1:length(times), labels =times)
points(temp_mult, type = "o", col = "red")
legend("bottomright", col = c("black", "red"), legend = c("sum", "mult"), pch = c(1, 1))
