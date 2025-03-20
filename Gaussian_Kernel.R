set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
h_distance <- 125000# These three values are up to the students
h_date <- 20
h_time <- 1.7
a <- 58.4274
b <- 14.826 #long, lat

date <- "2012-07-07"
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00","12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00","22:00:00", "06:00:00", "06:00:00", "24:00:00")
temp <- vector(length=length(times))

#filters out temperature measurements that are posterior/the same as our forecast
st_filtered <- st[!as.Date(st$date) >= date,]

#choosing appropriate smoothing coefficient for physical distance
dist = seq(0,500000)
plot(exp(-abs(dist^2)/(2*h_distance^2)), type = 'l', xlab = "Physical distance", ylab = "Kernel")

#choosing appropriate smoothing coefficient for days
dist = seq(0,185)
plot(exp(-abs(dist^2)/(2*h_date^2)), type = 'l', xlab = "Distance in days", ylab = "Kernel")

#choosing appropriate smoothing coefficient for hours
dist = seq(0,12)
plot(exp(-abs(dist^2)/(2*h_time^2)), type = 'l', xlab = "Distance in hours", ylab = "Kernel")

#1 gaussian kernel for physical dist
distance_kern <- function(x,y){
  distance_k = distHaversine(x,y)
  return (exp(-abs(distance_k^2)/(2*h_distance^2)))
} 

#2 gaussian kernel for day dist
date_kern <- function(x,y){
  n = length(x)
  distance = rep(0,n)
  for (i in 1:n) {
    distance[i] = (length(seq(from = x[i], to = y, by = 'day'))-1)%%365
    if (distance[i] >= 182.5){
      distance[i] = 365-distance[i]
    }  
  }
  return((exp(-abs(distance)^2))/(2*h_date^2))
}

#3 gaussian kernel for hour dist
time_kern <- function(x,y){
  distance <- abs(as.numeric(difftime(strptime(x, format = "%H:%M:%S"), strptime(y, format = "%H:%M:%S"), units = "hours")))
  for (i in 1:length(y)){
    if (distance[i] >= 12){
      distance[i] = 24-distance[i]
    }
  }
  return((exp(-distance^2))/(2*h_time^2))
}

#function that calculates the kernel that is the sum/mult of the three gaussian kernels
kernel <- function(st_filtered){
  distance_k = distance_kern(data.frame(st_filtered$longitude,st_filtered$latitude),data.frame(b,a))
  date_k = date_kern(as.Date(st_filtered$date), as.Date(date))
  
  sum_predictions = rep(0,11)
  mult_predictions = rep(0,11)
  
  for (i in 1:length(times)) {
    time_k = time_kern(times[i],st_filtered$time)
    
    sum_k = distance_k + date_k + time_k
    mult_k = distance_k * date_k * time_k 
    sum_predictions[i] =sum(sum_k*st_filtered$air_temperature)/sum(sum_k)
    mult_predictions[i] =sum(mult_k*st_filtered$air_temperature)/sum(mult_k) 
  }
  output = data.frame(sum_predictions, mult_predictions)
  return(output)
}

temp = kernel(st_filtered)

plot(temp[,1], type="o", xlab ="Time", ylab = "Temperature", xaxt ='n', main = "Prediction using sum kernel")
axis(1, at = 1:length(times), labels = times)

plot(temp[,2], type="o", xlab ="Time", ylab = "Temperature", xaxt ='n', main = "Prediction using mult. kernel")
axis(1, at = 1:length(times), labels = times)

