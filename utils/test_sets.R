# Create a random time series
set.seed(154)
time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
                          t.values = c(
                            seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
                            seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
                            seq(0.1, 0.5, by = 0.1) * runif(5) + 4)
)

# Execute function
control.chart.data <- ccpoints(time.series, "t.dates", "t.values")
print(control.chart.data)

windows()
cc2plot(control.chart.data)
# To extract only the data frame
control.chart.data[["data"]]

time.series

t <- cbind(eje = c(1:nrow(time.series)), time.series)
t <- t[,-2]
q <- as.data.frame(cbind(c(1,2,3,4,5,6,7), c(3.03, 3.20, 3.03, 3.20, 3.03, 3.20, 3.03)))
colnames(q) <- c("eje", "t.values")
w <- rbind(q,t)
e <- cbind(1:nrow(w), w)
e <- e[, -2]
colnames(e)[1] <- "eje"
head(e)

control.chart.data <- ccpoints2(e, "eje", "t.values", date.type = F)
print(control.chart.data)
windows()
cc2plot(control.chart.data)

data <- control.chart.data[["data"]]





for (i in 1:length(unique(data$data.mean))){
  d_set <- data[data$data.mean == unique(data$data.mean)[i],]
  data[data$data.mean == unique(data$data.mean)[i], c("data.mean")] <- mean(d_set$t.values)
  data[data$data.mean == unique(data$data.mean)[i], c("data.ll")] <- mean(d_set$t.values) - sd(d_set$t.values)
  data[data$data.mean == unique(data$data.mean)[i], c("data.ul")] <- mean(d_set$t.values) + sd(d_set$t.values)
  rm(d_set)
}


last <- data[which(data$data.mean == unique(data$data.mean)[length(unique(data$data.mean))]), ]
l[["point_last_break"]] <- last[1, dates]
if(date.type){
  l[["weeks_since_last_break"]] <- floor(difftime(Sys.Date(), last[1, dates], units = "weeks"))
} else {
  l[["weeks_since_last_break"]] <- "No Date data type provided"
}



