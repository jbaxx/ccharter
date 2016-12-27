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
  data[data$data.mean == unique(data$data.mean)[i], c("data.mean")] <- mean(data[, values])
  data[data$data.mean == unique(data$data.mean)[i], c("data.ll")] <- mean(data[, values]) - sd(data[, values])
  data[data$data.mean == unique(data$data.mean)[i], c("data.ul")] <- mean(data[, values]) + sd(data[, values])
  rm(d_set)
}



ap <- read.csv("~/cc_test/ts_val.csv", strip.white = T, stringsAsFactors = F)
ap <- cbind(eje = c(1:nrow(ap)), ap)



control.chart.data <- ccpoints2(ap, "eje", "t.values", date.type = F)
print(control.chart.data)
windows()
cc2plot(control.chart.data)


epa <- data.frame(ax = c(1:length(rep(e[1:6,2], 12))), val = rep(e[1:6,2], 12))
epa[14, 2] <- 3.7
epa[60:65, 2] <- c(3.15, 3.16, 3.17, 3.16, 3.15, 3.16)

control.chart.data <- ccpoints2(epa, "ax", "val", date.type = F)
print(control.chart.data)
windows()
cc2plot(control.chart.data)




truena <- read.csv("~/cc_test/truena.csv", strip.white = T, stringsAsFactors = F)
colnames(truena) <- c("ejex", "ejey")
control.chart.data <- ccpoints2(truena, "ejex", "ejey", date.type = F)
windows()
cc2plot(control.chart.data)

#Adding ooc
truena_v2 <- truena
truena_v2[6, "ejey"] <- 20
control.chart.data <- ccpoints2(truena_v2, "ejex", "ejey", date.type = F)
windows()
cc2plot(control.chart.data)




control.chart.data <- ccpoints2(ga.data, "date", "sessions", date.type = F)
windows()
cc2plot(control.chart.data)


bucket_list <- lapply(which(colnames(ga.data) %in% c("users", "avgTimeOnPage", "hits", "avgSessionDuration")),
                      function(x) ccpoints2(ga.data,
                                           names(ga.data[1]),
                                           names(ga.data[x]), points.vs.avg = 5, date.type = F))
vector_index <- sapply(c("users", "avgTimeOnPage", "hits", "avgSessionDuration"),
                       function(x) which(colnames(ga.data) %in% x))
bucket_list <- lapply(vector_index,
                      function(x) ccpoints2(ga.data,
                                            names(ga.data[1]),
                                            names(ga.data[x]), points.vs.avg = 5, date.type = F))

windows()
a <- cc2plot(bucket_list[[1]], data.title = "users")
b <- cc2plot(bucket_list[[2]], data.title = "avgTimeOnPage")
c <- cc2plot(bucket_list[[3]], data.title = "hits")
d <- cc2plot(bucket_list[[4]], data.title = "avgSessionDuration")
grid.arrange(a, b, c, d, ncol = 1, nrow = 4)





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
windows()
cc2plot(control.chart.data)


