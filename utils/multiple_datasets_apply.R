set.seed(154)
time.series <- data.frame(t.dates = seq.Date(as.Date("2014-02-01"), as.Date("2016-08-01"), "month"),
                          t.values = c(
                            seq(0.1, 0.8, by = 0.1) * runif(8) + 3,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 4,
                            seq(0.1, 0.7, by = 0.1) * runif(7) + 5,
                            seq(0.1, 0.4, by = 0.1) * runif(4) + 4,
                            seq(0.1, 0.5, by = 0.1) * runif(5) + 4))


time.series<-cbind(time.series,
                   t.values.a = time.series[,2]+2*runif(length(time.series[,2])),
                   t.values.b = time.series[,2]+rnorm(1))

head(time.series)

#Option 1: Pass columns index to the X lapply argument
bucket_list <- lapply(c(2:4),
                      function(x) ccpoints(time.series,
                                           names(time.series[1]),
                                           names(time.series[x])))
#Option 2: Pass columns index by column names to the X lapply argument
bucket_list <- lapply(which(colnames(time.series) %in% c("t.values", "t.values.b")),
                      function(x) ccpoints(time.series,
                                           names(time.series[1]),
                                           names(time.series[x])))

summary(bucket_list)

head(bucket_list[[2]]$data)

which(colnames(time.series) %in% c("t.values", "t.values.b"))

