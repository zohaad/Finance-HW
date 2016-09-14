mydata <- read.csv("b.CSV", TRUE)
ff <- read.csv("c.CSV", TRUE)
mydata <- mydata[mydata$Date < 200701,]
mydata <- mydata[mydata$Date > 198912, ]
ff <- ff[ff$Date < 200701,]
ff <- ff[ff$Date > 198912,]
mydata$Date <- paste(paste(substr(mydata$Date, 1, 4), substr(mydata$Date, 5, 6),  sep = "-"))
mytime <- mydata$Date # backup for time series (xts)
rownames(mydata) <- mydata$Date
mydata$Date <- NULL
mydata[mydata == -99.99 | mydata == -999] <- NA
# || only evaluates FIRST element and returns one boolean value in a one-dimensional vector
# | evaluates everything and returns boolean values in a n-dimensional vector
mydata_mean <- colMeans(mydata, na.rm = TRUE)
mydata_sd <- apply(mydata, 2, sd, na.rm = TRUE)
# uses Bessel's correction
mydata_min <- apply(mydata, 2, min, na.rm = TRUE) 
mydata_max <- apply(mydata, 2, max, na.rm = TRUE)
#b: summary statistics
summ_stat <- data.frame(rbind(mean = mydata_mean, volty = mydata_sd, min = mydata_min, max = mydata_max ))
print(summ_stat)
#d: One Sample t-test, mu = 0,alpha is at 0.05 (conf.level=0.95)
apply(mydata, 2, t.test)
#f
par(mar=c(5.1,5.1,4.1,2.1), xaxs="i", yaxs="i", cex.lab=1.2, pch=20) # margins 4.1 to 4.5
plot(mydata_sd, mydata_mean, xlim=c(0,12), ylim=c(0,2),main=expression("Risk versus Return"), xlab=expression(sigma), ylab="Avg. Return")
#h: regression
betas <- c()
for(i in 1:49){
  single_beta <- lm((mydata[,i] - ff$RF) ~ ff$Mkt.RF)
  betas[i]<- coef(single_beta)["ff$Mkt.RF"]
}
#i
lm_1 <- lm(betas ~ mydata_sd)
plot(mydata_sd,betas, xlim= c(0,20), ylim=c(0,2), main=expression(beta ~ "versus" ~ sigma), ylab = expression(beta), xlab = expression(sigma))
abline(lm_1)
abline(a=0,b=1)
#j
lm_2 <- lm(mydata_mean ~ betas)
plot(betas,mydata_mean, xlim=c(0,2),ylim=c(0,1.8),main=expression("Systematic Risk versus Return"), ylab="Avg. Return", xlab=expression(beta))
abline(a=0, b=1)
