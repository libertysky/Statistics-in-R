install.packages("reshape")
install.packages("readx1")
install.packages("randomForest")
install.packages("nnet")
install.packages("TTR")
install.packages("quantmod")
install.packages("xlsx")

library("TTR")
library("quantmod")
library("xlsx")

setwd("C:\\Users\\vli\\Desktop\\Spring 2017\\FIN 6832 Num Stats Fri\\datasets")

# set ticker and date range
basic<-getYahooData("SPY",20050101,20170426)

# read from csv
# basic<-read.csv("spy0617.csv",header=T)

temp<-data.frame(basic)
data<-temp[c(1:5)]
data[!grepl('-', data),]

high<-data[,c('High')]
low<-data[,c('Low')]
close<-data[,c('Close')]

# calc log return
z<-with(data, diff(log(close)))
ret<-cbind(data,c(NA,z))

# add technical indicators

dataplus<-data.frame(data,
	SMA(close,n=20),
	SMA(close,n=50),
	SMA(close,n=120),
	SMA(close,n=200),
	VWAP(close,data[,c('Volume')],n=9),
	BBands(close,sd=1.96),
	RSI(close,n=14),
	MACD(close,nFast=12, nSlow=26, nSig=9, maType=SMA),
	OBV(close,data[,c('Volume')]),
	stoch(high,low,close, nFastK=14,nFastD=3,nSlowD=3,maType=SMA, bounded=TRUE,smooth=1)
)

colnames(dataplus) <- c('Open', 'High', 'Low','Close','Volume',
	'SMA20','SMA50','SMA120','SMA200','VWAP9',
	'dn','mavg','up','pctB',
	'RSI14',
	'MACD','signal',
	'OBV',
	'fastK','fastD','slowD'
)

#drop empty rows pre 2006
test<-dataplus[-(1:252),]

# export
write.xlsx(x=test,file="SPYTA8.xlsx",row.names=TRUE)

# load data
library(readxl, quietly=TRUE)
crs$dataset <- read_excel("C:/Users/vli/Desktop/Spring 2017/FIN 6832 Num Stats Fri/datasets/SPYTA8.xlsx")
crs$dataset

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) 
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) 
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) 

crs$input <- c("SMA20", "SMA50", "SMA120", "SMA200",
     "VWAP9", "dn", "mavg", "up",
     "pctB", "RSI14", "MACD", "signal",
     "OBV", "fastK", "fastD", "slowD")

crs$numeric <- c("SMA20", "SMA50", "SMA120", "SMA200",
     "VWAP9", "dn", "mavg", "up",
     "pctB", "RSI14", "MACD", "signal",
     "OBV", "fastK", "fastD", "slowD")

# Transform variables by rescaling. 
		# Rescale SMA
		crs$dataset[["RRC_SMA20"]] <-scale(crs$dataset[["SMA20"]])[,1]
		crs$dataset[["RRC_SMA50"]] <-scale(crs$dataset[["SMA50"]])[,1]
		crs$dataset[["RRC_SMA120"]] <-scale(crs$dataset[["SMA120"]])[,1]
		crs$dataset[["RRC_SMA200"]] <-scale(crs$dataset[["SMA200"]])[,1]

		# Rescale VWAP9.
		  crs$dataset[["RRC_VWAP9"]] <- scale(crs$dataset[["VWAP9"]])[,1]

		# Rescale BBands
		  crs$dataset[["RRC_dn"]] <- scale(crs$dataset[["dn"]])[,1]
		  crs$dataset[["RRC_mavg"]] <-scale(crs$dataset[["mavg"]])[,1]
		  crs$dataset[["RRC_up"]] <-scale(crs$dataset[["up"]])[,1]
		  crs$dataset[["RRC_pctB"]] <-scale(crs$dataset[["pctB"]])[,1]
		
		# Rescale RSI14.
		  crs$dataset[["RRC_RSI14"]] <-	scale(crs$dataset[["RSI14"]])[,1]
		
		# Rescale MACD.
		  crs$dataset[["RRC_MACD"]] <- scale(crs$dataset[["MACD"]])[,1]
		  crs$dataset[["RRC_signal"]] <-scale(crs$dataset[["signal"]])[,1]

		# Rescale OBV.
		  crs$dataset[["RRC_OBV"]] <-scale(crs$dataset[["OBV"]])[,1]

		# Rescale fastK.
		  crs$dataset[["RRC_fastK"]] <-	scale(crs$dataset[["fastK"]])[,1]
		
		# Rescale fastD.
  		  crs$dataset[["RRC_fastD"]] <-scale(crs$dataset[["fastD"]])[,1]

		# Rescale slowD.
		  crs$dataset[["RRC_slowD"]] <-scale(crs$dataset[["slowD"]])[,1]
		
crs$input <- c("RRC_SMA20", "RRC_SMA50", "RRC_SMA120", "RRC_SMA200",
     "RRC_VWAP9", "RRC_dn", "RRC_mavg", "RRC_up",
     "RRC_pctB", "RRC_RSI14", "RRC_MACD", "RRC_signal",
     "RRC_OBV", "RRC_fastK", "RRC_fastD", "RRC_slowD")

crs$numeric <- c("RRC_SMA20", "RRC_SMA50", "RRC_SMA120", "RRC_SMA200",
     "RRC_VWAP9", "RRC_dn", "RRC_mavg", "RRC_up",
     "RRC_pctB", "RRC_RSI14", "RRC_MACD", "RRC_signal",
     "RRC_OBV", "RRC_fastK", "RRC_fastD", "RRC_slowD")

crs$target  <- "logR"
crs$ident   <- "Date"
crs$ignore  <- c("Open", "High", "Low", "Close", "Volume", "SMA20", "SMA50", "SMA120", "SMA200", "VWAP9",
				"dn", "mavg", "up", "pctB", "RSI14", "MACD", "signal", "OBV", "fastK", "fastD", "slowD")
				
# Principal Components Analysis (on numerics only).

	pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

	# Show the output of the analysis.

	pc

	# Summarise the importance of the components found.

	summary(pc)

	# Display a plot showing the relative importance of the components.

	plot(pc, main="")
	title(main="Principal Components Importance SPYTA8.xlsx")
	axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

	# Display a plot showing the two most principal components.

	biplot(pc, main="")
	title(main="Principal Components SPYTA8.xlsx")

# KMeans 

	# Reset the random number seed to obtain the same results each time.

	set.seed(crv$seed)

	# The 'reshape' package provides the 'rescaler' function.

	library(reshape, quietly=TRUE)

	# Generate a kmeans cluster of size 10.

	crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 10)
	# Cluster sizes:

	paste(crs$kmeans$size, collapse=' ')

	# Data means:

	colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

	# Cluster centers:

	crs$kmeans$centers

	# Within cluster sum of squares:

	crs$kmeans$withinss

	# Generate a discriminant coordinates plot.

	cluster::clusplot(na.omit(crs$dataset[crs$sample, intersect(crs$input, crs$numeric)]), crs$kmeans$cluster, color=TRUE, shade=TRUE, main='Discriminant Coordinates SPYTA8.xlsx')

# Random Forest 

	# The 'randomForest' package provides the 'randomForest' function.

	library(randomForest, quietly=TRUE)

	# Build the Random Forest model.

	set.seed(crv$seed)
	crs$rf <- randomForest::randomForest(logR ~ .,
		  data=crs$dataset[crs$sample,c(crs$input, crs$target)], 
		  ntree=200,
		  mtry=4,
		  importance=TRUE,
		  na.action=randomForest::na.roughfix,
		  replace=FALSE)

	# Generate textual output of 'Random Forest' model.

	crs$rf

	# List the importance of the variables.

	rn <- round(randomForest::importance(crs$rf), 2)
	rn[order(rn[,1], decreasing=TRUE),]

	# Plot the error rate against the number of trees.
	plot(crs$rf, main="")
	legend("topright", c(""), text.col=1:6, lty=1:3, col=1:3)
	title(main="Error Rates Random Forest SPYTA8.xlsx")

# Neural Network 

	# Build a neural network model using the nnet package.

	library(nnet, quietly=TRUE)

	# Build the NNet model.

	set.seed(199)
	crs$nnet <- nnet(logR ~ .,
		data=crs$dataset[crs$sample,c(crs$input, crs$target)],
		size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

	# Print the results of the modelling.

	cat(sprintf("A %s network with %d weights.\n",
		paste(crs$nnet$n, collapse="-"),
		length(crs$nnet$wts)))
	cat(sprintf("Inputs: %s.\n",
		paste(crs$nnet$coefnames, collapse=", ")))
	cat(sprintf("Output: %s.\n",
		names(attr(crs$nnet$terms, "dataClasses"))[1]))
	cat(sprintf("Sum of Squares Residuals: %.4f.\n",
		sum(residuals(crs$nnet) ^ 2)))
	cat("\n")
	print(summary(crs$nnet))
	cat('\n')

# Evaluate model performance. 

# RF: Generate a Predicted v Observed plot for rf model on SPYTA8.xlsx [test].

	crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

	# Obtain the observed output for the dataset.

	obs <- subset(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]), select=crs$target)

	# Handle in case categoric target treated as numeric.

	obs.rownames <- rownames(obs)
	obs <- as.numeric(obs[[1]])
	obs <- data.frame(logR=obs)
	rownames(obs) <- obs.rownames

	# Combine the observed values with the predicted.

	fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

	# Obtain the pseudo R2 - a correlation.

	fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

	# Plot settings for the true points and best fit.

	op <- par(c(lty="solid", col="blue"))

	# Display the observed (X) versus predicted (Y) points.

	plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="logR", ylab="Predicted")

	# Generate a simple linear fit between predicted and observed.

	prline <- lm(fitpoints[,2] ~ fitpoints[,1])

	# Add the linear fit to the plot.

	abline(prline)

	# Add a diagonal representing perfect correlation.

	par(c(lty="dashed", col="black"))
	abline(0, 1)

	# Include a pseudo R-square on the plot

	legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

	# Add a title and grid to the plot.

	title(main="Predicted vs. Observed
	 Random Forest Model
	 SPYTA8.xlsx [test]")
	grid()

# NNET: Generate a Predicted v Observed plot for nnet model on SPYTA8.xlsx [test].

	crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

	# Obtain the observed output for the dataset.

	obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

	# Handle in case categoric target treated as numeric.

	obs.rownames <- rownames(obs)
	obs <- as.numeric(obs[[1]])
	obs <- data.frame(logR=obs)
	rownames(obs) <- obs.rownames

	# Combine the observed values with the predicted.

	fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

	# Obtain the pseudo R2 - a correlation.

	fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

	# Plot settings for the true points and best fit.

	op <- par(c(lty="solid", col="blue"))

	# Display the observed (X) versus predicted (Y) points.

	plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="logR", ylab="Predicted")

	# Generate a simple linear fit between predicted and observed.

	prline <- lm(fitpoints[,2] ~ fitpoints[,1])

	# Add the linear fit to the plot.

	abline(prline)

	# Add a diagonal representing perfect correlation.

	par(c(lty="dashed", col="black"))
	abline(0, 1)

	# Include a pseudo R-square on the plot

	legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

	# Add a title and grid to the plot.

	title(main="Predicted vs. Observed
	 Neural Net Model
	 SPYTA8.xlsx [test]")
	grid()

# Fundamental Analysis

library(readxl, quietly=TRUE)
crs$dataset <- read_excel("C:/Users/vli/Desktop/Spring 2017/FIN 6832 Num Stats Fri/datasets/SPYFA.xlsx")
crs$dataset

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) 
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.14*crs$nobs) 
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) 

crs$input <- c("BV per sh", "Div growth", "E growth", "P/B",
     "P/E", "P/S", "P/S growth")

crs$numeric <- c("BV per sh", "Div growth", "E growth", "P/B",
     "P/E", "P/S", "P/S growth")

crs$target  <- "logR"
crs$ident   <- "Date"

# Principal Components Analysis (on numerics only).

	pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

	# Show the output of the analysis.

	pc

	# Summarise the importance of the components found.

	summary(pc)

	# Display a plot showing the relative importance of the components.

	plot(pc, main="")
	title(main="Principal Components Importance SPYFA.xlsx",
		sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
	axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

	# Display a plot showing the two most principal components.

	biplot(pc, main="")
	title(main="Principal Components SPYFA.xlsx")
	
# repeat same models as above
# combi.xlsx was manually created since the time intervals do not match exactly
# e.g. A month may be recorded as 2/29 or 3/01 between the two sets but the closest record is the 1st

# import combi.xlsx containing both FA and TA data 
# After combining the two, repeat PC and other tests, etc.
