# Script for NLM_Workshop
rm(list=ls())

# Load the data:
load("~/Desktop/nls Workshop/NLM_Workshop.RData")

library(nlstools)

# Plot NEE
par(mai=c(1,1,0.1,0.1))
plot(harv$TIMESTAMP, harv$NEE, 
     ylab=expression(paste("NEE (",mu,"mol m"^{-2} ~ s^{-1} ~ ")" )),
     xlab="")

# Plot NEE vs. PAR:
plot( NEE ~ PAR, data= day)

# NLS Model for LRC:
y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=day[which(day$MONTH == 07),], 
         start=list(a1= -1 , ax= -1, r= 1),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

summary(y)

# Selfstart Function:
## 1. Create a function of the model:

lrcModel <- function(PAR, a1, ax, r) {
  NEE <- (a1 * PAR * ax)/(a1 * PAR + ax) + r
  return(NEE)
}

## 2. Initial: create a function that calculates the initial values from the data.

lrc.int <- function (mCall, LHS, data){
  x <- data$PAR
  y <- data$NEE
  
  r <- max(na.omit(y), na.rm=T) # Maximum NEE
  ax <- min(na.omit(y), na.rm=T) # Minimum NEE
  a1 <- (r + ax)/2 # Midway between r and a1
  
  # ## Create limits for the parameters:
  a1[a1 > 0]<- -0.1
  r[r > 50] <- ax*-1
  r[r < 0] <- 1
  
  value = list(a1, ax, r) # Must include this for the selfStart function
  names(value) <- mCall[c("a1", "ax", "r")] # Must include this for the selfStart function
  return(value)
}

## Selfstart Function
SS.lrc <- selfStart(model=lrcModel,initial= lrc.int)

## 3. Find initial values:
iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == 07),]) 
iv

y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, day[which(day$MONTH == 07),], 
         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

summary(y)

results <- nlsBoot(y, niter=100 )
summary(results)

#_____________________________________________________________________________

# Fit montlhy models for HARV:

## Dataframe to store parms and se

parms.Month <- data.frame(
  MONTH=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a1.pvalue=numeric(),
  ax.pvalue=numeric(),
  r.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

# Adds months to the file
parms.Month[1:12, 1] <- seq(1,12,1)


nee.day <- function(dataframe){ y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, dataframe, 
                                         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                                         na.action=na.exclude, trace=F,
                                         control=nls.control(warnOnly=T))


y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:3, 1]), t(coef(summary(y)) [1:3, 4])))
names(y.df) <-c("a1","ax", "r", "a1.pvalue", "ax.pvalue", "r.pvalue") 
return (y.df )}

try(for(j in unique(day$MONTH)){
  
  # Determines  Starting values:
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == j),]) 
  
  # Fits light response curve:
  y3 <- try(nee.day(day[which(day$MONTH == j),]), silent=T) # Fit the day model
  
  # Extracts data and saves it in the dataframe
  try(parms.Month[c(parms.Month$MONTH == j ), 2:7 ] <- cbind(y3), silent=T)
  
  rm( y3)
}, silent=T)

parms.Month

# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a1.est <- 0
boot.NEE$ax.est<- 0
boot.NEE$r.est<- 0
boot.NEE$a1.se<- 0
boot.NEE$ax.se<- 0
boot.NEE$r.se<- 0

for ( j in unique(boot.NEE$MONTH)){
  
  y1 <-day[which(day$MONTH == j),] # Subsets data
  
  # Determines the starting values:
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = y1) 
  
  # Fit curve:
  day.fit <- nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                  start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                  na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  
  # Bootstrap and extract values:
  try(results <- nlsBoot(day.fit, niter=100 ), silent=T)
  try(a <- t(results$estiboot)[1, 1:3], silent=T)
  try(names(a) <- c('a1.est', 'ax.est', 'r.est'), silent=T)
  try( b <- t(results$estiboot)[2, 1:3], silent=T)
  try(names(b) <- c('a1.se', 'ax.se', 'r.se'), silent=T)
  try(c <- t(data.frame(c(a,b))), silent=T)
  
  # Add bootstrap data to dataframe:
  try(boot.NEE[c(boot.NEE$MONTH == j), 2:7] <- c[1, 1:6], silent=T)
  try(rm(day.fit, a, b, c, results, y1), silent=T)
  
}

lrc <- merge( parms.Month, boot.NEE, by.x="MONTH", by.y="MONTH") # Merge dataframes
lrc





