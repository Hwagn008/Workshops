# Set your working directory to the Malone NLM Workshop
rm(list=ls())

# Import Ameriflux file for Harvard Forest (Ha1)

load("~/OneDrive - Florida International University/Teaching/Workshops/Workshops/NLM_Workshop.RData")
library(nlstools)

# Format the timestamp for the file:
library(lubridate)
harv$TIMESTAMP <- ymd_hm( harv$TIMESTAMP_END)
harv$MONTH <- as.numeric(format(harv$TIMESTAMP, '%m' ))
harv$YEAR <- as.numeric(format(harv$TIMESTAMP, '%Y' ))

# Remove all -9999:           
harv[harv == -9999]<- NA

# What is in the file:
summary(harv)

# Calculate NEE: NEE= FC+ SF
harv$NEE <- harv$FC + harv$SC
harv$PAR <- harv$PPFD_IN_1_2_1
harv$TA <- harv$TA_PI_F_1_1_1


par(mfrow=c(3,1), mai=c(0.25,0.5,0.1,0.1)) # plots in rows of 3 columns of 1

plot(harv$TIMESTAMP, harv$NEE)
plot(harv$TIMESTAMP, harv$PAR)
plot(harv$TIMESTAMP, harv$TA)

par(mfrow=c(1,1))
plot(harv$PAR, harv$NEE )

par(mfrow=c(1,1))
plot(harv$TA, harv$NEE )

# Day and Night Files:
day <-harv[which(harv$PAR > 0 ),]
night <-harv[which(harv$PAR <= 0),]

save(harv,day, night, file = "~/Desktop/NLM_Workshop.RData")
load("NLM Workshop.RData")
#__________________________________________________________________________________________

# Trail 1: Just fit a model:

# FIT LRC
y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, day[which(day$MONTH ==07 & day$YEAR == 1995),], 
         start=list(a1= -7 , ax= -9, r= 3),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

summary(y)


#__________________________________________________________________________________________

# Try the Selfstart:

# The model for the Light Response Curve:
lrcModel <- function(PAR, a1, ax, r) {
  NEE <- (a1 * PAR * ax)/(a1 * PAR + ax) + r
  return(Nee)
}

# Create a function to find initial values for the selfstart function:

lrc.int <- function (mCall, LHS, data){
  x <- data$PAR
  y <- data$NEE
  
  r <- max(na.omit(y), na.rm=T)
  ax <- min(na.omit(y), na.rm=T)
  a1 <- (r + ax)/2
  
  a1[a1 > 0]<- -0.1
  r[r > 50] <- ax*-1
  r[r < 0] <- 1
  
  value = list(a1, ax, r)
  names(value) <- mCall[c("a1", "ax", "r")]
  return(value)
}

# Selfstart Function
SS.lrc <- selfStart(model=lrcModel,initial= lrc.int)

# Find initial values:
iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == j &  day$YEAR == i),]) 

y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, day[which(day$MONTH ==j & day$YEAR == i),], 
         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))

summary(y)


# Checking Assumptions:
x=nlsResiduals(y)

par(mfrow=c(2,2))
plot(x, which=1) # non-transformed residuals against fitted values 
plot(x, which=3) #sqrt of absolute value of standardized residuals against fitted values 


plot(x, which=4) #  auto-correlation residuals (i+1th residual against ith residual) 
plot(x, which=5) #histogram of the residuals

x=nlsResiduals(y) # histogram of the residuals 
test.nlsResiduals (x)

print(x)


# Selfstart for the trc:

trcModel <- function(TA, a, b) {
  y=a * exp(b*TA)
  return(y)
}

# Create a function to find initial values for the selfstart function:
trc.int <- function (mCall, LHS, data){
  x <- data$TA
  y <- data$NEE
  
  a <-1.00703982 + -0.08089044* (min(na.omit(y)))
  b <- 0.051654 + 0.001400 * (min(na.omit(y))) 
  
  value = list(a, b)
  names(value) <- mCall[c("a", "b")]
  return(value)
}

# Selfstart Function
SS.trc <- selfStart(model=trcModel,initial= trc.int)

#__________________________________________________________________________________________
#__________________________________________________________________________________________
#__________________________________________________________________________________________

# Fitting monthly models:

# Create Dataframe to store the data:

parms <- parms


# Create file to store parms and se
parms.Annual <- data.frame(
  YEAR=numeric(),
  MONTH=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a1.pvalue=numeric(),
  ax.pvalue=numeric(),
  r.pvalue=numeric(),
  a=numeric(),
  b=numeric(), 
  a.pvalue=numeric(),
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

parms.Month <- data.frame(
  MONTH=numeric(),
  a1=numeric(),
  ax=numeric(),
  r=numeric(),
  a1.pvalue=numeric(),
  ax.pvalue=numeric(),
  r.pvalue=numeric(),
  a=numeric(),
  b=numeric(), 
  a.pvalue=numeric(),
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)

# Creates time file to merge with parm file:
time <-  harv[,c("YEAR", "MONTH")] 
time <- time[!duplicated( time[ ,c( "YEAR", "MONTH")]),]
parms.Annual <- merge(time, parms.Annual, all=T) # merge parms file with time file 


parms.Month[1:12, 1] <- seq(1,12,1)

#Functions:

nee.day <- function(dataframe){ y = nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, dataframe, 
                                         start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                                         na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))


y.df <- as.data.frame(cbind(t(coef(summary(y)) [1:3, 1]), t(coef(summary(y)) [1:3, 4])))
names(y.df) <-c("a1","ax", "r", "a1.pvalue", "ax.pvalue", "r.pvalue") 
return (y.df )}

nee.night <- function(dataframe){y.df = nls(NEE ~ a * exp(b*TA), 
                                            dataframe, start=list(a= iv$a , b=iv$b ),
                                            na.action=na.exclude, trace=F,
                                             control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))

names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                      
return(y.df)}

# This loop fits monthly models (1:12):
try(for(j in unique(day$MONTH)){
    print(j)
    iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = day[which(day$MONTH == j),]) 
    
    y3 <- try(nee.day(day[which(day$MONTH == j),]), silent=T) # Fit the day model
    
    iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = day[which(day$MONTH == j),]) 
    
    y4 <- try(nee.night(night[which(night$MONTH == j),]), silent=T) # Fit night model
    
    try(parms.Month[c(parms.Month$MONTH == j ), 2:11 ] <- cbind(y3,y4), silent=T)
    
    rm( y3, y4)
  }, silent=T)


library(ggplot2)

summary(as.numeric(parms.NEE$a1))
summary(as.numeric(parms.NEE$ax))
summary(as.numeric(parms.NEE$r))

summary(as.numeric(parms.NEE$a))
summary(as.numeric(parms.NEE$b))

plot(parms.Month$MONTH, parms.Month$ax)
plot(parms.Month$MONTH, parms.Month$a1)
plot(parms.Month$MONTH, parms.Month$r)

plot(parms.Month$MONTH, parms.Month$a)
plot(parms.Month$MONTH, parms.Month$b)

#__________________________________________________________________________________________
#__________________________________________________________________________________________
#__________________________________________________________________________________________

# Emax in kg C/ day/MJ
((((min( parms.Month$ax, na.rm = T)/4.7)*0.000001)*-1)*86400)/1000

#__________________________________________________________________________________________
#__________________________________________________________________________________________

#__________________________________________________________________________________________
#__________________________________________________________________________________________

# Bootstrapping in R
library(nlstools)

# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "Month"
boot.NEE$a1.est <- 0
boot.NEE$ax.est<- 0
boot.NEE$r.est<- 0
boot.NEE$a1.se<- 0
boot.NEE$ax.se<- 0
boot.NEE$r.se<- 0
boot.NEE$a.est<- 0
boot.NEE$b.est<- 0
boot.NEE$a.se<- 0
boot.NEE$b.se<- 0

# Day Model:
for ( j in unique(boot.NEE$Month)){
  print("Annual Model")
  print(j)
  y1 <-day[which(day$MONTH == j),]
  
  iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = y1) 
  
  day.fit <- nls( NEE ~ (a1 * PAR * ax)/(a1 * PAR + ax) + r, data=y1, 
                      start=list(a1= iv$a1 , ax= iv$ax, r= iv$r),
                      na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  
  results <- nlsBoot(day.fit, niter=100 )
  a <- t(results$estiboot)[1, 1:3]
   names(a) <- c('a1.est', 'ax.est', 'r.est')
  b <- t(results$estiboot)[2, 1:3]
  names(b) <- c('a1.se', 'ax.se', 'r.se')
  c <- t(data.frame(c(a,b)))
  boot.NEE[c(boot.NEE$Month == j), 2:7] <- c[1, 1:6]
  rm(day.fit, a, b, c, results, y1)
}

# Night Models

for ( j in unique(boot.NEE$Month)){

  print(j)
  
  y1 <-night[which(night$MONTH == j),]
  
  iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = y1)
  
  nee.night <- nls(NEE ~ a * exp(b*TA),data=y1
                       , start=list(a= iv$a , b=iv$b ),
                       na.action=na.exclude, trace=F, control=nls.control(warnOnly=T))
  summary(nee.night)
  results <- nlsBoot( nee.night, niter=10 )
  a <- t(results$estiboot)[1, 1:2]
  names(a) <- c('a.est', 'b.est')
  b <- t(results$estiboot)[2, 1:2]
  names(b) <- c('a.se', 'b.se')
  c <- t(data.frame(c(a,b)))
  
  boot.NEE[c(boot.NEE$Month == j), 8:11] <- c[1, 1:4]
  rm(day.fit, a, b, c, results, y1)
  }

#__________________________________________________________________________________________
#__________________________________________________________________________________________
# Barplots showing SE:

#__________________________________________________________________________________________
#__________________________________________________________________________________________



plot(harv$PAR[harv$YEAR == 2016], harv$NEE[harv$YEAR == 2016], cex=0.5)

parms$MONTH <- as.factor(parms$MONTH)
parms$MONTH2<-factor(parms$MONTH,levels=levels(parms$MONTH)[c(1,2,3,4,5,6,7,8,9,10,11,12)])
levels( parms$MONTH2) <- c(1,2,3,4,5,6,7,8,9,10,11,12)

ggplot(data=parms, aes(x=MONTH2, y=ax),na.rm=TRUE) +
  geom_boxplot() + theme_minimal() + ylab("something")

