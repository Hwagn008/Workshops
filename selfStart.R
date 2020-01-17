# FIT LRC Self Start:

# The model for the Light Response Curve:
lrcModel <- function(PAR, a1, ax, r) {
  y=(a1 * PAR * ax)/(a1 * PAR + ax) + r
  return(y)
}

# Create a function to find initial values for the selfstart function:
lrc.int <- function (mCall, LHS, data){
  x <- data$PAR
  y <- data$NEE
  
  r <- max(na.omit(y), na.rm=T)
  ax <- min(na.omit(y), na.rm=T)
  a1 <- (r + ax)/2
  
  a1[a1 > 0]<- -0.1
  r[r>50] <- ax*-1
  r[r < 0] <- 1
  
  value = list(a1, ax, r)
  names(value) <- mCall[c("a1", "ax", "r")]
  return(value)
}

# Selfstart Function
SS.lrc <- selfStart(model=lrcModel,initial= lrc.int)

# Find initial values:
iv <- getInitial(NEE ~ SS.lrc('PAR', "a1", "ax", "r"), data = data) 


# FIT TRC Self Start:

# The model for the Light Response Curve:
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

# Find initial values:
iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = data) 



library(dplyr)

SUM <-night %>%
  group_by(MONTH) %>%
  summarize(
    NEE.max = max(na.omit(NEE)),
    NEE.min = min(na.omit(NEE)),
    TA.max = max(na.omit(TA)),
    TA.min = min(na.omit(TA)))
    
d <- merge(parms.Month[c("MONTH", "a", "b")], SUM)
d$diff <- d$TA.max - d$TA.min

plot(d$TA.min, d$a)
plot(d$TA.min, d$a)
plot(d$TA.min, d$b)
plot(d$TA.max, d$b)
plot(d$diff, d$a)
plot(d$diff, d$b)




plot(NEE ~ TA, data = night[which(night$MONTH == 7),])
dataframe <- night[which(night$MONTH == 7),]

nls(NEE ~ a * exp(b*TA), dataframe, start=list(a= 1 , b=0.01 ),
    na.action=na.exclude, trace=F,control=nls.control(warnOnly=T))

