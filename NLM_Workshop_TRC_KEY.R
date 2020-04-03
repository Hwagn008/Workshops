library(nlstools)
# Set your working directory to the Malone NLM Workshop
rm(list=ls())

load("~/OneDrive - Florida International University/Teaching/Workshops/Workshops/NLM_Workshop.RData")

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
parms.Month <- data.frame(
  MONTH=numeric(),
  a=numeric(),
  b=numeric(), 
  a.pvalue=numeric(),
  b.pvalue=numeric(), stringsAsFactors=FALSE, row.names=NULL)


parms.Month[1:12, 1] <- seq(1,12,1) # Creates time file to merge with parm file:

#Functions:
nee.night <- function(dataframe){y.df = nls(NEE ~ a * exp(b*TA), 
                                            dataframe, start=list(a= iv$a , b=iv$b ),
                                            na.action=na.exclude, trace=F,
                                            control=nls.control(warnOnly=T))

y.df <- as.data.frame(cbind(t(coef(summary(y.df))[1:2, 1]), t(coef(summary(y.df)) [1:2, 4])))

names(y.df) <- c("a", "b", "a.pvalue", "b.pvalue")                      
return(y.df)}

# This loop fits monthly models (1:12):
try(for(j in unique(night$MONTH)){
  print(j)
  
  iv <- getInitial(NEE ~ SS.trc('TA', "a", "b"), data = night[which(night$MONTH == j),]) 
  
  y4 <- try(nee.night(night[which(night$MONTH == j),]), silent=T) # Fit night model
  
  try(parms.Month[c(parms.Month$MONTH == j ), 2:5 ] <- cbind(y4), silent=T)
  
  rm(y4)
}, silent=T)



# Create file to store parms and se
boot.NEE <- data.frame(parms.Month[, c("MONTH")]); names (boot.NEE) <- "MONTH"
boot.NEE$a.est<- 0
boot.NEE$b.est<- 0
boot.NEE$a.se<- 0
boot.NEE$b.se<- 0

# Night Model:
for ( j in unique(boot.NEE$MONTH)){
  print(j)
  y1 <-night[which(night$MONTH == j),]
  
  iv <- getInitial(NEE ~ SS.trc('TA',"a", "b"), data = y1) 
  
  night.fit <- nls(NEE ~ a * exp(b*TA), 
                   data=y1, start=list(a= iv$a , b=iv$b ),
                   na.action=na.exclude, trace=F,
                   control=nls.control(warnOnly=T))
  
  results <- nlsBoot(night.fit, niter=100 )
  a <- t(results$estiboot)[1, 1:2]
  names(a) <- c('a.est', 'b.est')
  b <- t(results$estiboot)[2, 1:2]
  names(b) <- c('a.se', 'b.se')
  c <- t(data.frame(c(a,b)))
  boot.NEE[c(boot.NEE$MONTH == j), 2:5] <- c[1, 1:4]
  rm(day.fit, a, b, c, results, y1)
}

trc <- merge( parms.Month, boot.NEE)

