library(smoof)
library(GA)
Alpine1d <- makeAlpine01Function(c(1))
Alpine2d <- makeAlpine01Function(c(2))
Alpine10d <- makeAlpine01Function(c(10))
Alpine20d <- makeAlpine01Function(c(20))
Alpine2d(c(1,2))

rosenGrok1d<-makeRosenbrockFunction(1)
rosenGrok2d<-makeRosenbrockFunction(c(2))
rosenGrok10d<-makeRosenbrockFunction(c(10))
rosenGrok20d<-makeRosenbrockFunction(c(20))
rosenGrok2d(c(5,2))

PRS <- function(lbound, upbound, ourRange, ourFunction, size) {
  minVal=100000000000000000000000000000000000000000;
  for (i in ourRange) {
    x<-runif(size, lbound, upbound)
    newVal = ourFunction(x)
    if(newVal<minVal){
      minVal = newVal
    }
  }
  return(minVal)
}

runComparison<-function(lbound, upbound, functionToUse, dimenstion){
  prsVal<-replicate(50,PRS(lbound, upbound, 1:1000, functionToUse, dimenstion))
  hist(prsVal, title="Alipne01 2d", main=paste("Histogram of PRS in ", dimenstion, "d", sep = ""), xlab="Minimum values", breaks=10)
  boxplot(prsVal, title="Alipne01 2d", main=paste("Boxplot of PRS in ", dimenstion, "d", sep = ""))
  cat("PRS:",mean(prsVal), "\n")
  
  gaVal<-replicate(50,(-1)*ga(type = "real-valued", fitness = function(x) -functionToUse(x), lower = c(th = lbound), upper = upbound, maxiter=100, monitor = FALSE)@fitnessValue)
  hist(gaVal, title="Alipne01 2d", main=paste("Histogram of GA in ", dimenstion, "d", sep = ""), xlab="Minimum values",breaks=10)
  boxplot(gaVal, title="Alipne01 2d", main=paste("Boxplot of GA in ", dimenstion, "d", sep = ""))
  cat("GA:", mean(gaVal), "\n")
  
  t.test(prsVal,gaVal)
}

firstHalf <- function(lbound2, upbound2, lbound10, upbound10, lbound20, upbound20) {
  cat('Alpine01 in 2d', "\n")
  runComparison(lbound2, upbound2, Alpine2d, 2)
  cat("\n")
  
  cat("Alpine01 in 10d", "\n")
  runComparison(lbound10, upbound10, Alpine10d, 10)
  cat("\n")
  
  cat("Alpine01 in 20d", "\n")
  runComparison(lbound20, upbound20, Alpine20d, 20)
  cat("\n")
  
  cat("Rosenbrock in 2d", "\n")
  runComparison(lbound2, upbound2, rosenGrok2d, 2)
  cat("\n")
  
  cat("Rosenbrock in 10d", "\n")
  runComparison(lbound10, upbound10, rosenGrok10d, 10)
  cat("\n")
  
  cat("Rosenbrock in 20d", "\n")
  runComparison(lbound20, upbound20, rosenGrok20d, 20)
  
}

#Values have to be between -5 and 10
lbound2 = c(0,0)
upbound2 = c(1,2)
lbound10 = c(0,0,1,2,3,4,5,6,7,8)
upbound10 = c(1,2,3,4,5,6,7,8,9,9)
lbound20 = c(0,0,1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,8,8,0)
upbound20 =c(1,2,3,4,5,6,7,8,9,10,2,3,4,5,6,7,8,9,9,9)

firstHalf(lbound2, upbound2, lbound10, upbound10, lbound20, upbound20)

getParamSet(Alpine20d)
getParamSet(rosenGrok20d)



