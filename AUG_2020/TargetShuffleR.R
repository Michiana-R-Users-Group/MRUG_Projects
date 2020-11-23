# Read in real estate data for regression
myData <- read.csv("HousingPrices.csv",header=T,sep=",",quote="\"",dec=".",fill=T)

attach(myData)
#myReg <- lm(Price ~ SqrFoot + Age, data = myData)
myReg <- lm(Price ~ Age, data = myData)
myR2 <- summary(myReg)$r.squared

n <- 100000
CoefDet <- dim(n)
for(i in 1:n){
  tempPrice <- sample(myData$Price)
  #tempReg <- lm(tempPrice ~ SqrFoot + Age, data = myData)
  tempReg <- lm(tempPrice ~ Age, data = myData)
  CoefDet[i] <- summary(tempReg)$r.squared
}
quantile(CoefDet,.99)
max(CoefDet)
myR2
length(CoefDet[CoefDet>myR2])
