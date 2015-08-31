sd(c(5,8,12))
which.min(c(4,1,6))
# Intro to R
WHO <- read.csv("~/Work/Learning/Analytical Edge/WHO.csv")
View(WHO)
names(WHO)
str(WHO)
summary(WHO)
WHO_Europe = subset(WHO,Region == "Europe")
write.csv(WHO_Europe,"WHO_Europe.csv")
rm(WHO_Europe)
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
attach(WHO)
plot(GNI,FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]
#
# Quick Questions
summary(Over60)
which.min(Over60)
Country[183]
summary(LiteracyRate)
which.max(LiteracyRate)
Country[44]
#
#
hist(CellularSubscribers)
boxplot(LifeExpectancy ~ Region,ylab="Life Expectancy",main="Life Exectancy of Countries by Region")
table(Region)
tapply(Over60,Region,mean)
tapply(LiteracyRate,Region,mean,na.rm=TRUE)
#
# Quick Question
tapply(ChildMortality, Region, mean, na.rm=TRUE)

#Recitation
USDA <- read.csv("USDA.csv")
View(USDA)
str(USDA)
summary(USDA)
attach(USDA)
Sodium
which.max(Sodium)
Description[265]
HighSodium = subset(USDA, Sodium > 10000)
View(HighSodium)
nrow(HighSodium)
HighSodium$Description
Sodium[match("CAVIAR",Description)]
Sodium[4154]
summary(Sodium)
sd(Sodium,na.rm=TRUE)
# plots
plot(Protein,TotalFat,xlab = "Protein", ylab = "Fat", main = "Protein vs. Fat")
hist(VitaminC, xlab = "VitaminC", main = "Histogram of VitaminC",xlim = c(0,100),breaks = 2000)
boxplot(Sugar, main = "Boxplot of Sugar Leve", ylab = "Sugar in Grams")
# new variables
Sodium[1] > mean(Sodium, na.rm = TRUE)
Sodium[50] > mean(Sodium, na.rm = TRUE)
HighSodium = Sodium > mean(Sodium,na.rm = TRUE)
str(HighSodium)
HighSodium = as.numeric(Sodium > mean(Sodium,na.rm = TRUE))
str(HighSodium)
USDA$HighSodium = as.numeric(Sodium > mean(Sodium,na.rm = TRUE))
str(USDA)
USDA$HighProtein = as.numeric(Protein > mean(Protein,na.rm = TRUE))
USDA$HighFat = as.numeric(TotalFat > mean(TotalFat,na.rm = TRUE))
USDA$HighCarb = as.numeric(Carbohydrate > mean(Carbohydrate,na.rm = TRUE))
str(USDA)
attach(USDA)
table(HighSodium)
table(HighFat)
names(USDA)
table(HighSodium,HighFat)
tapply(Iron,HighProtein,mean,na.rm=TRUE)
tapply(VitaminC,HighCarb,max,na.rm=TRUE)
tapply(VitaminC,HighCarb,summary,na.rm=TRUE)
