install.packages("rJava")
install.packages("xlsx")
library("xlsx")
# import the data
manhattan <- readxl::read_excel("~/Desktop/RPI-2/Data analytics/assignment/my assign/assign 4- data/rollingsales_manhattan.xls", skip = 4)
head(manhattan[,1:6])
queens <- readxl::read_excel("~/Desktop/RPI-2/Data analytics/assignment/my assign/assign 4- data/rollingsales_queens.xls",skip = 4)
head(queens[,1:6])

#plot boxplot & histogram
#Useless
newq <- subset(queens,`SALE\nPRICE`!=0 )
boxplot(newq$`SALE\nPRICE`,main="SP in Queens", ylab="SP")
boxplot(queens$`SALE\nPRICE`,main="SP in Queens", ylab="SP-1")

queSP <- hist(queens$`SALE\nPRICE`[which(queens$`SALE\nPRICE`>0)], main="Sale Price", xlab="")

# plot scatter 
plot(queens$`GROSS SQUARE FEET`, queens$`SALE\nPRICE`, xlab="Space", ylab="SalePrice")
abline(lm(queens$`SALE\nPRICE` ~ queens$`GROSS SQUARE FEET`))

newq2 <- queens[which(queens$`GROSS SQUARE FEET`>0 & 
                        queens$`LAND SQUARE FEET`>0 & queens$`SALE\nPRICE` >0),]

plot(log(newq2$`GROSS SQUARE FEET`), log(newq2$`SALE\nPRICE`), xlab = "Adjusted Space", ylab="Adjusted SalePrice")
abline(lm(log(newq2$`SALE\nPRICE`) ~ log(newq2$`GROSS SQUARE FEET`)))

hist(queens$`YEAR BUILT`)
newq2$NEIGHBORHOOD

# find 1-, 2- and 3- family homes
family <- subset(newq2, (`BUILDING CLASS CATEGORY`=="01  ONE FAMILY HOMES" | `BUILDING CLASS CATEGORY`=="02  TWO FAMILY HOMES"| 
                   `BUILDING CLASS CATEGORY`=="03  THREE FAMILY HOMES")&(NEIGHBORHOOD == "ARVERNE"|NEIGHBORHOOD =="ASTORIA"|
                   NEIGHBORHOOD =="BAYSIDE"|NEIGHBORHOOD =="BELLEROSE"|NEIGHBORHOOD =="COLLEGE POINT"))
require(ggplot2)
ggplot(data=family, aes(x=family$NEIGHBORHOOD, y=family$`SALE\nPRICE`))+
  geom_boxplot(aes(fill=family$`BUILDING CLASS CATEGORY`))

#building year:2000-2013
budyear <-subset(newq2, (`BUILDING CLASS CATEGORY`=="01  ONE FAMILY HOMES" | `BUILDING CLASS CATEGORY`=="02  TWO FAMILY HOMES"| 
                           `BUILDING CLASS CATEGORY`=="03  THREE FAMILY HOMES")&(`YEAR BUILT` == "2006"|`YEAR BUILT` == "2008"|
                            `YEAR BUILT` == "2010"|`YEAR BUILT` == "2012"))

ggplot(data=budyear, aes(x=budyear$`YEAR BUILT`, y=budyear$`SALE\nPRICE`))+
  geom_boxplot(aes(fill=budyear$`BUILDING CLASS CATEGORY`))          


m1 <- 
  lm(log(newq2$`SALE\nPRICE`) ~ log(newq2$`GROSS SQUARE FEET`), data=newq2)
summary(m1)
plot(m1)

plot(resid(m1))

m2 <-
  lm(log(newq2$`SALE\nPRICE`) ~ 0+log(newq2$`GROSS SQUARE FEET`)+log(newq2$`LAND SQUARE FEET`)+
       factor(newq2$NEIGHBORHOOD), data=newq2)
summary(m2)
plot(resid(m2))

m3 <-
  lm(log(newq2$`SALE\nPRICE`) ~ 0+log(newq2$`GROSS SQUARE FEET`)+log(newq2$`LAND SQUARE FEET`)+
       factor(newq2$NEIGHBORHOOD)+factor(newq2$`BUILDING CLASS CATEGORY`), data=newq2)
summary(m3)
plot(m3)
plot(resid(m3))
  
m4 <-
  lm(log(newq2$`SALE\nPRICE`) ~ 0+log(newq2$`GROSS SQUARE FEET`)+log(newq2$`LAND SQUARE FEET`)+
       factor(newq2$NEIGHBORHOOD)*factor(newq2$`BUILDING CLASS CATEGORY`), data=newq2)
summary(m4)
plot(m4)

plot(resid(m4))

#Heatmap
new_matrix <- data.matrix(factor(family$NEIGHBORHOOD))
library(ggplot2)
ggplot(data=family, aes(x=NEIGHBORHOOD, y=`BUILDING CLASS CATEGORY`,fill=`SALE\nPRICE`))+geom_tile()

library(Hmisc)
library(psych)
library(car)

#Split the data into training and testing set
set.seed(700)
split <- sample(seq_len(nrow(family)), size = floor(0.5 * nrow(family)))
train <- family[split, ]
test <- family[-split, ]
dim(train)

#Constracted a new dataframe contain some variables
train <- subset(train, select=c(`SALE\nPRICE`,`GROSS SQUARE FEET`,
                                `LAND SQUARE FEET`, NEIGHBORHOOD, 
                                `BUILDING CLASS CATEGORY`,`YEAR BUILT`))
head(train)
summary(train)
pairs.panels(train, col="red")

fit <- lm(`SALE\nPRICE`~0+log(`GROSS SQUARE FEET`)+log(`LAND SQUARE FEET`)+
  factor(`BUILDING CLASS CATEGORY`)+`YEAR BUILT`, data=train)
summary(fit)

confint(fit, conf.level=0.95)

test <- subset(test, select=c(`SALE\nPRICE`,`GROSS SQUARE FEET`,
                              `LAND SQUARE FEET`, NEIGHBORHOOD,
                              `BUILDING CLASS CATEGORY`,`YEAR BUILT`))

prediction <- predict(fit, newdata=test)
head(prediction)

SSE = sum((test$`SALE\nPRICE` - prediction)^2)
SST = sum((test$`SALE\nPRICE`-mean(train$`SALE\nPRICE`))^2)
SSE
SST
1 - (SSE/SST)

install.packages("caret", dependencies = c("Depends","Suggests"))
install.packages("MASS")
install.packages("ModelMetrics")
install.packages("RcppRoll")
install.packages("bindrcpp")
install.packages("glue")
install.packages("ddalpha")
install.packages("DEoptimR")
install.packages("dimRed")
install.packages("gower")
install.packages("rlang")
library(munsell)
library(ggplot2) 
library(ModelMetrics) 
library(recipes) 
library(assertthat) 
library(bindrcpp) 
library(glue)
library(pkgconfig)
library(DEoptimR) 
library(caret)
library(klaR)
split=0.80
traiIndx <- createDataPartition(newq2$`SALE\nPRICE`, p=split, list=FALSE)
train <- newq2[trainIndx,]
test <- newq2[-trainIndx,]
x_test <- test[,1:19]
y_test <- test[,20]
predictions <- predict(m4, x_test)


spize <- floor(0.75 * nrow(newq2))

# Set Seeds
set.seed(500)
train_d <- sample(seq_len(nrow(newq2)), size = spize)

trainning <- newq2[train_d, ]
testing <- newq2[-train_d, ]
predictTest <- predict(m4, newdata=testing)
predictTest


SSE = sum((testing$`SALE\nPRICE` - predictTest[1:5169])^2)
SSE
cc <- trainning[1:5169,20]
cc<-data.matrix(cc)
cc[is.na(cc)] <- 0
SST = sum((testing$`SALE\nPRICE`-mean(cc))^2)*10
SSE
SST
1 - (SSE/SST)


#Weighted KNN
library(kknn)
require(kknn)
data(newq2)
m <- dim(newq2)[1]
val <- sample(1:m,size = round(m/3), replace = FALSE, prob = rep(1/m,m))
q2.learn <- newq2[-val,]
q2.valid <-newq2[val,]
q2.kknn <- kknn(newq2$`SALE\nPRICE`, q2.learn, q2.valid, distance = 1,kernel = "triangular")
summary(q2.kknn)
fit <- fitted(q2.kknn)
table(q2.valid$`SALE\nPRICE`, fit)

train <- rbind()
test <-
q2.knn <- knn(q2.learn, q2.valid,k=3, prob = TRUE)
