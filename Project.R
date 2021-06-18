setwd("C:/Users/YOONSU/OneDrive/바탕 화면/3-1/다변량 자료분석")
exercise<-read.csv('exercise.csv')
str(exercise)

#change columns name
names(exercise)<-c("area","age","sex","height","weight","BMI","fat","waist","x1","x2","x3","x4","x5")

# convert data type of age to factor
for (i in 1:length(exercise$age)) {
  if (exercise$age[i] >= 19 & exercise$age[i] <=29) {
    exercise$age[i] <- 20 
  }
  else if (exercise$age[i] >= 30 & exercise$age[i] <=39) {
    exercise$age[i] <- 30
  }
  else if (exercise$age[i] >= 40 & exercise$age[i] <=49) {
    exercise$age[i] <- 40
  }
  else if (exercise$age[i] >= 50 & exercise$age[i] <=59) {
    exercise$age[i] <- 50
  }
  else if (exercise$age[i] >= 60 & exercise$age[i] <=64) {
    exercise$age[i] <- 60
  }
  else {0}
}

#산점도 행렬
pairs(~height+weight+BMI+fat+waist+x1+x2+x3+x4+x5,exercise,lower.panel=panel.smooth, main="산점도 행렬")

# histogram
x11()
par(mfrow=c(1,5))
hist(exercise$x1,main='x1',xlab='x1',freq=F)
lines(density(exercise$x1))
hist(exercise$x2,main='x2',xlab='x2',freq=F)
lines(density(exercise$x2))
hist(exercise$x3,main='x3',xlab='x3',freq=F)
lines(density(exercise$x3))
hist(exercise$x4,main='x4',xlab='x4',freq=F)
lines(density(exercise$x4))
hist(exercise$x5,main='x5',xlab='x5
',freq=F)
lines(density(exercise$x5))

# x3 => sqrt 변환
# x5 => log 변환
# transformation
exercise$x3 = sqrt(exercise$x3)
exercise$x5 = log(exercise$x5)
x11(); par(mfrow=c(1,2))
hist(exercise$x3,freq=F,main="sqrt transform",xlab="x3")
lines(density(exercise$x3))
hist(exercise$x5,freq=F,main="log transform",xlab="x5")
lines(density(exercise$x5))

# ANOVA
exercise$age<-as.factor(exercise$age)
exercise$area<-as.factor(exercise$area)
aov1<-aov(exercise$fat~exercise$age*exercise$area)
summary(aov1)
x11();boxplot(exercise$fat~exercise$age*exercise$area,
              col=c("red","orange","yellow","green","blue"),
              ylab="체지방률",
              xlab="나이.지역",
              main="나이와 지역이 체지방률에 미치는 상호작용효과")

#after eliminate interaction
aov.1<-aov(exercise$fat~exercise$age+exercise$area)
summary(aov.1)
model.tables(aov.1,type="means")

x11()
boxplot(fat~age,exercise,
        ylab="체지방률",
        xlab="나이",
        main="나이의 주효과")
boxplot(fat~area,exercise,
        ylab="체지방률",
        xlab="지역",
        main="지역의 주효과")

# Categorize body fat rates by gender before manova
exercise<-within(exercise,{
  fat<-ifelse(sex==1&fat<=17,1,
              ifelse(sex==1&fat>17&fat<=24,2,
                     ifelse(sex==1&fat>24,3,
                            ifelse(sex==2&fat<=24,1,
                                   ifelse(sex==2&fat>24&fat<=31,2,
                                          ifelse(sex==2&fat>31,3,0))))))
})  
table(exercise$fat)

# MANOVA
exercise$fat <- as.factor(exercise$fat)
x = cbind(exercise[,9:13])
x = as.matrix(x)
mfit<-manova(x~fat+age+fat:age,data=exercise)
summary(mfit)

# PCA
library("corrplot")
exercise$area<-as.numeric(exercise$area)
exercise$age<-as.numeric(exercise$age)

cor(exercise[,-7]) 
x=cor(exercise[,-7])
x11(); corrplot(x)


pc=prcomp(~height+weight+BMI+waist+x1+x2+x3+x4+x5, data=exercise, scale=TRUE)
summary(pc)
pc$sdev^2
screeplot(pc, type='l', npcs=9)
print(pc)

# biplot
x11()
par(mfrow=c(1,3))
biplot(pc, choices=c(1,2), main='PC1~PC2')
abline(h=0,v=0)
biplot(pc, choices=c(1,3), main='PC1~PC3')
abline(h=0,v=0)
biplot(pc, choices=c(2,3), main='PC2~PC3')
abline(h=0,v=0)