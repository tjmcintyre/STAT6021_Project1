#EDA for project

diamonds= read.csv("diamonds4.csv")

head(diamonds)
#5 columns 2 quantitiative 3 categorical

unique(diamonds$cut)
#cut is made of very good, good, ideal,Astor Ideal

unique(diamonds$clarity)
#clarity is made of vvs2, vs2, if, vvs1, vs1,si1,si2,fl

unique(diamonds$color)

#color is made of g, h, f, j, e, d, i

max(diamonds$price)
min(diamonds$price)
median(diamonds$price)
mean(diamonds$price)

max(diamonds$carat)
min(diamonds$carat)
median(diamonds$carat)
mean(diamonds$carat)

par(mfrow=c(2,2))
plot(diamonds$carat,diamonds$price)
plot(diamonds$carat,log(diamonds$price))
plot(log(diamonds$carat),log(diamonds$price))


a1=subset(diamonds, cut == "Very Good")
a2=subset(diamonds, cut == "Good")
a3=subset(diamonds, cut == "Ideal")
a4= subset(diamonds, cut== 'Astor Ideal')

reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

par(mfrow=c(1,1))

plot(a1$carat, a1$price, main="Price against Carats")
points(a2$carat, a2$price, pch=2, col='red')
points(a3$carat, a3$price, pch=3, col='blue')
points(a4$carat, a4$price, pch=4, col='green')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')

legend("topleft", c("Very Good","Good","Ideal", 'Astor Ideal'), lty=c(1,2,3,4), pch=c(1,2,3,4), col=c("black","red","blue", 'green')) 

diamonds = subset(diamonds, price<= 5000)

unique(diamonds$clarity)

par(mfrow=c(1,1))

b1=subset(diamonds, clarity == 'VVS2')
b2=subset(diamonds, clarity == 'VS2')
b3=subset(diamonds, clarity == 'IF')
b4=subset(diamonds, clarity == 'VVS1')
b5=subset(diamonds, clarity == 'VS1')
b6=subset(diamonds, clarity == 'SI1')
b7=subset(diamonds, clarity == 'SI2')
#b8=subset(diamonds, clarity == 'FL')

reg1<-lm(price~carat,data=b1)
reg2<-lm(price~carat,data=b2)
reg3<-lm(price~carat,data=b3)
reg4<-lm(price~carat,data=b4)
reg5<-lm(price~carat,data=b5)
reg6<-lm(price~carat,data=b6)
reg7<-lm(price~carat,data=b7)
#reg8<-lm(price~carat,data=b8)

plot(b1$carat, b1$price, main="Price against Carats")
points(b2$carat, b2$price, pch=2, col='red')
points(b3$carat, b3$price, pch=3, col='blue')
points(b4$carat, b4$price, pch=4, col='green')
points(b5$carat, b5$price, pch=5, col='orange')
points(b6$carat, b6$price, pch=6, col='purple')
points(b7$carat, b7$price, pch=7, col='gray')
#points(b8$carat, b8$price, pch=8, col='pink')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')
#abline(reg8,lty=8, col="pink") 

legend("topleft", 
       c("VVS2","Vs2","IF", 'VVS1','VS1','SI1','SI2','FL'),
       lty=c(1,2,3,4,5,6,7,8),
       pch=c(1,2,3,4,5,6,7,8), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray','pink')) 

