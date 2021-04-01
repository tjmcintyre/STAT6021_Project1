#EDA for project

diamonds= read.csv("diamonds4.csv")
diamonds = data.frame(diamonds)
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

par(mfrow=c(1,1))
plot(diamonds$carat,diamonds$price, main = "Price against Carat")

##these plots indicate that price and carat may not follow a linear pattern

#################################### CUT PLOT ###########################################

par(mfrow=c(1,1))

a1=subset(diamonds, cut == "Very Good")
a2=subset(diamonds, cut == "Good")
a3=subset(diamonds, cut == "Ideal")
a4=subset(diamonds, cut== 'Astor Ideal')

reg1<-lm(price~carat,data=a1)
reg2<-lm(price~carat,data=a2)
reg3<-lm(price~carat,data=a3)
reg4<-lm(price~carat,data=a4)

par(mfrow=c(1,1))

plot(a1$carat, a1$price, main="Price against Carats, by cut score")
points(a2$carat, a2$price, pch=2, col='red')
points(a3$carat, a3$price, pch=3, col='blue')
points(a4$carat, a4$price, pch=4, col='green')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')

legend("topleft", c("Very Good","Good","Ideal", 'Astor Ideal'), lty=c(1,2,3,4), pch=c(1,2,3,4), col=c("black","red","blue", 'green')) 

#viewing this plot we see that all the slopes by cut type seem to be pretty similar, thus we will not worry much of an interaction here

################################ Cut Plot End ###########################################


################################ Clarity Plot ###########################################
unique(diamonds$clarity)

b1=subset(diamonds, clarity == 'VVS2')
b2=subset(diamonds, clarity == 'VS2')
b3=subset(diamonds, clarity == 'IF')
b4=subset(diamonds, clarity == 'VVS1')
b5=subset(diamonds, clarity == 'VS1')
b6=subset(diamonds, clarity == 'SI1')
b7=subset(diamonds, clarity == 'SI2')
b8=subset(diamonds, clarity == 'FL')

reg1<-lm(price~carat,data=b1)
reg2<-lm(price~carat,data=b2)
reg3<-lm(price~carat,data=b3)
reg4<-lm(price~carat,data=b4)
reg5<-lm(price~carat,data=b5)
reg6<-lm(price~carat,data=b6)
reg7<-lm(price~carat,data=b7)
reg8<-lm(price~carat,data=b8)

plot(b1$carat, b1$price, main="Price against Carats, by clarity score")
points(b2$carat, b2$price, pch=2, col='red')
points(b3$carat, b3$price, pch=3, col='blue')
points(b4$carat, b4$price, pch=4, col='green')
points(b5$carat, b5$price, pch=5, col='orange')
points(b6$carat, b6$price, pch=6, col='purple')
points(b7$carat, b7$price, pch=7, col='gray')
points(b8$carat, b8$price, pch=8, col='pink')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')
abline(reg8,lty=8, col="pink") 

legend("topleft", 
       c("VVS2","Vs2","IF", 'VVS1','VS1','SI1','SI2','FL'),
       lty=c(1,2,3,4,5,6,7,8),
       pch=c(1,2,3,4,5,6,7,8), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray','pink')) 



############################# Clarity Plot End ####################################################

unique(diamonds$color)

c1=subset(diamonds, color == 'G')
c2=subset(diamonds, color == 'H')
c3=subset(diamonds, color == 'F')
c4=subset(diamonds, color == 'J')
c5=subset(diamonds, color == 'E')
c6=subset(diamonds, color == 'D')
c7=subset(diamonds, color == 'I')

reg1<-lm(price~carat,data=c1)
reg2<-lm(price~carat,data=c2)
reg3<-lm(price~carat,data=c3)
reg4<-lm(price~carat,data=c4)
reg5<-lm(price~carat,data=c5)
reg6<-lm(price~carat,data=c6)
reg7<-lm(price~carat,data=c7)

plot(c1$carat, c1$price, main="Price against Carats, by color")
points(c2$carat, c2$price, pch=2, col='red')
points(c3$carat, c3$price, pch=3, col='blue')
points(c4$carat, c4$price, pch=4, col='green')
points(c5$carat, c5$price, pch=5, col='orange')
points(c6$carat, c6$price, pch=6, col='purple')
points(c7$carat, c7$price, pch=7, col='gray')

abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="blue")
abline(reg4,lty=4, col='green')
abline(reg5,lty=5, col="orange") 
abline(reg6,lty=6, col="purple")
abline(reg7,lty=7, col='gray')

legend("topleft", 
       c("G","H","F", 'J','E','D','I'),
       lty=c(1,2,3,4,5,6,7),
       pch=c(1,2,3,4,5,6,7), 
       col=c("black","red","blue", 'green', 'orange', 'purple', 'gray')) 

#### Viewing these plots it is interesting to see that there may be an interaction between carat and color, and carat and clarity from the
#### past plots. Maybe we will look into these variable
