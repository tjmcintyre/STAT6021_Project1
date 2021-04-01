'''
STATS 6021
Project 1
'''

library(MASS) 
library(dplyr)

diamonds_data<-read.csv("diamonds4.csv", header=TRUE)


summary(diamonds_data)
attach(diamonds_data) 

head(diamonds_data)

### EDA

is.factor(clarity)
is.factor(color)
is.factor(cut)

clarity <- factor(clarity, levels = c('SI2', 'SI1', 'VS2', 'VS1', 'VVS2', 'VVS1', 'IF', 'FL'))
color <- factor(color, levels = c('J', 'I', 'H', 'G', 'F', 'E', 'D'))
cut <- factor(cut, levels = c('Good', 'Very Good', 'Ideal', 'Astor Ideal'))

levels(clarity) 
levels(color)
levels(cut)

is.numeric(carat)
is.numeric(price)

### run regression

r_full <- lm(price~., data = diamonds_data)
summary(r_full)
anova(r_full)

count(diamonds_data, c(cut))
count(diamonds_data, c(clarity))
count(diamonds_data, c(color))

### subsets

p_cut <- c('price', 'cut')
p_color <- diamonds_data[c('price', 'color')]
p_clarity <- diamonds_data[c('price', 'clarity')]

## cut
AI<-subset(diamonds_data,cut=="Astor Ideal") 
I<-subset(diamonds_data,cut=="Ideal") 
G<-subset(diamonds_data,cut=="Good") 
VG<-subset(diamonds_data,cut=="Very Good") 


price_AI <- lm(price~carat,data=AI)
price_I <- lm(price~carat,data=I)
price_G <- lm(price~carat,data=G)
price_VG <- lm(price~carat,data=VG)

# summary(price_AI)

### scatterplots

plot(carat, price, main="Price by Carat and Cut")
points(I$carat, I$price, pch=2, col="blue")
points(G$carat, G$price, pch=3, col="orange")
points(VG$carat, VG$price, pch=4, col="red")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_G,lty=3, col="orange")
abline(price_VG,lty=4, col="red")
legend("topleft", c("Astor Ideal", "Ideal", "Good", "Very Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,12), col=c("black", "blue", "orange", "red")) 


### linear relationship just b/w carat and price

price_carat <- lm(price~carat)
summary(price_carat)

plot(price~carat, main = "Price by Carat")
abline(price_carat, col = 'red')
grid()

plot(price_carat$fitted.values, price_carat$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

### run box cox 
boxcox(price_carat, lambda = seq(.27, .33, by = 0.01)) 
grid()

### transform price by lambda = 3/10 to fix variance

diamonds_data <- mutate(diamonds_data, pt3_price = (price)^(3/10))

head(diamonds_data)

attach(diamonds_data)

t_price_mod <- lm(pt3_price~carat)

summary(t_price_mod)

plot(pt3_price~carat, main = "Price by Carat")
abline(t_price_mod, col = 'red')
grid()

plot(t_price_mod$fitted.values, t_price_mod$residuals, main = 'Residual Plot') # residual plot
abline(h=0,col="red")
grid()

boxcox(t_price_mod, lambda = seq(.8, 1.2, by = 0.1)) # Box-Cox plot
grid()


### categorize cat variables

colors()[1:50]

# color:
D <-subset(data_t,color=="E") 
E <-subset(data_t,color=="E") 
eF<-subset(data_t,color=="F") # f is a reserved variable
G<-subset(data_t,color=="G") 
H<-subset(data_t,color=="H") 
I<-subset(data_t,color=="I") 
J<-subset(data_t,color=="J") 

# fit separate regressions
price_D <- lm(log_t_price~log_carat,data=D)
price_E <- lm(log_t_price~log_carat,data=E)
price_eF <- lm(log_t_price~log_carat,data=eF)
price_G <- lm(log_t_price~log_carat,data=G)
price_H <- lm(log_t_price~log_carat,data=H)
price_I <- lm(log_t_price~log_carat,data=I)
price_J <- lm(log_t_price~log_carat,data=J)

plot(log_carat, log_t_price, main="Price by Carat and Color")
points(E$log_carat, E$log_t_price, pch=2, col="brown1")
points(eF$log_carat, eF$log_t_price, pch=3, col="blue")
points(G$log_carat, G$log_t_price, pch=4, col="orange")
points(H$log_carat, H$log_t_price, pch=5, col="red")
points(I$log_carat, I$log_t_price, pch=6, col="bisque1")
points(J$log_carat, J$log_t_price, pch=7, col="chartreuse")

abline(price_D,lty=1, col = "black")
abline(price_E,lty=2, col = "chartreuse")
abline(price_eF,lty=3, col="blue") 
abline(price_G,lty=4, col="orange")
abline(price_H,lty=5, col="red")
abline(price_I,lty=6, col="bisque1")
abline(price_J,lty=7, col="aquamarine")


legend("topleft", c("D", "E", "F", "G", "H", "I", "J"), lty=c(1, 2, 3, 4, 5, 6, 7), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "chartreuse", "blue", "orange", "red", "bisque1", "aquamarine")) 

# clarity:
FL <-subset(data_t,clarity=="FL") 
IF <-subset(data_t,clarity=="IF") 
SI1 <-subset(data_t,clarity=="SI1") 
SI2 <-subset(data_t,clarity=="SI2") 
VS1 <-subset(data_t,clarity=="VS1") 
VS2 <-subset(data_t,clarity=="VS2") 
VVS1 <-subset(data_t,clarity=="VVS1") 
VVS2 <-subset(data_t,clarity=="VVS2") 

# fit separate regressions
price_FL <- lm(log_t_price~log_carat,data=FL)
price_IF <- lm(log_t_price~log_carat,data=IF)
price_SI1 <- lm(log_t_price~log_carat,data=SI1)
price_SI2 <- lm(log_t_price~log_carat,data=SI2)
price_VS1 <- lm(log_t_price~log_carat,data=VS1)
price_VS2 <- lm(log_t_price~log_carat,data=VS2)
price_VVS1 <- lm(log_t_price~log_carat,data=VVS1)
price_VVS2 <- lm(log_t_price~log_carat,data=VVS2)

plot(log_carat, log_t_price, main="Price by Carat and Clarity")
points(IF$log_carat, IF$log_t_price, pch=2, col="chartreuse")
points(SI1$log_carat, SI1$log_t_price, pch=3, col="blue")
points(SI2$log_carat, SI2$log_t_price, pch=4, col="orange")
points(VS1$log_carat, VS1$log_t_price, pch=5, col="red")
points(VS2$log_carat, VS2$log_t_price, pch=6, col="bisque1")
points(VVS1$log_carat, VVS1$log_t_price, pch=7, col="aquamarine")
points(VVS2$log_carat, VVS2$log_t_price, pch=8, col="burlywood")

abline(price_FL,lty=1, col = "black")
abline(price_IF,lty=2, col = "chartreuse")
abline(price_SI1,lty=3, col="blue") 
abline(price_SI2,lty=4, col="orange")
abline(price_VS1,lty=5, col="red")
abline(price_VS2,lty=6, col="bisque1")
abline(price_VVS1,lty=7, col="aquamarine")
abline(price_VVS2,lty=8, col="burlywood")


legend("topleft", c("FL", "IF", "SI1", "SI2", "VS1", "VS2", "VVS1", "VVS2"), lty=c(1, 2, 3, 4, 5, 6, 7, 8), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "chartreuse", "blue", "orange", "red", "bisque1", "aquamarine", "burlywood")) 