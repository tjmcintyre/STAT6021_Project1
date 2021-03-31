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

## I want to now see how many prices are over and under certain threshholds for price

thresh = 50000
under = subset(diamonds, price < thresh)
over = subset(diamonds, price >= thresh)

length(under$price)
length(over$price)


#1184 prices under 50k, 30 over 50k

#so lets set diamonds to under and revisit these prices against carat scatterplots

diamonds1 = under


#################################### CUT PLOT ###########################################

a1=subset(diamonds1, cut == "Very Good")
a2=subset(diamonds1, cut == "Good")
a3=subset(diamonds1, cut == "Ideal")
a4= subset(diamonds1, cut== 'Astor Ideal')

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

#Comparing the data plots now with price less than 50k, we can see astor ideal has the steepest slope of by cut score, however
#there are very few observations and all in the lower price ranges

################################ Cut Plot End ###########################################


################################ Clarity Plot ###########################################
unique(diamonds$clarity)

b1=subset(diamonds1, clarity == 'VVS2')
b2=subset(diamonds1, clarity == 'VS2')
b3=subset(diamonds1, clarity == 'IF')
b4=subset(diamonds1, clarity == 'VVS1')
b5=subset(diamonds1, clarity == 'VS1')
b6=subset(diamonds1, clarity == 'SI1')
b7=subset(diamonds1, clarity == 'SI2')
b8=subset(diamonds1, clarity == 'FL')

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

#This plot tells a similar story but now doesnt have the extreme outliers for FL, however there are only two 2 obs of FL now
#Three before hand

boxplot(diamonds$price~diamonds$color, main='Boxplots Price against Color (full set)')
boxplot(diamonds$price~diamonds$cut, main='Boxplots Price against Cut (full set)')
boxplot(diamonds$price~diamonds$clarity, main='Boxplots Price against Clarity(full set)')

#FL is an outlier however the rest seem pretty in line


############################# Clarity Plot End ####################################################

unique(diamonds$color)

c1=subset(diamonds1, color == 'G')
c2=subset(diamonds1, color == 'H')
c3=subset(diamonds1, color == 'F')
c4=subset(diamonds1, color == 'J')
c5=subset(diamonds1, color == 'E')
c6=subset(diamonds1, color == 'D')
c7=subset(diamonds1, color == 'I')

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

#### Viewing this plot now there are more parallel slopes of lines, however there still may be an interaction coming into play



plot(diamonds1$carat,diamonds1$price, main = "Price against Carat")

#removing the values greater than 50k make this plot appear less exponential as well.

#going to less than 50k removes 30 data points from the set of 1214

boxplot(diamonds1$price~diamonds1$color, main='Boxplots Price against Color (price less than 50k)')
boxplot(diamonds1$price~diamonds1$cut, main='Boxplots Price against Cut (price less than 50k)')
boxplot(diamonds1$price~diamonds1$clarity, main='Boxplots Price against Clarity(price less than 50k)')

#FL is an outlier, however the rest seem pretty similar mean wise


#Now i want to see 25k and less

thresh2= 25000

under2 = subset(diamonds, price < thresh2)
over2 = subset(diamonds, price >= thresh2)

length(under2$price)
length(over2$price)

#going down to under 25k loans loses 60 data points from the set of 1214

diamonds2=under2
plot(diamonds1$carat,diamonds1$price, main = "Price against Carat")
#removal of some of the outliers makes it look much more linear


a1=subset(diamonds2, cut == "Very Good")
a2=subset(diamonds2, cut == "Good")
a3=subset(diamonds2, cut == "Ideal")
a4=subset(diamonds2, cut== 'Astor Ideal')

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

#Viewing this plot now it looks like very good and ideal are significant compared to the other two levels. Thinking i'd set the reference
#level of this one to "Good" to see if there is really a difference

################################ Cut Plot End ###########################################


################################ Clarity Plot ###########################################
unique(diamonds2$clarity)

b1=subset(diamonds2, clarity == 'VVS2')
b2=subset(diamonds2, clarity == 'VS2')
b3=subset(diamonds2, clarity == 'IF')
b4=subset(diamonds2, clarity == 'VVS1')
b5=subset(diamonds2, clarity == 'VS1')
b6=subset(diamonds2, clarity == 'SI1')
b7=subset(diamonds2, clarity == 'SI2')
b8=subset(diamonds2, clarity == 'FL')

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

#This plot continues to tell the same story, that there may be an interaction going on between carat and clarity score since the slopes differ

boxplot(diamonds2$price~diamonds2$color, main='Boxplots Price against Color (price less than 25k)')
boxplot(diamonds2$price~diamonds2$cut, main='Boxplots Price against Cut (price less than 25k)')
boxplot(diamonds2$price~diamonds2$clarity, main='Boxplots Price against Clarity(price less than 25k)')

#fl is an outlier but rest look pretty similar




############################# Clarity Plot End ####################################################

unique(diamonds2$color)
par(mfrow=c(1,1))
c1=subset(diamonds2, color == 'G')
c2=subset(diamonds2, color == 'H')
c3=subset(diamonds2, color == 'F')
c4=subset(diamonds2, color == 'J')
c5=subset(diamonds2, color == 'E')
c6=subset(diamonds2, color == 'D')
c7=subset(diamonds2, color == 'I')

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


###DEF high group
###HGI low group
#### Viewing this plot the slopes appear to be grouped by a couple colors, maybe we can condense groupings and us J as a base class


diamonds3 = subset(diamonds2, price >500)

model = lm(price~., data=diamonds)
summary(model)
par(mfrow=c(2,2))
plot(model)
sum(model$fitted.values)
sum(diamonds$price)

diamonds_sub = diamonds[-c(384, 665, 816,339,940,15),]

model2 = lm(price~., data=diamonds_sub)
summary(model2)
par(mfrow=c(2,2))
plot(model2)

model3 = lm(price~carat*clarity+carat*cut+carat*color, data=diamonds3)
summary(model3)
par(mfrow=c(2,2))
plot(model3)
boxcox(model3)

model3_red=lm(price~carat*color, data=diamonds2)
summary(model3_red)

anova(model3_red,model3)


model4=lm(sqrt(price)~carat*color,data=diamonds2)
par(mfrow=c(2,2))
plot(model4)
summary(model4)

library(forcats)
###DEF high group
###HGI low group



diamonds3 = diamonds2
diamonds3$color<-factor(diamonds3$color)
diamonds3$color =relevel(diamonds3$color, ref= 'J') #J was the lowest on the scatter plot
diamonds3$color = fct_collapse(diamonds3$color, High_Slope_Colors = c("E","F", "D"))
diamonds3$color = fct_collapse(diamonds3$color, Low_Slope_Colors = c("H","G", "I"))
contrasts(diamonds3$color)


model5=lm((price)~carat*color,data=diamonds3)
par(mfrow=c(2,2))
plot(model5)
summary(model5)

model6 =lm((price)~carat+color,data=diamonds3)
par(mfrow=c(2,2))
plot(model6)
summary(model6)


anova(model6,model5) ## Very small p-value so we cant drop interaction terms




#diamonds$color<-factor(diamonds$color)
#diamonds$color =relevel(diamonds$color, ref= 'J') #J was the lowest on the scatter plot
#diamonds$color = fct_collapse(diamonds$color, High_Slope_Colors = c("E","F", "D"))
#diamonds$color = fct_collapse(diamonds$color, Low_Slope_Colors = c("H","G", "I"))

model7 = lm((price)^(1/3)~carat*color, data = diamonds)
summary(model7)
par(mfrow=c(2,2))
plot(model7)
boxcox(model7)



##### We can clearly see from EDA and trying to fit models, there are outliers in our dataset
##### to fit a more accurate model than excluding prices greater than 50k or greater than 25k
##### lets decide outliers based on quantiles and the inter quantile range


Q1 <- quantile(diamonds$price, .25)
Q3 <- quantile(diamonds$price, .75)
IQR <- IQR(diamonds$price)

iqr_no_out_diamonds <-subset(diamonds, diamonds$price > (Q1 - 1.5*IQR) & diamonds$price < (Q3 +1.5*IQR))

clean = iqr_no_out_diamonds

########### Clean plots

unique(diamonds$color)

c1=subset(clean, color == 'G')
c2=subset(clean, color == 'H')
c3=subset(clean, color == 'F')
c4=subset(clean, color == 'J')
c5=subset(clean, color == 'E')
c6=subset(clean, color == 'D')
c7=subset(clean, color == 'I')

reg1<-lm(price~carat,data=c1)
reg2<-lm(price~carat,data=c2)
reg3<-lm(price~carat,data=c3)
reg4<-lm(price~carat,data=c4)
reg5<-lm(price~carat,data=c5)
reg6<-lm(price~carat,data=c6)
reg7<-lm(price~carat,data=c7)

plot(c1$carat, c1$price, main="Price against Carats, by color, clean set iqr based")
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

#### DF high group
#### JI lowest group
####GHE middle high group



plot(clean$carat,clean$price, main = "Price against Carat")

boxplot(clean$price~clean$color, main='Boxplots Price against Color (price less than 50k)')
boxplot(clean$price~clean$cut, main='Boxplots Price against Cut (price less than 50k)')
boxplot(clean$price~clean$clarity, main='Boxplots Price against Clarity(price less than 50k)')

#FL only has 1 observation in clean

par(mfrow=c(1,1))

a1=subset(clean, cut == "Very Good")
a2=subset(clean, cut == "Good")
a3=subset(clean, cut == "Ideal")
a4= subset(clean, cut== 'Astor Ideal')

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

#no interactions jump  out at me here for cut


b1=subset(clean, clarity == 'VVS2')
b2=subset(clean, clarity == 'VS2')
b3=subset(clean, clarity == 'IF')
b4=subset(clean, clarity == 'VVS1')
b5=subset(clean, clarity == 'VS1')
b6=subset(clean, clarity == 'SI1')
b7=subset(clean, clarity == 'SI2')
b8=subset(clean, clarity == 'FL')

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
#no drastic interactions apparant here

model10 = lm((price)^(1/5)~carat+color, data=clean)
summary(model10)
par(mfrow=c(2,2))
plot(model10)
boxcox(model10)
acf(model10$residuals)





