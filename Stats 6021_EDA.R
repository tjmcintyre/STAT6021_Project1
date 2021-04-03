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


### linear relationship just b/w carat and price ##################################################################

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

### transform price by lambda = 3/10 to fix variance ##############################################################

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

####################################################################################################################

model3 = lm(log(price)~log(carat+.2), data=diamonds_data)
par(mfrow=c(2,2))                         # Dont rerun this bit after the code beneath #
plot(model3)
boxcox(model3, seq(-2,2, by =.2))
acf(model3$residuals)

#Now that the response has been transformed lets revisit the price against carats by color plot
#adjusting the data frame variables to the transformed versions
diamonds$price = log(diamonds$price)
diamonds$carat = log(diamonds$carat + .2)

##################################### TEST CODE - IGNORE ######################################################
diamonds_data <- mutate(diamonds_data, log_carat = log(carat+.2), log_price = log(price))  # transform the data

diamonds_data2 <- diamonds_data[c('log_carat', 'clarity', 'color', 'cut', 'log_price')] # create separate dataset to use

attach(diamonds_data2)

# Create scatters:

plot(log_carat, log_price, main="log Price by log Carat and Cut")
points(I$log_carat, I$log_price, pch=2, col="blue")
points(VG$log_carat, VG$log_price, pch=3, col="red")
points(G$log_carat, G$log_price, pch=4, col="green")

abline(price_AI,lty=1)
abline(price_I,lty=2, col="blue") 
abline(price_VG,lty=3, col="red")
abline(price_G,lty=4, col="green")
legend("topleft", c("Astor Ideal", "Ideal", "Very Good", "Good"), lty=c(1, 2, 3, 4), 
       pch=c(1,2,3, 4), col=c("black", "blue", "red", "green")) 

m_cut <- lm(log_price~log_carat + cut, data = diamonds_data2)

summary(m_cut)

# Tukey tests
#install.packages("multcomp")
library(lawstat)
library(multcomp)

pairwise<-glht(m_cut, linfct = mcp(cut = "Tukey")) 
summary(pairwise)

########################## CATEGORICAL GROUPINGS ############################################################

###### CUT 

high_cut <- c('Astor Ideal', 'Ideal') # high level cut

diamonds$cut_group <- ifelse(diamonds_data2$cut %in% high_cut, 'High Cut', 'Low Cut') # Low Cut includes V Good & Good

head(diamonds_data2)

# consider each cut as a subset
hc<-subset(diamonds_data2,cut_group=="High Cut") 
lc<-subset(diamonds_data2,cut_group=="Low Cut") 

# fit separate regressions
price_HighCut <- lm(log_price~log_carat,data=hc)
price_LowCut <- lm(log_price~log_carat,data=lc)

# Create scatters:
plot(hc$log_carat, hc$log_price, col = 'blue', main="Log Price by Log Carat and Grouped Cut")
points(lc$log_carat, lc$log_price, pch=2, col="red")

abline(price_HighCut,lty=1, col = 'blue')
abline(price_LowCut,lty=2, col="red") 
legend("topleft", c("High Cut", "Low Cut"), lty=c(1, 2), 
       pch=c(1,2), col=c("blue", "red")) 
grid()

is.factor(cut_group)
cut_group <- factor(cut_group, levels = c('High Cut', 'Low Cut'))
levels(cut_group)
is.factor(cut_group)

attach(diamonds_data2)

count(diamonds_data2, c(cut_group))

m_cut <- lm(log_price~log_carat + cut_group)

summary(m_cut)

pairwise<-glht(m_cut, linfct = mcp(cut_group = "Tukey")) 
summary(pairwise)

###### COLOR #######

D <-subset(diamonds_data2,color=="D") 
E <-subset(diamonds_data2, color=="E") 
eF<-subset(diamonds_data2,color=="F") # can't use just f ?? 
G<-subset(diamonds_data2,color=="G") 
H<-subset(diamonds_data2,color=="H") 
I<-subset(diamonds_data2,color=="I") 
J<-subset(diamonds_data2,color=="J") 

# fit separate regressions
price_D <- lm(log_price~log_carat,data=D)
price_E <- lm(log_price~log_carat,data=E)
price_eF <- lm(log_price~log_carat,data=eF)
price_G <- lm(log_price~log_carat,data=G)
price_H <- lm(log_price~log_carat,data=H)
price_I <- lm(log_price~log_carat,data=I)
price_J <- lm(log_price~log_carat,data=J)

plot(log_carat, log_price, main="Price by Carat and Color")
points(E$log_carat, E$log_price, pch=2, col="green")
points(eF$log_carat, eF$log_price, pch=3, col="blue")
points(G$log_carat, G$log_price, pch=4, col="orange")
points(H$log_carat, H$log_price, pch=5, col="red")
points(I$log_carat, I$log_price, pch=6, col="bisque1")
points(J$log_carat, J$log_price, pch=7, col="aquamarine")

abline(price_D,lty=1, col = "black")
abline(price_E,lty=2, col = "green")
abline(price_eF,lty=3, col="blue") 
abline(price_G,lty=4, col="orange")
abline(price_H,lty=5, col="red")
abline(price_I,lty=6, col="bisque1")
abline(price_J,lty=7, col="aquamarine")

legend("topleft", c("D", "E", "F", "G", "H", "I", "J"), lty=c(1, 2, 3, 4, 5, 6, 7), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "green", "blue", "orange", "red", "bisque1", "aquamarine")) 

count(diamonds_data2, c(color))

count(diamonds_data2, c(clarity))

best_color <- c('D', 'E', 'F', 'G', 'H') # all but bottom 2

diamonds_data2$color_group <- ifelse(diamonds_data2$color %in% best_color, 'High Color', 'Low Color')

# color:
high_color <-subset(diamonds_data2,col_group=="High Color") 
low_color <-subset(diamonds_data2, col_group=="Low Color") 

# fit separate regressions
price_highcolor <- lm(log_price~log_carat,data=high_color)
price_lowcolor <- lm(log_price~log_carat,data=low_color)

plot(log_carat, log_price, main="log Price by log Carat and Grouped Color")
points(low_color$log_carat, low_color$log_price, pch=2, col="red")

abline(price_highcolor,lty=1, col = "black")
abline(price_lowcolor,lty=2, col = "red")

legend("topleft", c("High Colors", "Low Colors"), lty=c(1, 2), 
       pch=c(1,2), col=c("black", "red")) 

plot(price_topc$fitted.values, price_topc$residuals, main = 'Residual Plot')
abline(h=0,col="red")
grid()

plot(price_bottomc$fitted.values, price_bottomc$residuals, main = 'Residual Plot')
abline(h=0,col="red")
grid()

is.factor(color_group)
color_group <- factor(color_group, levels = c('High Color', 'Low Color'))
levels(color_group)
is.factor(color_group)

attach(diamonds_data2)

count(diamonds_data2, c(color_group))

m_color <- lm(log_price~log_carat + color_group)

summary(m_color)

pairwise<-glht(m_color, linfct = mcp(color_group = "Tukey")) 
summary(pairwise)

###### CLARITY #######
FL <-subset(diamonds_data2,clarity=="FL") 
IF <-subset(diamonds_data2,clarity=="IF") 
VVS1 <-subset(diamonds_data2,clarity=="VVS1") 
VVS2 <-subset(diamonds_data2,clarity=="VVS2") 
VS1 <-subset(diamonds_data2,clarity=="VS1") 
VS2 <-subset(diamonds_data2,clarity=="VS2") 
SI1 <-subset(diamonds_data2,clarity=="SI1") 
SI2 <-subset(diamonds_data2,clarity=="SI2") 

# fit separate regressions
price_FL <- lm(log_price~log_carat,data=FL)
price_IF <- lm(log_price~log_carat,data=IF)
price_VVS1 <- lm(log_price~log_carat,data=VVS1)
price_VVS2 <- lm(log_price~log_carat,data=VVS2)
price_VS1 <- lm(log_price~log_carat,data=VS1)
price_VS2 <- lm(log_price~log_carat,data=VS2)
price_SI1 <- lm(log_price~log_carat,data=SI1)
price_SI2 <- lm(log_price~log_carat,data=SI2)

plot(log_carat, log_price, main="log Price by log Carat and Clarity")
points(IF$log_carat, IF$log_price, pch=2, col="green")
points(VVS1$log_carat, VVS1$log_price, pch=3, col="aquamarine")
points(VVS2$log_carat, VVS2$log_price, pch=4, col="brown")
points(VS1$log_carat, VS1$log_price, pch=5, col="red")
points(VS2$log_carat, VS2$log_price, pch=6, col="bisque1")
points(SI1$log_carat, SI1$log_price, pch=7, col="blue")
points(SI2$log_carat, SI2$log_price, pch=8, col="orange")

abline(price_FL,lty=1, col = "black")
abline(price_IF,lty=2, col = "green")
abline(price_VVS1,lty=3, col="aquamarine")
abline(price_VVS2,lty=4, col="brown")
abline(price_VS1,lty=5, col="red")
abline(price_VS2,lty=6, col="bisque1")
abline(price_SI1,lty=7, col="blue") 
abline(price_SI2,lty=8, col="orange")

legend("topleft", c("FL", "IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2"), lty=c(1, 2, 3, 4, 5, 6, 7, 8), 
       pch=c(1,2,3,4,5,6,7), col=c("black", "green", "aquamarine", "brown", "red", "bisque1", "blue", "orange")) 

clarity_group <- factor(clarity_group, levels = c('Flawless_VVSI', 'Very Slightly', 'Slightly'))
 
levels(clarity_group)
 
# clarity by 3:
FL <-subset(diamonds_data2,clarity_group=="Flawless_VVSI")
VS <-subset(diamonds_data2,clarity_group=="Very Slightly")
S <-subset(diamonds_data2,clarity_group=="Slightly")
 
head(FL)

 
# fit separate regressions
price_fl <- lm(log_price~log_carat,data=FL)
price_vs <- lm(log_price~log_carat,data=VS)
price_s <- lm(log_price~log_carat,data=S)
 
 
 
plot(log_carat, log_price, main="log Price by log Carat and Grouped Clarity")
points(VS$log_carat, VS$log_price, pch=2, col="red")
points(S$log_carat, S$log_price, pch=3, col="blue")
 
 
abline(price_fl,lty=1, col = "black")
abline(price_vs,lty=2, col = "red")
abline(price_s,lty=3, col="blue")
 

legend("topleft", c("Flawless_VVSI", 'Very Slightly Included', 'Included'), lty=c(1, 2, 3),
      pch=c(1,2,3), col=c("black", "red", "blue"))
 grid()
 
# clarity by 2: 
fl_cl <- c('FL', 'IF', 'VVS1', 'VVS2')
vs_cl <- c('VS1', 'VS2')
 
diamonds_data2$clarity_group <- ifelse(diamonds_data2$clarity %in% fl_cl,
                                  'High Clarity', 'Low Clarity')
 
diamonds_data2$clarity_group <- factor(diamonds_data2$clarity_group, levels = c('High Clarity', 'Low Clarity'))
 
levels(diamonds_data2$clarity_group)
 
table(diamonds_data2$clarity_group)
 
# clarity:
FL <-subset(diamonds_data2,clarity_group=="High Clarity")
VS <-subset(diamonds_data2,clarity_group=="Low Clarity")
 
head(FL)
 
# fit separate regressions
price_fl <- lm(log_price~log_carat,data=FL)
price_vs <- lm(log_price~log_carat,data=VS)

 
plot(log_carat, log_price, main="log Price by log Carat and Grouped Clarity")
points(VS$log_carat, VS$log_price, pch=2, col="red")

 
abline(price_fl,lty=1, col = "black")
abline(price_vs,lty=2, col = "red")

 
### all 3 groupings together 
m_clarity <- lm(log_price~log_carat + clarity_group + color_group + cut_group)
pairwise<-glht(m_clarity, linfct = mcp(clarity_group= "Tukey"))
summary(pairwise)

############## TEST ########################
plot(cut_group~price) # frequency plot to check variance; nothing weird
plot(color_group~cut_group)
plot(clarity_group~color_group)

attach(diamonds_data2)
m_clarity <- lm(log_price~log_carat + clarity_group)

pairwise<-glht(m_clarity, linfct = mcp(clarity_group= "Tukey"))
summary(pairwise)

boxplot(log_carat~cut_group, main="Boxplot of log Carat and Grouped Cut")
boxplot(log_carat~color_group, main="Boxplot of log Carat and Grouped Color")
boxplot(log_carat~clarity_group, main="Boxplot of log Carat and Grouped Clarity")

boxplot(log_price~cut_group, main="Boxplot of log Price and Grouped Cut")
boxplot(log_price~color_group, main="Boxplot of log Price and Grouped Color")
boxplot(log_price~clarity_group, main="Boxplot of log Price and Grouped Clarity")

diamonds_data2$clarity_group <- relevel(clarity_group, ref = 'Bottom Clarity')
levels(clarity_group)
attach(diamonds_data2)

m_clarity <- lm(log_price~log_carat + clarity_group)

summary(m_clarity)
summary(m_color)
summary(m_cut)

m_group <- lm(log_price~log_carat + color_group + clarity_group + cut_group)
m_group2 <- lm(log_price~log_carat + color_group + clarity_group)

m_col_int <- lm(log_price~ log_carat*color_group)
m_simple <- lm(log_price~ log_carat)

m_cl_int <- lm(log_price~ log_carat*clarity_group)
m_cut_int <- lm(log_price~ log_carat*cut_group)

summary(m_group)
summary(m_col_int)
summary(m_color)

summary(m_col_int)
summary(m_cl_int)
summary(m_cut_int)

anova(m_col_int, m_simple)
anova(m_color, m_col_int)
anova(m_cut, m_group)
anova(m_color, m_group)
anova(m_col_int, m_group)
anova(m_clarity, m_group)
anova(m_simple, m_group)

anova(m_group2, m_group)

m_group_ <- lm(log_price~log_carat*color_group)
summary(m_group_)

m_group2 <- lm(log_price~log_carat*color_group + log_carat*clarity_group + color_group*clarity_group + log_carat*cut_group)
summary(m_group2)

group_full <- lm(log_price~log_carat*color_group + log_carat*clarity_group + color_group*clarity_group + log_carat*cut_group + color_group*cut_group + clarity_group*cut_group)
summary(group_full)

reduced <- lm(log_price~log_carat*clarity_group + log_carat*cut_group + log_carat*color_group)
summary(reduced)
anova(reduced)