##Load package
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)

##Load the Diamond dataset
diamond_Price = read.csv("Diamonds Prices2022.csv")
str(diamond_Price)
# drop the column with character variable
diamond_Price[, 8] <- as.numeric(as.character(diamond_Price[, 8]))
diamond_Price = subset(diamond_Price, select = -c(X,color,cut,clarity))
summary(diamond_Price)
# drop the row with x, y, and z equal to 0
diamond_Price <- subset(diamond_Price, x != 0 & y != 0 & z != 0)
str(diamond_Price)
summary(diamond_Price)

quantile(diamond_Price$carat)
quantile(diamond_Price$depth)
quantile(diamond_Price$table)
quantile(diamond_Price$x)
quantile(diamond_Price$y)
quantile(diamond_Price$z)
quantile(diamond_Price$price)

##Check data distribution (dependent variable)
hist(diamond_Price$carat)
hist(diamond_Price$depth)
hist(diamond_Price$table)
hist(diamond_Price$x)
hist(diamond_Price$y)
hist(diamond_Price$z)
hist(diamond_Price$price)

#2. MULTIPLE REGRESSION
##Check relationship independent, if smaller is better, for dependent, if higher is better
cor(diamond_Price)

##Check distribution data point (linear/non linear) 
plot(price ~ carat, data = diamond_Price)
plot(price ~ depth, data = diamond_Price)
plot(price ~ table, data = diamond_Price)
plot(price ~ x, data = diamond_Price)
plot(price ~ y, data = diamond_Price)
plot(price ~ z, data = diamond_Price)

########################################
#2_1. Multiple regression
#Make linear model (check relationship between carat and depth and price)
diamond.lm <- lm(price ~ carat + depth, data = diamond_Price)
summary(diamond.lm)
#The estimated effect of carat on price is 7765.152, while the estimated effect of depth is -102.40
#every 1 increase in carat, there is a correlated $7765 increase in the price
#every 1 increase in depth, there is a correlated $102 decrease in the price
#The standard errors for these regression coefficients are very small, and the t-statistics are very large (554.30 and -22, respectively). 
#The p-values reflect these small errors and large t-statistics.

#2. Multiple regression
#choose min, mean,max values of depth to predict prices to make 3 level of depth
plotting.data <- expand.grid(
  carat = seq(min(diamond_Price$carat), max(diamond_Price$carat), length.out = 30), 
  depth = c(min(diamond_Price$depth), mean(diamond_Price$depth), max(diamond_Price$depth))
)

#predict value price based on linear model
plotting.data$predicted.y <- predict.lm(diamond.lm, newdata=plotting.data)

#run depth number to 2 decimals
plotting.data$depth <- round(plotting.data$depth, digits = 2)

#change depth variable into factor (interaction between carat and price at each 3 level of depth)
plotting.data$depth <- as.factor(plotting.data$depth)

#plot original data
diamondP.plot <- ggplot(diamond_Price, aes(x=carat, y=price)) + geom_point()
diamondP.plot2<- ggplot(diamond_Price, aes(x=depth, y=price)) + geom_point()

#add regression line
diamondP.plot <- diamondP.plot +
  geom_line(data=plotting.data, aes(x=carat, y=predicted.y, color=depth), size=1.25)


#make graph ready
diamondP.plot <-
  diamondP.plot +
  theme_bw() +
  labs(title = "Rates of Diamond Price \n as a function of carat and depth",
       x = "carat",
       y = "Diamond Price",
       color = "depth")


##############################################
#2_2. Multiple regression
diamond2.lm <- lm(price ~ x + depth, data = diamond_Price)
summary(diamond2.lm)
#The estimated effect of x (length) on price is 3146.468, while the estimated effect of depth is 32.703
#every 1 increase in x, there is a correlated $7765 increase in the price
#every 1 increase in depth, there is a correlated $102 decrease in the price
#The standard errors for these regression coefficients are very small, and the t-statistics are very large (554.30 and -22, respectively). 
#The p-values reflect these small errors and large t-statistics.

#choose min, mean,max values of depth to predict price to make 3 level of depth
plotting2.data <- expand.grid(
  x = seq(min(diamond_Price$x), max(diamond_Price$x), length.out = 30), 
  depth = c(min(diamond_Price$depth), mean(diamond_Price$depth), max(diamond_Price$depth))
)

#predict value orice based on linear model
plotting2.data$predicted2.y <- predict.lm(diamond2.lm, newdata=plotting2.data)

#run price number to 2 decimals
plotting2.data$depth <- round(plotting2.data$depth, digits = 2)

#change depth variable into factor (interaction between x and price at each 3 level of depth)
plotting2.data$depth <- as.factor(plotting2.data$depth)

#plot original data
diamondP2.plot <- ggplot(diamond_Price, aes(x=x, y=price)) + geom_point()
diamondP2.plot2<- ggplot(diamond_Price, aes(x=depth, y=price)) + geom_point()

#add regression line
diamondP2.plot <- diamondP2.plot +
  geom_line(data=plotting2.data, aes(x=x, y=predicted2.y, color=depth), size=1.25)

#make graph ready
diamondP2.plot <-
  diamondP2.plot +
  theme_bw() +
  labs(title = "Rates of Diamond \n as a function of x and depth",
       x = "x",
       y = "Diamond Price",
       color = "depth ")


########################################################
#2_3. Multiple regression
diamond3.lm <- lm(price ~ y + depth, data = diamond_Price)
summary(diamond3.lm)
#choose min, mean,max values of depth to predict price to make 3 level of depth
plotting3.data <- expand.grid(
  y = seq(min(diamond_Price$y), max(diamond_Price$y), length.out = 30), 
  depth = c(min(diamond_Price$depth), mean(diamond_Price$depth), max(diamond_Price$depth))
  
)

#predict value price based on linear model
plotting3.data$predicted3.y <- predict.lm(diamond3.lm, newdata=plotting3.data)

#run depth number to 2 decimals
plotting3.data$depth <- round(plotting3.data$depth, digits = 2)

#change depth variable into factor (interaction between y and price at each 3 level of depth)
plotting3.data$depth <- as.factor(plotting3.data$depth)

#plot original data
diamondP3.plot <- ggplot(diamond_Price, aes(x=y, y=price)) + geom_point()
diamondP3.plot2<- ggplot(diamond_Price, aes(x=depth, y=price)) + geom_point()

#add regression line
diamondP3.plot <- diamondP3.plot +
  geom_line(data=plotting3.data, aes(x=y, y=predicted3.y, color=depth), size=1.25)

#make graph ready
diamondP3.plot <-
  diamondP3.plot +
  theme_bw() +
  labs(title = "Rates of Diamond Price \n as a function of y and depth",
       x = "y",
       y = "Diamond Price",
       color = "depth")

#########################################
#2_4. Multiple regression
diamond4.lm <- lm(price ~ z + depth, data = diamond_Price)
summary(diamond4.lm)
#choose min, mean,max values of depth to predict price to make 3 level of depth
plotting4.data <- expand.grid(
  z = seq(min(diamond_Price$z), max(diamond_Price$z), length.out = 30), 
  depth = c(min(diamond_Price$depth), mean(diamond_Price$depth), max(diamond_Price$depth))
  
)

#predict value price based on linear model
plotting4.data$predicted4.y <- predict.lm(diamond4.lm, newdata=plotting4.data)

#run depth number to 2 decimals
plotting4.data$depth <- round(plotting4.data$depth, digits = 2)

#change depth variable into factor (interaction between z and price at each 3 level of depth)
plotting4.data$depth <- as.factor(plotting4.data$depth)

#plot original data
diamondP4.plot <- ggplot(diamond_Price, aes(x=z, y=price)) + geom_point()
diamondP4.plot2<- ggplot(diamond_Price, aes(x=depth, y=price)) + geom_point()
#add regression line
diamondP4.plot <- diamondP4.plot +
  geom_line(data=plotting4.data, aes(x=z, y=predicted4.y, color=depth), size=1.25)

#make graph ready
diamondP4.plot <-
  diamondP4.plot +
  theme_bw() +
  labs(title = "Rates of Diamond Price \n as a function of z and depth",
       x = "z",
       y = "Diamond Price",
       color = "depth")

graphP <- ggarrange(diamondP.plot, diamondP2.plot, diamondP3.plot, diamondP4.plot, 
                    ncol = 2, nrow = 2)
graphP
