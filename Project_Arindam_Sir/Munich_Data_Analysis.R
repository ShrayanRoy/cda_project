rm(list = ls(all = T))  #removes all objects


library(catdata)
library(smurf)

#=======================================================================================
data(rent)
str(rent)

## Rentm is numerical variable, which we want to predict
# Size is living space in sqaure meter
# rooms is number of rooms which is discrete variable
# year is year of construction, which is discrete variable
# area is municipality
# good is whether address is good
# best is whether address is best
# warm is whether warm water is available
# central is whether central heating is available
# tiles is bathroom tiles
# bathextra is special furniture in bathroom
# kitchen is upmarket kitchen or not. i.e. expensive or not ! 

sum(is.na(rent))  #so, no na values

# Urban district in Munich
rent$area <- as.factor(rent$area)

# Decade of construction
rent$year <- as.factor(floor(rent$year / 10) * 10)

# Number of rooms
rent$rooms <- as.factor(rent$rooms)

#Let's make a house quality variable
rent$quality <- as.factor(rent$good + 2*rent$best)
levels(rent$quality) <- c("Fair","Good","Excellent")

# We will make size variable also categorical variable, in our dataset it takes finitely many
# values only.
summary(rent$size)

#But we need to also look that, at end parts, we have very few observations ! 
table(rent$size)

# Floor space divided in categories (0, 30), [30, 40), ...,  [120, 130),[130, Inf)
sizeClasses <- c(0, seq(30, 130, 10))
rent$size <- as.factor(sizeClasses[findInterval(rent$size, sizeClasses)])

barplot(table(rent$size))  #not very less observations

# Is warm water present?
rent$warm <- factor(rent$warm, labels = c("yes", "no"))

# Is central heating present?
rent$central <- factor(rent$central, labels = c("yes", "no"))

# Does the bathroom have tiles?
rent$tiles <- factor(rent$tiles, labels = c("yes", "no"))

# Is there special furniture in the bathroom?
rent$bathextra <- factor(rent$bathextra, labels = c("no", "yes"))

# Is the kitchen well-equipped?
rent$kitchen <- factor(rent$kitchen, labels = c("no", "yes"))


#Final Data set with Structure ! 
str(rent)

# We will use size,rooms,year,area,warm,central,tiles,bathextra,kitchen,quality as predictors
# Notice, all these are categorical variables

#===========================================================================================
# Some Exploratory Data Analysis

library(ggplot2)

#ploting theme
defined_theme <- theme(plot.subtitle = element_text(family = "mono",size = 11,
                   face = "bold",hjust = 0.01),axis.title = element_text(family = "serif"),
                   axis.text = element_text(size = 10),plot.title = element_text(family = "serif",
                   colour = "red", hjust = -0.01),legend.text = element_text(size = 10,family = "serif"), 
                   legend.title = element_text(family = "serif"),legend.background = element_blank(),
                   legend.box.background = element_rect(colour = "black")) + 
                   theme(strip.background = element_rect(fill = "#FFE5B4"))

#Histogram
ggplot(data = rent,aes(x = rentm)) + geom_histogram(aes(y = after_stat(density)),
  col = "black",fill = "aquamarine") + labs(x = "Rent of Flat per square meter in Euros",y = "Density",
                                            title = "Histogram of Rent of Flats") +  
  theme_bw(16) + defined_theme


#Rooms
ggplot(data = rent,aes(y = rentm,x = rooms,col = rooms)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Number of Rooms",title = "ScatterPlot of Number of Rooms vs. Rent",
       ) + theme_bw(16) + defined_theme

#Year 
ggplot(data = rent,aes(y = rentm,x = year,col = year)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Year of Construction",title = "ScatterPlot of Year of Construction vs. Rent",
       ) + theme_bw(16) + defined_theme

#Area
ggplot(data = rent,aes(y = rentm,x = area,col = area)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Area Code",title = "ScatterPlot of Area Code vs. Rent",
       ) + theme_bw(16) + defined_theme

#warm
ggplot(data = rent,aes(y = rentm,x = warm,col = warm)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Warm Water Available",title = "ScatterPlot of Warm vs. Rent",
       ) + theme_bw(16) + defined_theme

#central
ggplot(data = rent,aes(y = rentm,x = central,col = central)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Central Heating Availabilty",title = "ScatterPlot of Central vs. Rent",
       ) + theme_bw(16) + defined_theme


#Tiles
ggplot(data = rent,aes(y = rentm,x = tiles,col = tiles)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Bathroom Tiles ?",title = "ScatterPlot of Number of Tiles vs. Rent",
       ) + theme_bw(16) + defined_theme

#Tiles
ggplot(data = rent,aes(y = rentm,x = size,col = size)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Size",title = "ScatterPlot of Size vs. Rentm",
  ) + theme_bw(16) + defined_theme


#Special Furniture
ggplot(data = rent,aes(y = rentm,x = bathextra,col = bathextra)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Special Furniture Availability ",title = "ScatterPlot of bathExtra vs. Rent",
       ) + theme_bw(16) + defined_theme


#Kitchen
ggplot(data = rent,aes(y = rentm,x = kitchen,col = kitchen)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Special Kitchen ?",title = "ScatterPlot of Kitchen vs. Rent",
       ) + theme_bw(16) + defined_theme

#Quality
ggplot(data = rent,aes(y = rentm,x = quality,col = quality)) + geom_point() + geom_smooth(se = F) + 
  labs(y = "Rent of Flat per square meter in Euros",x = "Quality of Flat",title = "ScatterPlot of Quality vs. Rent",
  ) + theme_bw(16) + defined_theme

# EDA Suggest that, the predictors are not that nice in predicting, whether the rentm is higher or lower

#=================================================================================================

#what is we fit all variables ?
rentData <- rent[,!colnames(rent)%in%c("rent","good","best")]
str(rentData)

summary(lm  (rentm ~.,data = rentData))
#But, we have so many variables, This is because, we have so many indicators!
length(coef(lm(rentm ~. ,data = rentData)))  #56 variables ! 

#Should do variable selection ! 

#lasso ?
X.mat <- model.matrix(rentm ~ . ,data = rentData)[,-1]   
#otherwise it is taking size0 into consideration also

lasso.model <- glmnet::cv.glmnet(X.mat,rentData$rentm,alpha = 1,family = gaussian)
coef(lasso.model)
plot(lasso.model)

#You can clearly see that, not all coeffcients corresponding to dummy variables of a variable 
#are non-zero together !  

# serious problem ! 

#stepwise selection can be done !
library(leaps)
stepAIC(lm(rentm ~. ,data =  rentData))

#Not much of decrease in number of variables! 51 variables.

#Another problem ! 
district.cof <- coef(lm(rentm ~ 0 + area,data = rentData))
data.frame(District = 1:25,District.cof = as.vector(district.cof))


#library(gamlss.data)
#library(gamlss.spatial)
#draw.polys(rent99.polys)

#All these problems suggests usto to use some other approach !

formu <- rentm ~ p(area, pen = "gflasso") + 
  p(year, pen = "flasso") + p(rooms, pen = "flasso") + 
  p(quality, pen = "flasso") + p(size, pen = "flasso") +
  p(warm, pen = "lasso") + p(central, pen = "lasso") + 
  p(tiles, pen = "lasso") + p(bathextra, pen = "lasso") + 
  p(kitchen, pen = "lasso") 
munich.fit <- glmsmurf(formula = formu, family = gaussian(), data = rent, 
                       pen.weights = "glm.stand", lambda = 0.014)
plot(munich.fit)


area.cof <- sort(unique(coef(munich.fit)[2:25]))
for(i in 1:length(area.cof)){
  print(names(coef(munich.fit)[2:25])[coef(munich.fit)[2:25] == area.cof[i]])
}

size.cof <- sort(unique(coef(munich.fit)[42:52]))
for(i in 1:length(size.cof)){
  print(names(coef(munich.fit)[42:52])[coef(munich.fit)[42:52] == size.cof[i]])
}

year.cof <- sort(unique(coef(munich.fit)[26:34]))
for(i in 1:length(year.cof)){
  print(names(coef(munich.fit)[26:34])[coef(munich.fit)[26:34] == year.cof[i]])
}

room.cof <- sort(unique(coef(munich.fit)[35:39]))
for(i in 1:length(room.cof)){
  print(names(coef(munich.fit)[35:39])[coef(munich.fit)[35:39] == room.cof[i]])
}

rentData1 <- rentData
rentData1$central <- as.numeric(rentData$central) - 1
my.formu <- central ~ p(area, pen = "gflasso")
inde.model <- glmsmurf(formula = my.formu,family = binomial(),data = rentData1,
                       pen.weights = "glm.stand", lambda = 0.01)

plot(inde.model)

sort(coef(inde.model))

set.seed(seed = 123456789)
prob.0 <- c(0.05,0.01,0.1,0.24,0.14,0.01,0.2,0.15,0.1)/2
prob.1 <- sample(prob.0,size = length(prob.0),replace = T)
n <- 90
my.conti <- matrix(as.vector(rmultinom(1,n,c(prob.0,prob.1))),ncol = 9,nrow = 2,byrow = T)
y <- rep(0:1,times = c(sum(my.conti[1,]),sum(my.conti[2,])))
x <- c(rep(1:9,times = my.conti[1,]),rep(1:9,times = my.conti[2,]))
my.df <- data.frame(y=as.factor(y),x=as.factor(x))

my.formu <- y ~ p(x,pen = "gflasso")
summary(glmsmurf(formula = my.formu,family = binomial(),data = my.df,pen.weights = "glm.stand",lambda = 2))
