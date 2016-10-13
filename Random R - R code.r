#########################################################################################
# Name    : Random R
# Project : Proj 1 - Panel Data Analysis
# Date    : 10/08/2016

rm(list = ls())

library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
#########################################################################################
###################################### Exploratory Analysis #############################

Rossman <- read.csv("~/train.csv") # reading dataset

store <- read.csv("~/store.csv")

View(Rossman)
attach(Rossman)

View(store)

#Maximum stores are closed on Sunday

#############################################################
x= table(factor(DayOfWeek),factor(Open))
prop.table(x,margin = 1)


View(Sales)
#Average Sales is right skewed

#Visualization 1

opar = par() 
par(bg="grey",mfrow=c(1,2),las=2,col="blue") # creating one row and two columns

# plotting a distribution of sales against the day of the week
hist(Sales[Sales>0], prob = T, ylim = c(0, 0.0002), xlab = "Sales", main = "Distribution of Average Sales")
lines(density(Sales[Sales>0])) #only considering sales greater than zero
label = paste("Median = ", floor(median(Sales[Sales>0]))) #labelling
text(27000, 0.00015, label, col="red")

#plotting a distribution of customers against the day of the week
hist(Customers[Customers>0], prob = T, ylim = c(0, 0.002), xlab = "Customers",main = "Distribution of Average Customers")
lines(density(Customers[Customers>0]))
label1 = paste("Median = ", floor(median(Customers[Customers>0]))) #labelling
text(4000, 0.0015, label1, col="red")
par(opar)
#Approximately 1 customer shops 6369/676 = $9.421

######################################################

#Visualization 2

m=tapply(Sales,Store,mean) #separting mean sales by store Id
n = tapply(Customers,Store,mean) #separting mean customers by store Id
df=data.frame(id=names(m),sales=m, customers = n) # creating a new data frame with customers and sales against Id
View(df)
df$avg = sales/customers # sales/customers
attach(df)

View(df)

#Plotting a distribution of average sales per customer in every store
plot(density(avg), xlab = "Average Sales per Customer", main = "Distribution of Average Sales per customer for every store")
abline(v = mean(avg)- sd(avg), col = "deeppink") # drawing lines a standard deviation +/- the mean
abline(v = mean(avg)+ sd(avg), col = "deeppink")
label1 = paste("Mean = ", round(mean(avg),2)) # creating a label for mean of sales
text(mean(avg), 0.1, label1, col="red") # pasting the label on the plot in a appropriate location

label2 = paste("mean - sd = ", round(mean(avg) - sd(avg), 2)) # creating a label for sd - mean
text(mean(avg) - sd(avg)-0.5, 0.1, label2, srt=90, col="red") # pasting the label on the plot in a appropriate location

label3 = paste("mean + sd = ", round(mean(avg) + sd(avg), 2)) # creating a label for sd + mean
text(mean(avg) + sd(avg)+0.5, 0.1, label3, srt=90, col="red") # pasting the label on the plot in a appropriate location

########################################################################

#Visualization 3

# Comparing Sales with School Holiday
label1 = seq(1,1115) # creating a sequence of 1-1115 for the number of stores
value1 = rep(1/1115,1115)
ssample=sample(x=label1, prob = value1  , replace = T, size =1)
x= Rossman[Rossman$Store==ssample,] # taking any random store out of 1115 stores
y = x[x$Open==1, ]
qplot(factor(SchoolHoliday), Sales, data=y, geom=c("boxplot", "jitter"), # creating a boxplot of sales based on school holiday
      fill=factor(SchoolHoliday), main="Comparison of sales on Public School Holidays",
      xlab="School Holiday", ylab="Sales")

######################################################################################

#Visualization 4

#Checking Sales with promo

label1 = seq(1,1115) # creating a sequence of 1-1115 for the number of stores
value1 = rep(1/1115,1115)
ssample =sample(x=label1, prob = value1  , replace = T, size =1)
x= Rossman[Rossman$Store==ssample,] #taking any random store out of 1115 stores
y = x[x$Open==1, ]
qplot(factor(Promo), Sales, data=y, geom=c("boxplot", "jitter"), # creating a boxplot sales based on promo
      fill=factor(Promo), main="Comparison of sales on Promotions",
      xlab="Promo", ylab="Sales")

####################################################################################

#Visualization 5

p = data.frame(tapply(Rossman$Sales, Rossman$DayOfWeek, mean)) # grouping mean sales based on day of week

# adding a new column to the data frame for day of the week
p$day = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday")
colnames(p) = c

# plotting a bar chart for comparing the total sales against the day of the week
p2 <- ggplot(p, aes(x = reorder(day, -Average_Sales), y = Average_Sales)) +
  geom_bar(stat = "identity", fill = "tomato", color = "black") + 
  labs(y="Average Sales", x = "Day of the week", main = "Number of days open by Day of Week")

grid.arrange(arrangeGrob(p2)) # arranging in decreasing order total sales by day of the week

##########################################################################################

#Visualization 6

store$avgsales = tapply(Rossman$Sales,Rossman$Store,mean) # separating mean sales based on store type
store$avgcustomers = tapply(Rossman$Customers,Rossman$Store,mean) # separating mean of number of customers based on store type
store$avgSalespercustomer = store$avgsales/store$avgcustomers # calculating average sales/average customers

# plotting Distribution of Average Sales by Store Types
plot1 <- qplot(store$avgSalespercustomer, data=store, geom="density", fill=StoreType, alpha=I(.5), # creating a density distribution of smoking with relative to age
      main="Distribution of Average Sales per Customer By Store Types", xlab="Average Sales", 
      ylab="Density")

# creating labels on the plot to display proportion of store types
plot1 + annotate(geom="text", x=12, y=0.77, label="Proportion",color="red")+
  annotate(geom="text", x=12, y=0.70, label="a = 0.54",color="red")+
  annotate(geom="text", x=12, y=0.62, label="b = 0.02",color="red")+
  annotate(geom="text", x=12, y=0.55, label="c = 0.13",color="red")+
  annotate(geom="text", x=12, y=0.47, label="d = 0.31",color="red")

x = table(store$StoreType)
round(prop.table(x),2) #finding the proportions of store types

################################################################################################

#Visualization 7

# Distribution of Average Sales by Store Types Assortment
plot2 <- qplot(store$avgSalespercustomer, data=store, geom="density", fill=Assortment, alpha=I(.5), # creating a density distribution of smoking with relative to age
      main="Distribution of Average Sales per Customer By Assortment", xlab="Average Sales", 
      ylab="Density")

# creating labels on the plot to display proportion of assortment types
plot2 + annotate(geom="text", x=12, y=0.95, label="Proportion",color="red")+
annotate(geom="text", x=12, y=0.85, label="a = 0.53",color="red")+
annotate(geom="text", x=12, y=0.75, label="b = 0.01",color="red")+
annotate(geom="text", x=12, y=0.65, label="c = 0.46",color="red")

x = table(store$Assortment)
round(prop.table(x),2) #finding the proportions of assortments type

#########################################################################################
#Visualization 8 - Observing Individual Store Ability
Rossmann$Date = as.Date(Rossmann$Date)
Rossmann$monyear  = as.factor(format(Rossmann$Date,'%Y-%m'))
x1 = Rossmann[Rossmann$Store == 2,]
x2 = Rossmann[Rossmann$Store == 3,]


##################### X1 - Store = 2 #####################################
# Plotting Sales over years
xnames = names(tapply(x1$Sales, x1$monyear, sum))

plot(tapply(x1$Sales, x1$monyear, sum), labels=xnames)
lines(tapply(x1$Sales, x1$monyear, sum), labels=xnames)
lines(tapply(values,index,mean))

axis(1, at=1:length(xnames), labels=xnames)
axis(2)

##################### X2 - Store = 3 #####################################

xnames = names(tapply(x2$Sales, x2$monyear, sum))

plot(tapply(x2$Sales, x2$monyear, sum), labels=xnames)
lines(tapply(x2$Sales, x2$monyear, sum), labels=xnames)
lines(tapply(values,index,mean))
axis(1, at=1:length(xnames), labels=xnames)
axis(2)

#####################################################################


########################################## Panel Data Modelling ######################################

rm(list = ls())

#################################################### Loading data ###########################################################

Rossman <- read.csv("H:/R/Rossman Project/train.csv/train.csv")
Store <- read.csv("H:/R/Rossman Project/store.csv/store.csv")

################################################### Joining Store and Sales File ############################################
Ross = merge(Rossman,Store,by = "Store",all = TRUE)

################################################## Loaading Plm package #####################################################
library(plm)
attach(Ross)

Ross$DayOfWeek = as.factor(factor(Ross$DayOfWeek))
View(Ross)

################################################# Identifying Dependent and Independent Variables ###########################

Y <- cbind(Sales) # Dependent variable
X <- cbind(Customers,DayOfWeek,Promo,StateHoliday,StoreType,Assortment,Open,CompetitionDistance,CompetitionOpenSinceYear) #Independent Variables

# Set data as panel data - Setting store as id and Date as time variable
pdata <- plm.data(Ross, index=c("Store","Date"))

# Descriptive statistics   
summary(Y)
summary(X)

#################################################### Pooled OLS estimator ##################################################
pooling <- plm(Y ~ X, data=pdata, model= "pooling")
summary(pooling)
################################################### Between estimator ######################################################
between <- plm(Y ~ X, data=pdata, model= "between")
summary(between)
################################################## First differences estimator #############################################
firstdiff <- plm(Y ~ X, data=pdata, model= "fd")
summary(firstdiff)
################################################## Fixed effects or within estimator #######################################
fixed <- plm(Y ~ X, data=pdata, model= "within")
summary(fixed)
################################################# Random effects estimator #################################################
random <- plm(Y ~ X, data=pdata, model= "random")
summary(random)

################################################# LM test for random effects versus OLS ####################################
plmtest(pooling)
# p<0.05 i.e. Random effect is consistent

################################################ LM test for fixed effects versus OLS ######################################
pFtest(fixed, pooling) 
# p<0.05 fixed model model is more consitent

############################################### Hausman test for fixed versus random effects model #########################
phtest(random, fixed)
# p<0.05 i.e. fixed effect model is better 

############################################### pcd test to check whether errors have serial correlation ###################
pcdtest(fixed, test = c("cd"))
# p>0.05 i.e. errors don't have serial correlation


# Fixed 
# Calculating alpha (individual specific effect) for every individual after averaging out the time effect
Ability_Stores = data.frame(Store_Id = integer(0),Alpha = numeric()) # Data frame to capture alpha values
a = c() # Null vector to use in loop
for (i in 1:1115)
{
  x = Rossman[Rossman$Store==i,]
  a[i] = mean(x$Sales) - 8.454* mean(x$Customers) - 947.56*mean(x$Promo) + 26.821*mean(x$DayOfWeek) + 196.64*mean(as.numeric(x$StateHoliday)) - 118.24*mean(as.numeric(x$Open)) # Equation for alpha value calculation
  Ability_Stores[i,1] = i
  Ability_Stores[i,2] = a[i]
}
View(Ability_Stores) # View Data frame

