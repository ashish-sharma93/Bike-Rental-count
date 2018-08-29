#remove all objects stored
rm(list = ls())

#set working directory
setwd("D:/edwisor_project/R_files")

#Load libraries
x = c("ggplot2","ggcorrplot", "corrgram", "MASS", "rpart", "gbm")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


#load data
data_original <- read.csv("day.csv",stringsAsFactors = FALSE)

#use copy of coriginal data
data <- data_original

#rename variables,if required
names(data)[names(data)=="dteday"] <- "date"
names(data)[names(data)=="yr"] <- "year"
names(data)[names(data)=="mnth"] <- "month"
names(data)[names(data)=="hum"] <- "humidity"
names(data)[names(data)=="cnt"] <- "count"
names(data)[names(data)=="atemp"] <- "feel_temp"
names(data)[names(data)=="weathersit"] <- "weather"


#convert to required data types
data$season=factor(data$season,levels = c(1,2,3,4), labels = c("spring","summer","fall","winter"))
data$weather=factor(data$weather,levels = c(1,2,3,4), labels = c("weather1","weather2","weather3","weather4"))
data$weekday=factor(data$weekday,levels = c(0,1,2,3,4,5,6), labels = c("sunday","monday","tuesday","wednesday","thursday","friday","saturday"))
data$workingday=as.factor(data$workingday)
data$holiday=as.factor(data$holiday)

#------------------------------------------DATA PRE-PROCESSING----------------------------------------------
#removing column instant(index numbers) as it has no corelation to any other variable.
#day,season,workingday,holiday are already derived in the table therfore date attribute not required.
data <- data[,c(-1,-2)]


#check for missing values ,if any
#can be tested using is.null() also.
if(sum(is.na(data))== 0) {
  print("No missing values found")
} else {
  print("missing value(s) existing")
}


#check for duplicates
if(sum(duplicated(data))== 0) {
  print("No duplicates found")
} else {
  print("duplicate data existing")
}
  

#####1.univariate analysis

#setting plot margins
par(mfrow=c(1,2),mar = rep(3,4))##Creates a multi-paneled plotting window & sets the margin size.
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))


#season barplot 
png(file = "barplot_seasons.png")
season_plot <- barplot(table(data$season), names.arg= c("spring","summer","fall","winter"),col="cyan",ylim = c(0,200),
                       ylab = "Frequency",xlab = "Seasons")
text(season_plot,table(data$season),labels = table(data$season),pos = 3)
dev.off() 


#weather barplot
png(file = "barplot_weather.png")
weather_plot <- barplot(table(data$weather),col="cyan",ylim = c(0,500),
                        ylab = "Frequency",xlab = "weather")
text(weather_plot,table(data$weather),labels = table(data$weather),pos = 3)
dev.off() 


 #holiday barplot
png(file = "barplot_holiday.png")
holiday_plot <- barplot(table(data$holiday),col="cyan",ylim = c(0,1000),
                        ylab = "Frequency",names.arg = c("No holiday","holiday"))
text(holiday_plot,table(data$holiday),labels = table(data$holiday),pos = 3)
dev.off()  


# workingday barplot
png(file = "barplot_workingday.png")
workingday_plot <- barplot(table(data$workingday),col="cyan",ylim = c(0,700),
                           ylab = "Frequency",names.arg = c("No working day","working day"))
text(workingday_plot,table(data$workingday),labels = table(data$workingday),pos = 3)
dev.off()  


#Boxplot for Count
png(file = "boxplot_count.png")
boxplot(data$count, data = data,
        xlab = "count",
        ylab = "frequency", main = "Boxplot for Count")
dev.off()  

#Boxplot for temp
png(file = "boxplot_temp.png")
boxplot(data$temp, data = data,
        xlab = "temperature",
        ylab = "frequency", main = "Boxplot for temp")
dev.off()  

#Boxplot for humidity
png(file = "boxplot_humidity.png")
boxplot(data$humidity, data = data,
        xlab = "humidity",
        ylab = "frequency", main = "Boxplot for humidity")
dev.off() 

#Boxplot for windspeed
png(file = "boxplot_windspeed.png")
boxplot(data$windspeed, data = data,
        xlab = "windspeed",
        ylab = "frequency", main = "Boxplot for windspeed")
dev.off() 


####2.Multivariate Analysis

#boxplot of rental count & season
png(file = "rental count vs season.png",res = 100)
ggplot(data, aes(x = season, y = count,fill = season)) +
  geom_boxplot(outlier.colour = "black",na.rm = TRUE) +
  coord_cartesian(ylim=c(0,max(data$count))) +
  scale_y_continuous(breaks=seq(0, max(data$count),1000)) +
  xlab("season") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs Season") +
  scale_fill_manual(values=c("red", "blue", "green","yellow"), 
                    name="Season:",
                    labels=c("spring", "Summer", "Fall", "Winter"))
dev.off()


#boxplot of rental count & weather
png(file = "rental count vs weather.png",width = 500,height = 500,res = 100)
ggplot(data, aes(x = weather, y = count,fill = weather)) +
  geom_boxplot(outlier.colour = "black",na.rm = TRUE) +
  coord_cartesian(ylim=c(0,max(data$count))) +
  scale_y_continuous(breaks=seq(0, max(data$count),1000)) +
  xlab("weather") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs Weather") +
  scale_fill_manual(values=c("red", "green", "yellow","blue"), 
                    name="weather:",
                    labels=c("1:clear,few clouds", "2:mist+cloudy", 
                             "3:light rain, thunder", "4:heavy rain,thunderstorm"))
dev.off()


#boxplot of rental count & holiday
png(file = "rental count vs holiday.png",res = 125)
ggplot(data, aes(x = holiday, y = count,fill = holiday)) +
  geom_boxplot(outlier.colour = "black",na.rm = TRUE) +
  coord_cartesian(ylim=c(0,max(data$count))) +
  scale_y_continuous(breaks=seq(0, max(data$count),1000)) +
  xlab("holiday or not") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs holiday") +
  scale_fill_manual(values=c("orange", "cyan"), 
                    name="holiday:",
                    labels=c("0:No holiday","1:holiday"))
  dev.off()


#boxplot of rental count & working day
png(file = "rental count vs workingday.png",res = 100)
ggplot(data, aes(x = workingday, y = count,fill = workingday)) +
  geom_boxplot(outlier.colour = "black",na.rm = TRUE) +
  coord_cartesian(ylim=c(0,max(data$count))) +
  scale_y_continuous(breaks=seq(0, max(data$count),1000)) +
  xlab("working day or not") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs working_day") +
  scale_fill_manual(values=c("orange", "cyan"), 
                    name="workingday:",
                    labels=c("0:No working day","1:working day"))
dev.off()


#boxplot of rental count & weekday
png(file = "rental count vs weekday.png",res = 100)
ggplot(data, aes(x = weekday, y = count,fill = weekday)) +
  geom_boxplot(outlier.colour = "black",na.rm = TRUE) +
  coord_cartesian(ylim=c(0,max(data$count))) +
  scale_y_continuous(breaks=seq(0, max(data$count),1000)) +
  xlab("Day of the week") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs week_day") +
  scale_fill_brewer(palette="Set1",name="Day of week", 
                      labels=c("0:Sunday","1:Monday","2:Tuesday","3:Wednesday",
                                "4:Thursday","5:Friday","6:Saturday"))

dev.off()


#line plot of rental count & month
png(file = "rental count vs month.png",res = 100)
ggplot(data, aes(x = month, y = count, color = month)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  coord_cartesian(xlim = c(0,max(data$month)), ylim=c(0,max(data$count))) +
  xlab("month") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs month") +
  theme_classic()
dev.off()


# line plot of rental v.s. year
png(file = "rental count vs year.png",res = 100)
ggplot(data, aes(x = year, y = count, color = year)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  coord_cartesian(xlim = c(0,max(data$year)), ylim=c(0,max(data$count))) +
  xlab("year") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs year") +
  theme_classic()
dev.off()



# line plot of rental v.s. temperature
png(file = "rental count vs temp.png",res = 100)
ggplot(data, aes(x = temp, y = count, color = temp)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  coord_cartesian(xlim = c(0,max(data$temp)), ylim=c(0,max(data$count))) +
  xlab("Temperature") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs temperature") +
  theme_classic()
dev.off()


# line plot of rental v.s. humidity
png(file = "rental count vs humidity.png",res = 100)
ggplot(data, aes(x = humidity, y = count,color = humidity)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  coord_cartesian(xlim = c(0,max(data$humidity)), ylim=c(0,max(data$count))) +
  xlab("humidity") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs humidity") +
  theme_classic()  
dev.off()


# line plot of rental v.s. wind speed
png(file = "rental count vs windspeed.png",res = 100)
ggplot(data, aes(x = windspeed, y = count,color = windspeed)) +
  geom_point(aes(color="red")) +
  geom_smooth(fill = NA) +
  coord_cartesian(xlim = c(0,max(data$windspeed)), ylim=c(0,max(data$count))) +
  xlab("windspeed") + 
  ylab("rental count") +
  ggtitle("Rental_count Vs windspeed") +
  theme_classic()
dev.off()


#####3. outlier analysis and treatment

#save numeric names
cnames <-  c("count", "temp", "humidity", "windspeed")

for (i in cnames) {
  q25 <- quantile(data[,i],probs = 0.25)
  q75 <- quantile(data[,i],probs = 0.75)
  iqr = q75 - q25
  min = q25 - (iqr*1.5)
  max = q75 + (iqr*1.5)
  print(min)
  print(max)
  data <- data[!data[,i] < min,]
  data <- data[!data[,i] > max,]
  
}

#####4.Feature Engineering

#correlation Plot

numeric_index <- sapply(data,is.numeric) #selecting only numeric
numeric_data <- data[,numeric_index]


#plot1
png(file = "correlation_matrix_1.png",width = 1000,height = 1000)
corrgram(numeric_data, order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
dev.off() 

#Plot2
png(file = "correlation_matrix_2.png",width = 1000,height = 1000)
ggcorrplot(cor(numeric_data),method = "square",type = "full", lab = TRUE)
dev.off()


#Anova analysis
#count wrt seasons
anova_one_way1 <- aov(count~season,data[order(data$season),])
summary(anova_one_way1)

#count wrt workingday
anova_one_way2 <- aov(count~workingday,data[order(data$workingday),])
summary(anova_one_way2) #p > .05

#count wrt holiday
anova_one_way3 <- aov(count~holiday,data[order(data$holiday),])
summary(anova_one_way3) #p > .05

#count wrt weather
anova_one_way4 <- aov(count~weather,data[order(data$weather),])
summary(anova_one_way4)

#count wrt weekday
anova_one_way5 <- aov(count~weekday,data[order(data$weekday),])
summary(anova_one_way5) #p > .05


#------------------------------------------------------------------------------
#conclusion based on anova,correlation plot
#remove weekday,holiday,workingday,atemp,casual,registered


drop <- c("holiday","weekday","feel_temp","workingday","casual","registered")
data <- data[ , !names(data) %in% drop]
rm(drop)



#dummy variable(one hot) encoding---------------------
#install.packages("fastDummies")
library("fastDummies")

data <- dummy_cols(data,remove_first_dummy = TRUE)
data <- data[ , !names(data) %in% c("season","weather")]
data <- data[,c(1:5,7:11,6)]

#--------------------------------------------------------------------------------------
#sampling
set.seed(200)
sample_index <- sample(nrow(data), nrow(data)*0.20,replace = FALSE)
test_data <- data[sample_index,]
train_data <- data[-sample_index,]


#--------------------------------------MODEL BUILDING-------------------------------------------------------

#multiple linear regression
# Build the model
lm_model1 <- lm(count ~., data = train_data)
summary(lm_model1)


#predict
prediction_model1 <- predict(lm_model1,test_data[1:10])

#modelevaluation
#1.Root Mean Squared Error Loss 
RMSE = function(yhat,y_tru ){
  sqrt(mean((yhat - y_tru)^2))
}

RMSE(prediction_model1,test_data[,11])


#2.Root Mean Squared Logarithmic Error Loss 
RMSLE <- function(y_true, y_pred) {
  sqrt(mean((log1p(y_true)-log1p(y_pred))^2))
}

RMSLE(test_data[,11],prediction_model1)


#random forest algorithm-------------------------------------
library(randomForest)
rf_model1 <- randomForest(formula = count ~ ., data = train_data, importance = TRUE, ntree = 100)
print(rf_model1)

#predict
prediction_model2 <- predict(rf_model1,test_data[1:10])

#model evaluation
RMSE(prediction_model2,test_data[,11])
RMSLE(test_data[,11],prediction_model2)

#write to csv file
submit = data.frame(test_data, predicted_count = round(prediction_model2))
write.csv(submit,file = "submit_r.csv",row.names = FALSE)

#------------------------------------------------------------------------------------
