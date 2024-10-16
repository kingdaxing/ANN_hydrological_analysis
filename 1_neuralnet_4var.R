# packages and inputs #####
library(ggplot2)
devtools::install_github("bbc/bbplot",force=TRUE)
# library(bbplot) -> bb_plot()

setwd('D:/PXX-NUS/05-S2 NUS CE in SCR/NEWS/04-CE5310-Hydroinformatics/HW5_ANN')
library(openxlsx)
krevents <- read.xlsx("kentridgerrdata-55events.xlsx", sheet="Selected Events")  # read the 2nd sheet
colnames(krevents)[2:7] <- c("Rainfall", "Q_MD01", "Q_MD02", "Q_MD04", "Q_CNTRLIB", "Q_OPPRLINK")
# unit: Q [L/s]; Rainfall[mm]
rainfall <- krevents[, 2]
Q01 <- krevents[, 3]
Q02 <- krevents[, 4]
Q04 <- krevents[, 5]
# ignore: Qcenter <- krevents[, 6]
Qopp <- krevents[, 7]
df=data.frame(rainfall, Q01, Q02, Qopp, Q04)  # chosen 4 variables vs Q4

#1) neuralnet package #####
library(neuralnet)
xnum=length(krevents[,1])
krevents[,15] <- seq(1:xnum)
colnames(krevents)[15] <- c("xaxis")

# 55-events is seen as a continue events
ggplot(data=krevents, aes(x = xaxis, group = 1))+
  geom_line(aes(y=Q_MD01, col="Q01"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=Q_MD02, col="Q02"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=Q_MD04, col="Q04"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=Q_CNTRLIB, col="Qcenter"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=Q_OPPRLINK, col="Qopp"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=Rainfall, col="Rainfall"), linewidth = 1, na.rm = TRUE)+
  labs(title="Rainfall-Runoff in one continue event", subtile="2011/4/9 (recorded per min)",
       x="time", y="Discharge and Rainfall data", color = "Condition" ) +
  scale_color_manual(values = c("Q01" = "pink", "Q02" = "blue", "Q04" = "green", 
                                "Qcenter" = "black", "Qopp" = "orange", "Rainfall" = "red"))+
  theme_minimal()

# splitting #####
# sample size = 80% test + 20% train [default]
xnum=length(krevents[,1])
train_size <- round(0.8 * xnum)
# with time, the data is not random
train <- df[0:train_size,1:5]
test <- df[(train_size + 1):xnum,1:5]
pre_de_test <- df[(train_size + 1):xnum,1:5]
dim(train)
dim(test)

#normalisation
min_max <- function(x){
  y<-(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) - min(x, na.rm=TRUE))
  return (y)
} 

train=as.data.frame(lapply(train, min_max))
test=as.data.frame(lapply(test, min_max))

# model1 - 2 hidden layers with 5 and 3 neurons (trials)
# logistic sigmoid function; (rainfall, Q01, Q02, Qopp, Q04)
nn<- neuralnet(Q04 ~ rainfall+Q01+Q02+Qopp, data=train, 
               hidden=c(4,3), linear.output=FALSE, threshold=0.01)
plot(nn)
nn$result.matrix
# ?neuralnet()

# prediction ####
# testing
temp_test<- subset(test, select=c("rainfall", "Q01", "Q02", "Qopp"))
nn.results <- compute(nn, temp_test)
actual=test$Q04
prediction=nn.results$net.result
# denormalise
denormalize <- function(y, min_value, max_value){
  x <- y * (max_value - min_value) + min_value
  return(x)
}

prediction_de <- denormalize(prediction, min_value =min(pre_de_test$Q04), max_value = max(pre_de_test$Q04))
actual_de <-  denormalize(actual,  min_value =min(pre_de_test$Q04), max_value = max(pre_de_test$Q04))

#accuracy
deviation=(actual_de - prediction_de)/actual_de
accuracy_rate=1-abs(mean(deviation)) 
accuracy_rate
rmse=(sum((actual_de-prediction_de)^2)/nrow(test))^0.5 
rmse
cor(actual,prediction)

# test plot
xaxis=length(actual_de)
comparison=data.frame(prediction_de, actual_de, deviation, index=1:xaxis)

ggplot(data=comparison, aes(x = index, group = 1))+
  geom_line(aes(y=actual_de, col="actual"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=prediction_de, col="predicted"), linewidth = 0.5, na.rm = TRUE)+
  labs(title="Actual vs. Predicted - Q_04 by 'neuralnet' ",
       x="Index - (20%) Test dataset", y="Discharge Q_04", color = "Condition" ) +
  scale_color_manual(values = c("actual" = "blue", "predicted" = "red"))+
  theme_minimal()


# extract 10-, 20-, 60-mins data as test 




