# packages and inputs #####
library(reticulate)
use_condaenv("C:/Users/DELL/anaconda3/envs/r_reticulate")
library(keras)
library(tensorflow)
library(ggplot2)
library(rsample)

setwd('D:/PXX-NUS/05-S2 NUS CE in SCR/NEWS/04-CE5310-Hydroinformatics/HW5_ANN')
library(openxlsx)
krevents <- read.xlsx("kentridgerrdata-55events.xlsx", sheet="Selected Events")  # read the 2nd sheet
colnames(krevents)[2:7] <- c("Rainfall", "Q_MD01", "Q_MD02", "Q_MD04", "Q_CNTRLIB", "Q_OPPRLINK")

# 2. keras #####
# initial plot  ####
xnum=length(krevents[,2])
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


# differencing #####
# differencing is a way to make a non stationary time series
# unit: Q [L/s]; Rainfall[mm]
d_Q04 = diff(krevents$Q_MD04, differences = 1)

undiff_Q04 <- numeric(length(d_Q04) + 1)
undiff_Q04[1] <- krevents$Q_MD04[1]
for (i in 2:length(undiff_Q04)) {
  undiff_Q04 [i] <- undiff_Q04 [i - 1] + d_Q04[i - 1]
}

plot(krevents$Q_MD04, type="l", col="red", lwd=2)
plot(undiff_Q04, type="l", col="blue", lwd=2)
# undiff_Q is the same as diff()
# a tiny discrepancy occurs between the original series and its inverse first differences 
# due to rounding.
# thus, the diff() is not necessary for this case, and discharge is analyzed directly by an explicit way.

## data for ANN (specifically for RNNs)#####
rainfall <- krevents[, 2]
Q01 <- krevents[, 3]
Q02 <- krevents[, 4]
Q04 <- krevents[, 5]
Qcenter <- krevents[, 6]
Qopp <- krevents[, 7]
df=data.frame(rainfall, Q01, Q02, Qopp, Q04)  # chosen 4 variables vs Q4
# data set-> df

# wrap function 
get_wrapped <- function(dataset, wrap_length = 10) {
  data_x <- list()
  data_y <- list()
  
  for (i in 1:(nrow(dataset) - wrap_length)) {
    # Extract input matrix for each sample
    input_matrix <- dataset[i:(i + wrap_length - 1), c("rainfall", "Q01", "Q02", "Qopp")]
    data_x[[i]] <- as.matrix(input_matrix)
    
    # Extract target for each sample
    target <- dataset[i + wrap_length, "Q04"]
    data_y[[i]] <- target
  }
  
  return(list(data_x = data_x, data_y = data_y))
}
# call wrap function to prepare data for LSTM model
wrap_length <- 10
wrapped_data <- get_wrapped(df, wrap_length = 10)
data_x <- wrapped_data$data_x
length(data_x)  # data_x: (10330 - wrap_length) samples, each sample has xx time steps and 4 features
data_y <- wrapped_data$data_y


# splitting #####
# sample size = 80% test + 20% train
N = nrow(krevents) # 10330
n = round(N *0.8, digits = 0) 
train_x <- data_x[1:n]    # 8264
test_x <- data_x[n:(N-wrap_length),] # 2066 
train_y <- data_y[1:n]    # 8264
test_y <- data_y[seq(n+1,N,1),] # 2066


# ranging into [0,1]
normalize <- function(train, test, feature_range = c(0, 1)) {
  num_features = ncol(train)
  scaled_train = train
  scaled_test = test
  
  for (i in 1:num_features) {
    x = train[, i]
    fr_min = feature_range[1]
    fr_max = feature_range[2]
    std_train = ((x - min(x)) / (max(x) - min(x)))
    std_test = ((test[, i] - min(x)) / (max(x) - min(x)))
    
    scaled_train[, i] = std_train * (fr_max - fr_min) + fr_min
    scaled_test[, i] = std_test * (fr_max - fr_min) + fr_min
  }
  
  return(list(scaled_train = scaled_train, scaled_test = scaled_test, scaler = list(min = apply(train, 2, min), max = apply(train, 2, max))))
}


Scaled_x = normalize(train_x, test_x, c(0, 1))
Scaled_y = normalize(train_y, test_y, c(0, 1))
# df (rainfall, Q01, Q02, Qopp, Q04)
y_train = Scaled_y$scaled_train[, 5]
x_train = Scaled$scaled_train[, seq(1:4)]

y_test = Scaled$scaled_test[, 5]
x_test = Scaled$scaled_test[, seq(1:4)]
class(x_train)
dim(x_train)


# reshaping #####
x_train=as.matrix(x_train)
y_train=as.matrix(y_train)

x_test=as.matrix(x_test)
y_test=as.matrix(y_test)
class(x_train)
dim(x_train)

# keras model setup - MLP #####
# 1) dense (fully connected) layers
model = keras_model_sequential() %>%
  layer_dense(units = 4, activation = 'relu', input_shape = dim(x_train)[2]) %>%  # Input layer #https://keras.io/activations/
  layer_dense(units = 8, activation = 'relu') %>%  # Hidden layer
  layer_dense(units = 1)  # Output layer

#model compile
model %>% compile(loss = 'mse',
                  optimizer = 'adam', #optimizer_rmsprop(lr = 0.001)
                  metrics = list("mean_absolute_error")
)

# 2) Long Short-Term Memory Network (LSTM):
x_train_reshaped <- array(x_train, dim = c(dim(x_train)[1], dim(x_train)[2], 1))
model = keras_model_sequential() %>%
  layer_lstm(units = 8, dropout = 0.1, recurrent_dropout = 0.4, return_sequences = TRUE,
             input_shape = list(NULL, dim(x_train_reshaped)[[length(dim(x_train_reshaped))]])) %>%  # Input layer
  #layer_lstm(units = 8, activation="relu", 
  #           dropout=0.1, recurrent_dropout=0.5) %>%  # Hidden layer
  layer_dense(units = 1) # Output layer

model %>% compile(loss="mse",
                  optimizer = "rmsprop",
                  metrics = list("mean_absolute_error")
  )

# summary
summary(model)
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 40, min_delta=0.01)

# fit model
result<-model %>% fit(x_train, y_train, epochs=60, verbose=1, 
                      validation_split = 0.2, callbacks = list(early_stop))  
## blue one always smaller than green one, as the 
#model %>% save_model_hdf5("model_lstm_lay.h5")
# model %>% save_model_hdf5("model_dense_expli_4v_betterr.h5")
plot(result)
# call: loaded_model <- load_model_hdf5("model_lstm_lay.h5")
# model <- load_model_hdf5("model_dense_expli_4v_better.h5")

# evaluate #####
scores = model %>% evaluate(x_test, y_test, verbose = 0)
print(scores)

predictions <- model %>% predict(x_test)

denormalize <- function(scaled_data, min_val, max_val, feature_range = c(0, 1)) {
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  # Denormalize the data
  original_data = (scaled_data - fr_min) / (fr_max - fr_min) * (max_val - min_val) + min_val
  
  return(original_data)
}

denormalized_predictions <- denormalize(predictions, Scaled$scaler$min[5], Scaled$scaler$max[5], c(-1, 1))
actual_data <- test[, 5] 
# Comparison plot for actual & predicted
comp_act_pred <- data.frame(actual_data, denormalized_predictions, index=1:length(actual_data))
ggplot(data=comp_act_pred, aes(x = index, group = 1))+
  geom_line(aes(y=actual_data, col="actual"), linewidth = 1, na.rm = TRUE)+
  geom_line(aes(y=denormalized_predictions, col="predicted"), linewidth = 0.5, na.rm = TRUE)+
  labs(title="Actual vs. Predicted - Q_04",
       x="Index - (20%) Test dataset", y="Discharge Q_04", color = "Condition" ) +
  scale_color_manual(values = c("actual" = "blue", "predicted" = "red"))+
  theme_minimal()

# accuracy #####
deviation=(actual_data - denormalized_predictions)/actual_data
# deviation
accuracy_rate=1-abs(mean(deviation)) # total
accuracy_rate

plot(1:length(deviation), deviation, type="l", col="red", xlab="index",ylab="deviation")

rmse<- sqrt(sum((actual_data-denormalized_predictions)^2)/nrow(test)) # root mean square error
rmse
cor(actual_data, denormalized_predictions)


# group by 10-, 20-, 60-min
calculate_group_acc <- function(deviation, group_size) {
  data <- deviation
  total_samples <- length(data)
  num_groups <- total_samples %/% group_size
  group_means <- numeric(num_groups)
  accuracy_rate <- numeric(num_groups)
  
  for (i in 1:num_groups) {
    start_index <- (i - 1) * group_size + 1
    end_index <- i * group_size
    group_data <- data[start_index:end_index]
    group_data_valid <- group_data[is.finite(group_data)]
    group_means[i] <- mean(group_data_valid)
    accuracy_rate[i]=1-abs((group_means[i]))
  }
  
  return(accuracy_rate)
}


group_10min <- calculate_group_acc(deviation, 10)
group_20min <- calculate_group_acc(deviation, 20)
group_60min <- calculate_group_acc(deviation, 60)
plot(group_10min, col = 'red', lwd = 1, type="l", xlab="Index (by gruop)", ylab="accuracy rate")
lines(group_20min, col = 'blue', lwd = 1)
lines(group_60min, col = 'green', lwd = 1.5)


# boxplot #####
# Combine into df
df_acc <- data.frame(
  value = c(group_10min, group_20min, group_60min),
  group = factor(rep(1:3, c(206, 103, 34)))
)
group_names <- c("10-min forecast", "20-min forecast", "60-min forecast")
df_acc$group <- factor(df_acc$group, levels = 1:3)

ggplot(df_acc, aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  labs(x = "Index (by gruop)", y = "accuracy rate") +
  ggtitle("Boxplot of Accuracy in Different Leading time") 
