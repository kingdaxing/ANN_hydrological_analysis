# The plot_split() takes one split and return a visual of the sampling strategy.

library(dplyr)
library(ggplot2)

rainfall <- krevents[, 2]
Q01 <- krevents[ 3]
Q02 <- krevents[, 4]
Q04 <- krevents[, 5]
Qcenter <- krevents[, 6]
Qopp <- krevents[, 7]
df=data.frame(rainfall, Q01, Q02, Qopp, Q04)  # chosen 4 variables vs Q4
N = nrow(df) # 10330
n = round(N *0.75, digits = 0) # 8264
# custom the split range

df <- df %>%
  mutate(Index = 1:nrow(df))  # index
Q4 = df[,5:6]
Q4_train = df[0:n,5:6]
Q4_test = df[seq(n+1,N,1),5:6] # 2066 

# define a function to plot the split
plot_split <- function(train_data, test_data, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # combine train and test data
  data_manipulated <- bind_rows(
    train_data %>% mutate(key = "training"),
    test_data %>% mutate(key = "testing")
  )
  
  # time index
  train_time_summary <- train_data %>%
    summarise(start = min(Index), end = max(Index))
  
  test_time_summary <- test_data %>%
    summarise(start = min(Index), end = max(Index))
  
  # ggplot
  g <- data_manipulated %>%
    ggplot(aes(x = Index, y = Q04, color = key)) +
    geom_line(linewidth = size, alpha = alpha) +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red")) +
    labs(
      title = glue::glue("Split: Train (75%) and Test(25%) "),
      y = "Q_MD04", x = "Index"
    ) +
    theme(legend.position = "none")
  
  if(expand_y_axis) {
    data_summary <- rbind(train_data, test_data) %>%
      summarise(start = min(Index), end = max(Index))
    
    g <- g +
      scale_x_continuous(limits = c(data_summary$start, data_summary$end))
  }
  
  return(g)
}

# visualize the split
plot_split(Q4_train , Q4_test)



