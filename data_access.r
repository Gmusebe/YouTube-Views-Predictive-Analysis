# YouTube Data Analysis:

# Setting the environment
# Explain the tuber package in depth
library(broom)
library(dplyr)
library(tibble)
library(tuber)
library(naniar)
library(ggplot2)
library(magrittr)
library(reshape2)
library(corrplot)
library(ggthemes)
library(tidyverse)
library(PerformanceAnalytics)

# After obtaining tokens, save them into two objects(client_id and client_secret).

client_id <- "201724301171-sina4isjr04n6f93a9mbcsjqrd3pgbmr.apps.googleusercontent.com"
client_secret <- "z0Czo17ooY-CDlqSsUh6D4jx"

# Now you can run tuber’s yt_oauth() function to authenticate your application. 
yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

# If for the first time:
# Output:
# httpuv not installed, defaulting to out-of-band authentication
# Enter authorization code: xxxxx

# Accessing YouTube data
# ______________________
# Marques Brownlee YouTube Channel:
# url "https://www.youtube.com/user/marquesbrownlee"
# Channel Id in R: UCBJycsmduvYEL83R_U4JriQ

marquesbrownlee_channel_stats <- get_channel_stats(channel_id = "UCBJycsmduvYEL83R_U4JriQ")
marquesbrownlee_channel_stat

# The Element in the List:
marquesbrownlee_channel_stats[[5]][[1]]

# Filter videos by date
videos = yt_search(term="", type="video", channel_id = "UCBJycsmduvYEL83R_U4JriQ")
videos = videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2016-01-01")

# Get all the videos from the channel:  
marquesbrownlee_video_stats <- tuber::get_all_channel_video_stats(channel_id = "UCBJycsmduvYEL83R_U4JriQ")

# Other Columns
marquesbrownlee_video_stats <- marquesbrownlee_video_stats %>% 
  add_column(subscriberCount = marquesbrownlee_channel_stats[["statistics"]][[2]],
             channelVideoCount = marquesbrownlee_channel_stats[["statistics"]][[4]],
             channelViewCount = marquesbrownlee_channel_stats[["statistics"]][[1]],
             channelCountry = marquesbrownlee_channel_stats[["snippet"]][[7]],
             PublishedYear = format(
               as.POSIXct(as.Date(marquesbrownlee_video_stats$publication_date), format = "%m/%d/%Y %H:%M:%S"),
               format="%Y"),
             ChannelAge = format(
               as.POSIXct(as.Date(marquesbrownlee_channel_stats[["snippet"]][[4]]), format = "%m/%d/%Y %H:%M:%S"),
               format="%Y")
             )

# Sort by published_date from most recent to least recent:
marquesbrownlee_video_stats <- marquesbrownlee_video_stats %>%
  add_column(
    PublishedDate = format(
      as.POSIXct(as.Date(marquesbrownlee_video_stats$publication_date), format = "%m/%d/%Y %H:%M:%S"),
      format="%m/%d/%Y")
  )
marquesbrownlee_video_stats <- marquesbrownlee_video_stats[rev(order(as.Date(marquesbrownlee_video_stats$PublishedYear.1, format="%m/%d/%Y"))),]

# Previous data columns.
marquesbrownlee_video_stats <- marquesbrownlee_video_stats %>%
  add_column(
    PrevCommentCount = lead(marquesbrownlee_video_stats$commentCount),
    PrevDislikeCount = lead(marquesbrownlee_video_stats$dislikeCount),
    PrevLikeCount = lead(marquesbrownlee_video_stats$likeCount),
    PrevPublishedAt = lead(marquesbrownlee_video_stats$publication_date),
    PrevViewCount = lead(marquesbrownlee_video_stats$viewCount),
    PrevTitle = lead(marquesbrownlee_video_stats$title),
  )

# Do the same for Oliur / UltraLinx and Dave2D
# Dave2D: https://www.youtube.com/channel/UCzJjUHizQfPYywqt1mSEMww,
# Oliur / UltraLinx: https://www.youtube.com/channel/UCVYamHliCI9rw1tHR1xbkfw

# Merge the three data sets to have the data we will use to predicts the number of views
tech_merged_channels <- merge(marquesbrownlee_video_stats, UltraLinx_video_stats, dave2D, all = TRUE)

library(readxl)
tech_merged_channels <- read_excel("merged_video_stats.xlsx")

# Data Structure:
str(tech_merged_channels)

# Numerical data:
i <- c(7, 8, 9, 10, 11, 13, 14, 15, 17, 18, 20, 21, 22, 24)
tech_merged_channels[, i] <- apply(tech_merged_channels[ , i], 2,   
                                   function(x) as.numeric(as.character(x)))

# Change the date to date:
tech_merged_channels$PublishedDate <- as.Date(tech_merged_channels$PublishedDate)

# Factor:
tech_merged_channels$channel_title <- as.factor(tech_merged_channels$channel_title)

# Check for any missing values:
summary(tech_merged_channels)

# To visualise this:
gg_miss_var(tech_merged_channels)

# Trends in missing values
gg_miss_upset(tech_merged_channels)

# Heatmap
vis_miss(tech_merged_channels)
# 0.3%

# Number of columns with missing values.
n_var_miss(tech_merged_channels)
# [1] 9

# Number of rows with missing values
sum(apply(tech_merged_channels, 1, anyNA))
# [1] 77

#Count of missing values per column:
data.frame(sapply(tech_merged_channels, function(y) sum(length(which(is.na(y))))))

# Missing data by Year
gg_miss_fct(x = tech_merged_channels, fct = PublishedYear) + 
  labs(title = "Year NA in YouTube Data")

# Missing data by Channel:
gg_miss_fct(x = tech_merged_channels, fct = channel_title) + 
  labs(title = "Channel NA in YouTube Data ")

# Remove missing values.
tech_merged_channels <- tech_merged_channels[complete.cases(tech_merged_channels), ]
# You can confirm this by running the line 249

# Visualization:
# Significant Outliers in Views:
library(ggpubr)
library(rstatix)

# Data Exploration:
# View the distribution of views:
tech_merged_channels %>%
  ggplot( aes(x=viewCount)) +
  geom_histogram(bins = 50, fill="#0c4c8a", color="#e9ecef", alpha=0.9) +
  theme_minimal() +
  labs(title = "YouTube View",
       subtitle = "YouTube Views from The marquesbrownlee, Dave2d, UltraLinx",
       x = "View Count",
       y = " ",
       caption = "YouTube")

# To see outliers clearly we shall have a boxplot:
tech_merged_channels %>%
  ggplot(aes(x="", y = viewCount)) +
  geom_boxplot(outlier.colour="red")
  theme_minimal()

# By Channel title:  
tech_merged_channels %>%
  ggplot(aes(x=viewCount, y = channel_title)) +
  geom_boxplot(outlier.colour="red") +
  theme_calc()+ scale_colour_calc() +
  ggtitle("YouTube Data") +
  labs(title = "YouTube View Outliers",
       subtitle = "YouTube Views from The marquesbrownlee, Dave2d, UltraLinx",
       x = "View Count",
       y = "Channel",
       caption = " ©YouTube")

# To check outliers:
Outliers <-  tech_merged_channels %>%
  group_by(channel_title) %>%
    identify_outliers(viewCount)

# Significant outliers i.e. is.extreme = TRUE
Significant_Outliers <- Outliers[which(Outliers$is.extreme == TRUE),]

#Plotting the data with and without the Outliers comparing them with like counts
par(mfrow=c(1, 2))
plot(tech_merged_channels$viewCount, tech_merged_channels$likeCount,
     main="With Outliers",
     xlab = "View Count",
     ylab = "Like Count",
     pch="*",
     col="red", cex=2)
abline(lm(likeCount ~ viewCount, data=tech_merged_channels), col="blue", lwd=3, lty=2)

# Without
Without_Outliers_data <- tech_merged_channels[-which(tech_merged_channels$viewCount %in% c(Outliers$viewCount)),]
plot(Without_Outliers_data$viewCount, Without_Outliers_data$likeCount,
     main="Without Outliers",
     xlab = "View Count",
     ylab = "Like Count",
     pch="*",
     col="red", cex=2)
abline(lm(likeCount ~ viewCount, data=Without_Outliers_data), col="blue", lwd=3, lty=2)

# Correlation of numerical data:
# _____________________________
data <- Without_Outliers_data[,c("viewCount","likeCount","dislikeCount",
                         "commentCount", "subscriberCount", "channelVideoCount",
                         "channelViewCount", "PrevCommentCount", "PrevDislikeCount",
                         "PrevLikeCount", "PrevViewCount")]

date_data <- data.frame(
  # Find the number days in between posting videos:
  Days = as.numeric(difftime(strptime(format(
    as.POSIXct(as.Date(Without_Outliers_data$publication_date), format = "%m/%d/%Y %H:%M:%S"),
    format="%m/%d/%Y"), format = "%m/%d/%Y"),strptime(format(
      as.POSIXct(as.Date(Without_Outliers_data$PrevPublishedAt), format = "%m/%d/%Y %H:%M:%S"),
      format="%m/%d/%Y"), format = "%m/%d/%Y"), units = "days")),
  
  # Find the number of hours in between posting videos:
  Hours_published = as.numeric(difftime(strptime(format(
    as.POSIXct(as.Date(Without_Outliers_data$publication_date), format = "%m/%d/%Y %H:%M:%S"),
    format="%m/%d/%Y"), format = "%m/%d/%Y"),strptime(format(
      as.POSIXct(as.Date(Without_Outliers_data$PrevPublishedAt), format = "%m/%d/%Y %H:%M:%S"),
      format="%m/%d/%Y"), format = "%m/%d/%Y"), units = "hours")),
  
  # Age of Channel to Video publication:
  Age_Channel = Without_Outliers_data$PublishedYear - Without_Outliers_data$ChannelAge)


data <- add_column(data, date_data, .after = 11)

# Correlation heatmap
cormat <- round(cor(data), 2)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# For Correlation significance:
PerformanceAnalytics::chart.Correlation(data, pch=19)

# The view counts by day:
tech_merged_channels %>%
  ggplot(aes(x=PublishedDate,y=viewCount)) +
  geom_line(color="steelblue", size = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FF0000")) +
  labs(title = "YouTube Videos Posted Trend from 2008 - 2021",
       caption = " Source: YouTube") +
  xlab("Date") + ylab("View Count") +
  theme_minimal()

# There has been an increase in views with the age of the account.

# By individual accounts:
tech_merged_channels %>% ggplot(aes(x = PublishedDate , y = viewCount, color = channel_title)) +
  geom_line() +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#FF0000")) +
  labs(title = "YouTube Videos Posted Trend from 2008 - 2021",
       subtitle = "Trends by Channels",
       caption = " Source: YouTube") +
  xlab("Date") + ylab("View Count") +
  theme_minimal()

# Time Visualization:
library(lubridate)
time <- data.frame( Date = gsub("T", " ", tech_merged_channels$publication_date))
time <- data.frame(Date = as.POSIXct(gsub("Z", "", time$Date), format = "%Y-%m-%d %H:%M:%S"))

# Time of Day
time <- time %>% 
  add_column(Time = format(time$Date, format = "%H:%M:%S"),
             # Time of Day
             TOD = with(time, ifelse(Time > "053000" & Time < "120000", "Morning",
                                ifelse(Time >= "120000" & Time < "170000", "Afternoon",
                                       ifelse(Time >= "170000" & Time < "200000", "Evening", "Night")))),
             # Day of Week
             DOW = weekdays(time$Date, abbreviate = F),
             year = tech_merged_channels$PublishedYear,
             channel_title = tech_merged_channels$channel_title
             )

# Count by Day of the week & Time of Day:
time$DOW <- factor(time$DOW, levels= c("Sunday", "Monday", 
                                        "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
time %>%
  ggplot(aes(x=DOW, fill = TOD)) + geom_bar(position = "stack") +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" ) +
  labs(title = "YouTube Daily Trend",
       subtitle = "Daily Trends by Time of Week",
       caption = " Source: ©YouTube") +
  xlab("") + ylab("View Count")

# By DOW by Channel:
time %>%
  ggplot(aes(x=DOW, fill = channel_title)) + geom_bar(position = "stack") +
  coord_flip() +
  scale_fill_brewer( palette = "YlGnBu" ) +
  theme_minimal() + theme( legend.position = "bottom" ) +
  labs(title = "YouTube Daily Trend",
       subtitle = "Daily Trends by Channel",
       caption = " Source: ©YouTube") +
  xlab("") + ylab("View Count")


# Model:
# Install and load the random forest package:
install.packages(c("randomForest", "rsample", "ranger","caret"))

# Load data:
library(randomForest)
library(rsample)
library(ranger)
library(caret)

# Create training (70%) and test (30%) sets for the YouTube data:
# Use set.seed for reproducibility
set.seed(123)
youtube_split <- initial_split(data, prop = .7)
youtube_train <- training(youtube_split)
youtube_test <- testing(youtube_split)

# Default RF model:
set.seed(123)
YouTube.rf1 <- randomForest(
  formula = viewCount ~ .,
  data = youtube_train)

YouTube.rf1

# Plot Model:
plot(YouTube.rf1)

# Mean Squared Error
YouTube.rf1$mse

# number of trees with lowest MSE
which.min(YouTube.rf1$mse)
# 475

# RMSE of this optimal random forest
sqrt(YouTube.rf1$mse[which.min(YouTube.rf1$mse)])
# 523149.6


# Validation:
# create training and validation data 
set.seed(123)
valid_youtube_index <- rsample::initial_split(youtube_train, .8)

# training data
valid_youtube_train <- analysis(valid_youtube_index)

# validation data
youtube_valid <- assessment(valid_youtube_index)

# Supply the validation data in the xtest and ytest arguments:
x_test <- youtube_valid[setdiff(names(youtube_valid), "viewCount")]
y_test <- youtube_valid$viewCount

rf_oob_comp <- randomForest(
  formula = viewCount ~ .,
  data    = valid_youtube_train,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
validation <- sqrt(rf_oob_comp$test$mse)

# compare error rates:
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, RMSE, -ntrees) %>%
  ggplot(aes(ntrees, RMSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(breaks=c(523000,580000, 600000, 700000)) +
  xlab("Number of trees")

# Tuning
# names of features
features <- setdiff(names(youtube_train), "viewCount")

set.seed(123)

YouTube.rf2 <- tuneRF(
  x          = youtube_train[features],
  y          = youtube_train$viewCount,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)

# -0.02509524 0.01 
# -0.005616687 0.01

plot(YouTube.rf2)
lines(YouTube.rf2, col="blue")

# Larger grid search
## hyperparameter grid search:
hyper_grid <- expand.grid(
  mtry       = seq(2, 9, by = 2),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = viewCount ~ ., 
    data            = youtube_train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

# Best model mtry = 8, nodes = 5 and sample of 80%
# Lets repeat this model to get a better expectation of our error rate:
OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = viewCount ~ ., 
    data            = youtube_train, 
    num.trees       = 500,
    mtry            = 8,
    min.node.size   = 5,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)

# Variable importance:
optimal_ranger$variable.importance %>% 
  broom::tidy() %>%
  dplyr::arrange(desc(x)) %>%
  dplyr::top_n(7) %>%
  ggplot(aes(reorder(names, x), x)) +
  geom_col() +
  coord_flip() +theme_bw() +
  ggtitle("Top 5 important variables")

# Predicting:
ames_ranger <- ranger(formula   = viewCount ~ ., 
                      data      = youtube_train,
                      num.trees = 500,
                      min.node.size = 5,
                      sample.fraction = 0.8,
                      mtry = 8)

# Predicting:
prediction <- predict(ames_ranger, youtube_test,type='response')
mean(table(prediction, youtube_test))










pred_randF <- predict(rf, newdata = youtube_test[-1])


# For the model accuracy using the caret package:
