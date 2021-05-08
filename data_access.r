# YouTube Data Analysis:

# Setting the environment
# Explain the tuber package in depth
library(dplyr)
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
  geom_histogram(bin = 50, fill="#0c4c8a", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
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

date_data <- data.frame(Before = format(
  as.POSIXct(as.Date(Without_Outliers_data$PrevPublishedAt), format = "%m/%d/%Y %H:%M:%S"),
  format="%m/%d/%Y"),
  After = format(
    as.POSIXct(as.Date(Without_Outliers_data$publication_date), format = "%m/%d/%Y %H:%M:%S"),
    format="%m/%d/%Y"))


# Find the number days in between posting videos:
data$Days <- as.numeric(difftime(strptime(date_date$After, format = "%m/%d/%Y"),
                  strptime(date_date$Before, format = "%m/%d/%Y"), units = "days"))

# Channel Age in Years:
data$ChannelAge <- Without_Outliers_data$PublishedYear - Without_Outliers_data$ChannelAge

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

library(lubridate)
time_data <- data.frame(Before = format(
  as.Date(tech_merged_channels$PrevPublishedAt, format = %H:%M:%S"),
  format="%H:%M:%S")
  )

format(
  as.POSIXct(as.Date(Without_Outliers_data$PrevPublishedAt), format = "%m/%d/%Y %H:%M:%S"),
  format="%m/%d/%Y")


# ______________________________________________________________________________________________
# 1. How to download playlists
# _____________________________
# Let us download the playlist data for Data Science Full Course For Beginners | Python Data Science Tutorial | Data Science With Python
# from the codebasics

codebasics_datascience_playlist <- stringr::str_split(
  string = "https://www.youtube.com/playlist?list=PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV", 
  pattern = "=", 
  n = 2,
  simplify = TRUE)[ , 2]
codebasics_datascience_playlist



# Returns the playlist ID
# [1] "PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV"

# now we can use the tuber::get_playlist_items() with the playlist ID to collect the videos into a data.frame.
codeBasicsDS <- tuber::get_playlist_items(filter = 
                                            c(playlist_id = "PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV"), 
                                          part = "contentDetails",
                                          # set this to the number of videos
                                          max_results = 200)

# This returns all the videos in the playlist.
# Check that only one video is each row. Knowing that the playslist has 111 videos we expect 111 rows.
# Check unique data:
# We should check these data to see if there is one row per video from the playlist
# # check the data for Data Science
codeBasicsDS %>% dplyr::glimpse(78)

# Collecting statistics from a YouTube playlist
# Get ID's of the videos:
codeBasicsDS_id <- base::as.vector(codeBasicsDS$contentDetails.videoId)

# View the IDs
dplyr::glimpse(codeBasicsDS_id) # Returns the first eight video ID. 
video_ID <- data.frame(codeBasicsDS_id)

# a function that extracts the statistics for each video on the playlist
# Function to scrape stats for all vids
videostats = lapply(as.character(video_ID$codeBasicsDS_id), function(x){
  get_stats(video_id = x)
})
videostats = do.call(rbind.data.frame, videostats)

codeBasicsDS_stats_data <- merge(codeBasicsDS, videostats, by.x = "contentDetails.videoId", 
                                 by.y = "id", all = TRUE)



# To search for videos with a phrase e.g aws:
aws_yt_videos <- yt_search("aws")

# find channels from which videos of playlist are from:
# Mixed Playlist url: https://www.youtube.com/watch?v=mC-zw0zCCtg&list=PLIVbZhwLbExKoDlYntZV_Y1c3bZYz7hAs