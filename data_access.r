# YouTube scrapping:

# Setting the environment
# Explain the tuber package in depth
library(tuber)
library(magrittr)


# From API setting and authorising te OAurtho cliennt and obtained the credentials 
# register the credentials. Read the API and OAurtho instriction to have an in depth knowlwdge of the same.
# Save them into two objects in RStudio (client_id and client_secret).

client_id <- "201724301171-sina4isjr04n6f93a9mbcsjqrd3pgbmr.apps.googleusercontent.com"
client_secret <- "z0Czo17ooY-CDlqSsUh6D4jx"

# Now you can run tuber’s yt_oauth() function to authenticate your application. 
yt_oauth(app_id = client_id,
         app_secret = client_secret,
         token = '')

# Output:
# if for the first time:
# httpuv not installed, defaulting to out-of-band authentication
# Enter authorization code: 4/1AY0e-g60CVX6AZ00HqOehHm_TjcKcLw2sZ9lbIW7j2OzOXHO1uvRvxHSeVY

# You will be sent an authentication code that you will enter to approve you are an added user
# sent via google login.

# Accessing YouTube data
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

# Statistics on a single video
# ____________________________
# From a youtube video R Programming it's link  is https://www.youtube.com/watch?v=_V8eKsto3Ug
# To get the video stats copy the id which is after the "=" sighn and use the function beow to have it'ts stats
get_stats(video_id = "_V8eKsto3Ug")

# get all comments of a video:
comments <-get_all_comments(video_id = "_V8eKsto3Ug")

# Get all the captions:
# Note: It was previously possible to get captions for all videos that had 
# “Community contributions” enabled. However, since YouTube removed that option in
# September 2020, the get_captions function now only works for videos created with 
# the same account as the API credentials you use. An alternative for collecting
# YouTube video captions is the youtubecaption package.
library('youtubecaption')

# In the tuber this wo
captions <- get_captions(video_id = "-6RqxhNO2yY")
# For the second video:
url <- "https://www.youtube.com/watch?v=6lBPM-MiGNI&list=PLeo1K3hjS3us_ELKYSj_Fth2tIEkdKXvV&index=3"
caption <- get_caption(url)
caption

# Get video details:
vid_details <- tuber::get_video_details(video_id = "-6RqxhNO2yY")
# In a data.frame:
for (i in seq_along(vid_details)) {
  colnames(vid_details[[i]]) = paste0('V', seq_len(ncol(vid_details[[i]])))
}

vid_details = do.call(rbind, vid_details)

# Channels
# _____________________________________________________________________
# Lets check out Marques Brownlee YouTube Channel:
# url <- "https://www.youtube.com/user/marquesbrownlee"

# Get Channel Id in R: UCBJycsmduvYEL83R_U4JriQ
marquesbrownlee_channel_stats <- get_channel_stats(channel_id = "UCBJycsmduvYEL83R_U4JriQ")
marquesbrownlee_channel_stats = do.call(rbind.data.frame, marquesbrownlee_channel_stats)

# Get all the videos from the channel:
videos = yt_search(term="", type="video", channel_id = "UCBJycsmduvYEL83R_U4JriQ")
videos = videos %>%
  mutate(date = as.Date(publishedAt)) %>%
  filter(date > "2016-01-01")

marquesbrownlee_video_stats <- tuber::get_all_channel_video_stats(channel_id = "UCBJycsmduvYEL83R_U4JriQ")

# To search for videos with a phrase e.g aws:
aws_yt_videos <- yt_search("aws")

# find channels from which videos of playlist are from:
# Mixed Playlist url: 