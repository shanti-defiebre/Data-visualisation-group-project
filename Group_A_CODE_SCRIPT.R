#BIO319 Group A

#Install and load tidyverse packages
install.packages("tidyverse") #Package for data tidying and reshaping
install.packages("dplyr") #Package for data manipulation
library(tidyverse)
library(dplyr)
library(ggplot2) #Load the ggplot2 package to give graphical images
#Install and load packages to represent data on a map
install.packages("rnaturalearthdata")
install.packages("sf")
library(rnaturalearthdata)
library(sf)
#Install and load packages to combine graphs
install.packages("patchwork")
library(patchwork)
# Install and load package for handling dates
install.packages("lubridate")
library(lubridate) #For handling dates

#Read Spotify data into a data frame called "spotify_songs"
spotify_songs <- tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'))

#View the data
View(spotify_songs)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Hannah Florence Catherine Behan (200463443)

# Preprocess the data
# Convert the release date to a Date object and extract the year
spotify_songs <- spotify_songs %>%
  mutate(Year = year(as.Date(track_album_release_date, format="%Y-%m-%d"))) %>%
  filter(Year >= 1960 & Year <= 2020)  # Filter for the desired year range

# Summarize the data
# Group by Year and calculate the average valence
spotify_songs_summary <- spotify_songs %>%
  group_by(Year) %>%
  summarise(Avg_Valence = mean(valence, na.rm = TRUE))

# Create the line graph
# Plot the average valence per year
linegraph <- 
  ggplot(spotify_songs_summary, aes(x = Year, y = Avg_Valence)) +
  geom_line() +  # Connect points with lines
  geom_point() +  # Show individual points for each year
  scale_x_continuous(breaks = seq(1960, 2020, by = 5)) +  # X-axis labels every 5 years
  labs(title = "Trend in Music Valence Over Time", x = "Year", y = "Average Valence") +
  theme_minimal()  # Minimal theme for a clean look

ggsave(filename = "linegraph.png", plot = linegraph, width = 300, height = 200, units = "mm")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fiza Ali (210268513)

#format to give a year column and only make data numeric

spotify_songs$track_album_release_date <- as.Date(spotify_songs$track_album_release_date, format="%Y-%m-%d")
spotify_songs$year <- format(spotify_songs$track_album_release_date, "%Y")
#Converts the track_album_release_date column in the spotify_songs data frame to Date objects, assuming dates are in the "YYYY-MM-DD" format.#
#Extracts the year from the track_album_release_date column of each record in spotify_songs and stores it in a new column named year.

# Assuming spotify_songs_summary_filtered is your initial dataframe
# First, ensure the 'year' column is numeric
spotify_songs$year <- as.numeric(spotify_songs$year)

# Then, filter rows where the 'year' is between 2005 and 2015
# The code filters the spotify_songs data frame to include only those records where the release year of the track's album (as indicated in the year column) falls between 2005 and 2015, inclusive. 
#The filtered dataset is then stored in a new data frame called spotify_filtered_data.
spotify_filtered_data <- spotify_songs %>%
  filter(year >= 2005 & year <= 2015)

# Display the first few rows of the filtered data
head(spotify_filtered_data)

# Ensure the release date is treated as an integer
# This code updates spotify_filtered_data by grouping it by playlist_genre and year, calculating the mean valence for each group while excluding missing values, 
#It  then arranging the summarized results in ascending order by year and playlist_genre.
#The result is a streamlined dataset that facilitates analysis of how average musical positivity (valence) varies across different genres and over the years.
spotify_filtered_data <- spotify_filtered_data %>%
  group_by(playlist_genre, year) %>%
  summarise(average_valence = mean(valence, na.rm = TRUE), .groups = 'drop') %>%
  arrange(year, playlist_genre)
# Arrange for consistent plotting

# Assuming spotify_songs_summary is already prepared as shown in your previous code


# Create the heatmap
#use geom_tile to create a heatmap. each tiles location is determined by  by 'year' (x-axis) and 'playlist_genre' (y-axis),
# and its color represents 'average_valence'.
heatmap <- 
  ggplot(spotify_filtered_data, aes(x = as.factor(year), y = playlist_genre, fill = average_valence)) +
  geom_tile() + 
  # Use geom_tile for the heatmap tiles
  scale_fill_gradient2(low = "red", mid = "pink", high = "yellow", midpoint = median(spotify_filtered_data$average_valence), name = "Valence") +
  labs(x = "Release Year", y = "Playlist Genre", title = "Heatmap of Average Valence by Playlist Genre and Release Year") +
  #Define the color gradient for the tiles. 'low' is the color for low values, 'high' for high values, and 'mid' for the median.
  # 'midpoint' specifies the value that should be considered the middle of the scale (in this case, the median of average_valence),
  # adjusting the color gradient accordingly. This helps in visualizing the distribution of valence across genres and years.
  
  
  # Apply a minimalistic theme for a cleaner look.
  theme_minimal() +
  # # Rotate the x-axis text for better readability, especially useful when there are many years and the labels might overlap.
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Find the most happy genre in each year based on average valence
most_happy_genre_each_year <- spotify_filtered_data %>%
  group_by(year) %>%
  slice_max(order_by = average_valence, n = 1) %>%
  ungroup()

# View the results
print(most_happy_genre_each_year)
#results show that latin music was considered the most happiest music in all but in 2005, EDM was considered the happiest presenting to be an anomaly

ggsave(filename = "heatmap.png", plot = heatmap, width = 300, height = 200, units = "mm")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Dalia El-Shoura (200034287)

#Tidy Spotify data
spotify_songs <- spotify_songs %>%
  mutate(playlist_genre = ifelse(playlist_genre == "edm", toupper(playlist_genre), #Change "edm" to full uppercase
                                 str_to_title(playlist_genre))) #Capitalize all playlist_genre variables

#Calculate average valence for each playlist genre
average_valence <- spotify_songs %>%
  group_by(playlist_genre) %>% #Group each playlist genre
  summarise(average_valence = mean(valence)) #Calculate the mean valence for each group (genre)
spotify_songs <- left_join(spotify_songs, average_valence, by = "playlist_genre") #Join the average valence data to the original dataset

#Create box plot showing the distribution of danceability across playlist genres
boxplot <- 
  ggplot(data = spotify_songs,
       mapping = aes(x = playlist_genre, y = danceability)) + #Map playlist genres against danceability
  geom_boxplot(mapping = aes(fill = playlist_genre), outlier.shape = NA) + #Create box plots for each playlist genre
  geom_point(aes(y = average_valence, color = "Average Valence"), size = 2.5, shape = 16) + #Add points for average valence of each playlist genre
  geom_label(data = data.frame(playlist_genre = "Latin"), #Add label for "Latin" genre
            aes(label = "Highest Valence", x = 2.5, y = 1.1), #Name label "Highest Valence" and set coordinates
            fill = "white", color = "black", size = 4) + #Set colours of label
  geom_label(data = data.frame(playlist_genre = "EDM"), #Add label for "EDM" genre
            aes(label = "Lowest Valence", x = 2, y = 0.2),#Name label "Lowest Valence"
            fill = "white", color = "black", size = 4) + #Set colours of label
  annotate("segment", x = 2, y = 0.61, xend = 2.5, yend = 1.05, #Add arrow pointing from "Latin" average valence point to "Highest Valence" label
               arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("segment", x = 1, y = 0.4, xend = 2, yend = 0.25, #Add arrow pointing from "EDM" average valence point to "Lowest Valence" label
               arrow = arrow(length = unit(0.3, "cm"))) + 
  labs(title = "Distribution of Danceability Across Playlist Genres", #Set title of graph
       subtitle = "Plot generated by Dalia El-Shoura (200034287)", #Set subtitle of graph
       x = "Music Genre", #Set title of x axis
       y = "Danceability Score", #Set title of y axis
       caption = "Data from Spotify package\nhttps://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv/", #Set caption of graph
       color = NULL) + #Remove title from second legend
  scale_fill_viridis_d(name = "Playlist Genre", #Set legend title
                      labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock")) + #Set legend labels (capitalizing the genre names)
  scale_y_continuous(breaks = seq(0, 1.5, by = 0.2)) + #Set scale of y axis
  theme_bw() #Set theme of graph
print(boxplot)

#Save graph
ggsave(filename = "boxplot.png", plot = boxplot, width = 300, height = 200, units = "mm")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Gretchel Mouthe Munoz (210449086)

# latin subgenre dataset only
latin_genre <- spotify_songs[spotify_songs$playlist_subgenre %in% c("latin hip hop", "latin pop", "reggaeton", "tropical"), ]

# reshaping the data to long format
spotify_songs_long <- tidyr::gather(latin_genre, key = "variable", value = "value", valence, danceability)

# creating grouped box plot
boxplot_comparison <- ggplot(spotify_songs_long, aes(x = playlist_subgenre, y = value, fill = variable)) +
  geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
  labs(title = "Comparison of Valence and Danceability by Latin Subgenre: Exploring which Latin subgenre is more happier",
       subtitle = "Plot generated by G. Mouthe Munoz, 3 April 2024",
       caption = "Data from Spotify package https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv",
       x = "Latin Subgenre",
       y = "Valence/Danceability",
       fill = "Variable") +
  scale_fill_manual(values = c("yellow", "mediumseagreen"), labels = c("Valence", "Danceability")) + 
  theme_minimal() + theme(panel.background = element_rect(fill = "white"))

# display boxplot
print(boxplot_comparison)

# saving plot
ggsave(filename = "spotify_boxplot_plot.png", boxplot_comparison, width = 300, height = 200, units = "mm")

# calculating average danceability and valence for each subgenre 
subgenre_stats <- latin_genre %>%
  group_by(playlist_subgenre) %>%
  summarise(avg_danceability = mean(danceability),
            avg_valence = mean(valence))

# finding subgenre with highest danceabilty/valence
highest_danceability <- latin_genre[which.max(latin_genre$danceability), ]
highest_valence <- latin_genre[which.max(latin_genre$valence), ]

# finding subgenre with lowest danceability/valence
lowest_danceability <- latin_genre[which.min(latin_genre$danceability), ]
lowest_valence <- latin_genre[which.min(latin_genre$valence), ]

# printing results
cat("Subgenre with the highest danceability:", highest_danceability$playlist_subgenre, "\n")
cat("Subgenre with the lowest danceability:", lowest_danceability$playlist_subgenre, "\n")
cat("Subgenre with the highest valence:", highest_valence$playlist_subgenre, "\n")
cat("Subgenre with the lowest valence:", lowest_valence$playlist_subgenre, "\n")

# which artist has highest danceability 
highest_danceability <- latin_genre[which.max(latin_genre$danceability), ]

# which artist has highest valence
highest_valence <- latin_genre[which.max(latin_genre$valence), ]

# printing results
cat("Artist with the highest danceability within Latin genre:", highest_danceability$track_artist, "\n")
cat("Artist with the highest valence within Latin genre:", highest_valence$track_artist, "\n")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Luisa Shanti Defiebre (210535530)

#MAP 1 : which country produces the hapier music?

# 1st - creating a simplified new data table, just entailing the track_artist + their mean valence (from spotify_songs)
# Group by artist and calculate mean valence
artists_valence <-  spotify_songs %>%
  group_by(track_artist) %>%
  summarise(mean_valence = mean(valence, na.rm = TRUE))

# View the new dataset
head(artists_valence)
str(artists_valence)

#uploading the external data:  top artists native country 
artists_country <- read.csv("Top_Charts_Artists_Country.csv", sep = ";", header = TRUE)
view(artists_country)

#combining the new country data set with my artist_valance data:
# Rename columns for consistency:
artists_country <- rename(artists_country, track_artist = Artist)

# Perform the inner join
artists_country_valance <- inner_join(artists_country, artists_valence, by = "track_artist")
#when joining the data, we lost 23% of the data from the artist_country data, but we still have 737observations, which is eghnogh to see a trend 

# View the combined data
head(artists_country_valance)
str(artists_country_valance)
#when joining the data, we lost 23% of the data from the artist_country data, but we still have 737observations, which is eghnogh to see a trend 

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

#correcting mistakes:
#checking, as some countries in my data are called differently in the world data:
world_countries <- world$name # List of country names from the world map data
your_countries <- unique(artists_country_valance$Country)# List of country names from my dataset
# Identify country names in my dataset not in the world map data:
mismatches <- setdiff(your_countries, world_countries)

#fixing data in artists_country_valance so it aligns with the world data:
artists_country_valance <- artists_country_valance %>%
  mutate(
    Country = gsub(pattern = "United States", replacement = "United States of America", Country),
    Country = gsub(pattern = "Hawaii", replacement = "United States of America", Country),
    Country = gsub(pattern = "Stockholm", replacement = "Sweden", Country),
    Country = gsub(pattern = "British Virgin Islands", replacement = "United Kingdom", Country),
    Country = gsub(pattern = "Barcelona", replacement = "Spain", Country)
  )

# now, we need to aggregate the data by country to get mean valence per country
country_valence <- artists_country_valance %>%
  group_by(Country) %>%
  summarise(mean_valence = mean(mean_valence, na.rm = TRUE))

# now we can join this aggregated data with the world map data:
map_data <- left_join(world, country_valence, by = c("name" = "Country"))

# Create the map
# Define breaks for the color scale
valence_breaks <- c(0, 0.5, 1)
#creating the map:
map1 <- ggplot(map_data) +
  geom_sf(aes(fill = mean_valence), color = "white") +
  scale_fill_gradient2(low = "red", mid = "pink", high = "yellow",
                       limits = c(0, 1),
                       midpoint = 0.5, # Adjust midpoint accordingly
                       breaks = valence_breaks, 
                       name = "Mean Valence") +
  labs(title = "Which country produces the more positive music?", 
       fill = "Mean Valence") +
  theme_bw() #same theme asthe other graphs
print(map1)

ggsave("artist_map.png", map1, width = 300, height = 200, units = "mm")

#MAP 2 : which country listests to the most happy music?

# prep work:
#so that when combining the data sets later on, i only transfer actually the correct songs:
#i need to create a new column combining track_name and track_artist for spotify_song:
spotify_songs <- spotify_songs %>%
  mutate(track = paste(track_name, track_artist, sep = " - "))
str(spotify_songs)

#importing new data of top songs listentd in each country:
spotify_listeners <- read_csv("SpotifyTopSongsByCountry - May 2020.csv")
#checking the data:
head(spotify_listeners)
str(spotify_listeners)

#prep work:
#same thing, i need to combine the $Title + Artists and create a new column, to assure the data combines correctly:
spotify_listeners <- spotify_listeners %>%
  mutate(tracks = paste(Title, Artists, sep = " - "))
str(spotify_listeners)
head(spotify_listeners)

#creating a simplified new data tables:
# Select only the 'track' and 'valence' columns from spotify_songs
simplified_spotify_songs <- select(spotify_songs, track, valence)
#some of the tracks are reapeated, to fix that:
simplified_spotify_songs <- unique(simplified_spotify_songs)
# Select only the 'Country' and 'Title' columns from spotify_listeners
simplified_spotify_listeners <- select(spotify_listeners, Country, tracks)
#checking data:
str(simplified_spotify_listeners)
str(simplified_spotify_songs)
table(simplified_spotify_listeners$Country)

#now we can combine the data:
combined_data2 <- merge(simplified_spotify_listeners, simplified_spotify_songs, by.x = "tracks", by.y = "track", all = FALSE)
#checking data:
table(combined_data2$Country)
str(combined_data2)

#simpplifiying the data to calculate the mean valence for each country
country_valence2 <- combined_data2 %>%
  group_by(Country) %>%
  summarise(mean_valence = mean(valence, na.rm = TRUE))

# reloading the world map data
world2 <- ne_countries(scale = "medium", returnclass = "sf")

#correcting mistakes:
#checking, as some countries in my data are called diffently in the world data:
world_countries2 <- world2$name # List of country names from the world map data
your_countries2 <- unique(country_valence2$Country)# List of country names from your dataset
# Identify country names in your dataset not in the world map data:
mismatches <- setdiff(your_countries2, world_countries2)

#fixing data so it aligns with the world data:
country_valence2 <- country_valence2 %>%
  mutate(
    Country = gsub(pattern = "United States", replacement = "United States of America", Country),
    Country = gsub(pattern = "Dominican Republic", replacement = "Dominican Rep.", Country),
    Country = gsub(pattern = "Czech Republic", replacement = "Czechia", Country))

# Merging the map data with the country_valence data
map_data2 <- left_join(world2, country_valence2, by = c("name" = "Country"))

# Plot of the map
# Define breaks for the color scale
valence_breaks <- c(0, 0.5, 1)

map2 <- ggplot(map_data2) +
  geom_sf(aes(fill = mean_valence), color = "white") +
  scale_fill_gradient2(low = "red", mid = "pink", high = "yellow",
                       limits = c(0, 1), 
                       midpoint = 0.5, # Since your range is 0-1, midpoint for pink color would be 0.5
                       breaks = valence_breaks, 
                       name = "Mean Valence") +
  labs(title = "Which country listens to the more positive music?", 
       fill = "Mean Valence") +
  theme_bw() #same theme as the other graphs
print(map2)

ggsave("listeners_map.png", map2, width = 300, height = 200, units = "mm")




#to look at the distribution of the data,:
#firstly creating anew column with the years only :
spotify_songs$track_album_release_date <- as.Date(spotify_songs$track_album_release_date, format="%Y-%m-%d")
spotify_songs$release_year <- format(spotify_songs$track_album_release_date, "%Y")
str(spotify_songs)

# plotting the data distribution, to check hoe much data we have for each year:
distribution <- ggplot(spotify_songs, aes(x = release_year)) +
  geom_bar() + #bar plot, each year showing how many tracks we have in the data set
  theme_bw() +
  labs(title = "Distribution of data: Tracks released in each year",
       x = "Release Year",
       y = "Count of Tracks")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # For better readability
  scale_x_discrete(breaks = function(x) x[!is.na(as.numeric(x)) & as.numeric(x) %% 5 == 0]) # Exclude NAs and only years divisible by 5
print(distribution)

ggsave("distribution.png", distribution, width = 300, height = 200, units = "mm")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Combine all of the graphs using patchwork
combined_graphs <- (linegraph + distribution) / heatmap / (boxplot + boxplot_comparison) / (map1 + map2)

#Save the combined_graphs as an image
ggsave("combined_graphs.png", combined_graphs, width = 300, height = 500, units = "mm")