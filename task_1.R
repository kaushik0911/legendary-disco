candy_data = read.csv("CMM703 Candy Data 2024-2025.csv")

dim(candy_data)
colnames(candy_data)

candy_data_description = read.csv("CMM703 Candy Data Dictionary 2024-2025.csv")

library(dplyr)
library(tidyr)
library(ggplot2)

# Calculate average winpercent for each feature
feature_popularity <- candy_data %>%
  select(chocolate:pluribus, winpercent) %>%  # Select only ingredient columns + winpercent
  gather(key = "Feature", value = "Has_Feature", -winpercent) %>%  # Convert wide to long format
  filter(Has_Feature == 1) %>%  # Keep only candies that have the feature
  group_by(Feature) %>%
  summarise(Average_WinPercent = mean(winpercent, na.rm = TRUE)) %>%
  arrange(desc(Average_WinPercent))  # Sort by popularity

# View the table
print(feature_popularity)


# Plot features ranked by popularity
ggplot(feature_popularity, aes(x = reorder(Feature, Average_WinPercent), y = Average_WinPercent, fill = Feature)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Create a bar chart
  coord_flip() +  # Flip to horizontal bar chart
  labs(title = "Most Preferred Candy Features", x = "Feature", y = "Average Win Percent") +
  theme_minimal()



candy_data$combo <- apply(candy_data[, c("chocolate", "fruity", "caramel", "peanutyalmondy", 
                         "nougat", "crispedricewafer", "hard", "bar", "pluribus")], 1, paste, collapse = "")


# Group by combination and calculate average win percentage
combo_ranking <- candy_data %>%
  group_by(combo) %>%
  summarise(Average_WinPercent = mean(winpercent, na.rm = TRUE), Count = n()) %>%
  arrange(desc(Average_WinPercent))

# View top 5 most successful combinations
head(combo_ranking, 5)


ggplot(combo_ranking[1:10, ], aes(x = reorder(combo, Average_WinPercent), y = Average_WinPercent, fill = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip for readability
  labs(title = "Top 10 Most Preferred Candy Combinations",
       x = "Ingredient Combination",
       y = "Average Win Percentage") +
  theme_minimal()



# Group by combination and calculate average win percentage
combo_ranking <- candy_data %>%
  group_by(combo) %>%
  summarise(Average_WinPercent = mean(winpercent, na.rm = TRUE), 
            Count = n(),
            Candies = paste(competitorname, collapse = ", ")) %>%  # Combine candy names for labels
  arrange(desc(Average_WinPercent))

# Plot with ingredient combinations and candy names
ggplot(combo_ranking[1:10, ], aes(x = reorder(combo, Average_WinPercent), y = Average_WinPercent, fill = Count)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = Candies), hjust = 1.1, size = 3, color = "black") +  # Add candy names inside bars
  coord_flip() +  # Flip for readability
  labs(title = "Top 10 Most Preferred Candy Combinations",
       x = "Ingredient Combination",
       y = "Average Win Percentage") +
  theme_minimal()


# Scatter plot of sugar vs. winpercent with clusters
ggplot(candy_data, aes(x = sugarpercent, y = winpercent, color = combo)) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter points
  geom_text(aes(label = competitorname), vjust = -1, size = 3) +  # Add candy labels
  labs(title = "Candy Clusters: Sugar vs. Popularity",
       x = "Sugar Percentile",
       y = "Win Percentage",
       color = "Ingredient Combination") +
  theme_minimal()


# Load clustering library
library(cluster)

# Select numeric features for clustering
candy_features <- candy_data[, c("sugarpercent", "winpercent")]

# Run k-means clustering (choosing 3 clusters)
set.seed(123)
kmeans_result <- kmeans(candy_features, centers = 3)

# Add cluster labels to dataset
candy_data$cluster <- as.factor(kmeans_result$cluster)

# Scatter plot with clusters
ggplot(candy_data, aes(x = sugarpercent, y = winpercent, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = competitorname), vjust = -1, size = 3) +
  labs(title = "Candy Clusters (K-Means): Sugar vs. Popularity",
       x = "Sugar Percentile",
       y = "Win Percentage",
       color = "Cluster Group") +
  theme_minimal()
