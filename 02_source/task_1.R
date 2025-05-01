candy_data = read.csv(file.path("01_data", "candy-2024-2025.csv"), header = TRUE)

# these are the candy ingredients
ingredients = c("chocolate", "fruity", "caramel", "peanutyalmondy", "nougat", "crispedricewafer", "hard", "bar", "pluribus")

# lets analysis favorite candy ingredient
feature_average = data.frame(
      feature_name = character(),
      average_win_percentage = numeric()
    )

for (ingredient in ingredients) {
  has_feature     = candy_data[candy_data[[ingredient]] == 1, ]
  
  # calculate the average 'winpercent' where particular ingredient is in the candy
  average_win     = mean(has_feature$winpercent, na.rm = TRUE)
  feature_average = rbind(
                      feature_average,
                      data.frame(
                        feature_name = ingredient,
                        average_win_percentage = average_win
                      )
                    )
}

barplot(
  feature_average$average_win_percentage,
  names.arg = feature_average$feature_name,
  las  = 2,
  main = "Voters' Favourite Candy Ingredients",
  xlab = "Ingredient",
  ylab = "Average Win Percent (%)",
)

# as you can see here it is bit hard to analysis the information given in the chart.
# lets improve the chart by adding colors, sort the data and few extra labels.

# sort by popularity
feature_average = feature_average[order(-feature_average$average_win_percentage), ]

par(mar = c(6, 4, 4, 0))

bar_colors = terrain.colors(length(feature_average$average_win_percentage))

bar_chart = barplot(
  feature_average$average_win_percentage,
  names.arg = feature_average$feature_name,
  las  = 2,
  col  = bar_colors,
  main = "Voters' Favourite Candy Ingredients",
  xlab = "Ingredient",
  ylab = "Average Win Percent (%)",
  ylim = c(0, 100),
  cex.names = 0.8
)

# add values on top of the bars
text(
  x = bar_chart, 
  y = feature_average$average_win_percentage, 
  label = round(feature_average$average_win_percentage, 1), 
  pos = 3
)

# blue dashed line at 50%
abline(h = 50, col = "blue", lty = 2)

# according to the chart you can see that most popular candy ingredient is "crispedricewafer" and the least is candy which is "hard".
# there are 5 candy ingredients achieve more that 50%
# 'chocolate' and 'nougat' ingredients almost share the same popularity

# lets pick 4 ingredients where the most popular
candy_data$candy_type = "other"
candy_data$candy_type[candy_data$crispedricewafer == 1] = "crispedricewafer"
candy_data$candy_type[candy_data$peanutyalmondy == 1]   = "peanutyalmondy"
candy_data$candy_type[candy_data$bar == 1]              = "bar"

candy_data$sugarpercent = candy_data$sugarpercent * 100

plot(
  x = candy_data$sugarpercent,
  y = candy_data$winpercent,
  main = "Sugar Content vs Popularity",
  xlab = "Sugar Percentage",
  ylab = "Win Percentage (Popularity)",
)

# this scatter plot shows how candy distribute across win percentage against sugar percentage.
# as you can see it is not easy to get information from the chart.
# lets improve the chart by adding colors, some info replated to the price.

par(mar = c(4, 4, 4, 2))

scatter_colors = terrain.colors(length(unique(candy_data$candy_type)))

candy_price_sizes = 1 + (candy_data$pricepercent / max(candy_data$pricepercent)) * 2

plot(
  x = candy_data$sugarpercent,
  y = candy_data$winpercent,
  main = "Sugar Content vs Popularity",
  xlab = "Sugar Percentage",
  ylab = "Win Percentage (Popularity)",
  xlim = c(0, 100),
  ylim = c(0, 100),
  col = scatter_colors,
  cex = candy_price_sizes,
  pch = 19
)

mtext(
  "Each circle size shows the unit price percentile compared to the rest of the set",
  side = 3,
  line = 0.5,
  cex  = 0.9
)

points(
  x = candy_data$sugarpercent,
  y = candy_data$winpercent,
  cex = candy_price_sizes,
  pch = 21,
  bg  = scatter_colors,
  col = "black"
)

legend(
  "topleft",
  legend = c("crispedricewafer", "peanutyalmondy", "bar", "other"),
  pt.bg  = scatter_colors, 
  col    = "black",
  pch    = 21,
  pt.cex = 2,
  horiz  = TRUE,
)

# here in the chart you can see that data is scatter between (20 - 85)% in the y axis the win percentage
# according to the chart more popular candys have higher sugar content.
# popular and sugary candys are not cheep according to the chart