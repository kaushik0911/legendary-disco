bank_data = read.csv("Bank Churn Data CMM703.csv")
colnames(bank_data)
dim(bank_data)

head(bank_data, 10)
# by looking at the dataset description its seems that HasCrCard, IsActiveMember, and Exited are categorical variables
# let's convert them into categorical variables using factors

bank_data$HasCrCard = factor(
  bank_data$HasCrCard,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

bank_data$IsActiveMember = factor(
  bank_data$IsActiveMember,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

bank_data$Exited = factor(
  bank_data$Exited,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# we have to predict the churn status of the customer?
# y == "churn status"
# x == (other fields)
# lets analysis the data in the dataset

head(bank_data)

# lets do a quick summary statistics
# i install vtable package because it seems more eye plesent
# install.packages('vtable')
library(vtable)
?sumtable
sumtable(bank_data)

counts = table(bank_data$Exited)
percentages = round(counts / sum(counts) * 100, 2)
labels = paste(names(counts), percentages, "%")
colors = c("green", "red")
pie(counts, labels = labels, col = colors, main = "Churn(Exited) status of customers")

# install.packages(c("ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata"))



featureset = c("Age", "CreditScore", "Balance", "EstimatedSalary", "Tenure")

exited_customers = subset(bank_data, Exited == "Yes")
stayed_customers = subset(bank_data, Exited == "No")


generate_hist = function (param_feature, param_bank_data) {
  column_data = param_bank_data[[param_feature]]
  hist(column_data, main = paste("Boxplot of", param_feature), col = "lightblue", breaks = 10)

  abline(v = mean(column_data), col='red', lwd = 3)

  q1 = quantile(column_data, 0.25)
  q3 = quantile(column_data, 0.75)
  iqr_value = q3 - q1

  lower_bound = q1 - 1.5 * iqr_value
  upper_bound = q3 + 1.5 * iqr_value

  # Add vertical lines for Q1, Q3, and whisker bounds
  abline(v = q1, col = "blue", lwd = 2, lty = 2)   # Q1
  abline(v = q3, col = "blue", lwd = 2, lty = 2)   # Q3
  abline(v = lower_bound, col = "green", lwd = 2, lty = 2)  # Lower Bound
  abline(v = upper_bound, col = "green", lwd = 2, lty = 2)  # Upper Bound
}

generate_box = function(param_feature, param_bank_data) {
  # Extract numeric column
  original_data = param_bank_data[[param_feature]]
  
  # Split the data by 'Exited' and convert it to a dataframe
  grouped_data = split(original_data, param_bank_data[['Exited']])
  
  # Create a list including all data + grouped data
  final_data = c(list(All_Data = original_data), grouped_data)
  
  # Create the boxplot
  boxplot(final_data, 
          col = c("gray", "lightblue", "lightgreen"), 
          main = paste("Customer Exited status vs", param_feature, "Boxplot"),
          xlab = "Exited status",
          ylab = param_feature,
          horizontal = TRUE)
}


for(feature in featureset) {
  layout(matrix(1:4, ncol = 1, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # Reduce margins

  generate_hist(feature, bank_data)
  generate_hist(feature, exited_customers)
  generate_hist(feature, stayed_customers)
  
  generate_box(feature, bank_data)
}


featureset = c("Geography", "Gender", "NumOfProducts", "HasCrCard", "IsActiveMember")

generate_bar = function (param_feature, param_bank_data) {
  feature_count = table(param_bank_data[[param_feature]])
  
  barplot(feature_count, col = "lightblue", main = paste(param_feature), ylim = c(0, max(feature_count) + 10))
  axis(1, at = seq_along(feature_count), labels = names(feature_count)) 
}


for(feature in featureset) {
  layout(matrix(1:3, ncol = 1, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # Reduce margins

  generate_bar(feature, bank_data)
  generate_bar(feature, exited_customers)
  generate_bar(feature, stayed_customers)
}

