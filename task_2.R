original_bank_data = read.csv("Bank Churn Data CMM703.csv", header = TRUE)

# number of columns/variables
ncol(original_bank_data)
#columns names
colnames(original_bank_data)

# number of row
nrow(original_bank_data)

# quick overview of the dataset
str(original_bank_data)

# by looking at the dataset description its seems that HasCrCard, IsActiveMember, and Exited are categorical variables
# lets convert them into categorical variables using factors

# before that, customer id and surname is not required for the analysis, so remove it from the dataset
original_bank_data$CustomerId = NULL
original_bank_data$Surname    = NULL

# lets look at null values in the dataset
sapply(
  original_bank_data,
  FUN = function(x) sum(is.na(x))
)

# remove the null values if exists, since there are no null values
bank_data = na.omit(original_bank_data)

# variable delete because not used from here(had to remove because need memory for modeling)
original_bank_data = NULL

change_to_factor = function(param_bank_data, param_feature) {
  return(
    factor(
      param_bank_data[[param_feature]],
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )
}

bank_data$HasCrCard = change_to_factor(bank_data, "HasCrCard")
bank_data$IsActiveMember = change_to_factor(bank_data, "IsActiveMember")
bank_data$Exited = change_to_factor(bank_data, "Exited")

# lets do a quick summary statistics
# i install "vtable" package because it seems more eye pleasing
# install.packages('vtable')
library(vtable)
sumtable(bank_data)

# quantitative variables
featureset = c("Age", "CreditScore", "Balance", "EstimatedSalary", "Tenure")

exited_customers = subset(bank_data, Exited == "Yes")
stayed_customers = subset(bank_data, Exited == "No")

plot_histogram = function (param_feature, param_bank_data, param_title) {

  column_data = param_bank_data[[param_feature]]

  hist(
    column_data,
    main = paste("Histogram of", param_feature, param_title),
    col = "gray",
    breaks = 10
  )

  abline(v = mean(column_data), col='red', lwd = 3)

  q1 = quantile(column_data, 0.25)
  q3 = quantile(column_data, 0.75)
  iqr_value = q3 - q1

  lower_bound = q1 - 1.5 * iqr_value
  upper_bound = q3 + 1.5 * iqr_value

  # add vertical lines for q1, q3, and whisker bounds
  abline(v = q1, col = "blue", lwd = 2, lty = 2)
  abline(v = q3, col = "blue", lwd = 2, lty = 2)
  abline(v = lower_bound, col = "green", lwd = 2, lty = 2)  # lower Bound
  abline(v = upper_bound, col = "green", lwd = 2, lty = 2)  # upper Bound
}

plot_boxplot = function(param_feature, param_bank_data) {

  # extract numeric column
  original_data = param_bank_data[[param_feature]]

  # split the data by 'Exited' and convert it to a dataframe
  grouped_data = split(original_data, param_bank_data[["Exited"]])

  # create a list including all data + grouped data
  final_data = c(list(all_data = original_data), grouped_data)

  boxplot(
    final_data, 
    col = c("gray", "lightblue", "lightgreen"), 
    main = paste("Customer Exited status vs", param_feature, "Boxplot"),
    xlab = "Exited status",
    ylab = param_feature,
    horizontal = TRUE
  )
}


for(feature in featureset) {

  layout(matrix(1:4, ncol = 1, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # reduce margins

  plot_histogram(feature, bank_data, "")
  plot_histogram(feature, exited_customers, "(Exited Customers)")
  plot_histogram(feature, stayed_customers, "(Stayed Customers)")
  
  plot_boxplot(feature, bank_data)
}

# qualitative variables
featureset = c("Geography", "Gender", "NumOfProducts", "HasCrCard", "IsActiveMember")

plot_bargraph = function (param_feature, param_bank_data, param_title) {

  feature_data  = param_bank_data[[param_feature]]
  feature_count = sort(table(feature_data), decreasing = TRUE)

  bar_colors = terrain.colors(length(names(feature_count)))

  barplot(
    feature_count,
    col = bar_colors,
    main = paste("Bar Graph of", param_feature, param_title),
    ylim = c(0, max(feature_count) + 10)
  )
}

for(feature in featureset) {

  layout(matrix(1:3, ncol = 1, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # reduce margins

  plot_bargraph(feature, bank_data, "")
  plot_bargraph(feature, exited_customers, "(Exited Customers)")
  plot_bargraph(feature, stayed_customers, "(Stayed Customers)")
}


featureset = c("Age", "CreditScore", "Balance", "EstimatedSalary", "Tenure")
# correlation analysis
correlation_matrix = cor(bank_data[featureset])
print(correlation_matrix)

# visualize correlation in a heatmap
heatmap(correlation_matrix,
        main = "Correlation Heatmap")

exited_customers = NULL
stayed_customers = NULL

# we have to predict the churn status of the customer?
# y == "churn status", YES or NO, Qualitative Variable and Binary
# x == (other fields)
# model should be binary logistic regression model

counts = table(bank_data$Exited)
print(counts)
# you can see the percentages are not equal meaning dataset is not balance

percentages = round(counts / sum(counts) * 100, 2)
labels = paste(names(counts), percentages, "%")
colors = terrain.colors(2)
par(mfrow = c(1, 1))
pie(counts, labels = labels, col = colors, main = "Churn(Exited) status of customers")

# lets start create a model to check customer churn status
set.seed(2425499)

number_of_rows   = nrow(bank_data)
train_percentage = 0.8
train_ids = sample(1:number_of_rows, number_of_rows * train_percentage, replace = FALSE)

train_dataset = bank_data[train_ids, ]
test_dataset  = bank_data[-train_ids, ]

bank_model = glm(
  formula = Exited ~ .,
  data    = train_dataset,
  family = binomial
)

summary(bank_model)

refined_bank_model = glm(
  formula = Exited ~ CreditScore + Geography + Gender + Age + Balance + IsActiveMember,
  data = train_dataset,
  family = binomial,
)

# validate the model
column_index = which(names(test_dataset) == "Exited")

predictions = predict.glm(
  refined_bank_model,
  newdata = test_dataset[, -column_index],
  type = "response"  
)

test_dataset$predicted_class = factor(ifelse(predictions > 0.5, "Yes", "No"))

table(
  test_dataset$predicted_class, 
  test_dataset$Exited,
  dnn = c("Predicted", "Actual")
)

table(test_dataset$Exited)

mean(test_dataset$predicted_class == test_dataset$Exited)
# accuracy is 81.25% (0.8125)

# goodness of fit of the model 
# hosmer and lemeshow test 
# h0 : model is adequate
# h1 : model is not adequate 

# install.packages("modEvA")
library(modEvA)

hoslem_results = HLfit(
  model = refined_bank_model,
  bin.method = "n.bins",
  n.bins = 10,
  main = "Model Goodness-of-Fit Check (10 Bins)"
)

# Print the test results
par(mfrow = c(1, 1))
print(hoslem_results)

# p-value is 0.038 < 0.05 meaning we reject h0.
# the conclusion of the statement is model is the model is not adequate.

# model has 81.25% accuracy, but the Hosmer-Lemeshow (H-L) test shows p-value is 0.038
# that means a potential lack of fit.

# what is the next step then,
# * we can do a cross validation by splitting the dataset into sevaral gorups and redo the process
# * Also can change the weights meaning 'glm' function support 'weights' as a parameter.
#   Because there is a parameter called 'weight' that can give priority to the 'Yes' and redo the modeling.

# lets start create a model to check customer 'Tenure'

# lets check 'Tenure' frequency
table(bank_data$Tenure)

# 'Tenure' is recorded in whole years from 0 to 10, satisfying Poisson's integer requirement.
# no negative values
# if you look at Tenure == 10 it is skewed to right, but not severely imbalanced.

boxplot(bank_data$Tenure, horizontal = TRUE)
# mean and median almost equals to 5 years and no extreme outliers.

mean(bank_data$Tenure)
var(bank_data$Tenure)

# here poisson assumption (variance = mean) is violated.
# then use the negative binomial regression
library(MASS)
nb_model = glm.nb(Tenure ~ ., data = bank_data)
summary(nb_model)

# in the output the significants are 'HasCrCardYes' and 'IsActiveMemberYes', both are binary values meaning 'Yes' or 'No' therefore the model is not adequate to predict the 'Tenure' because the output will be 4, (YES-YES, NO-NO, YES-NO, NO-YES)
