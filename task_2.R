original_bank_data = read.csv("Bank Churn Data CMM703.csv")
colnames(original_bank_data)
dim(original_bank_data)

head(original_bank_data, 10)
# by looking at the dataset description its seems that HasCrCard, IsActiveMember, and Exited are categorical variables
# lets convert them into categorical variables using factors

# customer id and surname is not required for the analysis
original_bank_data$CustomerId = NULL
original_bank_data$Surname    = NULL

# remove the null values if exists
bank_data = na.omit(original_bank_data)

# variable delete because not used from here(had to remove because need memory for modeling)
original_bank_data = NULL

# change the data type as factor
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

# lets do a quick summary statistics

# i install "vtable" package because it seems more eye pleasing
# install.packages('vtable')
library(vtable)
sumtable(bank_data)

counts = table(bank_data$Exited)
percentages = round(counts / sum(counts) * 100, 2)
labels = paste(names(counts), percentages, "%")
colors = c("green", "red")
pie(counts, labels = labels, col = colors, main = "Churn(Exited) status of customers")

# quantitative variables
featureset = c("Age", "CreditScore", "Balance", "EstimatedSalary", "Tenure")

exited_customers = subset(bank_data, Exited == "Yes")
stayed_customers = subset(bank_data, Exited == "No")

plot_histogram = function (param_feature, param_bank_data) {

  column_data = param_bank_data[[param_feature]]

  hist(
    column_data,
    main = paste("Histogram of", param_feature),
    col = "lightblue",
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
  grouped_data = split(original_data, param_bank_data[['Exited']])

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

  plot_histogram(feature, bank_data)
  plot_histogram(feature, exited_customers)
  plot_histogram(feature, stayed_customers)
  
  plot_boxplot(feature, bank_data)
}

# qualitative variables

featureset = c("Geography", "Gender", "NumOfProducts", "HasCrCard", "IsActiveMember")

plot_bargraph = function (param_feature, param_bank_data) {

  feature_count = table(param_bank_data[[param_feature]])
  
  barplot(
    feature_count,
    col = "lightblue",
    main = paste(param_feature),
    ylim = c(0, max(feature_count) + 10)
  )
}


for(feature in featureset) {

  layout(matrix(1:3, ncol = 1, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # reduce margins

  plot_bargraph(feature, bank_data)
  plot_bargraph(feature, exited_customers)
  plot_bargraph(feature, stayed_customers)
}

exited_customers = NULL
stayed_customers = NULL

# we have to predict the churn status of the customer?
# y == "churn status", YES or NO, Qualitative Variable and Binary
# x == (other fields)
# model should be binary logistic regression model


# lets analysis the data in the dataset
set.seed(2425499)

number_of_rows   = nrow(bank_data)
train_percentage = 0.8
train_ids = sample(1:number_of_rows, number_of_rows * train_percentage, replace = FALSE)

train_dataset = bank_data[train_ids, ]
test_dataset  = bank_data[-train_ids, ]

names(bank_data)

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

# goodness of fit of the model 
# hosmer and lemeshow test 
# h0 : model is adequate
# h1 : model is not adequate 

install.packages("modEvA")
library(modEvA)

hoslem_results <- HLfit(
  model = refined_bank_model,  # Your logistic regression model
  bin.method = "n.bins",      # Group observations into bins
  n.bins = 10,                # Number of bins (typically 10-15)
  main = "Model Goodness-of-Fit Check (10 Bins)"  # Plot title
)

# Print the test results
print(hoslem_results)





# forward selection for a better model
# lets start from with an empty model and then adding parameters one by one based on significance (p-values)

# first the null model with the intercept(c)

null_model = glm(
  formula = Exited ~ 1,  # only the intercept
  data = train_dataset,
  family = binomial
)

all_model_variables = c( "CreditScore", "Geography", "Gender", "Age",  "Tenure", "Balance", "NumOfProducts", "HasCrCard", "IsActiveMember", "EstimatedSalary" )

current_model = null_model

remaining_model_variables = all_model_variables
selected_model_variables  = c()

threshold_p_value = 0.05  # significance level

# iterate until no significant predictors left
while (length(remaining_model_variables) > 0) {

  p_values = c()

  for (predictor in remaining_predictors) {

    temp_formula = as.formula(
      paste("Exited ~", paste(c(selected_predictors, predictor), collapse = " + "))
    )

    temp_model = glm(
      formula = temp_formula,
      data = train_dataset,
      family = binomial
    )

    # get p-values
    p_value  = coef(summary(temp_model))[predictor, "Pr(>|z|)"]
    p_values = c(p_values, p_value)
  }

  min_p_value = min(p_values)
  if (min_p_value > threshold_p_value) break

  
}







