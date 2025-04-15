# install.packages("DescTools")
# install.packages("modEvA")
library(DescTools) # used for mode function
library(modEvA)

my_data_analysis_function = function(dataset, response_variable, outlier_method="IQR") {
  number_of_rows     = nrow(dataset)
  found_index_column = FALSE

  qualitative_variables  = c()
  quantitative_variables = c()

   # find qualitative and quantitative variables
  for (variable in colnames(dataset)) {
    unique_count = length(unique(dataset[[variable]]))

    if (unique_count < 10) {
      dataset[[variable]] = as.factor(dataset[[variable]])
      qualitative_variables = c(qualitative_variables, variable)
    }
    # remove the index column
    else if (found_index_column == FALSE && unique_count == number_of_rows) {
      found_index_column  = TRUE
      dataset[[variable]] = NULL
    }
    else if (is.numeric(dataset[[variable]])) {
      quantitative_variables = c(quantitative_variables, variable)
    }
    else {
      dataset[[variable]] = NULL
    }
  }

  cat("qualitative variables : ", qualitative_variables)
  cat("quantitative variables : ", quantitative_variables)

  # Counting the missing values.
  missing_counts = sapply(
    dataset,
    FUN = function(x) { sum(is.na(x)) }
  )

  print(missing_counts)

  # impute the missing values using the mode value
  for(column_name in qualitative_variables) {
    dataset[[column_name]][is.na(dataset[[column_name]])] =
      Mode(dataset[[column_name]], na.rm = TRUE)
  }

  # impute the missing values using the mean value
  for(column_name in quantitative_variables) {
    dataset[[column_name]][is.na(dataset[[column_name]])] =
      mean(dataset[[column_name]], na.rm = TRUE)
  }

  outlier_results = list()
  for(column_name in quantitative_variables) {

    column_data = dataset[[column_name]]

    if (outlier_method == "IQR") {
      outliers = boxplot.stats(column_data)$out
    } else if (outlier_method == "Z") {
      outliers = dataset[abs(scale(column_data)) > 2]
    }
    else {
      paste(outlier_method, "is not a valid/supported outlier method")
      break
    }

    outlier_results[[column_name]] = outliers
  }

  cell_count = length(quantitative_variables)
  if (cell_count %% 2 != 0) {
    cell_count = cell_count + 1
  }

  for(column_name in quantitative_variables) {
    column_data = dataset[[column_name]]
    boxplot(
      column_data,
      main = paste("Boxplot of", column_name),
      col = "gray",
      outcol = "red",
      horizontal = TRUE
    )
  }

  for(column_name in qualitative_variables) {
    column_data = dataset[[column_name]]
    attribute_count = sort(table(column_data), decreasing = TRUE)

    barplot(
      attribute_count,
      main = paste("Bar chart of", column_name),
      col = terrain.colors(length(names(attribute_count)))
    )
  }

  all_variables = c(qualitative_variables, quantitative_variables)
  my_model      = NULL

  set.seed(2425499)
  number_of_rows   = nrow(dataset)
  train_percentage = 0.8
  train_ids = sample(1:number_of_rows, number_of_rows * train_percentage, replace = FALSE)
  
  train_dataset = dataset[train_ids, ]
  test_dataset  = dataset[-train_ids, ]
  
  if ( response_variable %in% all_variables) {

    x_variables = all_variables[all_variables != response_variable]
    my_formula = as.formula(
      paste(response_variable, "~", paste(x_variables, collapse= "+")))
    
    if (response_variable %in% quantitative_variables) {

      my_model = lm(formula=my_formula, data=train_dataset)

      test_prediction_data = predict(my_model, newdata = test_dataset)
      test_actuals_data    = test_dataset[[response_variable]]

      mse  = mean((test_actuals_data - test_prediction_data)^2)
      rmse = sqrt(mse)
      mae  = mean(abs(test_actuals_data - test_prediction_data))
      r_squared = cor(test_actuals_data, test_prediction_data)^2
      
      cat("MSE:", mse, "\nRMSE:", rmse, "\nMAE:", mae, "\nR²:", r_squared, "\n")

      # plot(x = test_actuals_data, y = test_prediction_data)
      # abline(a = 0, b = 1, col = "blue", lwd = 2)

    } else if (response_variable %in% qualitative_variables) {

      if (length(unique(dataset[[response_variable]])) == 2) {

        my_model = glm(formula=my_formula, data=train_dataset, family = binomial)
        
        test_probability_data = predict.glm(my_model, newdata = test_dataset, type = "response")
        test_prediction_data  = ifelse(test_probability_data > 0.5, 1, 0)
        test_actuals_data     = test_dataset[[response_variable]]

        table(
          test_actuals_data,
          test_prediction_data,
          dnn = c("Predicted", "Actual")
        )
        
        mean(test_actuals_data == test_prediction_data)
        
        str(HLfit(
          model = my_model,
          bin.method = "n.bins",
          n.bins = 10,
          main = "Model Goodness-of-Fit Check (10 Bins)"
        ))
      }
    }
  }

  print(summary(my_model))
}

bank_data = read.csv("Bank Churn Data CMM703.csv", header = TRUE)
my_data_analysis_function(bank_data, "Exited")
