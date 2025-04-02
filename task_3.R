# TODO
# 1. dentify qualitative and quantitative variables in the dataset.
#   * sapply function not getting correct output


# assumption that categorical data should be in factor format.
my_data_analysis_function = function(dataset, outlier_method="IQR") {
  # Task 3.1
  # Identify qualitative and quantitative variables in the dataset.

  qualitative_variables = names(which(sapply(
    dataset,
    function(x) {
      is.factor(x) == TRUE || is.numeric(x) == FALSE
    })))

  print(paste("qualitative variables list", list(qualitative_variables)))

  quantitative_variables = names(which(sapply(
    dataset,
    function(x) {
      is.numeric(x)
    })))

  print(paste("quantitative variables list", quantitative_variables))

  # Task 3.2
  # Count the missing values in each variable. Impute the missing values using: the mean value of the variable if it is numeric; the mode of the variable if it is categorical.

  # Counting the missing values.
  missing_counts = sapply(
    dataset,
    function(x) {
      sum(is.na(x))
    })
  
  print(missing_counts)
  
  # install.packages("DescTools")
  library(DescTools)
  for(column_name in qualitative_variables) {
    dataset[[column_name]][is.na(dataset[[column_name]])] = 
      Mode(dataset[[column_name]], na.rm = FALSE)
  }

  for(column_name in quantitative_variables) {
    dataset[[column_name]][is.na(dataset[[column_name]])] = mean(dataset[[column_name]], na.rm = TRUE)
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
    )
  }

  layout(matrix(1:length(quantitative_variables), ncol = 2, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))  # Reduce margins

  for(column_name in quantitative_variables) {
    column_data = dataset[[column_name]]
    boxplot(column_data, main = paste("Boxplot of", column_name), col = "lightblue",
            outcol = "red")
  }

  # return(list(
  #   outliers = outlier_results
  # ))
}

bank_data = read.csv("Bank Churn Data CMM703.csv")
my_data_analysis_function(bank_data)


