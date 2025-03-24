# TODO
# 1. dentify qualitative and quantitative variables in the dataset.
#   * sapply function not getting correct output

my_data_analysis_function = function(dataset) {
  # Task 3.1
  # Identify qualitative and quantitative variables in the dataset.

  qualitative_variables = names(which(sapply(
    dataset,
    function(x) {
      is.numeric(x) == FALSE
    })))

  quantitative_variables = names(which(sapply(
    dataset,
    function(x) {
      is.numeric(x)
    })))

  # Task 3.2
  # Count the missing values in each variable. Impute the missing values using: the mean value of the variable if it is numeric; the mode of the variable if it is categorical.

  # Counting the missing values.
  missing_counts = sapply(
    dataset,
    function(x) {
      sum(is.na(x))
    })
  
  # install.packages("DescTools")
  library(DescTools)
  for(column_name in qualitative_variables){
    dataset[[column_name]][is.na(dataset[[column_name]])] = 
      Mode(dataset[[column_name]], na.rm = FALSE)
  }

  for(column_name in quantitative_variables){
    data[[col]][is.na(data[[col]])] = mean(data[[col]], na.rm = TRUE)
  }
}