# install.packages("DescTools")
# install.packages("shiny")
library(DescTools)
library(shiny)

# TODO
# 1. dentify qualitative and quantitative variables in the dataset.
#   * sapply function not getting correct output

my_data_analysis_function = function(dataset, outlier_method="IQR") {
  # Task 3.1
  # Identify qualitative and quantitative variables in the dataset.
  
  number_of_rows     = nrow(dataset)
  found_index_column = FALSE

  for (variable in colnames(dataset)) {
    unique_count = length(unique(dataset[[variable]]))
    
    if (unique_count < 10) {
      dataset[[variable]] = as.factor(dataset[[variable]])
    } else if (found_index_column == FALSE && unique_count == number_of_rows) {
      found_index_column  = TRUE
      dataset[[variable]] = NULL
    }
  }

  qualitative_variables = names(which(sapply(
    dataset,
    function(variable) {
      is.factor(variable) == TRUE || is.numeric(variable) == FALSE
    })))

  quantitative_variables = names(which(sapply(
    dataset,
    function(variable) {
      is.numeric(variable)
    })))

  print("-----------------------------------------------------")
  print(paste("qualitative variables list", qualitative_variables))
  print("-----------------------------------------------------")
  print(paste("quantitative variables list", quantitative_variables))
  print("-----------------------------------------------------")

  # Task 3.2
  # Count the missing values in each variable. Impute the missing values using: the mean value of the variable if it is numeric; the mode of the variable if it is categorical.

  # Counting the missing values.
  missing_counts = sapply(
    dataset,
    function(x) {
      sum(is.na(x))
    })

  print(missing_counts)

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
  }

  # Task 3.4
  # Summarize each variable using a proper visualization tool for the respective variable (eg: histogram, boxplot etc.).

  cell_count = length(quantitative_variables)
  if (cell_count %% 2 != 0) {
    cell_count = cell_count + 1
  }
  
  layout(matrix(1:cell_count, ncol = 2, byrow = TRUE))

  par(mar = c(3, 3, 2, 1))

  for(column_name in quantitative_variables) {
    column_data = dataset[[column_name]]
    boxplot(column_data, main = paste("Boxplot of", column_name), col = "lightblue",
            outcol = "red")
  }
  
  layout(matrix(1:cell_count, ncol = 2, byrow = TRUE))
  par(mar = c(3, 3, 2, 1))
  
  for(column_name in qualitative_variables) {
    column_data = dataset[[column_name]]
    attribute_count = sort(table(column_data), decreasing = TRUE)
    
    bar_colors = terrain.colors(length(names(attribute_count)))
    
    barplot(
      attribute_count,
      main = paste("Bar chart of", column_name),
      col = bar_colors
    )
  }

  # Task 3.5
  # When the response variable is specified as an argument, it should run the best predictive model for that response category (consider only continuous and binary response variables) and select features considering all other meaningful variables. Your function should print relevant diagnostic metrics and plots for the selected model.

  # return(list(
  #   outliers = outlier_results
  # ))
}

bank_data = read.csv("Bank Churn Data CMM703.csv", header = TRUE)
my_data_analysis_function(bank_data)


app_ui = fluidPage(
  titlePanel("My Data Analysis APP"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File (Only one CSV file)", accept = c(".csv")),
      selectInput(
        "outlier_method", "Outlier Detection Method",
        choices = c("IQR", "Z-score"),
        selected = "IQR"),
      actionButton(
        "process", "Process Data")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      renderUI("plot_ui")
    )
  )
)

app_server = function(input, output) {
  csv_dataset = reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = TRUE)
  })

  processed_data = eventReactive(
    input$process, {
      my_data_analysis_function(
        csv_dataset(), outlier_method = input$outlier_method)
    }
  )

  output$summary = renderPrint({
    req(processed_data())
    list(
      "Missing Values" = processed_data()$missing_counts,
      "Outliers" = processed_data()$outlier_indices
    )
  })

  output$plot_ui <- renderUI({
    req(processed_data())
    plot_output_list <- lapply(names(processed_data()$plots), function(name) {
      plotOutput(outputId = paste0("plot_", name))
    })
    do.call(tagList, plot_output_list)
  })
  
  observe({
    req(processed_data())
    for (name in names(processed_data()$plots)) {
      local({
        plot_name <- name
        output[[paste0("plot_", plot_name)]] <- renderPlot({
          processed_data()$plots[[plot_name]]
        })
      })
    }
  })
}

shinyApp(ui = app_ui, server = app_server)

