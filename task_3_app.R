library(shiny)

app_ui = page_sidebar(
  title = "My Data Analysis APP",
  sidebar = sidebar(
    fileInput("csv_file", "Upload CSV File", accept = ".csv"),
    radioButtons(
      inputId = "outlier_method",
      label = "Outlier Method",
      choices = list(
        "IQR" = "IQR",
        "Z-Score" = "Z-Score"
      )
    ),
    actionButton("action", label = "Start Process")
  ),
  uiOutput("quantitative_plots"),
  uiOutput("qualitative_plots")
)


app_server = function(input, output) {

  observeEvent(input$action, {
    
    uploaded_file = input$csv_file
  
    if (is.null(uploaded_file)) { return(NULL) }

    outlier_method = input$outlier_method
  
    dataset = read.csv(uploaded_file$datapath, header = TRUE)
  
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
      }
    )

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

    output$quantitative_plots = renderUI({
      div(
        class = "d-flex flex-wrap gap-3",
        lapply(quantitative_variables, function(column_name) {
          temp_column_name = column_name

          card(
            full_screen = TRUE,
            style = "width: 32%; min-width: 250px;",
            card_header(paste("Boxplot of", temp_column_name)),
            plotOutput(outputId = paste0("plot", temp_column_name))
          )
        })
      )
    })

    lapply(quantitative_variables, function(column_name) {
      column_data = dataset[[column_name]]

      temp_column_name = column_name

      output[[paste0("plot", temp_column_name)]] = renderPlot({
        boxplot(
          column_data,
          col = terrain.colors(1),
          outcol = "red"
        )
      })
    })

    output$qualitative_plots = renderUI({
      div(
        class = "d-flex flex-wrap gap-3",
        lapply(qualitative_variables, function(column_name) {
          temp_column_name = column_name

          card(
            full_screen = TRUE,
            style = "width: 32%; min-width: 250px;",
            card_header(paste("Boxplot of", temp_column_name)),
            plotOutput(outputId = paste0("plot", temp_column_name))
          )
        })
      )
    })
    
    lapply(qualitative_variables, function(column_name) {
      counts = sort(table(dataset[[column_name]]), decreasing = TRUE)

      temp_column_name = column_name

      output[[paste0("plot", temp_column_name)]] = renderPlot({
        barplot(
          counts,
          main = paste("Bar Chart of", temp_column_name),
          col = terrain.colors(length(counts))
        )
      })
    })
    
    # for(column_name in qualitative_variables) {
    #   column_data = dataset[[column_name]]
    # 
    #   attribute_count = sort(table(column_data), decreasing = TRUE)
    #   print(attribute_count)
    # 
    #   bar_colors = terrain.colors(length(names(attribute_count)))
    # 
    #   barplot(
    #     attribute_count,
    #     main = paste("Bar chart of", column_name),
    #     col = bar_colors
    #   )
    # }

    # Task 3.5
    # When the response variable is specified as an argument, it should run the best predictive model for that response category (consider only continuous and binary response variables) and select features considering all other meaningful variables. Your function should print relevant diagnostic metrics and plots for the selected model.

    # return(list(
    #   outliers = outlier_results
    # ))
  })
}


shinyApp(ui = app_ui, server = app_server)