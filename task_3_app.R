library(shiny)
library(DescTools)
library(modEvA)

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
      ),
    ),
    textInput( 
      "response_variable", 
      "Response Variable", 
      placeholder = "Enter Name",
      value = "HasCrCard"
    ),
    actionButton("action", label = "Start Process")
  ),
  tableOutput("dataset_head"),
  uiOutput("selected_variables"),
  tableOutput("qualitative_variables"),
  tableOutput("quantitative_variables"),
  uiOutput("quantitative_plots"),
  uiOutput("qualitative_plots"),
  uiOutput("findings"),
  uiOutput("analysis_output"),
)

app_server = function(input, output) {

  observeEvent(input$action, {
    
    uploaded_file = input$csv_file
  
    if (is.null(uploaded_file)) { return(NULL) }

    outlier_method = input$outlier_method
    response_variable = input$response_variable
  
    dataset = read.csv(uploaded_file$datapath, header = TRUE)
    
    output$dataset_head = renderTable(head(bank_data), striped = TRUE) 
  
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

    output$selected_variables = renderUI({
      h5("Selected Variables", style = "font-weight: bold;")
    })
    
    output$qualitative_variables = renderTable(data.frame(QualitativeVariables = qualitative_variables))
    output$quantitative_variables = renderTable(data.frame(QuantitativeVariables = quantitative_variables))

    # Counting the missing values.
    missing_counts = sapply(
      dataset,
      FUN = function(x) { sum(is.na(x)) }
    )

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

    all_variables = c(qualitative_variables, quantitative_variables)
    my_model      = NULL

    set.seed(2425499)
    number_of_rows   = nrow(dataset)
    train_percentage = 0.8
    train_ids = sample(1:number_of_rows, number_of_rows * train_percentage, replace = FALSE)

    train_dataset = dataset[train_ids, ]
    test_dataset  = dataset[-train_ids, ]
    
    output$findings = renderUI({
      h5("Findings", style = "font-weight: bold;")
    })

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

        output$analysis_output = renderUI({
          layout_columns(
            card(
              "Test Predictions vs Test Actuals",
              full_screen = TRUE,
              plotOutput(outputId = "lm_plot")
            ),
            card(
              "Summary",
              verbatimTextOutput("summary"),
              card_footer(verbatimTextOutput("metrics_output"))
            )
          )
        })

        output$lm_plot = renderPlot({
          plot(x = test_actuals_data, y = test_prediction_data)
        })

        output$metrics_output = renderPrint({
          cat("MSE:", mse, "\nRMSE:", rmse, "\nMAE:", mae, "\nR²:", r_squared, "\n")
        })
        
        output$summary = renderPrint({
          print(summary(my_model))
        })

      } else if (response_variable %in% qualitative_variables) {

        if (length(unique(dataset[[response_variable]])) == 2) {

          my_model = glm(formula=my_formula, data=train_dataset, family = binomial)

          test_probability_data = predict.glm(my_model, newdata = test_dataset, type = "response")
          test_prediction_data  = ifelse(test_probability_data > 0.5, 1, 0)
          test_actuals_data     = test_dataset[[response_variable]]
          
          hoslem_results = HLfit(
            model = my_model,
            bin.method = "n.bins",
            n.bins = 10
          )
          
          output$analysis_output = renderUI({
            layout_columns(
              card(
                "Hoslem Results",
                verbatimTextOutput("hoslem")
              ),
              card(
                "Summary",
                verbatimTextOutput("summary"),
                card_footer(verbatimTextOutput("metrics_output"))
              )
            )
          })
          
          output$metrics_output = renderPrint({
            cat(
              "Confusion Matrix", table(
                test_actuals_data,
                test_prediction_data,
                dnn = c("Predicted", "Actual")
              ),
              "\nAccuracy", mean(test_actuals_data == test_prediction_data))
          })
          
          output$summary = renderPrint({
            print(summary(my_model))
          })
          
          output$hoslem = renderPrint({
            print(str(hoslem_results))
          })
        }
      }
    }
  })
}

shinyApp(ui = app_ui, server = app_server)