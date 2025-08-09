library(shiny)
library(readxl)
library(ggplot2)
library(corrplot)
library(cluster)
library(caret)
library(randomForest)
library(class)   
library(e1071)  
library(xgboost)
library(dplyr)
library(factoextra)  
library(pROC)  

ui <- fluidPage(
  titlePanel("Analiza statistikore dhe Modelet e Vlerësimit"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Ngarko skedarin Excel", accept = c(".xlsx")),
      helpText("Ngarko skedar me të dhëna."),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary i Datasetit",
                 verbatimTextOutput("data_summary")
        ),
        tabPanel("Histogram",
                 selectInput("hist_variable", "Zgjidh variablin për Histogram:", choices = NULL),
                 plotOutput("histPlot")
        ),
        tabPanel("Boxplot",
                 selectInput("boxplot_variable", "Zgjidh variablin për Boxplot:", choices = NULL),
                 plotOutput("boxPlot")
        ),
        tabPanel("Correlation Matrix",
                 plotOutput("corrPlot")
        ),
        tabPanel("Cluster Analysis",
                 numericInput("num_clusters", "Numri i klasterëve:", value = 3, min = 1, max = 10),
                 selectInput("cluster_var1", "Zgjidh variablin 1:", choices = NULL),
                 selectInput("cluster_var2", "Zgjidh variablin 2:", choices = NULL),
                 plotOutput("clusterPlot")
        ),
        tabPanel("Modelim & Vlerësim",
                 selectInput("target_variable", "Zgjidh variablin target (variabël binar):", choices = NULL),
                 selectInput("model_select", "Zgjidh modelin:",
                             choices = c("Regresion Logjistik", "Random Forest", "SVM", "XGBoost")),
                 actionButton("run_model", "Ekzekuto Modelin"),
                 verbatimTextOutput("model_summary"),
                 plotOutput("model_confusion"),
                 plotOutput("rocPlot"),
                 plotOutput("model_varimp_plot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file1)
    df <- read_excel(input$file1$datapath)
    # Pastrim për modelim: heq rreshtat me NA dhe kolona me shumë NA
    df <- df %>% na.omit()
    return(df)
  })
  
  # Përditëso zgjedhjet e variablave sipas të dhënave të ngarkuara
  observe({
    req(data())
    vars <- names(data())
    updateSelectInput(session, "hist_variable", choices = vars)
    updateSelectInput(session, "boxplot_variable", choices = vars)
    updateSelectInput(session, "target_variable", choices = vars)
  })
  
  # UI për zgjedhjen e variablave për korrelacion (multiple select)
  output$corr_vars_ui <- renderUI({
    req(data())
    selectInput("corr_vars", "Zgjidh variablat për korrelacion:", 
                choices = names(data()), multiple = TRUE,
                selected = names(data())[1:min(5, length(names(data())))])
  })
  
  # UI për zgjedhjen e variablave për clustering (multiple select)
  output$cluster_vars_ui <- renderUI({
    req(data())
    selectInput("cluster_vars", "Zgjidh variablat për clustering:", 
                choices = names(data()), multiple = TRUE,
                selected = names(data())[1:min(3, length(names(data())))])
  })
  
  # Summary të datasetit
  output$data_summary <- renderPrint({
    req(data()) 
    summary(data())
  })
  
  # Histogram
  output$histPlot <- renderPlot({
    req(data(), input$hist_variable)
    ggplot(data(), aes_string(x = input$hist_variable)) +
      geom_histogram(fill = "steelblue", color = "black", bins = 30) +
      theme_minimal() +
      labs(title = paste("Histogram për", input$hist_variable), x = input$hist_variable)
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    req(data(), input$boxplot_variable)
    ggplot(data(), aes_string(y = input$boxplot_variable)) +
      geom_boxplot(fill = "red", color = "black") +
      theme_minimal() +
      labs(title = paste("Boxplot për", input$boxplot_variable), y = input$boxplot_variable)
  })
  
  # Correlation matrix
  output$corrPlot <- renderPlot({
    req(data())
    df <- data()
    df_num <- df %>% select(where(is.numeric))
    corr_mat <- cor(df_num)
    corrplot(corr_mat, method = "color", addCoef.col = "black", number.cex = 0.7)
  })
  # Cluster Analysis
  # Variablat numerikë
  numeric_vars <- reactive({
    req(data())
    names(data())[sapply(data(), is.numeric)]
  })
  
  # Përditësoj opsionet për variablat e clusterit
  observe({
    req(numeric_vars())
    updateSelectInput(session, "cluster_var1", choices = numeric_vars(),
                      selected = numeric_vars()[1])
    updateSelectInput(session, "cluster_var2", choices = numeric_vars(),
                      selected = numeric_vars()[2])
  })
  
  # Klasterizimi dhe vizualizimi me fviz_cluster
  output$clusterPlot <- renderPlot({
    req(data(), input$num_clusters, input$cluster_var1, input$cluster_var2)
    
    # Marr vetëm dy variablat për klasterizim
    df <- data() %>% select(all_of(c(input$cluster_var1, input$cluster_var2))) %>% na.omit()
    
    # Klasterizimi K-means
    data_scaled <- scale(df)
    kmeans_res <- kmeans(data_scaled, centers = input$num_clusters)
    
    # Vizualizimi me elips konveks dhe pikat
    fviz_cluster(kmeans_res, data = data_scaled,
                 geom = "point",
                 ellipse.type = "convex",
                 ggtheme = theme_minimal()) +
      labs(title = paste("K-means Clustering me", input$num_clusters, "klasterë"),
           x = input$cluster_var1, y = input$cluster_var2)
  })
  # Analiza e modeleve
  observeEvent(input$run_model, {
    req(data(), input$target_variable, input$model_select)
    df <- data()
    
    # Sigurohemi që targeti është variabël binar
    if (!is.factor(df[[input$target_variable]])) {
      df[[input$target_variable]] <- as.factor(df[[input$target_variable]])
    }
    if (length(levels(df[[input$target_variable]])) != 2) {
      output$model_summary <- renderPrint("Variabli target nuk është binar. Ju lutem zgjidhni një variabël binar.")
      output$model_confusion <- renderTable(NULL)
      output$model_varimp_plot <- renderPlot(NULL)
      output$rocPlot <- renderPlot(NULL)
      return(NULL)
    }
    
    predictors <- setdiff(names(df), input$target_variable)
    df_model <- df[, c(predictors, input$target_variable)]
    
    set.seed(123)
    trainIndex <- createDataPartition(df_model[[input$target_variable]], p = 0.8, list = FALSE)
    trainData <- df_model[trainIndex, ]
    testData <- df_model[-trainIndex, ]
    
    model <- NULL
    pred <- NULL
    pred_prob <- NULL
    
    if (input$model_select == "Regresion Logjistik") {
      formula_str <- as.formula(paste(input$target_variable, "~ ."))
      model <- glm(formula_str, data = trainData, family = binomial)
      pred_prob <- predict(model, newdata = testData, type = "response")
      pred <- factor(ifelse(pred_prob > 0.5,
                            levels(trainData[[input$target_variable]])[2],
                            levels(trainData[[input$target_variable]])[1]),
                     levels = levels(trainData[[input$target_variable]]))
    }
    
    if (input$model_select == "Random Forest") {
      model <- randomForest(as.formula(paste(input$target_variable, "~ .")), data = trainData)
      pred_prob <- predict(model, newdata = testData, type = "prob")[, 2]  # probabiliteti për klasën pozitive
      pred <- factor(ifelse(pred_prob > 0.5,
                            levels(trainData[[input$target_variable]])[2],
                            levels(trainData[[input$target_variable]])[1]),
                     levels = levels(trainData[[input$target_variable]]))
    }
    
    if (input$model_select == "SVM") {
      model <- svm(as.formula(paste(input$target_variable, "~ .")), data = trainData, probability = TRUE)
      pred_prob <- attr(predict(model, newdata = testData, probability = TRUE), "probabilities")[, levels(trainData[[input$target_variable]])[2]]
      pred <- factor(ifelse(pred_prob > 0.5,
                            levels(trainData[[input$target_variable]])[2],
                            levels(trainData[[input$target_variable]])[1]),
                     levels = levels(trainData[[input$target_variable]]))
    }
    
    if (input$model_select == "XGBoost") {
      train_label <- as.numeric(trainData[[input$target_variable]]) - 1
      test_label <- as.numeric(testData[[input$target_variable]]) - 1
      train_matrix <- xgb.DMatrix(data = as.matrix(trainData[, predictors]), label = train_label)
      test_matrix <- xgb.DMatrix(data = as.matrix(testData[, predictors]), label = test_label)
      
      params <- list(objective = "binary:logistic", eval_metric = "error")
      model <- xgb.train(params, train_matrix, nrounds = 50, verbose = 0)
      pred_prob <- predict(model, test_matrix)
      pred <- factor(ifelse(pred_prob > 0.5,
                            levels(trainData[[input$target_variable]])[2],
                            levels(trainData[[input$target_variable]])[1]),
                     levels = levels(trainData[[input$target_variable]]))
    }
    
    # Output summary modeli
    output$model_summary <- renderPrint({
      if (is.character(model)) {
        cat(model)
      } else if (inherits(model, "glm")) {
        cat("Regresioni Logjistik - Përmbledhje e Modelit:\n\n")
        print(summary(model))
        cat("\nKoeficientët me Intervalet e Besueshmërisë:\n")
        print(confint(model))
        cat("\nAIC:", AIC(model), "\n")
      } else if (inherits(model, "randomForest")) {
        cat("Random Forest - Përmbledhje e Modelit:\n\n")
        print(model)
        cat("\nRëndësia e Variablave:\n")
        print(randomForest::importance(model))
      } else if (inherits(model, "svm")) {
        cat("SVM - Përmbledhje e Modelit:\n\n")
        print(model)
        cat("\nNumri i Support Vectors:", nrow(model$SV), "\n")
      } else if (inherits(model, "xgb.Booster")) {
        cat("XGBoost - Rëndësia e Variablave:\n\n")
        imp <- xgboost::xgb.importance(model = model)
        print(imp)
      } else {
        cat("Nuk ka summary të detajuar për këtë model.\n")
      }
    })
    
    # Confusion matrix
    output$model_confusion <- renderPlot({
      req(pred)
      cm <- confusionMatrix(pred, testData[[input$target_variable]])
      cm_table <- as.data.frame(cm$table)
      
      ggplot(cm_table, aes(Prediction, Reference, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = Freq), color = "white", size = 6) +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal() +
        labs(title = "Confusion Matrix Heatmap", x = "E parashkiuar", y = "E vërteta")
    })
    
    # Variable importance plot
    output$model_varimp_plot <- renderPlot({
      if (inherits(model, "randomForest")) {
        varImpPlot(model)
      } else if (inherits(model, "xgb.Booster")) {
        imp <- xgboost::xgb.importance(model = model)
        xgboost::xgb.plot.importance(imp)
      } else {
        plot.new()
        title("Grafiku i rëndësisë së variablave nuk është në dispozicion për këtë model.")
      }
    })
    
    # ROC Curve
    output$rocPlot <- renderPlot({
      req(pred_prob)
      true_labels <- testData[[input$target_variable]]
      if (!is.factor(true_labels)) {
        true_labels <- as.factor(true_labels)
      }
      roc_obj <- roc(response = true_labels,
                     predictor = pred_prob,
                     levels = rev(levels(true_labels)))
      plot(roc_obj, col = "blue", main = "ROC Curve")
      auc_val <- auc(roc_obj)
      legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lwd = 2)
    })
  })
}

shinyApp(ui = ui, server = server)
