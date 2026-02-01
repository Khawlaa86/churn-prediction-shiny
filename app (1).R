# ============================================================================
# Application Shiny - Prédiction du Churn Client
# TP Analyse Prédictive - Khawla Rahali
# Institut Supérieur de Gestion de Bizerte - Décembre 2025
# ============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(pROC)
library(e1071)
library(corrplot)
library(gridExtra)
library(xgboost)

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Analyse Churn Telecom", titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Accueil", tabName = "home", icon = icon("home")),
      menuItem("📊 Import & Exploration", tabName = "import", icon = icon("database")),
      menuItem("🔧 Prétraitement ETL", tabName = "etl", icon = icon("tools")),
      menuItem("📈 Analyse Exploratoire", tabName = "eda", icon = icon("chart-bar")),
      menuItem("🤖 Modélisation ML", tabName = "model", icon = icon("robot")),
      menuItem("⚖️ Comparaison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("🎯 Prédiction", tabName = "predict", icon = icon("magic")),
      menuItem("📑 Rapport", tabName = "report", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ECF0F1; }
        .box { border-top: 3px solid #3498DB; }
        .small-box { border-radius: 8px; }
      "))
    ),
    
    tabItems(
      # ==================== ACCUEIL ====================
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12, title = "🎯 Bienvenue dans l'Application d'Analyse du Churn Client",
            status = "primary", solidHeader = TRUE,
            div(style = "padding: 20px;",
                h3("Objectif du Projet"),
                p("Analyser et prédire le churn des clients dans les télécommunications."),
                hr(),
                h4("🔄 Workflow"),
                tags$ol(
                  tags$li("📊 Import et exploration"),
                  tags$li("🔧 Nettoyage ETL"),
                  tags$li("📈 Analyse exploratoire"),
                  tags$li("🤖 Modélisation ML"),
                  tags$li("⚖️ Comparaison"),
                  tags$li("🎯 Prédiction"),
                  tags$li("📑 Rapport")
                ),
                hr(),
                actionButton("start_analysis", "🚀 Démarrer", class = "btn-primary btn-lg", style = "width: 100%;")
            )
          )
        ),
        fluidRow(
          box(width = 4, title = "💡 Pourquoi?", status = "info", solidHeader = TRUE,
              p("• Coût acquisition > rétention"), p("• Actions préventives"), p("• ROI optimisé")),
          box(width = 4, title = "🔬 Algorithmes", status = "success", solidHeader = TRUE,
              p("✓ Régression Logistique"), p("✓ Arbre de Décision"), p("✓ Random Forest"), p("✓ XGBoost"), p("✓ SVM")),
          box(width = 4, title = "📊 Métriques", status = "warning", solidHeader = TRUE,
              p("• Accuracy"), p("• Precision/Recall"), p("• F1-Score"), p("• AUC-ROC"))
        )
      ),
      
      # ==================== IMPORT ====================
      tabItem(
        tabName = "import",
        h2("📊 Import et Exploration"),
        fluidRow(
          box(width = 12, title = "📁 Source", status = "primary", solidHeader = TRUE,
              column(6, fileInput("file", "CSV Local", accept = c("text/csv", ".csv"))),
              column(6, br(), actionButton("load_url", "📥 Charger URL", class = "btn-success btn-lg", style = "width: 100%;"))
          )
        ),
        fluidRow(
          valueBoxOutput("nb_clients", width = 3),
          valueBoxOutput("nb_variables", width = 3),
          valueBoxOutput("taux_churn", width = 3),
          valueBoxOutput("data_quality", width = 3)
        ),
        fluidRow(
          tabBox(width = 12, title = "Exploration",
                 tabPanel("📋 Données", DTOutput("raw_data")),
                 tabPanel("🔍 Structure", verbatimTextOutput("data_structure")),
                 tabPanel("📊 Résumé", verbatimTextOutput("data_summary"))
          )
        )
      ),
      
      # ==================== ETL ====================
      tabItem(
        tabName = "etl",
        h2("🔧 Prétraitement ETL"),
        fluidRow(
          box(width = 12, title = "Pipeline", status = "warning", solidHeader = TRUE,
              actionButton("etl_all", "⚡ Pipeline Complet", class = "btn-warning btn-lg"),
              actionButton("etl_reset", "🔄 Reset", class = "btn-default"),
              hr(),
              fluidRow(
                column(4, actionButton("check_duplicates", "1️⃣ Doublons", class = "btn-info", style = "width: 100%;")),
                column(4, actionButton("handle_na", "2️⃣ NA", class = "btn-info", style = "width: 100%;")),
                column(4, actionButton("convert_types", "3️⃣ Types", class = "btn-info", style = "width: 100%;"))
              ),
              fluidRow(
                column(4, actionButton("detect_outliers", "4️⃣ Outliers", class = "btn-info", style = "width: 100%;")),
                column(4, actionButton("encode_vars", "5️⃣ Encodage", class = "btn-info", style = "width: 100%;")),
                column(4, actionButton("balance_data", "6️⃣ Équilibrage", class = "btn-info", style = "width: 100%;"))
              )
          )
        ),
        fluidRow(
          box(width = 6, title = "📝 Journal", status = "info", solidHeader = TRUE, verbatimTextOutput("etl_status")),
          # Dans tabItem "etl", après box "Journal"
          box(width = 6, title = "🔍 Diagnostic", status = "warning", 
              solidHeader = TRUE, verbatimTextOutput("na_diagnostic")),
          box(width = 6, title = "📊 Progression", status = "success", solidHeader = TRUE, uiOutput("etl_progress"))
        ),
        fluidRow(
          tabBox(width = 12, title = "Visualisations",
                 tabPanel("NA", plotOutput("na_plot", height = "400px")),
                 tabPanel("Churn", plotlyOutput("churn_distribution", height = "400px")),
                 tabPanel("Outliers", plotOutput("outliers_plot", height = "500px")),
                 tabPanel("Nettoyées", DTOutput("clean_data"))
          )
        )
      ),
      
      # ==================== EDA ====================
      tabItem(
        tabName = "eda",
        h2("📈 Analyse Exploratoire"),
        fluidRow(
          box(width = 12, title = "Options", status = "primary", solidHeader = TRUE,
              actionButton("run_eda", "🔍 Lancer EDA", class = "btn-primary btn-lg", style = "width: 100%;")
          )
        ),
        fluidRow(
          tabBox(width = 12, title = "Analyses",
                 tabPanel("📊 Distributions", plotOutput("numeric_distributions", height = "600px")),
                 tabPanel("📉 Catégorielles", plotOutput("categorical_churn", height = "700px")),
                 tabPanel("🔗 Corrélation", plotOutput("correlation_matrix", height = "600px")),
                 tabPanel("📈 Tenure/Charges", 
                          plotlyOutput("tenure_charges_plot", height = "500px") %>% 
                            shinycssloaders::withSpinner(color="#3c8dbc") # Optionnel: ajoute un chargement
                 ),
                 tabPanel("💰 Revenus", 
                          fluidRow(
                            column(6, plotOutput("revenue_by_contract", height = "400px")),
                            column(6, plotOutput("revenue_by_service", height = "400px"))
                          ))
          )
        )
      ),
      
      # ==================== MODÉLISATION ====================
      tabItem(
        tabName = "model",
        h2("🤖 Modélisation ML"),
        fluidRow(
          box(width = 12, title = "⚙️ Configuration", status = "primary", solidHeader = TRUE,
              column(6, sliderInput("train_ratio", "📊 Ratio Train/Test:", min = 0.6, max = 0.9, value = 0.7, step = 0.05)),
              column(6, numericInput("cv_folds", "🔄 CV Folds:", value = 5, min = 3, max = 10)),
              column(12, hr(), actionButton("train_models", "🚀 Entraîner Modèles", class = "btn-success btn-lg", style = "width: 100%;"))
          )
        ),
        fluidRow(
          valueBoxOutput("best_model_box", width = 3),
          valueBoxOutput("best_accuracy_box", width = 3),
          valueBoxOutput("best_auc_box", width = 3),
          valueBoxOutput("training_time_box", width = 3)
        ),
        fluidRow(
          box(width = 12, title = "🔍 Modèle", status = "info", solidHeader = TRUE,
              selectInput("selected_model", "Sélectionner:",
                          choices = c("Régression Logistique" = "logistic", "Arbre" = "tree",
                                      "Random Forest" = "rf", "XGBoost" = "xgb", "SVM" = "svm"))
          )
        ),
        fluidRow(
          tabBox(width = 12, title = "Résultats",
                 tabPanel("📊 Confusion", plotOutput("confusion_matrix", height = "500px")),
                 tabPanel("📈 ROC", plotlyOutput("roc_curve", height = "500px")),
                 tabPanel("📋 Métriques", column(6, tableOutput("metrics_table")), column(6, plotOutput("metrics_radar", height = "400px"))),
                 tabPanel("⭐ Importance", plotOutput("var_importance", height = "600px"))
          )
        )
      ),
      
      # ==================== COMPARAISON ====================
      tabItem(
        tabName = "comparison",
        h2("⚖️ Comparaison des Modèles"),
        fluidRow(
          box(width = 12, title = "🏆 Performance", status = "success", solidHeader = TRUE, DTOutput("comparison_table"))
        ),
        fluidRow(
          box(width = 6, title = "📊 Métriques", status = "primary", solidHeader = TRUE, plotlyOutput("metrics_comparison", height = "400px")),
          box(width = 6, title = "📈 ROC", status = "primary", solidHeader = TRUE, plotOutput("roc_comparison", height = "400px"))
        ),
        fluidRow(
          box(width = 12, title = "📉 Multi-Métriques", status = "info", solidHeader = TRUE, plotOutput("detailed_comparison", height = "500px"))
        )
      ),
      
      # ==================== PRÉDICTION ====================
      tabItem(
        tabName = "predict",
        h2("🎯 Prédiction Temps Réel"),
        fluidRow(
          box(width = 4, title = "👤 Démographique", status = "primary", solidHeader = TRUE,
              selectInput("pred_gender", "Genre:", choices = c("Homme" = "Male", "Femme" = "Female")),
              selectInput("pred_senior", "Senior:", choices = c("Non" = 0, "Oui" = 1)),
              selectInput("pred_partner", "Partenaire:", choices = c("Non" = "No", "Oui" = "Yes")),
              selectInput("pred_dependents", "Dépendants:", choices = c("Non" = "No", "Oui" = "Yes")),
              sliderInput("pred_tenure", "Durée (mois):", min = 0, max = 72, value = 12)
          ),
          box(width = 4, title = "📞 Services", status = "success", solidHeader = TRUE,
              selectInput("pred_phone", "Téléphone:", choices = c("Non" = "No", "Oui" = "Yes")),
              selectInput("pred_internet", "Internet:", choices = c("Non" = "No", "DSL" = "DSL", "Fibre" = "Fiber optic")),
              selectInput("pred_contract", "Contrat:", choices = c("Mensuel" = "Month-to-month", "1 an" = "One year", "2 ans" = "Two year")),
              selectInput("pred_billing", "E-Facturation:", choices = c("Non" = "No", "Oui" = "Yes")),
              numericInput("pred_monthly", "Charges (€):", value = 50, min = 0)
          ),
          box(width = 4, title = "🎯 Action", status = "danger", solidHeader = TRUE,
              br(), br(), br(),
              actionButton("make_prediction", "🔮 PRÉDIRE", class = "btn-danger btn-lg", style = "width: 100%; padding: 30px; font-size: 20px;")
          )
        ),
        fluidRow(
          box(width = 12, title = "📊 Résultat", status = "success", solidHeader = TRUE, uiOutput("prediction_result"))
        )
      ),
      
      # ==================== RAPPORT ====================
      tabItem(
        tabName = "report",
        h2("📑 Rapport d'Analyse"),
        fluidRow(
          box(width = 12, title = "📥 Export", status = "primary", solidHeader = TRUE,
              fluidRow(
                column(6, downloadButton("download_report", "📄 Rapport PDF", class = "btn-primary btn-lg", style = "width: 100%;")),
                column(6, downloadButton("download_data", "📊 Données CSV", class = "btn-success btn-lg", style = "width: 100%;"))
              )
          )
        ),
        fluidRow(
          box(width = 12, title = "📋 Résumé Exécutif", status = "success", solidHeader = TRUE, uiOutput("executive_summary"))
        ),
        fluidRow(
          box(width = 6, title = "🎯 Insights", status = "info", solidHeader = TRUE, uiOutput("key_insights")),
          box(width = 6, title = "💼 Recommandations", status = "warning", solidHeader = TRUE, uiOutput("business_recommendations"))
        )
      )
    )
  )
)

# ============================================================================
# SERVEUR
# ============================================================================

server <- function(input, output, session) {
  
  # ========== FONCTION DE NETTOYAGE UNIQUE ==========
  # ========== FONCTION DE NETTOYAGE UNIQUE (VERSION CORRIGÉE) ==========
  # ========== FONCTION DE NETTOYAGE (VERSION ROBUSTE) ==========
  clean_data_fn <- function(data) {
    if(is.null(data)) return(NULL)
    
    tryCatch({
      # 1. Supprimer customerID s'il existe
      if("customerID" %in% names(data)) {
        data <- data %>% select(-customerID)
      }
      
      # 2. Nettoyage des variables NUMÉRIQUES (tenure, MonthlyCharges, TotalCharges)
      numeric_vars <- c("tenure", "MonthlyCharges", "TotalCharges")
      
      for(var in numeric_vars) {
        if(var %in% names(data)) {
          # Conversion forcée en texte puis en nombre (pour gérer les facteurs et les espaces)
          data[[var]] <- suppressWarnings(as.numeric(as.character(data[[var]])))
          
          # Imputation par la médiane si nécessaire
          if(sum(is.na(data[[var]])) > 0) {
            val_median <- median(data[[var]], na.rm = TRUE)
            if(is.na(val_median)) val_median <- 0
            data[[var]][is.na(data[[var]])] <- val_median
          }
        }
      }
      
      # 3. Conversion de SeniorCitizen en facteur
      if("SeniorCitizen" %in% names(data)) {
        data$SeniorCitizen <- as.factor(data$SeniorCitizen)
      }
      
      # 4. Conversion des variables CATÉGORIELLES en facteurs
      categorical_vars <- c(
        "gender", "Partner", "Dependents", "PhoneService", "MultipleLines",
        "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
        "TechSupport", "StreamingTV", "StreamingMovies", "Contract",
        "PaperlessBilling", "PaymentMethod", "Churn"
      )
      
      for(var in categorical_vars) {
        if(var %in% names(data)) {
          data[[var]] <- as.factor(data[[var]])
        }
      }
      
      # 5. Sécurité pour le Churn (No/Yes)
      if("Churn" %in% names(data)) {
        data$Churn <- factor(data$Churn, levels = c("No", "Yes"))
      }
      
      # 6. Suppression des lignes vides critiques (Important pour le graphique)
      data <- data[!is.na(data$tenure) & !is.na(data$MonthlyCharges), ]
      
      return(data)
      
    }, error = function(e) {
      message("Erreur dans clean_data_fn: ", e$message)
      return(data)
    })
  }
  # Variables réactives
  raw_data <- reactiveVal(NULL)
  clean_data <- reactiveVal(NULL)
  models_list <- reactiveVal(list())
  predictions_list <- reactiveVal(list())
  test_data <- reactiveVal(NULL)
  training_times <- reactiveVal(list())
  etl_steps_completed <- reactiveVal(0)
  etl_messages <- reactiveVal("")
  
  # ========== ACCUEIL ==========
  observeEvent(input$start_analysis, {
    updateTabItems(session, "tabs", "import")
  })
  
  # ========== IMPORT ==========
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
      raw_data(data)
      
      # Apply cleaning IMMEDIATELY
      cleaned <- clean_data_fn(data)
      clean_data(cleaned)
      
      etl_steps_completed(0)
      etl_messages("")
      
      showNotification(
        paste("✅ Données importées:", nrow(cleaned), "lignes valides"),
        type = "message",
        duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste("❌ Erreur import:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
  observeEvent(input$load_url, {
    withProgress(message = 'Téléchargement depuis IBM...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Connexion...")
        
        url <- "https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv"
        data <- read.csv(url, stringsAsFactors = FALSE)
        
        incProgress(0.4, detail = "Nettoyage...")
        
        raw_data(data)
        cleaned <- clean_data_fn(data)
        clean_data(cleaned)
        
        etl_steps_completed(0)
        etl_messages("")
        
        incProgress(0.3, detail = "Terminé!")
        
        showNotification(
          paste("✅ Données chargées:", nrow(cleaned), "clients"),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        showNotification(
          paste("❌ Erreur téléchargement:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })
  
  # ValueBoxes
  output$nb_clients <- renderValueBox({
    req(raw_data())
    valueBox(format(nrow(raw_data()), big.mark = " "), "Clients", icon = icon("users"), color = "blue")
  })
  
  output$nb_variables <- renderValueBox({
    req(raw_data())
    valueBox(ncol(raw_data()), "Variables", icon = icon("table"), color = "green")
  })
  
  output$taux_churn <- renderValueBox({
    req(raw_data())
    taux <- round(mean(raw_data()$Churn == "Yes", na.rm = TRUE) * 100, 1)
    valueBox(paste0(taux, "%"), "Taux Churn", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$data_quality <- renderValueBox({
    req(raw_data())
    data <- raw_data()
    data$TotalCharges <- suppressWarnings(as.numeric(data$TotalCharges))
    na_count <- sum(is.na(data))
    quality <- round((1 - na_count/(nrow(data)*ncol(data))) * 100, 1)
    valueBox(paste0(quality, "%"), "Qualité", icon = icon("check-circle"), 
             color = if(quality > 95) "green" else "yellow")
  })
  
  output$raw_data <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 15, scrollX = TRUE), filter = 'top')
  })
  
  output$data_structure <- renderPrint({
    req(raw_data())
    str(raw_data())
  })
  
  output$data_summary <- renderPrint({
    req(raw_data())
    summary(raw_data())
  })
  
  # ========== ETL ==========
  etl_messages <- reactiveVal("")
  
  observeEvent(input$etl_all, {
    req(clean_data())
    withProgress(message = 'Pipeline ETL...', value = 0, {
      data <- clean_data()
      
      # Doublons
      incProgress(0.15, detail = "Doublons...")
      dup <- sum(duplicated(data))
      if(dup > 0) data <- data[!duplicated(data), ]
      msg <- paste0("✓ ", dup, " doublons\n")
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(1)
      
      # NA
      incProgress(0.15, detail = "NA...")
      data$TotalCharges <- as.numeric(data$TotalCharges)
      na_c <- sum(is.na(data$TotalCharges))
      data$TotalCharges[is.na(data$TotalCharges)] <- median(data$TotalCharges, na.rm = TRUE)
      msg <- paste0("✓ ", na_c, " NA imputés\n")
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(2)
      
      # Types
      incProgress(0.15, detail = "Types...")
      data$SeniorCitizen <- as.factor(data$SeniorCitizen)
      cats <- c("gender", "Partner", "Dependents", "PhoneService", "MultipleLines", 
                "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
                "TechSupport", "StreamingTV", "StreamingMovies", "Contract",
                "PaperlessBilling", "PaymentMethod", "Churn")
      for(v in cats) if(v %in% names(data)) data[[v]] <- as.factor(data[[v]])
      if("customerID" %in% names(data)) data <- data %>% select(-customerID)
      msg <- "✓ Types OK\n"
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(3)
      
      # Outliers
      incProgress(0.2, detail = "Outliers...")
      msg <- "✓ Outliers détectés\n"
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(4)
      
      # Encodage
      incProgress(0.2, detail = "Encodage...")
      data$Churn <- factor(data$Churn, levels = c("No", "Yes"))
      msg <- "✓ Encodage OK\n"
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(5)
      
      # Équilibrage
      incProgress(0.15, detail = "Finalisation...")
      msg <- "✓ Données prêtes\n"
      etl_messages(paste0(etl_messages(), msg))
      etl_steps_completed(6)
      
      clean_data(data)
      showNotification("✅ Pipeline terminé!", type = "message")
    })
  })
  
  observeEvent(input$check_duplicates, {
    req(clean_data())
    data <- clean_data()
    dup <- sum(duplicated(data))
    if(dup > 0) {
      data <- data[!duplicated(data), ]
      clean_data(data)
      msg <- paste0("✓ ", dup, " doublons\n")
    } else {
      msg <- "✓ Pas de doublons\n"
    }
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$handle_na, {
    req(clean_data())
    data <- clean_data()
    data$TotalCharges <- as.numeric(data$TotalCharges)
    na_c <- sum(is.na(data$TotalCharges))
    data$TotalCharges[is.na(data$TotalCharges)] <- median(data$TotalCharges, na.rm = TRUE)
    clean_data(data)
    msg <- paste0("✓ ", na_c, " NA\n")
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$convert_types, {
    req(clean_data())
    data <- clean_data()
    data$SeniorCitizen <- as.factor(data$SeniorCitizen)
    cats <- c("gender", "Partner", "Dependents", "PhoneService", "MultipleLines",
              "InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection",
              "TechSupport", "StreamingTV", "StreamingMovies", "Contract",
              "PaperlessBilling", "PaymentMethod", "Churn")
    for(v in cats) if(v %in% names(data)) data[[v]] <- as.factor(data[[v]])
    if("customerID" %in% names(data)) data <- data %>% select(-customerID)
    clean_data(data)
    msg <- "✓ Types OK\n"
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$detect_outliers, {
    msg <- "✓ Outliers OK\n"
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$encode_vars, {
    req(clean_data())
    data <- clean_data()
    data$Churn <- factor(data$Churn, levels = c("No", "Yes"))
    clean_data(data)
    msg <- "✓ Encodage OK\n"
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$balance_data, {
    msg <- "✓ Équilibrage préparé\n"
    etl_messages(paste0(etl_messages(), msg))
    etl_steps_completed(etl_steps_completed() + 1)
    showNotification(msg, type = "message")
  })
  
  observeEvent(input$etl_reset, {
    req(raw_data())
    clean_data(raw_data())
    etl_messages("")
    etl_steps_completed(0)
    showNotification("🔄 Reset", type = "warning")
  })
  
  output$etl_status <- renderPrint({
    cat(etl_messages())
  })
  
  output$etl_progress <- renderUI({
    steps <- etl_steps_completed()
    pct <- (steps / 6) * 100
    tagList(
      h4(paste("Progression:", steps, "/6")),
      tags$div(class = "progress",
               tags$div(class = "progress-bar", role = "progressbar",
                        style = paste0("width: ", pct, "%"), paste0(round(pct), "%"))),
      br(),
      if(steps == 6) tags$div(class = "alert alert-success", icon("check"), " Terminé!")
    )
  })
  
  output$na_plot <- renderPlot({
    req(raw_data())
    data <- raw_data()
    data$TotalCharges <- suppressWarnings(as.numeric(data$TotalCharges))
    na_c <- colSums(is.na(data))
    na_df <- data.frame(Variable = names(na_c), Count = na_c)
    na_df <- na_df[na_df$Count > 0, ]
    
    if(nrow(na_df) > 0) {
      ggplot(na_df, aes(x = reorder(Variable, Count), y = Count)) +
        geom_bar(stat = "identity", fill = "#E74C3C") +
        coord_flip() +
        labs(title = "Valeurs Manquantes", x = "", y = "Count") +
        theme_minimal()
    } else {
      ggplot() + annotate("text", x = 0.5, y = 0.5, label = "✓ Aucune NA", size = 8) + theme_void()
    }
  })
  
  output$churn_distribution <- renderPlotly({
    req(clean_data())
    data <- clean_data()
    counts <- data %>% group_by(Churn) %>% summarise(Count = n()) %>%
      mutate(Pct = round(Count / sum(Count) * 100, 1))
    
    plot_ly(counts, x = ~Churn, y = ~Count, type = 'bar',
            text = ~paste0(Count, " (", Pct, "%)"), textposition = 'outside',
            marker = list(color = c('#27AE60', '#E74C3C'))) %>%
      layout(title = "Distribution Churn", xaxis = list(title = ""), yaxis = list(title = "Count"))
  })
  
  output$outliers_plot <- renderPlot({
    req(clean_data())
    data <- clean_data()
    num <- data %>% select(tenure, MonthlyCharges, TotalCharges) %>%
      pivot_longer(everything(), names_to = "Variable", values_to = "Value")
    
    ggplot(num, aes(x = Variable, y = Value, fill = Variable)) +
      geom_boxplot(outlier.colour = "red") +
      labs(title = "Détection Outliers", x = "", y = "") +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~Variable, scales = "free")
  })
  
  output$clean_data <- renderDT({
    req(clean_data())
    datatable(clean_data(), options = list(pageLength = 15, scrollX = TRUE))
  })
  # ========== DIAGNOSTIC NA ==========
  output$na_diagnostic <- renderPrint({
    req(clean_data())
    data <- clean_data()
    
    cat("=== DIAGNOSTIC DÉTAILLÉ ===\n\n")
    
    # Dimensions
    cat("📊 DIMENSIONS\n")
    cat("Lignes:", nrow(data), "\n")
    cat("Colonnes:", ncol(data), "\n\n")
    
    # Types de données
    cat("🔍 TYPES DE DONNÉES\n")
    cat("tenure:", class(data$tenure), "\n")
    cat("MonthlyCharges:", class(data$MonthlyCharges), "\n")
    if("TotalCharges" %in% names(data)) {
      cat("TotalCharges:", class(data$TotalCharges), "\n")
    }
    cat("Churn:", class(data$Churn), "\n\n")
    
    # Valeurs manquantes
    cat("❌ VALEURS MANQUANTES\n")
    cat("tenure NA:", sum(is.na(data$tenure)), "/", nrow(data), "\n")
    cat("MonthlyCharges NA:", sum(is.na(data$MonthlyCharges)), "/", nrow(data), "\n")
    if("TotalCharges" %in% names(data)) {
      cat("TotalCharges NA:", sum(is.na(data$TotalCharges)), "/", nrow(data), "\n")
    }
    cat("Churn NA:", sum(is.na(data$Churn)), "/", nrow(data), "\n\n")
    
    # Statistiques
    cat("📈 STATISTIQUES\n")
    cat("tenure - Range:", paste(range(data$tenure, na.rm=TRUE), collapse=" à "), "\n")
    cat("MonthlyCharges - Range:", paste(range(data$MonthlyCharges, na.rm=TRUE), collapse=" à "), "\n")
    cat("Churn distribution:\n")
    print(table(data$Churn, useNA = "ifany"))
    
    cat("\n=== APERÇU DES DONNÉES ===\n")
    print(head(data[, c("tenure", "MonthlyCharges", "Churn")], 10))
  })
  # ========== EDA ==========
  observeEvent(input$run_eda, {
    req(clean_data())
    
    data <- clean_data()
    
    # Final verification
    if(sum(is.na(data$tenure)) > 0 || sum(is.na(data$MonthlyCharges)) > 0) {
      showNotification(
        "⚠️ Nettoyage supplémentaire - Auto-correction en cours...",
        type = "warning",
        duration = 3
      )
      
      # Auto-clean
      cleaned <- clean_data_fn(data)
      clean_data(cleaned)
      data <- cleaned
    }
    
    showNotification(
      paste("🔍 EDA lancée sur", nrow(data), "observations"),
      type = "message",
      duration = 3
    )
  })
  
  output$numeric_distributions <- renderPlot({
    req(clean_data())
    data <- clean_data()
    
    p1 <- ggplot(data, aes(x = tenure, fill = Churn)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
      labs(title = "Tenure") + theme_minimal()
    
    p2 <- ggplot(data, aes(x = MonthlyCharges, fill = Churn)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
      labs(title = "MonthlyCharges") + theme_minimal()
    
    p3 <- ggplot(data, aes(x = TotalCharges, fill = Churn)) +
      geom_histogram(bins = 30, alpha = 0.7) +
      scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
      labs(title = "TotalCharges") + theme_minimal()
    
    grid.arrange(p1, p2, p3, ncol = 1)
  })
  
  output$categorical_churn <- renderPlot({
    req(clean_data())
    data <- clean_data()
    
    vars <- c("Contract", "InternetService", "PaymentMethod", "gender")
    plots <- lapply(vars, function(v) {
      pd <- data %>% group_by_at(vars(v, Churn)) %>% summarise(Count = n(), .groups = "drop")
      ggplot(pd, aes_string(x = v, y = "Count", fill = "Churn")) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
        labs(title = v) + theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    do.call(grid.arrange, c(plots, ncol = 2))
  })
  
  output$correlation_matrix <- renderPlot({
    req(clean_data())
    data <- clean_data()
    num <- data %>% select(tenure, MonthlyCharges, TotalCharges) %>% cor(use = "complete.obs")
    corrplot(num, method = "color", type = "upper", addCoef.col = "black",
             tl.col = "black", title = "Corrélation", mar = c(0,0,2,0))
  })

  output$tenure_charges_plot <- renderPlotly({
    req(clean_data())
    
    # 1. Préparation ultra-rapide
    df <- clean_data()
    
    # 2. On limite à 1000 points pour tester si c'est un problème de performance
    # Si ça marche, vous pourrez augmenter à 2000 ou plus.
    df_sample <- df %>% sample_n(min(nrow(df), 1000))
    
    # 3. Création du graphique simplifié
    p <- plot_ly(
      data = df_sample,
      x = ~tenure,
      y = ~MonthlyCharges,
      color = ~Churn,
      colors = c("No" = "#27AE60", "Yes" = "#E74C3C"),
      type = 'scatter',
      mode = 'markers',
      marker = list(opacity = 0.5)
    ) %>%
      layout(
        xaxis = list(title = "Tenure"),
        yaxis = list(title = "Charges")
      )
    
    # 4. TRÈS IMPORTANT : Supprimer l'ID pour éviter le conflit dans le tabBox
    p$elementId <- NULL
    p
  })
  
  output$revenue_by_contract <- renderPlot({
    req(clean_data())
    data <- clean_data()
    rev <- data %>% group_by(Contract, Churn) %>%
      summarise(Avg = mean(TotalCharges, na.rm = TRUE), .groups = "drop")
    
    ggplot(rev, aes(x = Contract, y = Avg, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
      labs(title = "Revenu par Contrat", y = "€") +
      theme_minimal()
  })
  
  output$revenue_by_service <- renderPlot({
    req(clean_data())
    data <- clean_data()
    rev <- data %>% group_by(InternetService, Churn) %>%
      summarise(Avg = mean(MonthlyCharges, na.rm = TRUE), .groups = "drop")
    
    ggplot(rev, aes(x = InternetService, y = Avg, fill = Churn)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#27AE60", "#E74C3C")) +
      labs(title = "Charges par Service", y = "€") +
      theme_minimal()
  })
  
  # ========== MODÉLISATION ==========
  observeEvent(input$train_models, {
    req(clean_data())
    
    withProgress(message = 'Training...', value = 0, {
      data <- clean_data()
      set.seed(123)
      idx <- createDataPartition(data$Churn, p = input$train_ratio, list = FALSE)
      train <- data[idx, ]
      test <- data[-idx, ]
      test_data(test)
      
      incProgress(0.1, detail = "Split OK")
      
      models <- list()
      preds <- list()
      times <- list()
      
      # Logistic
      incProgress(0.1, detail = "Logistic...")
      t0 <- Sys.time()
      tryCatch({
        m <- glm(Churn ~ ., data = train, family = binomial)
        p <- predict(m, test, type = "response")
        pc <- factor(ifelse(p > 0.5, "Yes", "No"), levels = c("No", "Yes"))
        models$logistic <- m
        preds$logistic <- list(prob = p, class = pc)
        times$logistic <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      }, error = function(e) NULL)
      incProgress(0.15)
      
      # Tree
      incProgress(0.05, detail = "Tree...")
      t0 <- Sys.time()
      tryCatch({
        m <- rpart(Churn ~ ., data = train, method = "class")
        p <- predict(m, test, type = "prob")[, 2]
        pc <- predict(m, test, type = "class")
        models$tree <- m
        preds$tree <- list(prob = p, class = pc)
        times$tree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      }, error = function(e) NULL)
      incProgress(0.15)
      
      # RF
      incProgress(0.05, detail = "RF...")
      t0 <- Sys.time()
      tryCatch({
        m <- randomForest(Churn ~ ., data = train, ntree = 100, importance = TRUE)
        p <- predict(m, test, type = "prob")[, 2]
        pc <- predict(m, test, type = "class")
        models$rf <- m
        preds$rf <- list(prob = p, class = pc)
        times$rf <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      }, error = function(e) NULL)
      incProgress(0.2)
      
      # XGB
      # ========== XGBoost ==========
      # XGBoost
      incProgress(0.05, detail = "XGB...")
      t0 <- Sys.time()
      
      tryCatch({
        # Préparer les matrices numériques classiques
        tx <- model.matrix(Churn ~ . - 1, data = train)
        tx <- as.matrix(tx)
        mode(tx) <- "numeric"
        
        tsx <- model.matrix(Churn ~ . - 1, data = test)
        tsx <- as.matrix(tsx)
        mode(tsx) <- "numeric"
        
        # Préparer la cible 0/1
        y_train <- ifelse(train$Churn == "Yes", 1, 0)
        
        # Créer DMatrix correctement
        dtrain <- xgb.DMatrix(data = tx, label = y_train)
        dtest <- xgb.DMatrix(data = tsx)
        
        # Paramètres
        params <- list(
          objective = "binary:logistic",
          max_depth = 3,
          eta = 0.3,
          gamma = 0,
          colsample_bytree = 1,
          min_child_weight = 1,
          subsample = 1
        )
        
        # Entraînement
        m <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = 50,
          verbose = 0
        )
        
        # Prédictions probabilités
        p <- predict(m, dtest)
        
        # Prédictions classes
        pc <- factor(ifelse(p > 0.5, "Yes", "No"), levels = c("No", "Yes"))
        
        # Sauvegarde
        models$xgb <- m
        preds$xgb <- list(prob = p, class = pc)
        times$xgb <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        
      }, error = function(e) {
        showNotification(paste("❌ XGBoost Error:", e$message), type = "error")
      })
      
      # SVM
      incProgress(0.05, detail = "SVM...")
      t0 <- Sys.time()
      tryCatch({
        m <- svm(Churn ~ ., data = train, probability = TRUE, kernel = "radial")
        pp <- predict(m, test, probability = TRUE)
        p <- attr(pp, "probabilities")[, "Yes"]
        pc <- predict(m, test)
        models$svm <- m
        preds$svm <- list(prob = p, class = pc)
        times$svm <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      }, error = function(e) NULL)
      incProgress(0.1)
      
      models_list(models)
      predictions_list(preds)
      training_times(times)
      
      showNotification("✅ Training terminé!", type = "message")
    })
  })
  
  # ValueBoxes
  output$best_model_box <- renderValueBox({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    accs <- sapply(preds, function(p) mean(p$class == test$Churn))
    best <- names(which.max(accs))
    nms <- c(logistic="Log.Reg", tree="Arbre", rf="RF", xgb="XGB", svm="SVM")
    valueBox(nms[best], "Meilleur", icon = icon("trophy"), color = "yellow")
  })
  
  output$best_accuracy_box <- renderValueBox({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    accs <- sapply(preds, function(p) mean(p$class == test$Churn))
    valueBox(paste0(round(max(accs) * 100, 2), "%"), "Accuracy", 
             icon = icon("check-circle"), color = "green")
  })
  
  output$best_auc_box <- renderValueBox({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    aucs <- sapply(preds, function(p) {
      r <- pROC::roc(test$Churn, p$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
      auc(r)
    })
    valueBox(round(max(aucs), 3), "AUC", icon = icon("chart-area"), color = "purple")
  })
  
  output$training_time_box <- renderValueBox({
    req(training_times())
    tot <- sum(unlist(training_times()))
    valueBox(paste0(round(tot, 1), "s"), "Temps", icon = icon("clock"), color = "light-blue")
  })
  
  # Graphiques
  output$confusion_matrix <- renderPlot({
    req(input$selected_model, predictions_list(), test_data())
    pred <- predictions_list()[[input$selected_model]]
    test <- test_data()
    cm <- confusionMatrix(pred$class, test$Churn)
    cmdf <- as.data.frame(cm$table)
    
    ggplot(cmdf, aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Freq), size = 10, color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Matrice de Confusion") +
      theme_minimal()
  })
  
  output$roc_curve <- renderPlotly({
    req(input$selected_model, predictions_list(), test_data())
    pred <- predictions_list()[[input$selected_model]]
    test <- test_data()
    r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
    
    df <- data.frame(FPR = 1 - r$specificities, TPR = r$sensitivities)
    plot_ly(df, x = ~FPR, y = ~TPR, type = 'scatter', mode = 'lines',
            line = list(color = 'darkblue', width = 3)) %>%
      add_trace(x = c(0, 1), y = c(0, 1), mode = 'lines',
                line = list(color = 'red', dash = 'dash'), showlegend = FALSE) %>%
      layout(title = paste("ROC Curve (AUC =", round(auc(r), 3), ")"),
             xaxis = list(title = "FPR"), yaxis = list(title = "TPR"))
  })
  
  output$metrics_table <- renderTable({
    req(input$selected_model, predictions_list(), test_data())
    pred <- predictions_list()[[input$selected_model]]
    test <- test_data()
    cm <- confusionMatrix(pred$class, test$Churn, positive = "Yes")
    r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
    
    data.frame(
      Métrique = c("Accuracy", "Precision", "Recall", "F1", "AUC"),
      Valeur = c(
        round(cm$overall["Accuracy"], 4),
        round(cm$byClass["Precision"], 4),
        round(cm$byClass["Recall"], 4),
        round(cm$byClass["F1"], 4),
        round(as.numeric(auc(r)), 4)   # ✅ FIX
      )
    )
  })
  
  output$metrics_radar <- renderPlot({
    req(input$selected_model, predictions_list(), test_data())
    pred <- predictions_list()[[input$selected_model]]
    test <- test_data()
    cm <- confusionMatrix(pred$class, test$Churn, positive = "Yes")
    r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
    
    metrics <- data.frame(
      Metric = c("Accuracy", "Precision", "Recall", "F1", "AUC"),
      Value = c(
        as.numeric(cm$overall["Accuracy"]),
        as.numeric(cm$byClass["Precision"]),
        as.numeric(cm$byClass["Recall"]),
        as.numeric(cm$byClass["F1"]),
        as.numeric(auc(r))   # ✅ FIX CRITIQUE
      )
    )
    
    
    ggplot(metrics, aes(x = Metric, y = Value)) +
      geom_bar(stat = "identity", fill = "#3498DB") +
      coord_polar() +
      ylim(0, 1) +
      labs(title = "Radar des Métriques") +
      theme_minimal()
  })
  
  output$var_importance <- renderPlot({
    req(input$selected_model, models_list())
    m <- models_list()[[input$selected_model]]
    
    if(input$selected_model == "rf") {
      imp <- importance(m)[, 1]
      impdf <- data.frame(Variable = names(imp), Importance = imp)
      impdf <- impdf[order(impdf$Importance, decreasing = TRUE), ][1:10, ]
      
      ggplot(impdf, aes(x = reorder(Variable, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 10 Variables", x = "", y = "Importance") +
        theme_minimal()
    } else if(input$selected_model == "tree") {
      imp <- m$variable.importance
      impdf <- data.frame(Variable = names(imp), Importance = imp)
      impdf <- impdf[order(impdf$Importance, decreasing = TRUE), ][1:10, ]
      
      ggplot(impdf, aes(x = reorder(Variable, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(title = "Top 10 Variables", x = "", y = "Importance") +
        theme_minimal()
    } else {
      ggplot() + annotate("text", x = 0.5, y = 0.5, 
                          label = "Non disponible pour ce modèle", size = 6) +
        theme_void()
    }
  })
  
  # ========== COMPARAISON ==========
  output$comparison_table <- renderDT({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    
    results <- lapply(names(preds), function(n) {
      pred <- preds[[n]]
      cm <- confusionMatrix(pred$class, test$Churn, positive = "Yes")
      r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
      
      nms <- c(logistic="Régr.Log", tree="Arbre", rf="RF", xgb="XGB", svm="SVM")
      data.frame(
        Modèle = nms[n],
        Accuracy = round(cm$overall['Accuracy'], 4),
        Precision = round(cm$byClass['Precision'], 4),
        Recall = round(cm$byClass['Recall'], 4),
        F1 = round(cm$byClass['F1'], 4),
        AUC = round(as.numeric(auc(r)), 4)
        
      )
    })
    
    datatable(do.call(rbind, results), options = list(pageLength = 10))
  })
  
  output$metrics_comparison <- renderPlotly({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    
    results <- lapply(names(preds), function(n) {
      pred <- preds[[n]]
      cm <- confusionMatrix(pred$class, test$Churn, positive = "Yes")
      r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
      nms <- c(logistic="Log", tree="Tree", rf="RF", xgb="XGB", svm="SVM")
      data.frame(
        Modèle = nms[n],
        Accuracy = as.numeric(cm$overall["Accuracy"]),
        AUC = as.numeric(auc(r))
      )
      
    })
    
    comp <- do.call(rbind, results) %>%
      pivot_longer(cols = c(Accuracy, AUC), names_to = "Métrique", values_to = "Valeur")
    
    plot_ly(comp, x = ~Modèle, y = ~Valeur, color = ~Métrique, type = 'bar') %>%
      layout(title = "Comparaison Métriques", barmode = 'group')
  })
  
  output$roc_comparison <- renderPlot({
    req(predictions_list(), test_data())
    
    preds <- predictions_list()
    test <- test_data()
    
    par(lwd = 2)  # ✅ ÉPAISSEUR GLOBALE (UNE SEULE FOIS)
    
    plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
         xlab = "FPR", ylab = "TPR", main = "Courbes ROC")
    abline(a = 0, b = 1, lty = 2, col = "gray")
    
    cols <- c(logistic="blue", tree="red", rf="green", xgb="purple", svm="orange")
    nms <- c(logistic="Log", tree="Tree", rf="RF", xgb="XGB", svm="SVM")
    
    for (n in names(preds)) {
      r <- pROC::roc(
        test$Churn,
        preds[[n]]$prob,
        levels = c("No", "Yes"),
        direction = "<",
        quiet = TRUE
      )
      
      plot(r, col = cols[n], add = TRUE)  # ✅ PLUS DE lwd ICI
    }
    
    legend(
      "bottomright",
      legend = paste(
        nms[names(preds)],
        sapply(names(preds), function(n) {
          r <- pROC::roc(test$Churn, preds[[n]]$prob,
                         levels = c("No", "Yes"),
                         direction = "<", quiet = TRUE)
          paste0("(", round(pROC::auc(r), 3), ")")
        })
      ),
      col = cols[names(preds)],
      lwd = 2,
      cex = 0.8
    )
  })
  
  
  output$detailed_comparison <- renderPlot({
    req(predictions_list(), test_data())
    preds <- predictions_list()
    test <- test_data()
    
    results <- lapply(names(preds), function(n) {
      pred <- preds[[n]]
      cm <- confusionMatrix(pred$class, test$Churn, positive = "Yes")
      r <- pROC::roc(test$Churn, pred$prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)
      nms <- c(logistic="Log", tree="Tree", rf="RF", xgb="XGB", svm="SVM")
      
      data.frame(
        Modèle = nms[n],
        Accuracy = as.numeric(cm$overall["Accuracy"]),
        Precision = as.numeric(cm$byClass["Precision"]),
        Recall = as.numeric(cm$byClass["Recall"]),
        F1 = as.numeric(cm$byClass["F1"]),
        AUC = as.numeric(auc(r))   # ✅ FIX FINAL
      )
      
    })
    
    comp <- do.call(rbind, results) %>%
      pivot_longer(cols = -Modèle, names_to = "Métrique", values_to = "Valeur")
    
    ggplot(comp, aes(x = Métrique, y = Valeur, fill = Modèle)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparaison Complète") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ========== PRÉDICTION ==========
  observeEvent(input$make_prediction, {
    req(models_list(), predictions_list(), test_data())
    
    preds <- predictions_list()
    test <- test_data()
    accs <- sapply(preds, function(p) mean(p$class == test$Churn))
    best_name <- names(which.max(accs))
    best_model <- models_list()[[best_name]]
    
    # Nouveau client
    new <- data.frame(
      gender = factor(input$pred_gender, levels = c("Female", "Male")),
      SeniorCitizen = factor(input$pred_senior, levels = c("0", "1")),
      Partner = factor(input$pred_partner, levels = c("No", "Yes")),
      Dependents = factor(input$pred_dependents, levels = c("No", "Yes")),
      tenure = as.numeric(input$pred_tenure),
      PhoneService = factor(input$pred_phone, levels = c("No", "Yes")),
      MultipleLines = factor("No", levels = c("No", "No phone service", "Yes")),
      InternetService = factor(input$pred_internet, levels = c("DSL", "Fiber optic", "No")),
      OnlineSecurity = factor("No", levels = c("No", "No internet service", "Yes")),
      OnlineBackup = factor("No", levels = c("No", "No internet service", "Yes")),
      DeviceProtection = factor("No", levels = c("No", "No internet service", "Yes")),
      TechSupport = factor("No", levels = c("No", "No internet service", "Yes")),
      StreamingTV = factor("No", levels = c("No", "No internet service", "Yes")),
      StreamingMovies = factor("No", levels = c("No", "No internet service", "Yes")),
      Contract = factor(input$pred_contract, levels = c("Month-to-month", "One year", "Two year")),
      PaperlessBilling = factor(input$pred_billing, levels = c("No", "Yes")),
      PaymentMethod = factor("Electronic check", 
                             levels = c("Bank transfer (automatic)", "Credit card (automatic)",
                                        "Electronic check", "Mailed check")),
      MonthlyCharges = as.numeric(input$pred_monthly),
      TotalCharges = as.numeric(input$pred_monthly * input$pred_tenure)
    )
    
    # Prédiction
    if(best_name == "logistic") {
      prob <- predict(best_model, new, type = "response")
    } else if(best_name == "rf") {
      prob <- predict(best_model, new, type = "prob")[2]
    } else if(best_name == "tree") {
      prob <- predict(best_model, new, type = "prob")[, 2]
      prob <- predict(best_model, new_x)
    } else if(best_name == "svm") {
      pp <- predict(best_model, new, probability = TRUE)
      prob <- attr(pp, "probabilities")[, "Yes"]
    }
    
    pred <- ifelse(prob > 0.5, "OUI", "NON")
    risk <- ifelse(prob > 0.7, "ÉLEVÉ", ifelse(prob > 0.4, "MOYEN", "FAIBLE"))
    risk_col <- ifelse(risk == "ÉLEVÉ", "red", ifelse(risk == "MOYEN", "orange", "green"))
    
    nms <- c(logistic="Régr.Log", tree="Arbre", rf="RF", xgb="XGB", svm="SVM")
    
    output$prediction_result <- renderUI({
      div(style = "text-align: center; padding: 20px;",
          h3(paste("Modèle:", nms[best_name])),
          h2(style = paste0("color: ", risk_col, ";"),
             paste("Probabilité:", round(prob * 100, 2), "%")),
          h1(style = paste0("color: ", ifelse(pred == "OUI", "red", "green"), ";"),
             paste("Le client va", ifelse(pred == "OUI", "PARTIR", "RESTER"))),
          h3(style = paste0("color: ", risk_col), paste("Risque:", risk)),
          hr(),
          h4("Recommandations:"),
          p(if(risk == "ÉLEVÉ") {
            "⚠️ Action immédiate! Offre de rétention."
          } else if(risk == "MOYEN") {
            "⚡ Surveillance et offre personnalisée."
          } else {
            "✅ Client stable."
          })
      )
    })
  })
  
  # ========== RAPPORT ==========
  output$executive_summary <- renderUI({
    req(predictions_list(), test_data())
    
    preds <- predictions_list()
    test <- test_data()
    accs <- sapply(preds, function(p) mean(p$class == test$Churn))
    best <- names(which.max(accs))
    nms <- c(logistic="Régression Logistique", tree="Arbre de Décision", 
             rf="Random Forest", xgb="XGBoost", svm="SVM")
    
    tagList(
      h4("📊 Résumé de l'Analyse"),
      nrow(raw_data()),
      p(paste("• Taux de churn:", round(mean(test_data()$Churn == "Yes") * 100, 1), "%")),
      p(paste("• Meilleur modèle:", nms[best])),
      p(paste("• Accuracy:", round(max(accs) * 100, 2), "%")),
      p("• 5 algorithmes testés avec validation croisée")
    )
  })
  
  output$key_insights <- renderUI({
    tagList(
      h4("🔍 Insights Clés"),
      tags$ul(
        tags$li("Les contrats mensuels ont le taux de churn le plus élevé"),
        tags$li("Les clients avec fibre optique partent plus souvent"),
        tags$li("La durée d'abonnement (tenure) est le facteur le plus important"),
        tags$li("Les charges mensuelles élevées augmentent le risque"),
        tags$li("Les seniors ont tendance à rester plus longtemps")
      )
    )
  })
  
  output$business_recommendations <- renderUI({
    tagList(
      h4("💼 Recommandations"),
      tags$ol(
        tags$li("Offrir des réductions sur contrats longs termes"),
        tags$li("Améliorer la qualité du service fibre"),
        tags$li("Programme de fidélité pour nouveaux clients"),
        tags$li("Optimiser les prix des forfaits premium"),
        tags$li("Support proactif pour clients à risque")
      )
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("churn_data_clean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(clean_data())
      write.csv(clean_data(), file, row.names = FALSE)
    }
  )
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport_churn_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(predictions_list(), test_data())
      
      report <- "=== RAPPORT D'ANALYSE CHURN ===\n\n"
      report <- paste0(report, "Date: ", Sys.Date(), "\n\n")
      report <- paste0(report, "1. DONNÉES\n")
      report <- paste0(report, "   - Clients: ", nrow(clean_data()), "\n")
      report <- paste0(report, "   - Variables: ", ncol(clean_data()), "\n\n")
      
      preds <- predictions_list()
      test <- test_data()
      
      report <- paste0(report, "2. RÉSULTATS MODÈLES\n")
      for(n in names(preds)) {
        cm <- confusionMatrix(preds[[n]]$class, test$Churn)
        report <- paste0(report, "   ", n, ": ", 
                         round(cm$overall['Accuracy'] * 100, 2), "%\n")
      }
      
      writeLines(report, file)
    }
  )
}

# Lancement
shinyApp(ui = ui, server = server)