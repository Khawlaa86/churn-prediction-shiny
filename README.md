# Customer Churn Prediction – Shiny Application

## Context
This project was developed as part of the **Travaux Pratiques in Predictive Analytics**
at **Institut Supérieur de Gestion de Bizerte (ISGB)**

The objective is to build an interactive **Shiny web application** to predict customer churn
in the telecommunications sector using machine learning models.

---

##  Objectives
- Import and explore a real churn dataset
- Perform interactive ETL (data cleaning & preprocessing)
- Train and evaluate multiple classification models
- Compare model performances using statistical metrics and ROC curves
- (Bonus) Predict churn for new customers

---

## Dataset
**Telco Customer Churn – IBM Sample Dataset**

- 7043 observations
- 21 variables
- Target variable: `Churn` (Yes / No)

Source:  
https://raw.githubusercontent.com/IBM/telco-customer-churn-on-icp4d/master/data/Telco-Customer-Churn.csv

---

## Technologies & Packages
- R, Shiny, shinydashboard
- ggplot2, dplyr, tidyr
- caret
- randomForest
- rpart
- pROC
- xgboost / e1071 (SVM)

---

## Application Structure
The application is organized into **four main tabs**:

1. **Data Import & Exploration**
   - CSV upload
   - Raw data visualization (DT)

2. **ETL & Data Cleaning**
   - Missing values handling
   - Duplicate removal
   - Type conversion
   - Outlier detection
   - Encoding categorical variables

3. **Modeling & Results**
   - Logistic Regression
   - Decision Tree
   - Random Forest
   - XGBoost / SVM
   - Confusion matrices
   - Performance metrics (Accuracy, Precision, Recall, F1, AUC)

4. **Model Comparison**
   - Comparative performance table
   - ROC curve visualization
   - Best model selection

---

## How to Run the Application
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "tidyr",
  "caret", "randomForest", "rpart", "rpart.plot", "pROC",
  "xgboost", "e1071"
))

shiny::runApp("app.R")
