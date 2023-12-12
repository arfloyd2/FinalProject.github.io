
library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)
library(shinythemes)


  hlthdta <- read_csv('insurance.csv')
 
  #  hlthdta$sex <- as.factor(hlthdta$sex)
  # #  hlthdta$smoker <- as.factor(hlthdta$smoker)
   hlthdta$region <- as.factor(hlthdta$region)

# levels(hlthdta$sex) <- c("male","female")
# levels(hlthdta$smoke) <- c("yes","no")
# levels(hlthdta$region) <- c("southwest","southeast","northwest","northeast")


# Define UI for application "Health Insurance Costs"

  
  
shinyUI(navbarPage(title = "Health Insurance Costs", collapsible = TRUE, theme = shinytheme("spacelab"), inverse = TRUE,
        
#Tabs that are used for the app 
# Introduction  

 tabPanel("About", h1("Welcome!"),
          br(),
          
          h2("The purpose of this project is to produce a shiny app that could analyze and predict medical insurance costs given a series of health, familial, and regional variables. For many poeple in the United States, purchasing medical insurance can often be an expensive and frustrating task. However, having access to tools, such as an app that considers lifestyle, and regional factors that have an impact on costs but often go unnoticed, could be useful for customers in the purchasing process. The dataset chosen uses variables such as age, sex, body mass indes, number of children that that the customer has, whether or not they are a smoker, and which region int the United States they live in, to provide insight on medical costs billed by health insurance providers. The data was created as a tool to aid in practice exercises geared towards machine learning and regression techniques. More information and in-depth  descriptions of the variables can be found on the",a(href="https://www.kaggle.com/datasets/mirichoi0218/insurance","Kaggle webiste"),". Below are descriptions of the remaining tabs in the app."), 
          br(),
          h2("Data Exploration : The Data Exploration tab provides some prelminary exploratory data analysis datasets and visualizations that would allow the user to have an in depth introduction to the factors that have an impact on the response variable Charges (medical costs billed by health insurance) "), 
             br(),
          h2("Modelling : The Modelling tab allows the user to specify variables and parameter requirements that will aid in modelling and predicting health insurance coverage costs. Cross validation techniques  are used in some of the modelling. Thus, the user will have the option to choose the ratios for which they would like to split the data into train/test sets. The models used for this data will be multiple linear regression and random forest and the numercal variables are standardized since the units in the data set are different. Thus, for the random forest model, the user will have the option to choose the tuning  and cross validaion parmeters that will be used in the model. Finally, the Prediction tab will allow the user to view predicted values for the charges using the models."),
          br(),
tags$img(src="cost-of-health-coverage.png", style = "width: 100%; padding: 0;")),

#Data Exploration tab
tabPanel("Data Exploration",

         fluidPage(h1("Exploratory Data Analysis"),
                   br(),
                   p("This tab conducts an exploratory data analysis to see how variables in the health insurance dataset interact each other and how the medical bill charges response behaves over variaous levels of region and smoker status."),
                   br(),

              sidebarPanel(

# Buttons to choose for the Graphical Summaries
                     h3("Graphical Summaries"),

                     radioButtons(inputId = "RB",label = "Select the Plot Type",
                                c("Summaries for Medical Bill Charges" = "BP",
                                  "Relationship Between BMI and Age" = "SC",
                                  "Medical Bill Charges by Smoker Status" = "CF",
                                  "Distribution of Age" = "KD")),

                     #Conditional Panel for Graph Types

                     conditionalPanel(
                       condition = "input.RB  =='BP'",
                       checkboxGroupInput(inputId = "rgn", label = "Which region[s] would you like to see?", choices = c("northeast","northwest","southeast","southwest"), selected = NULL)),

                     conditionalPanel(
                       condition = "input.RB == 'CF'",
                       checkboxGroupInput("sx", label = "Which sex would you like to see?", choices = c(levels(as.factor(hlthdta$sex))), selected =NULL)),

                     conditionalPanel(
                       condition = "input.RB  == 'KD'",
                       checkboxGroupInput("sm", label = "Which smoker usage status would you like to see?", choices = c(levels(as.factor(hlthdta$smoker))), selected =NULL)),


                    br(),

                     h4("Numerical Summaries"),

# Drop Down Menu for Numerical Summaries

                     selectizeInput(inputId = "NM", label = "Select which numerical summary you would like to view", selected = NULL, choices = c("Moments, Max, and Min of Charges" = "CM","Frequency of Health Contractors by Region" = "SX","Summary Values of Body Mass Index" = "SB"))

 ,

  #Conditional Panel for Numerical Summaries

  conditionalPanel(
condition = "input.NM == 'CM'",
selectInput(inputId = "rgn1", label = "Which regions would you like to see?", choices = c("northeast","northwest","southeast","southwest"), selected = NULL,multiple = TRUE)),

conditionalPanel(
condition = "input.NM == 'SB'",
checkboxGroupInput(inputId = "sm1", label = "Which smoker usage status would you like to see?", choices = c(levels(as.factor(hlthdta$smoker))), selected = NULL)),

),

# Show a plot and the numerical summaries from the options chosen

mainPanel(plotOutput("DataPlot") #dataplot for the bar graphs
          ,dataTableOutput(outputId = "dataTable") #data table for the summary values
))),



#Panel for Modelling the Data
tabPanel("Modelling",
         p("This is the modelling phase of the app. The purpose of this phase is to produce supervised learning models with the data to show the interation between medical bill charges from insurance companies and the explanatory variables of contractor gender, age, number of children, region of residence, body mass index,and smoker status. The two models used are multiple linear regression and random forest. The Model Info subtab provides a description of each model - its purposes for analysis, advantages, and disadvantages when describing the data. The Model Fitting tab goes through the process of creating multple regression or regression tree models to the data. Finally, the prediction tab will allow you to use the explanatory variables to predict the outcome of the charges."),

#Model Info tab that provides introductions to the models

         tabPanel(" Modeling Info", fluidPage(
           withMathJax(),

           helpText("Mutiple Linear Regrssion: Multiple Linear Regression is a form of supervised learning whose objective is to explain and predict the variation in a continuous respnse variable given a set of explanatory variables that can be categorical or numerical where the relationship between the response and the estimated parameters of the explanatory varaibles is linear. An example can be seen below. $$Y = \\beta_{0} + \\beta_{1}x_{1} + \\beta_{2}x_{2} + \\beta{3}x^{2} + \\beta_{4}x_{1}x_{2} + \\epsilon$$"),
           helpText(' where the \\(\\beta_{0}\\) is the slope of the regression that often consists of the expected value of the response, the rest of the \\(\\beta_{i}\\) terms are the estimated coefficients of the model, the x_{i} terms are the expanatory variables and the \\(\\epsilon\\) term is the sum of the  vertical distances between the estimated regression line and the data points that surround it ( also known as the error term). The model serves as an extension of simple linear regression since it can include polynomial and interaction terms in addition to the original explanatory variables. When fitting a multiple linear regression model, a main objective is for the sum of the squared deviations between the line and the data points to be as small as possible. This ensures that the estimates for the regression line explains as much of the variation as possible. Multiple linear regression is often used because it allows analysts to predict values for the response variable and to oberserve the contribution of each explanatory variable. However, there are other analysts who find multiple linear regression to come with strong assumtpions that fail to transalte to a variaty of data types. Some of these assumptions include the restriction that the error terms must follow a normatl distribution with a constant variance, and that the main test types for determining the significance of the model and overal variables follow an F-test or a T-test. When these restrictions are not met, analysts may turn to generalized linear models  where these restrictions are not required.'),
           helpText("Random Forest Model: The random forest model is another supervised machine learning technique but its objective is to take a random sapmle of  decision trees- which are created using boostrapping sample methods-, predict on the random sampled trees, and average over the predictions. Random forests are often used because the averaging helps to prevent overfitting of the data, they can reduce the variation in the estimation because the model seeks to construct independent trees, and the  predictions are often better overall than that of other tree based methods such as single tree models.To determine the appropriate random forest model, it is often the aim to reduces the average distance between te predicted and actual values - also known as the root mean square error. the calculation of the erorr is below: $$RMSE = \\sqrt{\\frac{1}N \\sum_{i=1}^n {\\hat{y_{i}} - y_{I}^2} }$$"),
           helpText('where N is the sample size, \\(\\hat{y_{i}}\\) is the predicted response, and y_{i} is the actual response value. While Random forests are chosen for their prediction advantages and abilities to reduce estimation variation, it is also noted that the complexity of the random forest trees require more computational resources than other regression or classification models, and when the regression trees encounter outliers or missing portions of data, this could lead to inaccurate predictions. Thus, one has to be careful when considering the usage of a random forest model when there is a large dataset or a data set with a large amount of explanatory variables. '))),
         br(),

#Model fitting tab

         tabPanel("Model Fitting",
                  fluidPage(
                    p("This tab fits he multiple regression and random forest model to the data. First, you will have the option on how you would like to split the data for training and testing. Next, you'll choose the model that you would like to fit. The main panel will contian summary statistics, fit statistics, and comparison statistics on the test set to show how well th emodel is performing."),
                    br(),

                    fluidRow(column(width = 2,

    #Select Bar for the train - test split

                     radioButtons("SP", "Select the test/train split", selected = NULL, choices = c("50% train - %50 test"= "hlf","75% train - 25% test"= "svn","80% train - %20 test" ="egt" ))),

    column(width = 2, offset = 2,
    #Grouup Select Check boxes to denote the variables used in the modelling
    checkboxGroupInput("MLRvars", "Select the variables that you want to use in the MLR model", choiceNames = c("Age","sex","bmi","Number of Children","Smoker Status","Region of Residence"), choiceValues = c( "age","sex", "bmi", "children", "smoker", "region"))),

 column(width = 2, offset = 2,
        checkboxGroupInput("RFvars", "Select the variables that you want to use in the Random Forest model", choiceNames = c("Age","sex","bmi","Number of Children","Smoker Status","Region of Residence"), choiceValues = c("age","sex", "bmi", "children", "smoker", "region")),

#Select Tuning Parameters and Cross Validation settings for Random Forest

                           radioButtons("mtry", label = "How many variables would you like to sample as a candidate for each node in each tree?", choiceNames = c("1:4","2:5","3:6"), choiceValues =c("1:4","2:5","3:6") ),

                           radioButtons("fld", label ="How many folds would youl like to use for the cross validation?", choiceNames =  c("3","5","7"), choiceValues = c( "3","5","7"))),

        br(),

    actionButton("fit", label = "Almost there ! Click here to fit the models")

            )),

#Building the Tab Panels for the Model Outputs

# Modelling Tabs

        tabsetPanel(
          tabPanel("MLR Model Output" ,h1("Multiple Linear Regression Model Outuput"),column(width = 4,verbatimTextOutput("smry")),column(width = 4, offset =4,tableOutput("fitst"), plotOutput("mlrplot"))),
          
          
          tabPanel("Random Forest Model Output", h1("Random Forest Model Output"),h4("The output could take close to a minute to load"),column(width = 3, verbatimTextOutput("rfsm"), verbatimTextOutput("rfimp")),column(width = 5, offset = 4,verbatimTextOutput("rfitstat"),plotOutput("rfplot"))),

  # prediction Tab

          tabPanel("Prediction",
                    sidebarLayout(
                     sidebarPanel(

                    h3("Multiple Linear Regression Options"),

                    numericInput(inputId="Age", label = "Age of Participant",value = 0,min = 0, max = 100, step = 1),
                    br(),
                    numericInput(inputId="BMI", label = "Body Mass Index",min = 0, max = 50, step = 1.5, value = 0),
                    br(),
                    numericInput(inputId = "child", label = "Number of Children", min = 0, max = 10, step = 1, value = 0),
                    br(),
                    radioButtons(inputId = "smk", label = "Participant Smoker Status", choices = c(levels(as.factor(hlthdta$smoker)))),
                    br(),
                    radioButtons(inputId = "rgn2", label = "Resident Region", choices = c("northeast","northwest","southeast","southwest"), selected = NULL),
                    br(),
                    radioButtons(inputId = "sx2", label = "Contractor Gender", choices = c(levels(as.factor(hlthdta$sex)))),


  #Random Forest Model

                    h3("Random Forest  Options"), 
                    br(),
                    

                    numericInput(inputId="Age1", label = "Age of Participant",min = 0, max = 100, step = 1, value = 0),
                    br(),
                    numericInput(inputId="BMI1", label = "Body Mass Index",min = 0, max = 50, step = 1.5,value = 0 ),
                    br(),
                    numericInput(inputId = "child1", label = "Number of Children", min = 0 , max = 10, step = 1, value = 0),
                  br(),
                  radioButtons(inputId = "smk2", label = "Participant Smoker Status", choices = c(levels(as.factor(hlthdta$smoker)))),
                  br(),
                  radioButtons(inputId = "rgn3", label = "Resident Region", choices = c("northeast","northwest","southeast","southwest"), selected = NULL),
                  br(),
                  radioButtons(inputId = "sx3", label = "Contractor Gender", choices = c(levels(as.factor(hlthdta$sex)))),
br(),
actionButton("predict", "Now for the Prediction")
),
 mainPanel(column(width = 4, h1("Multiple Linear Regression Prediction"),verbatimTextOutput("mlr1"), textOutput("mlrpred")),column(width = 4, offset = 4,h1("Random Forest Prediction"),verbatimTextOutput("rf1"),textOutput("rfpredt")))
)))))))
