
library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)


data <- read_csv("insurance.csv")

data$sex <- as.factor(data$sex)
data$smoker <- as.factor(data$smoker)
data$region <- as.factor(data$region)

levels(data$sex) <- c("male","female")
levels(data$smoke) <- c("yes","no")
levels(data$region) <- c("southwest","southeast","northwest","northeast")


# Define UI for application 
shinyUI(nvbarPage("Health Insurance Costs"),
        
#Tabs that are used for the app 
# Introduction  

tabPanel("About", mainpanel(p(tags$h3("The purpose of this project is to conduct a shiny app that could analyze and predict medical insurance costs given a series of health and region indicator variables. For many in the United States, purchasing medical insurance can often be an expensive and frustrating task. However, having access ot tools such as an app that considers lifestyle, regiona, and familial factors that have an impact on costs but often go unnoticed, could be useful for customers in the purchasing process. The dataset use health and geographic variables such as age, sex, body mass indes, number of children that that the customer has, whether or not they are a smoker, and which region int the United States they live in, to provide insight on medical costs billed by health insurance providers. The data was created as a tool to aid in practice exercises geared towards machine learning and regression techniques. More information and an description of the variables can be found on the"), tags$a(href="https://www.kaggle.com/datasets/mirichoi0218/insurance", "Kaggle webiste"), tags$h3("The Data Exploration tab provides some prelminary exploratory data analysis datasets and visualization that would allow the view to have an in depth introduction to the factors  that have an impact on the response variable Charges. The Modelling tab allows the user to specify modelling requirements that will aid in modelling and predicting health insurance coverage data. Since this data will utilize cross validation techniques for modelling,the user will have the option to choose the ratios for which they would like to split the data into train/test sets. The models used for this data will be multiple linear regression and random forest model. Thus, for the random forest model, the user will have the option to choose the tunig parameter and cross validaion parmeters that will be used in the model. Finally, the prediction tab will also the user to view predicted values for the charges using the models."), br(),
                    img(src='cost-of-health-coverage.png', height="80%", width="50%",  align = "center")               
))),

#Data Exploration tab 
tabPanel("Data Exploration",
         
         fluidPage(h1("Exploratory Data Analysis"),
                   p("This tab conducts an exploratory data analysis to see how variables in the health insurance dataset interact each other and how the medical bill charges response behaves over variaous levels of region and smoker status."),
                   br(),

                sidebarLayout( sidebarPanel(
                     
# Buttons to choose for the Graphical Summaries 
                     h4("Graphical Summaries"),
                     
                     radioButtons("RB",label = "Select the Plot Type",
                                c("Summaries for Medical Bill Charges" = "BP",
                                  "Relationship Between BMI-Age and BMI-Charges" = "SC",
                                  "Medical Bill Charges by Smoker Status" = "CF",
                                  "Distribution of Age" = "KD")),
                     #Conditional Panel for Graph Types 
                     
                     conditionalPanel(
                       condition = "input.RB == BP",
                       checkboxGroupInput("rgn", "Which region would you like to see?", c("All",levels(as.factor(data$region)))),
                       condition = "input.RB == CF", 
                       checkboxGroupInput("sx", "Which sex would you like to see?", c("All",levels(as.factor(data$sex)))),
                       condition = "input.RB == KD", 
                       checkboxGroupInput("sm", "Which smoker usage status would you like to see?", c("All",levels(as.factor(data$smoker))))),
                     
                     br(),
                     
                     h4("Numerical Summaries"),
                     
# Drop Down Menu for Numerical Summaries     

                     selectizeInput("NM", "Select which numerical summary you would like to view", selected = NULL, choices = c("Moments, Max, and Min of Charges" = "CM","Frequency of Smokers by Sex" = "SM","Summary Values of Body Mass Index" = "SB"))

 ,

  #Conditional Panel for Numerical Summaries
  conditionalPanel(
condition = "input.NM == CM",
checkboxGroupInpu("rgn1", "Which region would you like to see?", c("All",levels(as.factor(data$region)))),
condition = "input.NM == SM", 
checkboxGroupInput("sx1", "Which sex would you like to see?",  c("All",levels(as.factor(data$sex)))),
condition = "input.NM == SB", 
checkboxGroupInpu("sm1", "Which smoker usage status would you like to see?", c("All",levels(as.factor(data$smoker))))),

)),

# Show a plot and the numerical summaries from the options chosen

mainPanel(plotOutput("DataPlot"), #dataplot for the bar graphs
          dataTableOutput(outputId = "dataTable") #data table for the summary vaues
))),

#Panel for Modelling the Sata 
tabPanel("Modelling",
         p("This is the modelling phase of the app. The purpose of this phase is to produce supervised learning models with the data that will help explain the interation between medical bill charges and the explanatory variables for sex, age, number of children, region of residence, body mass index,and smoker status. The two models used are multiple linear regression and random forest. The Model Info subtab provides a description of each model - its purposes for analysis, advantages, and disadvantages when describing the data. The Model Fitting tab goes through the process of creating multple regression or regression tree models to the data. Finally, the prediction tab will allow you to use the explanatory variables to predict the outcome of the charges."),
         
#Model Info tab that prvides introductions to the models 

         tabPanel(" Modeling Info", fluidPage(
           withMathJax(),
           helpText('Mutiple Linear Regrssion: Multiple Linear Regression is a form of supervised learning whose objective is to explain and predict the variation in a continuous respnse variable given a set of explanatory variables that can be categorical or numerical where the relationship between the response and the estimated parameters of the explanatory varaibles is linear. And example can be seen below. $$Y = \\beta_{0} + \\beta_{1}x_{1} + \\beta_{2}x_{2} + \\beta{3}x^{2} + \\beta_{4}x_{1}x_{2} + \\epsilon'),
           helpText(' where the \\(\\beta{0}\\) is the slope of the regression that often consists of the expected value of the response, the rest of the \\(\\beta_{i}\\) terms are the estimated coefficients of the model, the \\(\\x_{i}\\) terms are the expanatory variables and the \\(\\epsilon\\) term is the sum of the  vertical distances between the estimated regression line and the data points that surround it ( also knows as the error term). The model serves as an extension of linear regression in that it can include polynomial terms and terms where one can observe the interaction between explanatory variables in addition to the original explanatory variables. When fitting a multiple linear regression model, of the main objectives is for the sum of the squared deviations between the line and the data points to be as small as possible. This ensures that the estimates for the regression line explains as much of the variation as possible. Multiple linear regression is often used because it allows analysts to predict values for the response variable and to oberserve the contribution of each explanatory variable. However, there are other analysts who find multiple linear regression to come with strong assumtpions that fail to transalte to a variaty of data types. Some of these assumptions include the restriction that the error terms must follow a normatl distribution with a constant variance, and that the main test types for determining the significance of the model and overal variables follow an F-test or a T-test. When these restrictions are not met, analysts may turn to generalized linear models  where these restrictions are not required.'),
           helpText('Random Forest Model: The random forest model another supervised machine learning technique but its objective is to take an random sapmle of  decision trees- which are created using boostrapping sample methods-, predict on the random sampled trees, and average over the predictions. Random forests are often used because the averaging helps to prevent overfitting of the data, they can reduce variation in of the estimation because the model seeks to construct indiependent trees, and the  predictions are often better overall than that of other tree based methods such as single tree models.To determine the appropriate random forest model, it is often the aim to have a model that reduces the average distance between te predicted and actual values - also known as the root mean square error. the calculation of the erorr is below $$RMSE = \\sqrt{\\frac{1}N\\sum\{i=1}^n(\\hat{y_{i}}-\\y_{i})^2}'),
           helpText('where N is the sample size, \\hat{y_{i}} is the predicted response, and \\y_{i} is the actual response value. While Random forests are chosen for their prediction advantages and abilities to reduce estimation variation, it is also noted that the complexity of the random forest trees require more computational resources than other regression or classification models, and dealing with outliers or missing portions of a data could lead to inaccurate predictions. Thus, one has to be careful when considering the usage of a random forest model when there is a large dataset or a data set with a large amount of explanatory variables. '))),
         br(),
         
#Model fitting tab 

         tabPanel("Model Fitting",
                  fluidPage( 
                    p("This tab fits he multiple regression and random forest model to the data. First, you will have the option on how you would like to split the data for training and testing. Next, you'll choose the model that you would like to fit. The main panel will contian summary statistics, fit statistics, and comparison statistics on the test set to show how well th emodel is performing."),
                    br(),
                    
                    sidebarLayout(sidebarPanel(
                      
    #Select Bar for the train - test split 
                      
                      selectizeInput("SP", "Select the test/train split", selected = NULL, choices = c("50% train - %50 test"= "hlf","75% train - 25% test"= "svn","80% train - %20 test" ="egt" )),
                
                    
                      conditionalPanel(
                        condition = "input.NM == CM",
                        selectizeInput("rgn1", "Which region would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$region)))),
                        condition = "input.NM == SM", 
                        selectizeInput("sx1", "Which sex would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$sex)))),
                        condition = "input.NM == SB", 
                        selectizeInput("sm1", "Which smoker usage status would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$smoker))))),
                      
                    ))
                    ))
                  ))
                  )
  
)
)

