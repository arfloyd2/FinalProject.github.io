
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
                       selectizeInput("rgn", "Which region would you like to see?", selected = "southeast", choices = c("All",levels(as.factor(data$region)))),
                       condition = "input.RB == CF", 
                       selectizeInput("sx", "Which sex would you like to see?", selected = "male", choices = c("All",levels(as.factor(data$sex)))),
                       condition = "input.RB == KD", 
                       selectizeInput("sm", "Which smoker usage status would you like to see?", selected = "yes", choices = c("All",levels(as.factor(data$smoker))))),
                     
                     br(),
                     
                     h4("Numerical Summaries"),
                     
# Drop Down Menu for Numerical Summaries     

                     selectizeInput("NM", "Select which numerical summary you would like to view", selected = NULL, choices = c("Moments, Max, and Min of Charges" = "CM","Frequency of Smokers by Sex" = "SM","Summary Values of Body Mass Index" = "SB"))

 ,

  #Conditional Panel for Numerical Summaries
  conditionalPanel(
condition = "input.NM == CM",
selectizeInput("rgn1", "Which region would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$region)))),
condition = "input.NM == SM", 
selectizeInput("sx1", "Which sex would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$sex)))),
condition = "input.NM == SB", 
selectizeInput("sm1", "Which smoker usage status would you like to see?", selected = "All", choices = c("All",levels(as.factor(data$smoker))))),

)),

# Show a plot and the numerical summaries from the options chosen

mainPanel(plotOutput("DataPlot"), #dataplot for the bar graphs
          dataTableOutput(outputId = "dataTable") #data table for the summary vaues
))),

tabPanel("Modelling",
         tabPanel(" Modeling Info", fluidPage(
           withMathJax()         )
                  )
  
)
)

)