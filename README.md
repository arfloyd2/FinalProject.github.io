# FinalProject.github.io
This is the repository for the Final Project

Description : This app is designed to aid in the prediction of medical costs billed by insurance agencies by analyzing lifestyle and familial variables such as age, smoker status, body mass index, and region of residence. The app contains three tabs. The first tab provides a description of the purpose of the app, some background knowledge about the data set, tye types of models used, and brief descriptions about the remaing tabs. The second tab allows the user to conduct a series of exploratory data analyses that provide insight on how the medical bill costs interact with the explanatory variables, and how the lifestyle, familial and regional factors interact with each other. This is done through a series of visualization plots and numerical summaries. The third tab allows the user to fit multiple linear regression and random forest models to the data after choosing various cross validation, and paramter selection options . Fit Statistics, diagnostic plots, and a summary of the model are provided as outputs of the model. The user also has the ability to click into the prediction subtab, enter parameter values, and predict charges for both models. An output of the full models and a prediction are provided.  

Libraries Used :  

library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)
library(MuMIn)
library(Metrics)
library(shinythemes)

Code to run the shiny app from GitHub: 

shiny::runGitHub(
'FinalProject.github.io', 'arfloyd2',subdir = 'FinalProject')

)

Line of Code to install Packages 

install. packages(c("shiny", "tidyverse","caret","mathjaxr","MuMIn","Metrics","shinythemes")) 
