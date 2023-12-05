
library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)


hlthdta <- read_csv("insurance.csv")

hlthdta$sex <- as.factor(hlthdta$sex)
hlthdata$smoker <- as.factor(hlthdata$smoker)
hlthdata$region <- as.factor(hlthdata$region)

levels(hlthdata$sex) <- c("male","female")
levels(hlthdata$smoke) <- c("yes","no")
levels(hlthdata$region) <- c("southwest","southeast","northwest","northeast")

function(input, output, session) {

   output$dataPlot <- renderPlot(
     if(input$RB == "BP" & input$rgn == "All"){
       ggplot(hlthdta,aes(y = charges,x = sex)) + 
         geom_boxplot() + 
         labs(x = "Participant Gender", y = "Amount of Charges", Title = "Box plot for Health Insurance Charges for Male and Female Participants") +
         facet_wrap(region)
     } else if(input$RB == "BP" & input$rgn != "All"){
       df_region <- reactive({
         req(input$rgn)
         filter(hlthdta, region %in% input$rgn)
      ggplot(df_region,aes(y = charges,x = sex)) +
        geom_boxplot() + 
        labs(x = "Participant Gender", y = "Amount of Charges", Title = "Box Plot for Health Insurance Charges for Male and Female Participants")
       })
     } else if(input$RB == "CF" & input$sx == "All"){
       ggplot(hlthdta,aes(y = bmi,x = age)) + 
         geom_point(aes(colour = region)) +
         geom_smooth(method = lm) +
         labs(x = "Age", y = "Body Mass Index", Title = "Scatter Plot of Body Mass INdex By Age for Participants")
     } else if(input$RB == "CF" & input$sx != "All"){
       df_sx <- reactive({
         req(input$sx)
         filter(hlthdta, region %in% input$sx)
         ggplot(df_region,aes(y = charges,x = sex)) +
           geom_boxplot(aes(colour = region)) +
           labs(x = "Age", y = "Body Mass Index", Title = "Scatter Plot of Body Mass INdex By Age for Participants")
       })
     } else if(input$RB == "KD" & input$sx == "All"){
       ggplot(hlthdta,aes(y = bmi,x = age)) + 
         ggplot(hlthdta) +
         geom_density(mapping=aes(x=age ), fill = "blue" ,position="identity") + 
         labs(x = "Age", title = "Distribution of Age for Subjects with Diabetes")
     } else if(input$RB == "KD" & input$sx != "All"){
       df_sx <- reactive({
         req(input$sx)
         filter(hlthdta, region %in% input$sx)
         ggplot(df_region,aes(y = charges,x = sex)) +
           geom_boxplot() +
           labs(x = "Age", y = "Body Mass Index", Title = "Scatter Plot of Body Mass INdex By Age for Participants")
       })
     }
     
     
     
   )
  

}
