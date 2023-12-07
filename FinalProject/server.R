
library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)

shinyServer(function(input, output){
  
  
  hlthdta <- read.csv( file = "insurance.csv")

  hlthdta$sex <- as.factor(hlthdta$sex)
  hlthdta$smoker <- as.factor(hlthdta$smoker)
  hlthdta$region <- as.factor(hlthdta$region)

  levels(hlthdata$sex) <- c("male","female")
  levels(hlthdata$smoke) <- c("yes","no")

   output$dataPlot <- renderPlot(

# Box plot for the Charges amount by Sex: User can choose all regions or a select of regions

     if(input$RB == "BP" & input$rgn == "All"){
       ggplot(hlthdta,aes(y = charges,x = sex)) +
         geom_boxplot() +
         labs(x = "Contractor Gender", y = "Amount of Charges", Title = "Box plot for Health Insurance Charges for Male and Female Contractors") +
         facet_wrap(region)
     } else if(input$RB == "BP" & input$rgn != "All"){
       df_region <- reactive({
         req(input$rgn)
         filter(hlthdta, region %in% input$rgn)
      ggplot(df_region,aes(y = charges,x = sex)) +
        geom_boxplot() +
        labs(x = "Participant Gender", y = "Amount of Charges", Title = "Box Plot for Health Insurance Charges for Male and Female Participants")
       }) +
        facet_wrap(region)

# Scatter plot for Body Mass Index by Age fill by Sex

     } else if(input$RB == "SC"){
       ggplot(hlthdta,aes(y = bmi,x = age,fill = sex), posiiton = "jitter") +
         geom_point(stat = "identity") +
         geom_smooth(method = lm) +
         labs(x = "Age", y = "Body Mass Index", Title = "Scatter Plot of Body Mass Index By Age for Participants")

#  Bar plot of Medical Charges by Smoker Status : User can choose which sex/gender


     } else if(input$RB == "CF" & input$sx == "All"){
       ggplot(hlthdta,mapping=aes(y = charges, x=smoker, fill = sex ),position="dodge") +
         geom_bar(stat = "identity") +
         labs(x = "Smoker Status", title = "Medical Charges of Participants by Smoker status")

     } else if(input$RB == "CF" & input$sx != "All"){
       df_sx <- reactive({
         req(input$sx)
         filter(hlthdta,sex %in% input$sx)
         ggplot(hlthdta,mapping=aes(y = charges, x=smoker, fill = sex ),position="dodge") +
           geom_bar(stat = "identity") +
           labs(x = "Smoker Status", title = "Medical Charges of Participants by Smoker status")
       })
     }


# Density plot for Age distribution

     else if(input$RB == "KD" & input$sm == "All"){
       ggplot(hlthdta,aes(x = age)) +
         geom_density(mapping=aes(x=age ), fill = "blue" ,position="identity") +
         labs(x = "Age", title = "Distribution of Age for Participants") +
         facet_wrap(smoker)

     } else if(input$RB == "KD" & input$sm != "All"){
       df_sm <- reactive({
         req(input$sm)
         filter(hlthdta,smoker %in% input$sm)
         ggplot(df_sx,x = age) +
           geom_density(mapping = aes(x = age) ,fill = "blue", position = "identity") +
           labs(x = "Age", title = "Distribution of Age for Participants") +
           facet_wrap(smoker)
       })
     }
     )

   output$dataTable <- DT::renderDataTable(

# Mean, minimum and Maximum of Charges Response by Region

     if(input$NM =="CM" & input$rgn == "All"){
       hlthdta %>%
         group_by(region) %>%
         summarize(Mean_cost = mean(charges), minimum_charges = min(charges), maximum_charges = max(charges))
     } else if(inputNM =="CM" & input$rgn != "All"){
       reactive({
         df_rgn <- req(input$rgn)
         filter(hlthdta,region %in% input$rgn)
         df_rgn %>%
           group_by(region) %>%
           summarize(Mean_cost = mean(charges), minimum_charges = min(charges), maximum_charges = max(charges))
       }
# Frequency of Health Contractors ( Male and Female ) by Region

       )} else if(input$NM =="SM"){
            table(hlthdta$sex, hlthdta$region)

# Mean and 5 number summary of BMI per smoker status

       } else if(input$NM == "SB" & input$sm1 == "All"){
         hlthdta %>%
           group_by(smoker) %>%
           summarize(Mean_BMI = mean(bmi), Min_BMI = min(bmi), Q1_BMI = quantile(bmi,0.25), Median_BMI = median(bmi), Q3_BMI = quantile(bmi,0.75)) } else if(input$NM == "SB" $ input$sm1 != "All"){
             df_sm1 <- req(input$sm1)
             filter(hlthdta,smoker %in% input$sm1)
             df_rgn %>%
               group_by(smoker) %>%
               summarize(Mean_BMI = mean(bmi), Q1_BMI = quantile(bmi,0.25), Median_BMI = median(bmi), Q3_BMI = quantile(bmi,0.75))
           })
     
})

