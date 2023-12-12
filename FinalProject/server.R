
library(shiny)
library(tidyverse)
library(caret)
library(mathjaxr)
library(MuMIn)
library(Metrics)

 hlthdta <- read_csv( file = 'insurance.csv')

  hlthdta$sex <- as.factor(hlthdta$sex)
 hlthdta$smoker <- as.factor(hlthdta$smoker)
  hlthdta$region <- as.factor(hlthdta$region)
  
 


shinyServer(function(input, output,session){
 
   output$DataPlot <- renderPlot(

# Box plot for the Charges amount by Sex: User can choose all regions or a select of regions

     if(input$RB == "BP"){
       if(is.null(input$rgn)){
         ggplot(hlthdta,aes(y = charges,x = sex)) +
           geom_boxplot() +
           labs(x = "Contractor Gender", y = "Amount of Charges", Title = "Box plot for Health Insurance Charges for Male and Female Contractors") 
       } else{
         
         df_region <- hlthdta %>%
           dplyr::filter(region %in% input$rgn)
           
           ggplot(data = df_region, aes(y = charges,x = sex)) +
             geom_boxplot() +
             labs(x = "Contractor Gender", y = "Amount of Charges", Title = "Box Plot for Health Insurance Charges for Male and Female Participants") +
           facet_wrap(df_region$region)
     }}
    

 # Scatter plot for Body Mass Index by Age fill by Sex
 
    else if(input$RB == "SC"){
        ggplot(hlthdta,aes(y = bmi,x = age,fill = sex), posiiton = "jitter") +
        geom_point(stat = "identity") +
         geom_smooth(method = lm) +
        labs(x = "Age", y = "Body Mass Index", Title = "Scatter Plot of Body Mass Index By Age for Participants")

    }  
 #  Bar plot of Medical Charges by Smoker Status : User can choose which sex/gender



     else if(input$RB == "CF"){
         if(is.null(input$sx)){
         ggplot(hlthdta,mapping=aes(y = charges, x=smoker,fill =sex ) ) +
           geom_bar(stat = "identity",position="dodge") +
           labs(x = "Smoker Status", title = "Medical Charges of Participants by Smoker status")
         
       }

    else {
      df_sx <- hlthdta %>%
        dplyr::filter(sex %in% input$sx)
      
         ggplot(df_sx,mapping=aes(y = charges, x=smoker, fill = sex ),position="dodge") +
           geom_bar(stat = "identity",position="dodge") +
           labs(x = "Smoker Status", title = "Medical Charges of Participants by Smoker status")
    }
     }

# Density plot for Age distribution

    else if(input$RB == "KD"){
      if(is.null(input$sm)){
        ggplot(hlthdta,aes(age, colour = region)) +
          geom_density(position="identity") +
          labs(x = "Age", title = "Distribution of Age for Participants") +
          facet_wrap(hlthdta$smoker)
        
      } else {
        
        df_sm <- hlthdta %>%
          dplyr::filter(smoker %in% input$sm)
        
        ggplot(df_sm,aes(age, color = region)) +
          geom_density( position="identity") +
          labs(x = "Age", title = "Distribution of Age for Participants") +
          facet_wrap(df_sm$smoker)
        
      }}
   )
   


   output$dataTable <- renderDataTable(

# Mean, minimum and Maximum of Charges Response by Region

     if(input$NM =="CM"){
       
       if(is.null(input$rgn1)){
         
       tibble(hlthdta) %>%
         group_by(region) %>%
         summarize(Mean_cost = mean(charges), minimum_charges = min(charges), maximum_charges = max(charges))
       } 
       
    else  {
     tibble(hlthdta) %>%
        filter(region %in% input$rgn1) %>%
        group_by(region) %>%
        summarize(Mean_cost = mean(charges), minimum_charges = min(charges), maximum_charges = max(charges))
      
     
    }}
# Frequency of Health Contractors ( Male and Female ) by Region

     else if(input$NM =="SX"){
            data.frame(table(hlthdta$sex, hlthdta$region)) %>%
         rename("Gender_of_Contractor" = "Var1", "Region_of_Customer" = "Var2", "Count" = "Freq")

# Mean and 5 number summary of BMI per smoker status

       }

   else if(input$NM == "SB"){
    
      if (is.null(input$sm1)){
        
        
         hlthdta %>%
           group_by(smoker) %>%
           summarize(Mean_BMI = mean(bmi), Min_BMI = min(bmi), Q1_BMI = quantile(bmi,0.25), Median_BMI = median(bmi), Q3_BMI = quantile(bmi,0.75)) }


  else {
             hlthdta %>%
             filter(smoker %in% input$sm1) %>%
             
               group_by(smoker) %>%
               summarize(Mean_BMI = mean(bmi), Q1_BMI = quantile(bmi,0.25), Median_BMI = median(bmi), Q3_BMI = quantile(bmi,0.75))


  }})
   
 observeEvent(input$fit,{
              
              
 #  Modelling Start
   storage <- reactiveValues()
   observe({
     
    if(input$SP == "hlf"){
     
     #set seed for reproducability
     
     set.seed(90)
     
     #create a training index that uses 50% of the data for training data
     
     trainindex <- createDataPartition(hlthdta$charges,p = 0.5, list= FALSE)
     
     #Now, split the data into 50% training
     
     hlthtrain <- hlthdta[trainindex, ]
     hlthtest <- hlthdta[-trainindex, ]
     
     # To help make our predictions valid, we are now going to standardize the numeric variables
     
     preprocval <- preProcess(hlthtrain,method = c("center","scale"))
     
     storage$trainTransformed <- as_tibble(predict(preprocval,hlthtrain))
     
     
     storage$testTransformed <- as_tibble(predict(preprocval,hlthtest))
     
   } else if(input$SP == "svn"){
     
     #set see for reproducability
     
     set.seed(90)
     
     #create a training index that uses 75% of the data for training data
     
     trainindex <- createDataPartition(hlthdta$charges,p = 0.75, list= FALSE)
     
     # Now, split the data into 75% training
     
     hlthtrain <- hlthdta[trainindex, ]
     hlthtest <- hlthdta[-trainindex, ]
     
     #To help make our predictions valid, we are now going to standardize the numeric variables
     
     preprocval <- preProcess(hlthtrain,method = c("center","scale"))
     
     storage$trainTransformed <- as_tibble(predict(preprocval,hlthtrain))
     
     storage$testTransformed <- as_tibble(predict(preprocval,hlthtest))
     
   } else if(input$SP == "egt"){
     
     #set seed for reproducability
     
     set.seed(90)
     
     #create a training index that uses 80% of the data for training data
     
     trainindex <- createDataPartition(hlthdta$charges,p = 0.8, list= FALSE)
     
     #Now, split the data into 80% training
     
     hlthtrain <- hlthdta[trainindex, ]
     hlthtest <- hlthdta[-trainindex, ]
     
     #To help make our predictions valid, we are now going to standardize the numeric variables
     
     preprocval <- preProcess(hlthtrain,method = c("center","scale"))
     
     storage$trainTransformed <- as_tibble(predict(preprocval,hlthtrain))
     
     
     storage$testTransformed <- as_tibble(predict(preprocval,hlthtest))
     
   }  
   
})
   

 # Output Summary

   output$smry <- renderPrint(

   # Fitting the Model
    if(is.null(input$MLRvars)){
      
    
           
           mlrfit <- lm(charges ~ .,data = storage$trainTransformed)
           summary(mlrfit)
    }

    else{
       mlrdta <- storage$trainTransformed[, c("charges", input$MLRvars)]

       mlrfit <- lm(charges ~ .,data = mlrdta)

       print(summary(mlrfit))

    } 

   )

# Outputting the fit statistics

output$fitst <- renderTable(

    # Split Data into Train and Test Set

       if(is.null(input$MLRvars)){
         
         mlrfit <- lm(charges ~ .,data = storage$trainTransformed)

         fitdta <- data.frame(fitStat = c("Adj R Square", "AIC", "BIC","RMSE"), fitVal = round(c(summary(mlrfit)$adj.r.squared,
                                                                                                AIC(mlrfit),
                                                                                                MuMIn::AICc(mlrfit), rmse(predict(mlrfit,type = "response",newdata = storage$trainTransformed), storage$trainTransformed$charges)) , 3))

         prediction <- predict(mlrfit,type = "response",newdata = storage$testTransformed)

         MSE <-  mse(prediction, storage$testTransformed$charges)

         SSE <- sum((prediction - storage$testTransformed$charges)^2)
         SST <- sum((prediction-mean(storage$testTransformed$charges))^2)
         R_squared <- 1 - SSE/SST

         compdta <- data.frame(compStat = c("R-Squared","SSE","SST","RMSE for Test"), compVal = c(round(R_squared,3),round(SSE,3),round(SST,3),round(MSE,3)))

         data.frame(fitdta,compdta)

         } else {

           trainTransformed1 <- storage$trainTransformed[, c("charges",input$MLRvars)]
           testTransformed1 <- storage$testTransformed[, c("charges",input$MLRvars)]

           mlrfit <- lm(charges ~ .,data = trainTransformed1)

           fitdta <- data.frame(fitStatistics = c("Adj R Square", "AIC", "BIC","RMSE"), fitValues = round(c(summary(mlrfit)$adj.r.squared,
                                                                                                   AIC(mlrfit),
                                                                                                   MuMIn::AICc(mlrfit), rmse(predict(mlrfit,type = "response",newdata = storage$trainTransformed1), storage$trainTransformed1$charges)) , 3))

           prediction <- predict(mlrfit,type = "response",newdata = testTransformed1)

           MSE <-  mse(prediction, testTransformed1$charges)

           SSE <- sum((prediction - testTransformed1$charges^2))
           SST <- sum((prediction-mean(testTransformed1$charges))^2)
           R_squared <- 1 - SSE/SST

           compdta <- data.frame(comparisonStatistics = c("R-Squared","SSE","SST","RMSE for Test"), comparisonValues = c(round(R_squared,3),round(SSE,3),round(SST,3),round(MSE,3)))

           data.frame(fitdta,compdta)

         })


# Residuals vs. Leverage Plot  


output$mlrplot <- renderPlot(
  if(is.null(input$MLRvars)){
    mlrfit <- lm(charges ~ .,data = storage$trainTransformed)

plot(mlrfit)
  } else {
    
    trainTransformed1 <- storage$trainTransformed[, c("charges",input$MLRvars)]
    testTransformed1 <- storage$testTransformed[, c("charges",input$MLRvars)]
    
    mlrfit <- lm(charges ~ .,data = trainTransformed1)
    
    plot(mlrfit)
  })


# Random Forest Model 

# Fitting the Model
output$rfsm <- renderPrint(
  
 
  if(is.null(input$RFvars)){
    req(input$mtry)
    req(input$fld)

    rffit <- train(charges~., data = storage$trainTransformed, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    print(rffit$finalModel)
  }

  else{
    rfrdta <- storage$trainTransformed[, c("charges", input$RFvars)]

    rffit <- train(charges~., data = rfrdta, method ="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    print(rffit$finalModel)

  }
)


# Variable Importance
output$rfimp <- renderPrint(


  if(is.null(input$RFvars)){

    rffit <- train(charges~., data = storage$trainTransformed, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    varImp(rffit)
  }

  else{
    rfrdta <- storage$trainTransformed[, c("charges", input$RFvars)]

    rffit <- train(charges~., data = rfrdta, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    varImp(rffit)

  }
)


# Variable Importance
output$rfitstat <- renderPrint(


  if(is.null(input$RFvars)){

    rffit <- train(charges~., data = storage$trainTransformed, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    rffit$resample
  }

  else{
    rfrdta <- storage$trainTransformed[, c("charges", input$RFvars)]

    rffit <- train(charges ~., data = rfrdta, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    rffit$resample

  }
)

# Variable Importance
output$rfplot <- renderPlot(


  if(is.null(input$RFvars)){

    rffit <- train(charges~., data = storage$trainTransformed, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    plot(rffit, main = "Root Mean Square Error for the Number of Randomly Selected Predictors")
  }

  else{
    rfrdta <- storage$trainTransformed[, c("charges", input$RFvars)]

    rffit <- train(charges~., data = rfrdta, method="rf",
                   trControl = trainControl(method = "cv", number = as.numeric(input$fld), search = "random"),
                   tuneGrid = data.frame(mtry = eval(parse(text = input$mtry))))

    plot(rffit, main = "Root Mean Square Error for the Number of Randomly Selected Predictors")

  })


})



})
   


