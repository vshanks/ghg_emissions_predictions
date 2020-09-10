# Runs code to generate predictions for scope 1 and scope 2
# Perform the following in order
# 1. source the file
# 2. generateResultsScope1()
# 3. generateResultsScope2()

carbon_2015_file <- "../data/csv/2015_carbon_data_modelling.csv"
carbondata_v2_file <- "../data/csv/2015_carbondataformodel_v2.csv"
techavail_file <- "../data/csv/tech_availability_score.csv"

readAndPrepData <- function(carbon_2015_file, carbondata_v2_file, techavail_file){
    carbon_2015 <- read.csv(carbon_2015_file, header = T, sep = ",", check.names = T, stringsAsFactors = F)
    carbon_v2_2015 <- read.csv(carbondata_v2_file, header=T, sep=",", check.names=T, stringsAsFactors = F)
    techavail <- read.csv(techavail_file, sep=",", header=T, check.names=T, stringsAsFactors = F, as.is = T)
    
    carbon_2015 <- addSubIndustryCodes(carbon_2015, carbon_v2_2015)
    carbon_2015 <- addTechAvailabilityScore(carbon_2015, techavail)
    carbon_df_scope1 <- prepData(carbon_2015, scope=1, imputeBeforeScopeNA=T)
    carbon_df_scope2 <- prepData(carbon_2015, scope=2, imputeBeforeScopeNA=T)
    data_to_ret <- list("scope1_data"=carbon_df_scope1, "scope2_data"=carbon_df_scope2, "carbon_2015"=carbon_2015)
    return(data_to_ret)
}

generateResultsScope1 <- function(whichModel="logemp"){
    data_df_list <- readAndPrepData(carbon_2015_file, carbondata_v2_file, techavail_file)
    carbon_df_scope1 <- data_df_list$scope1_data
    # Train models now
    #whichModel <- "logemp"
    print(paste0("Running: ", whichModel))
    if(whichModel=="logrev")
        model_data <- runScope1ModelByLogRevenue(carbon_df_scope1)
    if(whichModel=="logemp")
        model_data <- runScope1ModelByLogEmployee(carbon_df_scope1)
    model <- trainWith_randomForest(model_data$trainData, model_data$trainY)
    testRes <- predictAndReport(model, model_data$testData, model_data$testY)
    # Run the models on all the data
    carbon_df_all_scope1 <- prepData(data_df_list$carbon_2015, scope=1, imputeBeforeScopeNA=T, removeScopeNa = F)
    if(whichModel=="logrev")
        model_data_all <- learnWithColumns(carbon_df_all_scope1, chooseColumnsForModelByLogRevenue())
    if(whichModel=="logemp")
        model_data_all <- learnWithColumns(carbon_df_all_scope1, chooseColumnsForModelByLogEmployee())
    # Replace NA and 0 values
    model_pred_all <- predict(model, model_data_all)
    if(whichModel=="logrev"){
        carbon_df_all_scope1$Scope1ByLogRevPredictions <- model_pred_all
        carbon_df_all_scope1$Scope1Predictions <- carbon_df_all_scope1$Revenue*exp(model_pred_all)
        # Write predictions out into file
        write.csv(carbon_df_all_scope1, "../reports/predictions_rflr_scope1.csv")
    }
    if(whichModel=="logemp"){
        carbon_df_all_scope1$Scope1ByLogEmpPredictions <- model_pred_all
        carbon_df_all_scope1$Scope1Predictions <- carbon_df_all_scope1$Employees*exp(model_pred_all)
        # Write predictions out into file
        sys_time = Sys.time()
        predictions_filename = paste0("../reports/predictions_rfle_scope1-", sys_time, ".csv")
        metrics_filename = paste0("../reports/metrics_rfle_scope1-", sys_time, ".txt")
        write.csv(carbon_df_all_scope1, file=predictions_filename)
        metrics_string = paste("Model Summary results: R^2", testRes$Rsq, " adj R^2: ", testRes$AdjustedRsq, " RMSE: ", testRes$AdjustedRsq)
        capture.output(metrics_string, file=metrics_filename)
        capture.output(varImp(model, scale=T), file=metrics_filename, append=TRUE)
        capture.output(model, file=metrics_filename, append=TRUE)
        print(metrics_string)
    }
    a <- data.frame(A=carbon_df_all_scope1$Scope1Predictions, B=carbon_df_all_scope1$Scope1)
    
}

generateResultsScope2 <- function(whichModel="logrev"){
    data_df_list <- readAndPrepData(carbon_2015_file, carbondata_v2_file, techavail_file)
    carbon_df_scope2 <- data_df_list$scope2_data
    # Train models now
    #whichModel <- "logemp"
    print(paste0("Running: ", whichModel))
    if(whichModel=="logrev")
        model_data <- runScope2ModelByLogRevenue(carbon_df_scope2)
    if(whichModel=="logemp")
        model_data <- runScope2ModelByLogEmployee(carbon_df_scope2)
    model <- trainWith_randomForest(model_data$trainData, model_data$trainY)
    testRes <- predictAndReport(model, model_data$testData, model_data$testY)
    # Run the models on all the data
    carbon_df_all_scope2 <- prepData(data_df_list$carbon_2015, scope=2, imputeBeforeScopeNA=T, removeScopeNa = F)
    if(whichModel=="logrev")
        model_data_all <- learnWithColumns(carbon_df_all_scope1, chooseColumnsForModelByLogRevenue())
    if(whichModel=="logemp")
        model_data_all <- learnWithColumns(carbon_df_all_scope2, chooseColumnsForModelByLogEmployee())
    # Replace NA and 0 values
    model_pred_all <- predict(model, model_data_all)
    if(whichModel=="logrev"){
        carbon_df_all_scope2$Scope2ByLogRevPredictions <- model_pred_all
        carbon_df_all_scope2$Scope2Predictions <- carbon_df_all_scope2$Revenue*exp(model_pred_all)
        # Write predictions out into file
        write.csv(carbon_df_all_scope2, "../reports/predictions_rflr_scope2.csv")
    }
    if(whichModel=="logemp"){
        carbon_df_all_scope2$Scope2ByLogEmpPredictions <- model_pred_all
        carbon_df_all_scope2$Scope2Predictions <- carbon_df_all_scope2$Employees*exp(model_pred_all)
        # Write predictions out into file
        sys_time = Sys.time()
        predictions_filename = paste0("../reports/predictions_rfle_scope2-", sys_time, ".csv")
        metrics_filename = paste0("../reports/metrics_rfle_scope2-", sys_time, ".txt")
        write.csv(carbon_df_all_scope2, file=predictions_filename)
        metrics_string = paste("Model Summary results: R^2", testRes$Rsq, " adj R^2: ", testRes$AdjustedRsq, " RMSE: ", testRes$AdjustedRsq)
        capture.output(metrics_string, file=metrics_filename)
        capture.output(varImp(model, scale=T), file=metrics_filename, append=TRUE)
        capture.output(model, file=metrics_filename, append=TRUE)
        print(metrics_string)
    }
    a <- data.frame(A=carbon_df_all_scope2$Scope2Predictions, B=carbon_df_all_scope2$Scope2)
    
}


