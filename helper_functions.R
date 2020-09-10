addSubIndustryCodes <- function(carbon_data, subIndustryData){
    subIndustryData <- rename_(subIndustryData, "RBICS.subsector.CodeName"="RBICS.subsector.Code")
    subIndustryData <- rename_(subIndustryData, "RBICS.subsector.Code"="RBICS.subsector")
    # subIndustryData <- rename_(subIndustryData, "RBICS.subsector"="RBICS.subsector.CodeName")
    carbon_data_withsubInd <- dplyr::inner_join(carbon_data, subIndustryData)         
    carbon_data_withsubInd <- carbon_data_withsubInd %>% 
        select_(.dots=c(colnames(carbon_data), "RBICS.subindustry")) %>%
        rename_("RBICS.subindustry.Code"="RBICS.subindustry")
    carbon_data_withsubInd
}

addTechAvailabilityScore <- function(carbon_data, techavail){
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Korea, Rep.", "Korea; Republic (S. Korea)", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Hong Kong SAR", "Hong Kong", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Taiwan, China", "Taiwan", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("United States", "United States of America", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Ireland", "Ireland; Republic of", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Russian Federation", "Russia", x)}))
    techavail <- data.frame(lapply(techavail, function(x) {gsub("Serbia", "Republic of Serbia", x)}))
    techavail$LatestTechnologyAvailabilityScore <- as.numeric(as.character(techavail$LatestTechnologyAvailabilityScore))
    
    carbon_2015_withtech <- dplyr::inner_join(carbon_data, techavail, by=c("Headquarterscountry"="Country"))
    missing_rows <- dplyr::anti_join(carbon_data, carbon_2015_withtech)
    carbon_2015_withtech2 <- rbind(carbon_2015_withtech, cbind(missing_rows, LatestTechnologyAvailabilityScore=mean(techavail$LatestTechnologyAvailabilityScore)))
    carbon_2015_withtech2
}

prepData <- function(carbon_data, scope=1, imputeBeforeScopeNA=T, removeScopeNa=T){
    carbon_df <- data.frame(matrix(nrow=nrow(carbon_data), ncol=ncol(carbon_data)))
    # Select some for analysis
    carbon_df$ISIN <- carbon_data$ISIN
    carbon_df$Subsector <- as.character(carbon_data$RBICS.subsector.Code)
    carbon_df$SubIndustry <- as.character(carbon_data$RBICS.subindustry)
    carbon_df$TotalGHG <- carbon_data$Total.GHG.emssion..1..2.
    carbon_df$Scope1 <- carbon_data$GHG.Scope.1
    carbon_df$Scope2 <- carbon_data$GHG.Scope.2
    carbon_df$Revenue <- as.numeric(as.character(carbon_data$PPP..Revenue))
    carbon_df$Employees <- as.numeric(as.character(carbon_data$Number.of.employees))
    carbon_df$FixedAssets <-  as.numeric(as.character(carbon_data$Gross.Fixed.Assets..PPP...millions.))
    carbon_df$IntangibleAssets <- as.numeric(as.character(carbon_data$Disclosed.Intangible.Assets..PPP...millions.))
    carbon_df$LatestTechnologyAvailabilityScore <- as.numeric(as.character(carbon_data$LatestTechnologyAvailabilityScore))
    
    if(scope==1)
        Used <- !is.na(carbon_df$Scope1)
    else
        Used <- !is.na(carbon_df$Scope2)
    
    # Impute missing values and Remove data without any scope data
    if(imputeBeforeScopeNA){
        # Impute missing values
        carbon_df$Scope1[carbon_df$Scope1==0] <- median(carbon_df$Scope1, na.rm = T)
        carbon_df$Scope2[carbon_df$Scope2==0] <- median(carbon_df$Scope2, na.rm = T)
        carbon_df$Revenue[is.na(carbon_df$Revenue)] <- median(carbon_df$Revenue, na.rm = T)
        carbon_df$Revenue[carbon_df$Revenue==0] <- median(carbon_df$Revenue, na.rm = T)
        carbon_df$Employees[is.na(carbon_df$Employees)] <- median(carbon_df$Employees, na.rm = T)
        carbon_df$Employees[carbon_df$Employees==0] <- median(carbon_df$Employees, na.rm = T)
        carbon_df$FixedAssets[carbon_df$FixedAssets==0] <- median(carbon_df$FixedAssets, na.rm = T)
        carbon_df$FixedAssets[is.na(carbon_df$FixedAssets)] <- median(carbon_df$FixedAssets, na.rm = T)
        carbon_df$IntangibleAssets[is.na(carbon_df$IntangibleAssets)] <- median(carbon_df$IntangibleAssets, na.rm = T)
        carbon_df$IntangibleAssets[carbon_df$IntangibleAssets==0] <- median(carbon_df$IntangibleAssets, na.rm = T)
        # Remove mising scope data rows
        if(removeScopeNa)
            carbon_df <- carbon_df[Used,]
    }else{
        # Remove mising scope data rows
        if(removeScopeNa)
            carbon_df <- carbon_df[Used,]
        # Impute missing values
        carbon_df$Scope1[carbon_df$Scope1==0] <- median(carbon_df$Scope1, na.rm = T)
        carbon_df$Scope2[carbon_df$Scope2==0] <- median(carbon_df$Scope2, na.rm = T)
        carbon_df$Revenue[is.na(carbon_df$Revenue)] <- median(carbon_df$Revenue, na.rm = T)
        carbon_df$Revenue[carbon_df$Revenue==0] <- median(carbon_df$Revenue, na.rm = T)
        carbon_df$Employees[is.na(carbon_df$Employees)] <- median(carbon_df$Employees, na.rm = T)
        carbon_df$Employees[carbon_df$Employees==0] <- median(carbon_df$Employees, na.rm = T)
        carbon_df$FixedAssets[carbon_df$FixedAssets==0] <- median(carbon_df$FixedAssets, na.rm = T)
        carbon_df$FixedAssets[is.na(carbon_df$FixedAssets)] <- median(carbon_df$FixedAssets, na.rm = T)
        carbon_df$IntangibleAssets[is.na(carbon_df$IntangibleAssets)] <- median(carbon_df$IntangibleAssets, na.rm = T)
        carbon_df$IntangibleAssets[carbon_df$IntangibleAssets==0] <- median(carbon_df$IntangibleAssets, na.rm = T)
    }
    
    # Normalize by revenue
    carbon_df$TotalGHGByRevenue <- carbon_df$TotalGHG / carbon_df$Revenue
    carbon_df$Scope1ByRevenue <- carbon_df$Scope1 / carbon_df$Revenue
    carbon_df$Scope2ByRevenue <- carbon_df$Scope2 / carbon_df$Revenue
    carbon_df$EmployeesByRevenue <- carbon_df$Employees / carbon_df$Revenue
    carbon_df$AssetsByRevenue <- carbon_df$FixedAssets / carbon_df$Revenue
    carbon_df$IntangiblesByRevenue <- carbon_df$IntangibleAssets / carbon_df$Revenue
    
    # Normalize by employee
    carbon_df$TotalGHGByEmployee <- carbon_df$TotalGHG / carbon_df$Employees
    carbon_df$Scope1ByEmployee <- carbon_df$Scope1 / carbon_df$Employees
    carbon_df$Scope2ByEmployee <- carbon_df$Scope2 / carbon_df$Employees
    carbon_df$RevenueByEmployee <- carbon_df$Revenue / carbon_df$Employees
    carbon_df$AssetsByEmployee <- carbon_df$FixedAssets / carbon_df$Employees
    carbon_df$IntangiblesByEmployee <- carbon_df$IntangibleAssets / carbon_df$Employees
    
    # Now look at subsectors
    GHGbySubsectorsplit <- split(log(carbon_df$Scope1ByRevenue), carbon_df$Subsector)
    MedSectorGHGsplit <- sapply(GHGbySubsectorsplit,median,na.rm=TRUE)
    MedSectorGHGsplit[is.na(MedSectorGHGsplit)] <- min(MedSectorGHGsplit,na.rm=TRUE)
    RankMeds <- rank(MedSectorGHGsplit,ties.method = "random")
    carbon_df$SubsectorRank <- unsplit(RankMeds,carbon_df$Subsector) 
    
    # Now look at subindustries
    GHGbySubindsplit <- split(log(carbon_df$Scope1ByRevenue), carbon_df$SubIndustry)
    MedindGHGsplit <- sapply(GHGbySubindsplit,median,na.rm=TRUE)
    MedindGHGsplit[is.na(MedindGHGsplit)] <- min(MedindGHGsplit,na.rm=TRUE)
    RankMeds <- rank(MedindGHGsplit,ties.method = "random")
    carbon_df$SubindustryRank <- unsplit(RankMeds,carbon_df$SubIndustry) 
    
    # Log of variables
    carbon_df$LUTotGHGByRev <- log(carbon_df$TotalGHGByRevenue)
    carbon_df$LUScope1ByRev <- log(carbon_df$Scope1ByRevenue)
    carbon_df$LUScope2ByRev <- log(carbon_df$Scope2ByRevenue)
    carbon_df$LUEmpByRev <- log(carbon_df$EmployeesByRevenue)
    carbon_df$LUAssetsByRev <- log(carbon_df$AssetsByRevenue)
    carbon_df$LUIntangByRev <- log(carbon_df$IntangiblesByRevenue)
    carbon_df$LUTotGHGByEmp <- log(carbon_df$TotalGHGByEmployee)
    carbon_df$LUScope1ByEmp <- log(carbon_df$Scope1ByEmployee)
    carbon_df$LUScope2ByEmp <- log(carbon_df$Scope2ByEmployee)
    carbon_df$LURevByEmp <- log(carbon_df$RevenueByEmployee)
    carbon_df$LUAssetsByEmp <- log(carbon_df$AssetsByEmployee)
    carbon_df$LUIntangByEmp <- log(carbon_df$IntangiblesByEmployee)
    
    # Remove columns that are entirely empty (formed during init)
    carbon_df <- carbon_df[, colSums(!is.na(carbon_df)) != 0]
    carbon_df
    # carbon_df_nonas <- carbon_df[complete.cases(carbon_df),]
}

# Helper functions to run models for data normalized by different variables
chooseColumnsForModelByRevenue <- function(){
    c("EmployeesByRevenue", "AssetsByRevenue", "IntangiblesByRevenue", "SubsectorRank", "SubindustryRank", "LatestTechnologyAvailabilityScore")
}
runScope1ModelByRevenue <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope1, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByRevenue())
    trainY <- carbon_df[trainIdx, "Scope1ByRevenue"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByRevenue())
    testY <- carbon_df[-trainIdx, "Scope1ByRevenue"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}

chooseColumnsForModelByLogRevenue <- function(){
    c("LUEmpByRev", "LUAssetsByRev", "LUIntangByRev", "SubsectorRank", "SubindustryRank", "LatestTechnologyAvailabilityScore")
}
runScope1ModelByLogRevenue <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope1, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByLogRevenue())
    trainY <- carbon_df[trainIdx, "LUScope1ByRev"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByLogRevenue())
    testY <- carbon_df[-trainIdx, "LUScope1ByRev"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}
runScope2ModelByLogRevenue <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope2, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByLogRevenue())
    trainY <- carbon_df[trainIdx, "LUScope2ByRev"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByLogRevenue())
    testY <- carbon_df[-trainIdx, "LUScope2ByRev"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}


chooseColumnsForModelByEmployee <- function(){
    c("RevenueByEmployee", "AssetsByEmployee", "IntangiblesByEmployee", "SubsectorRank", "SubindustryRank", "LatestTechnologyAvailabilityScore")
}
runScope1ModelByEmployee <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope1, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByEmployee())
    trainY <- carbon_df[trainIdx, "Scope1ByEmployee"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByEmployee())
    testY <- carbon_df[-trainIdx, "Scope1ByEmployee"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}

chooseColumnsForModelByLogEmployee <- function(){
    c("LURevByEmp", "LUAssetsByEmp", "LUIntangByEmp", "SubsectorRank", "SubindustryRank", "LatestTechnologyAvailabilityScore")
}
runScope1ModelByLogEmployee <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope1, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByLogEmployee())
    trainY <- carbon_df[trainIdx, "LUScope1ByEmp"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByLogEmployee())
    testY <- carbon_df[-trainIdx, "LUScope1ByEmp"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}
runScope2ModelByLogEmployee <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df$Scope2, p=0.7, list=F)
    trainData<- learnWithColumns(carbon_df[trainIdx,], chooseColumnsForModelByLogEmployee())
    trainY <- carbon_df[trainIdx, "LUScope2ByEmp"]
    
    testData <- learnWithColumns(carbon_df[-trainIdx,], chooseColumnsForModelByLogEmployee())
    testY <- carbon_df[-trainIdx, "LUScope2ByEmp"]
    
    sapply(trainData, summary)
    trainingTestingList <- list("trainData"=trainData, "testData"=testData, "trainY"=trainY, "testY"=testY)
    trainingTestingList
}

runallModels <- function(carbon_df){
    trainIdx <- createDataPartition(carbon_df_nonas$Scope1, p=0.7, list=F)
    # trainData<- learnWithColumns(carbon_df_nonas[trainIdx,], c("Subsector", "SubIndustry", "Revenue",
    #                         "Employees","FixedAssets", "IntangibleAssets"))
    # trainY <- carbon_df_nonas[trainIdx, "TotalGHG"]
    
    trainData<- learnWithColumns(carbon_df_nonas[trainIdx,], c("Subsector", "SubIndustry",
    "EmployeesByRevenue","AssetsByRevenue", "IntangiblesByRevenue"))
    # trainY <- carbon_df_nonas[trainIdx, "TotalGHGbyRevenue"]
    trainY <- carbon_df_nonas[trainIdx, "Scope1"]/carbon_df_nonas[trainIdx, "Revenue"]
    
    # trainData<- learnWithColumns(carbon_df_nonas[trainIdx,], c("Subsector", "SubIndustry", "Revenue",
    #                                                            "LUEmpByRev","LUAssetsByRev", "LUIntangByRev"))
    # trainY <- carbon_df_nonas[trainIdx, "LUTotGHGbyRev"]
    
    # testData <- learnWithColumns(carbon_df_nonas[-trainIdx,], c("Subsector", "SubIndustry", "Revenue",
    # "Employees","FixedAssets", "IntangibleAssets"))
    # testY <- carbon_df_nonas[-trainIdx, "TotalGHG"]
    
    testData <- learnWithColumns(carbon_df_nonas[-trainIdx,], c("Subsector", "SubIndustry",
                                                                "EmployeesByRevenue","AssetsByRevenue", "IntangiblesByRevenue"))
    # testY <- carbon_df_nonas[-trainIdx, "TotalGHGbyRevenue"]
    testY <- carbon_df_nonas[-trainIdx, "Scope1"]/carbon_df_nonas[-trainIdx, "Revenue"]
    
    # testData <- learnWithColumns(carbon_df_nonas[-trainIdx,], c("Subsector", "SubIndustry", "Revenue",
                                                                # "LUEmpByRev","LUAssetsByRev", "LUIntangByRev"))
    # testY <- carbon_df_nonas[-trainIdx, "LUTotGHGbyRev"]
    
    sapply(trainData, summary)
    
    modelFunctions <- c("svmLinear", "ctree", "rpart2", "gamboost", "randomForest")
    testRes <- list()
    for(mdlFunc in modelFunctions){
        functionName <- paste0("trainWith_", mdlFunc)
        trainedModel <- do.call(functionName, args=list(trainData, trainY))
        testRes[mdlFunc] <- list(predictAndReport(trainedModel, testData, testY))
    }
    iter <- function(x) testRes[[x]]$Rsq
    for(i in 1:length(modelFunctions)){
        cat(paste0(modelFunctions[i], ":", round(iter(i),4), sep = "\n"))
    }
}

trainWith_randomForest <- function(trainData, trainY){
    set.seed(123)
    ctrl <- trainControl("oob")
    Grid <- expand.grid(mtry = seq(4,16,4))
    fit.rf <- train(x=trainData, y=trainY , method='rf', trControl=ctrl,
                    tuneGrid=Grid,metric='RMSE', importance=T)
}

trainWith_gamboost <- function(trainData, trainY){
    set.seed(123)
    ctrl <- trainControl(method = 'cv', number = 6, summaryFunction=defaultSummary)
    Grid <- expand.grid(.mstop=seq(100,1000,100),.prune=c(5))
    fit.gamboost <- train(x=trainData, y=trainY, method = 'gamboost', 
                     trControl=ctrl,tuneGrid=Grid,
                     metric='RSquared')
}

trainWith_rpart2 <- function(trainData, trainY){
    set.seed(123)
    ctrl <- trainControl(method = 'cv', number=6)
    # Grid <- expand.grid(cp=seq(0, 0.05, 0.005))
    # fit.rpartCV <- train(x=trainData, y=trainY, method = 'rpart', 
    #                      trControl=ctrl, metric='RMSE',
    #                      maximize=FALSE, tuneGrid = Grid)
    Grid<-expand.grid(.maxdepth=seq(5,20,5))
    fit.rpart2CV <- train(x=trainData, y=trainY, method = 'rpart2',
                          trControl=ctrl, metric = 'RMSE', maximize=FALSE,
                          tuneGrid=Grid)
    # plot(fit.rpart2CV)
}

trainWith_ctree <- function(trainData, trainY){
    fit.ctree <- train(x=trainData, y=trainY, method='ctree',
                       tuneGrid=expand.grid(mincriterion=0.95))
    fit.ctree
}

trainWith_svmLinear <- function(trainData, trainY){
    ctrl <- trainControl(method="repeatedcv", repeats=5, savePredictions=T)
    svm.tune <- train(x=trainData, y=trainY, method='svmLinear2', 
                      tuneGrid = expand.grid(cost=3**(-5:5)), trControl=ctrl, 
                      preProc=c("center", "scale"),
                      metric="RMSE")
    # plot(svm.tune$pred$pred~svm.tune$pred$obs, ylab = 'predicted', xlab = 'observed')
    # abline(0,1, col=2)
    svm.tune
}

trainWith_svmRadial <- function(trainData, trainY){
    ctrl <- trainControl(method="repeatedcv", repeats=5, savePredictions=T)
    svm.tune <- train(x=trainData, y=trainY, method='svmRadial', 
                      tuneLength = 9, trControl=ctrl, 
                      preProc=c("center", "scale"))
    svm.tune <- train(x=trainData, y=trainY, method='svmRadial', 
                      tuneGrid = expand.grid(sigma = c(.01, .51, 1.0),
                                             C = c(2, 16, 22)), 
                      trControl=ctrl, preProc=c("center", "scale"))
    # plot(svm.tune$pred$pred~svm.tune$pred$obs, ylab = 'predicted', xlab = 'observed')
    # abline(0,1, col=2)
    svm.tune
}

predictAndReport <- function(trainedModel, testData, testY){
    testYDash <- predict(trainedModel, testData)
    # trainYDash <- predict(trainedModel, trainData)
    modelvalues <- data.frame(obs=testY, pred=testYDash)
    # modelvalues2 <- data.frame(obs=trainY, pred=trainYDash)
    # a <- rbind(cbind(trainData, Pred=trainYDash, Original=trainY), cbind(testData, Pred=testYDash, Original=testY))
    # write.csv(a, file="./../reports/Nov09_2017_init_run_scope1.csv")
    p = length(trainedModel$finalModel$xNames)+1
    n = nrow(modelvalues)
    rsquared = defaultSummary(modelvalues)["Rsquared"]
    adjusted_rsquared = 1 - ((n-1)*(1-rsquared))/(n-p)
    list("model"=trainedModel, "Rsq"=rsquared, "RMSE"=defaultSummary(modelvalues)["RMSE"], "AdjustedRsq"=adjusted_rsquared)
}

learnWithColumns <- function(df, colNames){
    trainData <- df %>% 
        select_(.dots=colNames)
    trainData
}
