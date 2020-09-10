# RF predictions
pred_rfle <- read.csv("../reports/predictions_rfle_scope1.csv", header=T, stringsAsFactors = F)
carbon_2015 <- read.csv("../data/csv/2015_carbon_data_modelling.csv", header=T, stringsAsFactors = F) 
nrow(carbon_2015)
library(dplyr)
pred_rfle_with_country <- pred_rfle %>% dplyr::inner_join(carbon_2015, by="ISIN") %>%
    select_(.dots=c(colnames(pred_rfle), "Headquarterscountry"))
unique(pred_rfle_with_country$Headquarterscountry)
european <- c("Luxembourg", "Italy", "Germany", "Spain", "United Kingdom", "Sweden", "Netherlands", "Switzerland", "Belgium", "France", "Ireland; Republic of", "Norway", "Austria", "Denmark", "Finland")
pred_rfle_european <- pred_rfle_with_country %>% dplyr::filter(Headquarterscountry %in% european)
val_ratios <- pred_rfle_european$Scope1Predictions/ pred_rfle_european$Scope1
median(val_ratios, na.rm = T)
mean(val_ratios, na.rm = T)
summary(val_ratios)
other_val_ratios <- pred_rfle$Scope1Predictions/pred_rfle$Scope1
summary(other_val_ratios)

# RBICS separation --------------------------------------------------------
rbics_2015 <- read.csv("../data/csv/2015_RBICS_ISIN.csv", header=T, stringsAsFactors = F)
pred_rbics_2015 <- dplyr::inner_join(dplyr::select(pred_rfle, ISIN, Scope1Predictions), rbics_2015, by="ISIN")
colnames(pred_rbics_2015)[1:10]
unique(pred_rbics_2015$Scope1Predictions * (as.numeric(pred_rbics_2015$Direct.Marketing.Services))/100)

rbicsVal <- function(scopeVal, rbicsPer) {
    scopeVal * as.numeric(rbicsPer)/100 }
for(i in 4:ncol(pred_rbics_2015)){
a <-rbicsVal(pred_rbics_2015$Scope1Predictions, pred_rbics_2015[,i])
pred_rbics_2015[,i] <- a
}
