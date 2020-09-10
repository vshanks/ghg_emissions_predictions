library(MASS)
library(caret)
library(dplyr)

# Set directory 
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Creates a data frame with columns for scope 2 energy model creation
# Columns: Scope2, CO2Est = Total energy * CO2 intensity, log of these fields
create_lm_df <- function(energy_company_joined, removeNA=T){
    Used = rep(T, nrow(energy_company_joined))
    # Only use those rows with data for scope 2 emissions
    if(removeNA)
        Used <- !is.na(energy_company_joined$GHG.Scope.2)
    df <- data.frame(Scope2=energy_company_joined$GHG.Scope.2[Used], CO2Est=energy_company_joined$Total.Energy.use.GJ[Used]*energy_company_joined$CO2.Intensity[Used])
    df$Scope2[df$Scope2==0] <- 1e-1
    df <- cbind(df, lnScope2=log(df$Scope2), lnCO2Est=log(df$CO2Est), TotalEnergy=energy_company_joined$Total.Energy.use.GJ[Used], CountryCO2=energy_company_joined$CO2.Intensity[Used])
    df
}

# Load per company energy usage data from file
energy_per_company_2015_file <- "../data/csv/2015_energy_data.csv"
energy_per_company_2015 <- read.csv(energy_per_company_2015_file, header = T, sep = ",", check.names = T, stringsAsFactors = F)
View(energy_per_company_2015)

# Load CO2 per country energy usage
co2_per_energy_2015_file <- "../data/csv/2015_CO2_per_energy_intensity.csv"
co2_per_energy_2015 <- read.csv(co2_per_energy_2015_file, header = T, sep = ",", check.names = T, stringsAsFactors = F, fileEncoding="utf-16")
View(co2_per_energy_2015)

# Carbon 2015
carbon_2015_file <- "../data/csv/2015_carbon_data_modelling.csv"
carbon_2015 <- read.csv(carbon_2015_file, header = T, sep = ",", check.names = T, stringsAsFactors = F)
View(carbon_2015)

# Energy consumption per country and avg co2 intensity of the country
energy_company_joined <- dplyr::left_join(dplyr::inner_join(energy_per_company_2015, carbon_2015, by="ISIN"), co2_per_energy_2015, by=c("Country.headquarters"="Region"))
energy_company_joined$CO2.Intensity[is.na(energy_company_joined$CO2.Intensity)] <- 0.048

# this data frame will be used to build the lm model
df <- create_lm_df(energy_company_joined, removeNA=T)
lm_fit_df<-df[complete.cases(df),]


# Log Transformed ---------------------------------------------------------

# Investigate simple linear regression
lm_model <- lm(lnScope2 ~ lnCO2Est, data = lm_fit_df)
plot(lm_fit_df$lnScope2, lm_fit_df$lnCO2Est); abline(lm_model)

# Plot diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm_model)

# Remove outliers by cooks distance
cutoff <- 4/((nrow(lm_fit_df)-length(lm_model$coefficients)-2))
cooks_lm_outliers <- which(cooks.distance(lm_model) > cutoff)
lm_fit_df_wo <- lm_fit_df[-cooks_lm_outliers,]

# Investigate robust linear regression after removal of outliers
rlm_model_wo <- rlm(lnScope2 ~ lnCO2Est, data=lm_fit_df_wo, method="MM", psi=psi.bisquare, init="lts", maxit=200, acc=1e-9)
# summary(rlm_model_wo)
plot(lm_fit_df_wo$lnScope2, lm_fit_df_wo$lnCO2Est); abline(rlm_model_wo)
lm_model_wo <- lm(lnScope2 ~ lnCO2Est, data = lm_fit_df_wo)

# Plot lm and rlm to compare
plot(lm_fit_df_wo$lnScope2 ~ lm_fit_df_wo$lnCO2Est)
abline(lm_model)
abline(lm_model_wo, col="red")
abline(rlm_model_wo, col="blue")
legend("topleft", inset=0.05, bty="n",
       legend = c("lm", "lm Out", "rlm Out"),
       lty = c(1, 1, 1),      # 1 = "solid" ; 2 = "dashed"
       col = c("black", "red", "blue")
)


# BOXCOX Transform --------------------------------------------------------
bc<-boxcox(lm_fit_df_wo$lnScope2~lm_fit_df_wo$lnCO2Est)
lambda<-bc$x[which.max(bc$y)]
boxcoxTransform <- function(y,lambda){((y^lambda) - 1) / lambda}
tukeyTransform <- function(y,lambda){y^lambda}
lm_fit_df_wo$lnbcScope2 <- boxcoxTransform(lm_fit_df_wo$lnScope2, lambda)
lm_model_bc_wo <- lm(lnbcScope2 ~ lnCO2Est, data=lm_fit_df_wo)
# lm_model_tu_wo <- lm(tukeyTransform(lm_fit_df_wo$lnScope2, lambda) ~ lm_fit_df_wo$lnCO2Est)
rlm_model_bc_wo <- rlm(lnbcScope2 ~ lnCO2Est, data=lm_fit_df_wo, method="MM", psi=psi.bisquare, init="lts", maxit=200, acc=1e-9)
# rlm_model_tu_wo <- rlm(tukeyTransform(lm_fit_df_wo$lnScope2, lambda) ~ lm_fit_df_wo$lnCO2Est,  method="MM", psi=psi.bisquare, init="lts", maxit=200, acc=1e-9)

plot(boxcoxTransform(lm_fit_df_wo$lnScope2, lambda)~lm_fit_df_wo$lnCO2Est)
abline(lm_model_bc_wo, col="blue")
abline(rlm_model_bc_wo, col="red")
legend("topleft", inset=0.05, bty="n", 
       legend = c("lm BC", "rlm BC"),
       lty = c(1,1),      # 1 = "solid" ; 2 = "dashed"
       col = c("blue", "red"))

# plot(tukeyTransform(lm_fit_df_wo$lnScope2, lambda)~lm_fit_df_wo$lnCO2Est)
# abline(lm_model_tu_wo, col="blue")
# abline(rlm_model_tu_wo, col="red")
# legend("topleft", inset=0.05, bty="n", 
#        legend = c("lm TU", "rlm TU"),
#        lty = c(1,1),      # 1 = "solid" ; 2 = "dashed"
#        col = c("blue", "red"))


# Use rlm models to predict scope 2 carbon data
rlm_predict_df <- create_lm_df(energy_company_joined, removeNA=F)
summary(rlm_predict_df)
scope2_pred_1 <- predict(rlm_model_wo, rlm_predict_df)
# Transform by boxcox before predict
rlm_predict_df$lnbcScope2 <- boxcoxTransform(rlm_predict_df$lnScope2, lambda)
scope2_pred_2 <- predict(rlm_model_bc_wo, rlm_predict_df)

# Add it to my data df and write to file
# energy_company_joined$lnScope2Predictions <- scope2_predictions
energy_company_joined$Scope2Predictions_withParams <- exp(scope2_pred_1)
energy_company_joined$Scope2Predictions_BC <- exp((scope2_pred_2*lambda + 1)^(1/lambda))
cc<-read.csv("../reports/predictions_rlm_scope2.csv", header=T)
energy_company_joined$Scope2Predictions <- cc$Scope2_Predicted
# Write predictions to file
# write.csv(energy_company_joined %>% dplyr::select(ISIN, Country=Headquarterscountry, Total_Energy_Usage=Total.Energy.use.GJ, CO2_Intensity=CO2.Intensity, Scope2_Actual=GHG.Scope.2, Scope2_Predicted=Scope2Predictions, Scope2_Predicted_withparams=Scope2Predictions_withParams, Scope2_Predicted_BC=Scope2Predictions_BC), file="../reports/predictions_final_scope2.csv")


