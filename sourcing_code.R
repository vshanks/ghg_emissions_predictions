# Source this file FIRST
# POINT OF ENTRY
# The following directory structure is assumed:-
# code------
#          | sourcing_code.R
#          | helper_functions.R
#          | data_experiments.R
#          | model_comparison.R
#          | PreliminaryCalsFunction.R
#          | scope2_by_energy_intensity.R
# data/csv--
#          | 2015_carbon_data_modelling.csv
#          | 2015_carbondataformodel_v2.csv
#          | tech_availability_score.csv
# reports---
#          | results will be written out to this directory

library(dplyr)
library(stringr)
library(caret)
library(here)

this.dir <- here()
setwd(this.dir)
source("./PreliminaryCalcsFunction.R")
source("./helper_functions.R")
