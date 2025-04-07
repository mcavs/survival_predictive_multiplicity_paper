install.packages("readr")
install.packages("dplyr")
install.packages("remotes"); library(remotes)
remotes::install_github("mlr-org/mlr3proba", force = TRUE)
remotes::install_github("mlr-org/mlr3extralearners", force = TRUE)
remotes::install_github("binderh/CoxBoost", force = TRUE)
install.packages("ggplot2")
install.packages("survAUC")

library(readr)
library(dplyr)
library(mlr3proba)
library(ggplot2)
library(survAUC)
library(mlr3extralearners)
library(CoxBoost)
library(mlr3pipelines)

install_learners(c(
  "surv.ranger", "surv.cv_glmnet", "surv.coxph", "surv.penalized", 
  "surv.aorsf", "surv.coxboost", "surv.xgboost.aft", "surv.xgboost.cox", 
  "surv.flexible", "surv.mboost", "surv.nelson", "surv.parametric", 
  "surv.pchazard", "surv.priority_lasso", "surv.gamboost", 
  "surv.gbm", "surv.glmboost", "surv.glmnet", "surv.rfsrc", 
  "surv.ctree", "surv.cforest", "surv.loghaz", "surv.svm"
))