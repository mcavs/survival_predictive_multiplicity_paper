source("install_packages.R")
source("task.R")
source("data_split.R")
source("modeling.R")

train_FD001 <- read_csv("train_FD001.csv")

task_FD001  <- make_task()
split_FD001 <- data_split(task_FD001)
model_FD001 <- train_model(task_FD001, split_FD001)

preds <- model_FD001$predict(task = task_FD001, row_ids = split_FD001$test)
