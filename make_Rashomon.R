make_Rashomon <- function(task, split, hyps){
  grid <- expand.grid(hyps$ntree,                   
                      hyps$mtry,                    
                      hyps$nodesize,                
                      hyps$nodedepth,                 
                      hyps$splitrule,   
                      hyps$nsplit)
  colnames(grid) <- c("ntree", "mtry", "nodesize",
                      "nodedepth", "splitrule", "nsplit")
  
  model <- NULL
  for(i in 1:2000){
    model[[i]] <- train_model(task, split, hyp = grid[i,])  
    print(i)
  }
  
  
  
}


hyps <- list(ntree     = seq(100, 2000, 200),                   
             mtry      = seq(1, 10, 2),                    
             nodesize  = seq(5, 100, 20),                
             nodedepth = seq(5, 100, 20),                 
             splitrule = c("logrank", "logrankscore", "bs.gradient"),   
             nsplit    = seq(5, 15, 2))