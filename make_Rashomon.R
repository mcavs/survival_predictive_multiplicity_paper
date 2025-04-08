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
  cind  <- NULL 
  ibri  <- NULL
  resp  <- NULL
  cran  <- NULL
  for(i in 1:dim(grid)[1]){
    model      <- train_model(task, split, hyp = grid[i,]) 
    pr         <- model$predict(task, split$test)
    cind[i]    <- pr$score(msr("surv.cindex"))
    dcal[i]    <- pr$score(msr("surv.dcalib"))
    ibri[i]    <- pr$score(msr("surv.graf"))
    resp[[i]]  <- pr$response
    cran[[i]]  <- pr$crank
    print(i)
  }
}


hyps <- list(ntree     = seq(100, 2000, 200),                   
             mtry      = seq(1, 10, 2),                    
             nodesize  = seq(5, 100, 20),                
             nodedepth = seq(5, 100, 20),                 
             splitrule = c("logrank", "logrankscore", "bs.gradient"),   
             nsplit    = seq(5, 15, 2))