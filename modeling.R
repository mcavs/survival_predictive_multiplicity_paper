train_model <- function(task, split, hyp = NULL) {
  
  if(is.null(hyp)){
    rfsrc_model = ppl(
      "responsecompositor",
      learner = lrn("surv.rfsrc"),
      method = "rmst",
      overwrite = TRUE,
      graph_learner = TRUE
    )
  } else{
    rfsrc_model = ppl(
      "responsecompositor",
      learner = lrn("surv.rfsrc",
                    ntree          = hyp$ntree,                   
                    mtry           = hyp$mtry,                    
                    nodesize       = hyp$nodesize,                
                    nodedepth      = hyp$nodedepth,                 
                    splitrule      = as.character(hyp$splitrule),   
                    nsplit         = hyp$nsplit),
      method = "rmst",
      overwrite = TRUE,
      graph_learner = TRUE
    )
  }
  
  #rfsrc_model <- lrn("surv.rfsrc", id = "rfsrc")
  rfsrc_model$train(task, row_ids = split$train)

  return(rfsrc_model = rfsrc_model)
}
