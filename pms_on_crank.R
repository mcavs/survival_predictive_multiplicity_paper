calculate_pms_on_crank <- function(crank, perf, rash_par, thres){
  ref_id          <- which.max(perf$cindex)
  rash_ids        <- which(perf$cindex > (perf$cindex[ref_id] - rash_par))
  rash_set_cranks <- crank[rash_ids]
  ref_id          <- which.max(perf$cindex[rash_ids])
  diffs           <- lapply(rash_set_cranks[-ref_id], 
                            function(x) abs(rash_set_cranks[[ref_id]] - x))
  flags           <- lapply(diffs, function(x) (x / 100) >= thres)
  sum_flags       <- Reduce(`+`, flags)
  amb             <- mean(sum_flags > 0)
  dis             <- max(unlist(lapply(flags, mean)))
  obs             <- mean(sum_flags / length(diffs))
  return(list(ambiguity          = amb,
              discrepancy        = dis,
              obscurity          = obs,
              model_set_size     = length(crank),
              rashomon_set_size  = length(rash_set_cranks),
              rashomon_parameter = rash_par,
              threshold          = thres,
              ref_model_perf     = max(perf$cindex)))
}

perf_FD001  <- readRDS("~/Desktop/SurvPM/perf_FD001.rds")
perf_FD002  <- readRDS("~/Desktop/SurvPM/perf_FD002.rds")
perf_FD003  <- readRDS("~/Desktop/SurvPM/perf_FD003.rds")
perf_FD004  <- readRDS("~/Desktop/SurvPM/perf_FD004.rds")

crank_FD001 <- readRDS("~/Desktop/SurvPM/crank_FD001.rds")
crank_FD002 <- readRDS("~/Desktop/SurvPM/crank_FD002.rds")
crank_FD003 <- readRDS("~/Desktop/SurvPM/crank_FD003.rds")
crank_FD004 <- readRDS("~/Desktop/SurvPM/crank_FD004.rds")

pms_on_crank_FD001 <- calculate_pms_on_crank(crank_FD001, perf_FD001, 0.03, 0.05)
pms_on_crank_FD002 <- calculate_pms_on_crank(crank_FD002, perf_FD002, 0.03, 0.05)
pms_on_crank_FD003 <- calculate_pms_on_crank(crank_FD003, perf_FD003, 0.03, 0.05)
pms_on_crank_FD004 <- calculate_pms_on_crank(crank_FD004, perf_FD004, 0.03, 0.05)

rbind(pms_on_crank_FD001,
      pms_on_crank_FD002,
      pms_on_crank_FD003,
      pms_on_crank_FD004)
