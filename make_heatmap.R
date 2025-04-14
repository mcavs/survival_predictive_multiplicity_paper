make_heatmap <- function(crank, perf, metric, rash_par_length, thres_length){
  rash_par_values <- seq(0.01, 1, length.out = rash_par_length)  
  thres_values    <- seq(0, 0.5, length.out = thres_length)   
  
  pm_values <- matrix(NA, nrow = length(thres_values), ncol = length(rash_par_values))
  
  ifelse(metric == "ambiguity",   metric_ind <- 1, 
         ifelse(metric == "discrepancy", metric_ind <- 2, metric_ind <- 3))
  
  for (i in seq_along(rash_par_values)) {
    for (j in seq_along(thres_values)) {
      result <- calculate_pms_on_crank(crank, perf, rash_par_values[i], thres_values[j])
      result <- as.data.frame(result)
      pm_values[j, i] <- result[,metric_ind]
    }
    print(i)
  }
  
  rownames(pm_values) <- thres_values
  colnames(pm_values) <- rash_par_values
  
  heatmap_data           <- expand.grid(thres    = thres_values, 
                                        rash_par = rash_par_values)
  heatmap_data$pm_values <- as.vector(pm_values)
  
  library(ggplot2)
  library(scales)
  
  plot <- ggplot(heatmap_data, aes(x = rash_par, 
                                   y = thres, 
                                   fill = pm_values)) +
    geom_tile() +
    scale_fill_gradient(low    = "white", 
                        high   = "red",
                        breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    labs(x     = "Rashomon Parameter", 
         y     = "Threshold", 
         title = paste0("Heatmap of ", metric)) +
    theme_minimal() +
    theme(axis.text.x      = element_text(hjust = 1),
          legend.position  = "bottom",
          legend.title     = element_blank(),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(0.1, 1, by = 0.1)) + 
    scale_y_continuous(breaks = seq(0, 0.5, by = 0.1))
  
  return(list(plot = plot, data = heatmap_data))
}

amb_FD001 <- make_heatmap(crank_FD001, perf_FD001, "ambiguity", 100, 100)
amb_FD002 <- make_heatmap(crank_FD002, perf_FD002, "ambiguity", 100, 100)
amb_FD003 <- make_heatmap(crank_FD003, perf_FD003, "ambiguity", 100, 100)
amb_FD004 <- make_heatmap(crank_FD004, perf_FD004, "ambiguity", 100, 100)

dis_FD001 <- make_heatmap(crank_FD001, perf_FD001, "discrepancy", 100, 100)
dis_FD002 <- make_heatmap(crank_FD002, perf_FD002, "discrepancy", 100, 100)
dis_FD003 <- make_heatmap(crank_FD003, perf_FD003, "discrepancy", 100, 100)
dis_FD004 <- make_heatmap(crank_FD004, perf_FD004, "discrepancy", 100, 100)

obs_FD001 <- make_heatmap(crank_FD001, perf_FD001, "obscurity", 100, 100)
obs_FD002 <- make_heatmap(crank_FD002, perf_FD002, "obscurity", 100, 100)
obs_FD003 <- make_heatmap(crank_FD003, perf_FD003, "obscurity", 100, 100)
obs_FD004 <- make_heatmap(crank_FD004, perf_FD004, "obscurity", 100, 100)

heatmap_data <- rbind(
  data.frame(data = "FD001", metric = "ambiguity", amb_FD001$data),
  data.frame(data = "FD002", metric = "ambiguity", amb_FD002$data),
  data.frame(data = "FD003", metric = "ambiguity", amb_FD003$data),
  data.frame(data = "FD004", metric = "ambiguity", amb_FD004$data),
  
  data.frame(data = "FD001", metric = "discrepancy", dis_FD001$data),
  data.frame(data = "FD002", metric = "discrepancy", dis_FD002$data),
  data.frame(data = "FD003", metric = "discrepancy", dis_FD003$data),
  data.frame(data = "FD004", metric = "discrepancy", dis_FD004$data),
  
  data.frame(data = "FD001", metric = "obscurity", obs_FD001$data),
  data.frame(data = "FD002", metric = "obscurity", obs_FD002$data),
  data.frame(data = "FD003", metric = "obscurity", obs_FD003$data),
  data.frame(data = "FD004", metric = "obscurity", obs_FD004$data))

ggplot(heatmap_data |> 
         mutate(across(everything(), ~ifelse(.x == -Inf, 0, .x))) |> 
         mutate(across(everything(), ~replace_na(.x, 0))) |>
         filter(rash_par <= 0.50), 
       aes(x    = rash_par, 
           y    = thres, 
           fill = pm_values)) +
  geom_tile() +
  scale_fill_gradient(low    = "white", 
                      high   = "red",
                      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                      labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
  labs(x     = expression("Rashomon Parameter"~(epsilon)), 
       y     = expression("Threshold"~(delta))) +
  theme_bw() +
  theme(text             = element_text(size   = 15),
        legend.position  = "bottom",
        legend.title     = element_blank(),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),
        legend.key.width = unit(2, "cm"),
        strip.text.x     = element_text(face = "italic"), 
        strip.text.y     = element_text(family = "Courier")) + 
  scale_x_continuous(
    breaks = c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5), 
    labels = c("0.01", "0.1", "0.2", "0.3", "0.4", "0.5")) + 
  scale_y_continuous(
    breaks = seq(0, 0.5, by = 0.1), 
    labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5")) +
  facet_grid(data ~ metric)
